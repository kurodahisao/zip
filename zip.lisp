;;;; Copyright (c) 2004-2006 David Lichteblau <david@lichteblau.com>
;;;; Lizenz: (L)LGPL
;;;;
;;;; Urspruenglicher Autor: David Lichteblau.
;;;; Aenderungen durch knowledgeTools GmbH.

;;;; http://www.pkware.com/business_and_developers/developer/popups/appnote.txt
;;;; (http://www.pkware.com/company/standards/appnote/)

(in-package :zip)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+allegro
  (setf (symbol-function 'string-to-octets) #'excl:string-to-octets
        (symbol-function 'octets-to-string) #'excl:octets-to-string)
  #+sbcl(setf (symbol-function 'string-to-octets) #'sb-ext:string-to-octets
              (symbol-function 'octets-to-string) #'sb-ext:octets-to-string))

(defun make-byte-array (n)
  (make-array n :element-type '(unsigned-byte 8)))

(defun get-short (array offset)
  (logior (elt array offset)
	  (ash (elt array (1+ offset)) 8)))

(defun (setf get-short) (newval array offset)
  (setf (elt array (+ offset 0)) (logand newval #xff))
  (setf newval (ash newval -8))
  (setf (elt array (+ offset 1)) (logand newval #xff))
  newval)

(defun get-int (array offset)
  (logior (elt array offset)
	  (ash (elt array (+ offset 1)) 8)
	  (ash (elt array (+ offset 2)) 16)
	  (ash (elt array (+ offset 3)) 24)))

(defun (setf get-int) (newval array offset)
  (setf (elt array (+ offset 0)) (logand newval #xff))
  (setf newval (ash newval -8))
  (setf (elt array (+ offset 1)) (logand newval #xff))
  (setf newval (ash newval -8))
  (setf (elt array (+ offset 2)) (logand newval #xff))
  (setf newval (ash newval -8))
  (setf (elt array (+ offset 3)) (logand newval #xff))
  newval)

(defmacro define-record (constructor
			 (&key (length #-clisp (gensym) #+clisp (gentemp)))
			 &rest fields)
  `(progn
     (defconstant ,length
	 ,(loop
	      for (nil type) in fields
	      sum (ecase type (:int 4) (:short 2))))
     (defun ,constructor (&optional s)
       (let ((bytes (make-byte-array ,length)))
	 (when s
           (read-sequence bytes s))
	 bytes))
     ,@(loop
	   for (name type) in fields
	   for offset = 0 then (+ offset length)
	   for length = (ecase type (:int 4) (:short 2))
	   for reader = (ecase type (:int 'get-int) (:short 'get-short))
	   unless (eq name :dummy)
	   append `((defun ,name (r)
                      (,reader r ,offset))
                    (defun (setf ,name) (newval r)
                      (setf (,reader r ,offset) newval))))))

(define-record make-end-header (:length +end-header-length+)
  (end/signature :int)
  (end/this-disc :short)
  (end/central-directory-disc :short)
  (end/disc-files :short)
  (end/total-files :short)
  (end/central-directory-size :int)
  (end/central-directory-offset :int)
  (end/comment-length :short))

(define-record make-directory-entry ()
  (cd/signature :int)
  (cd/version-made-by :short)
  (cd/version-needed-to-extract :short)
  (cd/flags :short)
  (cd/method :short)
  (cd/time :short)
  (cd/date :short)
  (cd/crc :int)
  (cd/compressed-size :int)
  (cd/size :int)
  (cd/name-length :short)
  (cd/extra-length :short)
  (cd/comment-length :short)
  (cd/disc-number :short)
  (cd/internal-attributes :short)
  (cd/external-attributes :int)
  (cd/offset :int))

(define-record make-local-header ()
  (file/signature :int)
  (file/version-needed-to-extract :short)
  (file/flags :short)
  (file/method :short)
  (file/time :short)
  (file/date :short)
  (file/crc :int)
  (file/compressed-size :int)
  (file/size :int)
  (file/name-length :short)
  (file/extra-length :short))

(define-record make-data-descriptor ()
  (data/crc :int)
  (data/compressed-size :int)
  (data/size :int))

(defun compress (input output compressor)
  (let ((nin 0)
	(nout 0)
	(crc (make-instance 'salza2:crc32-checksum)))
    (flet ((callback (buffer count)
             (write-sequence buffer output :start 0 :end count)
             (incf nout count)))
      (setf (salza2:callback compressor) #'callback)
      (let* ((input-buffer (make-array 8192 :element-type '(unsigned-byte 8))))
	(loop
	  (let ((end (read-sequence input-buffer input)))
            (cond
              ((plusp end)
                (salza2:compress-octet-vector input-buffer compressor :end end)
                (incf nin end)
                (salza2:update crc input-buffer 0 end))
              (t
                (salza2:finish-compression compressor)
                (salza2:reset compressor)
                (return (values nin nout (salza2:result crc)))))))))))

(defun store (in out)
  "Copy uncompressed bytes from IN to OUT and return values like COMPRESS."
  (let ((buf (make-array 8192
                         :initial-element 0
                         :element-type '(unsigned-byte 8)))
        (ntotal 0)
        (crc (make-instance 'salza2:crc32-checksum)))
    (loop
        for n = (read-sequence buf in :end (length buf))
        until (zerop n)
        do
          (write-sequence buf out :end n)
          (incf ntotal n)
          (salza2:update crc buf 0 n))
    (values ntotal ntotal (salza2:result crc))))

(defun seek-to-end-header (s)
  (let* ((len (+ 65536 +end-header-length+))
	 (guess (max 0 (- (file-length s) len))))
    (file-position s guess)
    (let ((v (make-byte-array (min (file-length s) len))))
      (read-sequence v s)
      (let ((n (search #(80 75 5 6) v :from-end t)))
	(unless n
	  (error "end of central directory header not found"))
	(file-position s (+ guess n))))))

(defstruct zipfile
  stream
  entries
  external-format)

(defstruct zipfile-entry
  name
  stream
  offset
  size
  compressed-size
  comment
  date
  mode
  time)

(defstruct zipwriter
  stream
  compressor
  head
  tail
  external-format)

(defstruct zipwriter-entry
  mode
  name
  position
  header)

(defun decode-name (name external-format)
  (octets-to-string name :external-format external-format))

(defun read-entry-object (s)
  (let* ((header (make-directory-entry s))
	 (name (make-array (cd/name-length header)
                           :element-type '(unsigned-byte 8)))
	 (comment
	  (when (plusp (cd/comment-length header))
	    (make-array (cd/comment-length header)
			:element-type '(unsigned-byte 8))))
         (utf8p (if (logtest (cd/flags header) 2048) :utf8 *zip-filename-coding*)))
    ;; (assert (eql utf8p *zip-filename-coding*))
    (assert (= (cd/signature header) #x02014b50))
    (read-sequence name s)
    (setf name (decode-name name utf8p))
    (file-position s (+ (file-position s) (cd/extra-length header)))
    (when comment
      (read-sequence comment s)
      (setf comment (decode-name comment utf8p)))
    (make-zipfile-entry :name name
			:stream s
			:offset (cd/offset header)
			:size (cd/size header)
			:compressed-size (cd/compressed-size header)
			:comment comment
                        :date (cd/date header)
                        :time (cd/time header)
                        :mode (ash (cd/external-attributes header) -16))))

(defun open-zipfile (pathname)
  (let ((s (open pathname :element-type '(unsigned-byte 8))))
    (unwind-protect
        (progn
          (seek-to-end-header s)
          (let* ((end (make-end-header s))
                 (n (end/total-files end))
                 (entries (make-hash-table :test #'equal))
                 (zipfile (make-zipfile :stream s
                                        :entries entries)))
            (file-position s (end/central-directory-offset end))
            (dotimes (x n)
              (let ((entry (read-entry-object s)))
                (setf (gethash (zipfile-entry-name entry) entries) entry)))
            #+sbcl (let ((s s)) (sb-ext:finalize zipfile (lambda ()(close s))))
            (setf s nil)
            zipfile))
      (when s
        (close s)))))

(defgeneric close-zipfile (zipfile))
(defgeneric get-zipfile-entry (name zipfile))
(defgeneric zipfile-entry-contents (entry &optional stream))

(defmethod close-zipfile ((zipfile zipfile))
  (close (zipfile-stream zipfile)))

(defmethod get-zipfile-entry (name (zipfile zipfile))
  (gethash name (zipfile-entries zipfile)))

(defun encode-name (name)
  (values (string-to-octets name :external-format *zip-filename-coding* :null-terminate nil)
          (eql *zip-filename-coding* :utf8)))

(defmethod write-zipentry (z name (data stream) &key (file-write-date (file-write-date data))
                                                     (deflate t) (mode #o640))
  (setf name (substitute #\/ #\\ name))
  (let* ((s (zipwriter-stream z))
         (header (make-local-header))
         (entry (make-zipwriter-entry :mode mode
                                      :name name
                                      :position (file-position s)
                                      :header header)))
    (multiple-value-bind (encoded-name utf8p)
        (encode-name name)
      (setf (file/signature header) #x04034b50)
      (setf (file/version-needed-to-extract header) 20) ;version 2.0
      (setf (file/flags header) (if utf8p 2048 0))
      (setf (file/method header) (if deflate 8 0))
      (multiple-value-bind (s min h d m y)
          (decode-universal-time
           (or file-write-date (encode-universal-time 0 0 0 1 1 1980 0)))
        (setf (file/time header)
          (logior (ash h 11) (ash min 5) (ash s -1)))
        (setf (file/date header)
          (logior (ash (- y 1980) 9) (ash m 5) d)))
      (setf (file/crc header) 0)
      (setf (file/compressed-size header) 0)
      (setf (file/size header) 0)
      (setf (file/name-length header) (length encoded-name))
      (setf (file/extra-length header) 0)
      (setf (zipwriter-tail z)
        (setf (cdr (zipwriter-tail z)) (cons entry nil)))
      (write-sequence header s)
      (write-sequence encoded-name s)
      (multiple-value-bind (nin nout crc)
          (if deflate
              (compress data s (zipwriter-compressor z))
            (store data s))
        (let ((descriptor (make-data-descriptor))
              (fpos (file-position s)))
          (setf (data/crc descriptor) crc)
          (setf (data/compressed-size descriptor) nout)
          (setf (data/size descriptor) nin)
          ;; record same values for central directory
          (setf (file/crc header) crc)
          (setf (file/compressed-size header) nout)
          (setf (file/size header) nin)
          (file-position s (+ (zipwriter-entry-position entry) 14))
          (write-sequence descriptor s)
          (file-position s fpos)
          #+nil(write-sequence descriptor s)
          ))
      name)))

(defun write-central-directory (z)
  (let* ((s (zipwriter-stream z))
         (pos (file-position s))
         (n 0))
    (dolist (e (cdr (zipwriter-head z)))
      (incf n)
      (let ((header (zipwriter-entry-header e))
            (entry (make-directory-entry)))
        (setf (cd/signature entry) #x02014b50)
        (setf (cd/version-made-by entry) #x0314) ;dos compatible
        (setf (cd/version-needed-to-extract entry)
          (file/version-needed-to-extract header))
        (setf (cd/flags entry) (file/flags header))
        (setf (cd/method entry) (file/method header))
        (setf (cd/time entry) (file/time header))
        (setf (cd/date entry) (file/date header))
        (setf (cd/crc entry) (file/crc header))
        (setf (cd/compressed-size entry) (file/compressed-size header))
        (setf (cd/size entry) (file/size header))
        (setf (cd/name-length entry) (file/name-length header))
        (setf (cd/extra-length entry) (file/extra-length header))
        (setf (cd/comment-length entry) 0)
        (setf (cd/disc-number entry) 0) ;XXX ?
        (setf (cd/internal-attributes entry) 0)
        (setf (cd/external-attributes entry)
          (ash (zipwriter-entry-mode e) 16)) ;XXX directories
        (setf (cd/offset entry) (zipwriter-entry-position e))
        (write-sequence entry s)
        (write-sequence (encode-name (zipwriter-entry-name e))
                        s)))
    (let ((end (make-end-header)))
      (setf (end/signature end) #x06054b50)
      (setf (end/this-disc end) 0)      ;?
      (setf (end/central-directory-disc end) 0) ;?
      (setf (end/disc-files end) n)
      (setf (end/total-files end) n)
      (setf (end/central-directory-size end) (- (file-position s) pos))
      (setf (end/central-directory-offset end) pos)
      (setf (end/comment-length end) 0)
      (write-sequence end s))))

(defun close-zipfile-writer (z)
  (write-central-directory z)
  (close (zipwriter-stream z)))

(defun %zipfile-entry-contents (entry stream)
  (let ((s (zipfile-entry-stream entry))
        header)
    (file-position s (zipfile-entry-offset entry))
    (setf header (make-local-header s))
    (assert (= (file/signature header) #x04034b50))
    (file-position s (+ (file-position s)
                        (file/name-length header)
                        (file/extra-length header)))
    (let ((in (make-instance 'truncating-stream
                :input-handle s
                :size (zipfile-entry-compressed-size entry)))
          (outbuf nil)
          out)
      (if stream
          (setf out stream)
        (setf outbuf (make-byte-array (zipfile-entry-size entry))
              out (make-buffer-output-stream outbuf)))
      (ecase (file/method header)
        (0 (store in out))
        (8 (inflate in out)))
      outbuf)))

(defun make-zipfile-writer (pathname &key (if-exists :error))
  (let ((c (cons nil nil)))
    (make-zipwriter
     :stream (open pathname
                   :direction :output
                   :if-exists if-exists
                   :element-type '(unsigned-byte 8))
     :compressor (make-instance 'salza2:deflate-compressor)
     :head c
     :tail c)))

(defmethod zipfile-entry-contents ((entry zipfile-entry) &optional stream)
  (let ((s (zipfile-entry-stream entry))
	header)
    (file-position s (zipfile-entry-offset entry))
    (setf header (make-local-header s))
    (assert (= (file/signature header) #x04034b50))
    (file-position s (+ (file-position s)
			(file/name-length header)
			(file/extra-length header)))
    (let ((in (make-instance 'truncating-stream
                :input-handle s
                :size (zipfile-entry-compressed-size entry)))
          (outbuf nil)
          out)
      (if stream
          (setf out stream)
          (setf outbuf (make-byte-array (zipfile-entry-size entry))
                out (make-buffer-output-stream outbuf)))
      (ecase (file/method header)
        (0 (store in out))
        (8 (inflate in out)))
      outbuf)))

(defvar *zip-filename-coding* :default)

(defmacro with-zipfile-stream ((file stream) &body body)
  `(let ((,file (open-zipfile-from-stream ,stream)))
     ,@body))

(defmacro with-zipfile ((file pathname) &body body)
  `(let ((,file (open-zipfile ,pathname)))
     (unwind-protect
	 (progn ,@body)
       (close-zipfile ,file))))

(defun open-zipfile-from-stream (stream)
  (seek-to-end-header stream)
  (let* ((end (make-end-header stream))
         (n (end/total-files end))
         (entries (make-hash-table :test #'equal))
         (zipfile (make-zipfile :stream stream
                                :entries entries)))
    (file-position stream (end/central-directory-offset end))
    (dotimes (x n)
      (let ((entry (read-entry-object stream)))
        (setf (gethash (zipfile-entry-name entry) entries) entry)))
    zipfile))

(defmacro with-output-to-zipfile
    ((var pathname &key (if-exists :error)) &body body)
  `(let ((,var (make-zipfile-writer ,pathname :if-exists ,if-exists)))
     (unwind-protect
	 (progn ,@body)
       (close-zipfile-writer ,var))))

(defmacro do-zipfile-entries ((name entry zipfile) &body body)
  (setf name (or name (gensym)))
  (setf entry (or entry (gensym)))
  `(block nil
     (maphash (lambda (,name ,entry)
		(declare (ignorable ,name ,entry))
		,@body)
	      (zipfile-entries ,zipfile))))

(defun ymd (date)
  ;; reverse (logior (ash (- y 1980) 9) (ash m 5) d)
  (values (+ 1980 (ash (logand date #b1111111000000000) -9))
          (ash (logand date #b111100000) -5)
          (logand date #b11111)))

(defun hms (time)
  ;; reverse (logior (ash h 11) (ash min 5) (ash s -1))
  (values (ash (logand time #b1111100000000000) -11)
          (ash (logand time #b11111100000) -5)
          (ash (logand time #b11111) 1)))

(defun unzip (pathname target-directory &key (if-exists :error) (update nil) (preserve nil) verbose)
  ;; <Xof> "When reading[1] the value of any pathname component, conforming
  ;;       programs should be prepared for the value to be :unspecific."
  ;; When update, then update files, create if necessary.
  ;; When preserve, then archive's date/time will be set to writen files.
  (assert (or (null update) (eql if-exists :supersede)))
                                        ; when update, if-exists must be :supersede
  (when (set-difference (list (pathname-name target-directory)
                              (pathname-type target-directory))
                        '(nil :unspecific))
    (error "pathname not a directory, lacks trailing slash?"))
  (with-zipfile (zip pathname)
    (do-zipfile-entries (name entry zip)
      (let* (#+sbcl (name (namestring (uiop:ensure-pathname name)))
             (filename (namestring (merge-pathnames name target-directory))))
        (ensure-directories-exist filename)
        (unless (char= (elt name (1- (length name))) #\/)
          (when (or (null update)
                    (not (probe-file filename))
                    (let ((old (file-write-date filename))
                          (new (multiple-value-bind (h min sec)
                                   (hms (zipfile-entry-time entry))
                                 (multiple-value-bind (y m d)
                                     (ymd (zipfile-entry-date entry))
                                   (encode-universal-time sec min h d m y)))))
                      ;; (when (and verbose (< old new)) (write `(< ,old ,new)))
                      (< old new)))
            (ecase verbose
              ((nil))
              ((t) (write-string name) (terpri))
              (:dots (write-char #\.)))
            (force-output)
            (with-open-file
                (s filename :direction :output :if-exists if-exists
                 :element-type '(unsigned-byte 8))
              (zipfile-entry-contents entry s))
            (when preserve
              (multiple-value-bind (h min sec)
                  (hms (zipfile-entry-time entry))
                (multiple-value-bind (y m d)
                    (ymd (zipfile-entry-date entry))
                  (setf (file-write-date filename) (encode-universal-time sec min h d m y)))))))))))

(defun %directoryp (pathname)
  #+allegro (excl:file-directory-p pathname)
  #+lispworks (lispworks:file-directory-p pathname)
  #+clisp (ignore-errors
	   (ext:probe-directory
	    (concatenate 'string (princ-to-string pathname) "/")))
  #-(or lispworks allegro clisp)
  (and (null (pathname-name pathname))
       (null (pathname-type pathname))))

(defun %directory (d)
  #+allegro (directory d :directories-are-files nil)
  #+clisp (append (directory (concatenate 'string (princ-to-string d) "/*/"))
		  (directory (concatenate 'string (princ-to-string d) "/*")))
  #-(or allegro clisp) (directory d))

(defun %file-write-date (f)
  #+clisp (posix:file-stat-mtime (posix:file-stat f))
  #-clisp (file-write-date f))

(defun %pathname-for-directory (f)
  #+(or allegro clisp) f
  #-(or allegro clisp) (make-pathname :name :wild :type :wild :defaults f))

(defun %directory-namestring (d)
  #+clisp (directory-namestring
	   (truename (concatenate 'string (princ-to-string d) "/")))
  #-clisp (directory-namestring d))

(defun zip (pathname source-directory &key (if-exists :error))
  (let ((base (%directory-namestring (merge-pathnames source-directory))))
    (with-output-to-zipfile (zip pathname :if-exists if-exists)
      (labels ((recurse (d)
                 (dolist (f (%directory d))
                   (cond
                     ((%directoryp f)
                       (write-zipentry
                        zip
                        (enough-namestring (namestring f) base)
                        (make-concatenated-stream)
                        :file-write-date (%file-write-date f))
                       (recurse (%pathname-for-directory f)))
                     ((or (pathname-name f) (pathname-type f))
                       (with-open-file (s f :element-type '(unsigned-byte 8))
                         (write-zipentry
                          zip
                          (enough-namestring (namestring f) base)
                          s)))))))
        (recurse source-directory)))))

;;;; New Definitions

(defclass buffer-input-stream
    (trivial-gray-stream-mixin fundamental-binary-input-stream)
    ((buf :initarg :buf :accessor buf)
     (pos :initform 0 :accessor pos)))

(defun make-buffer-input-stream (inbuf)
  (make-instance 'buffer-input-stream :buf inbuf))

(defmethod stream-read-byte ((stream buffer-input-stream))
  (if (< (pos stream) (length (buf stream)))
      (prog1
	  (aref (buf stream) (pos stream))
	(incf (pos stream)))
    nil))

(defmethod stream-read-sequence ((stream buffer-input-stream) seq start end &key)
  (let* ((end (min end (- (length (buf stream)) (pos stream))))
         (len (- end start)))
  (replace seq
	   (buf stream)
	   :start1 start
	   :end1 end
	   :start2 (pos stream))
    (incf (pos stream) len)
    len))

(defmethod write-zipentry (z name (data zipfile-entry) &key (file-write-date (file-write-date (zipfile-entry-stream data)))
                                                            (deflate t) (mode (zipfile-entry-mode data)))
  (assert (equal name (zipfile-entry-name data)))
  (setf name (substitute #\/ #\\ name))
  (let* ((s (zipwriter-stream z))
         (header (make-local-header))
         (entry (make-zipwriter-entry :mode mode
                                      :name name
                                      :position (file-position s)
                                      :header header)))
    (multiple-value-bind (encoded-name utf8p)
        (encode-name name)
      (setf (file/signature header) #x04034b50)
      (setf (file/version-needed-to-extract header) 20) ;version 2.0
      (setf (file/flags header) (if utf8p 2048 0))
      (setf (file/method header) (if deflate 8 0))
      (multiple-value-bind (s min h d m y)
          (decode-universal-time (or file-write-date (get-universal-time)))
        (setf (file/time header)
          (logior (ash h 11) (ash min 5) (ash s -1)))
        (setf (file/date header)
          (logior (ash (- y 1980) 9) (ash m 5) d)))
      (setf (file/crc header) 0)
      (setf (file/compressed-size header) 0)
      (setf (file/size header) 0)
      (setf (file/name-length header) (length encoded-name))
      (setf (file/extra-length header) 0)
      (setf (zipwriter-tail z)
        (setf (cdr (zipwriter-tail z)) (cons entry nil)))
      (write-sequence header s)
      (write-sequence encoded-name s)
      (let* ((contents (zipfile-entry-contents data))
             (input (make-buffer-input-stream contents)))
        (multiple-value-bind (nin nout crc)
            (if deflate
                (compress input s (zipwriter-compressor z))
              (store input s))
          (let ((descriptor (make-data-descriptor))
                (fpos (file-position s)))
            (setf (data/crc descriptor) crc)
            (setf (data/compressed-size descriptor) nout)
            (setf (data/size descriptor) nin)
            ;; record same values for central directory
            (setf (file/crc header) crc)
            (setf (file/compressed-size header) nout)
            (setf (file/size header) nin)
            (file-position s (+ (zipwriter-entry-position entry) 14))
            (write-sequence descriptor s)
            (file-position s fpos)
            #+nil(write-sequence descriptor s)
            )))
      name)))

#+ignore
(defun compress-contents (input-buffer output compressor)
  (let ((nin 0)
	(nout 0)
	(crc (make-instance 'salza2:crc32-checksum)))
    (flet ((callback (buffer count)
             (write-sequence buffer output :start 0 :end count)
             (incf nout count)))
      (setf (salza2:callback compressor) #'callback)
      (let ((end (length input-buffer)))
        (salza2:compress-octet-vector input-buffer compressor :end end)
        (incf nin end)
        (salza2:update crc input-buffer 0 end)
        (salza2:finish-compression compressor)
        (salza2:reset compressor)
        (values nin nout (salza2:result crc))))))

#+ignore
(defun store-contents (buf out)
  (let ((ntotal 0)
        (crc (make-instance 'salza2:crc32-checksum))
        (end (length buf)))
    (write-sequence buf out :end end)
    (incf ntotal end)
    (salza2:update crc buf 0 end)
    (values ntotal ntotal (salza2:result crc))))

#+ignore
(defun zipfile-entry-raw-contents (entry &optional stream)
  (if (pathnamep stream)
      (with-open-file (s stream
			 :direction :output
			 :if-exists :supersede
                         :element-type '(unsigned-byte 8))
	(%zipfile-entry-raw-contents entry s))
      (%zipfile-entry-raw-contents entry stream)))

#+ignore
(defun %zipfile-entry-raw-contents (entry stream)
  (let ((s (zipfile-entry-stream entry))
        header)
    (file-position s (zipfile-entry-offset entry))
    (setf header (make-local-header s))
    (assert (= (file/signature header) #x04034b50))
    (file-position s (+ (file-position s)
                        (file/name-length header)
                        (file/extra-length header)))
    (let ((in (make-instance 'truncating-stream
                :input-handle s
                :size (zipfile-entry-compressed-size entry)))
          (outbuf nil)
          out)
      (if stream
          (setf out stream)
        (setf outbuf (make-byte-array (zipfile-entry-compressed-size entry))
              out (make-buffer-output-stream outbuf)))
      (store in out)
      outbuf)))

;;;; Optimization

(define-compiler-macro read-bits (br count)
  ;; return a value from the current bit reader.
  ;; the count can be from 1 to 16
  `(let ((br ,br)
         (count ,count))
     (if (eql count 0)
         0
       (let ((last-byte (bit-reader-last-byte br))
             (bits      (bit-reader-bits br)))
         (declare (type fixnum last-byte)
                  (type (integer -63 63) bits))
         (loop 
           (if (>= bits count)
                                        ; then we have enough now
               (if (> bits count)
                   (progn               ; then we have some left over
                     (setf (bit-reader-last-byte br)
                       (the fixnum (ash last-byte (- count))))
                     (setf (bit-reader-bits br) (- bits count))
                     (return (logand last-byte (the fixnum (svref *maskarray* count)))))
                 (progn                 ; else no bits left
                   (setf (bit-reader-bits br) 0)
                   (setf (bit-reader-last-byte br) 0)
                   (return last-byte)))
                                        ; else need a new byte
             (let ((new-byte (read-byte (bit-reader-stream br))))
               (declare (type fixnum new-byte))
               (setq last-byte (+ last-byte
                                  (the fixnum (ash new-byte bits))))
               (incf bits 8))))))))

(defun decode-huffman-tree (br tree minbits)
  ;; find the next huffman encoded value.
  ;; the minimum length of a huffman code is minbits so 
  ;; grab that many bits right away to speed processing and the
  ;; go bit by bit until the answer is found
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type bit-reader br)
           (type (integer -63 63) minbits)
           #+allegro (:explain :boxing :inlining))
  (let ((startval (read-bits br minbits)))
    (declare (type fixnum startval))
    (dotimes (i minbits)
      (if (logtest 1 startval)
	  (setq tree (cdr tree))
        (setq tree (car tree)))
      (setq startval (the fixnum (ash startval -1))))
    (loop
      (if (atom tree)
	  (return tree)
        (if (= 1 (read-bits br 1))
            (setq tree (cdr tree))
          (setq tree (car tree)))))))

;;;; Example

#+ignore
(defun square (x) (expt x 2))
#+ignore
(define-compiler-macro square (&whole form arg)
  (if (atom arg)
      `(expt ,arg 2)
    (case (car arg)
      (square (if (= (length arg) 2)
                  `(expt ,(nth 1 arg) 4)
                form))
      (expt   (if (= (length arg) 3)
                  (if (numberp (nth 2 arg))
                      `(expt ,(nth 1 arg) ,(* 2 (nth 2 arg)))
                    `(expt ,(nth 1 arg) (* 2 ,(nth 2 arg))))
                form))
      (otherwise `(expt ,arg 2)))))
