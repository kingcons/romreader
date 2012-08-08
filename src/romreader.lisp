(in-package :romreader)

(defvar *valid-formats* nil
  "A list of ROM formats with implemented readers. Do not manually modify this.")

(defclass rom ()
  ((metadata :initarg :metadata :reader rom-metadata)
   (binary :initarg :binary :reader rom-binary)
   (format :initarg :format :reader rom-format)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun ksymb (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defun load-rom (path)
  "Check to see if PATH exists and is a supported ROM format. If so, call the
appropriate reader and return a ROM instance, otherwise error."
  (if (and (probe-file path)
           (member (pathname-type path) *valid-formats* :test #'equalp))
      (parse-rom (ksymb (string-upcase (pathname-type path))) path)
      (error 'unknown-format :filename path)))

(defgeneric parse-rom (format pathname)
  (:documentation "Parse the file located at PATHNAME as a ROM of the given
FORMAT. FORMAT should be a symbol denoting a file extension. Returns a ROM
instance."))

(defgeneric rom-binary (rom)
  (:documentation "Return a bytevector of the ROM data."))

(defgeneric rom-metadata (rom)
  (:documentation "Return a plist of the ROM metadata."))

(defgeneric rom-format (rom)
  (:documentation "Return the file extension of the ROM as a symbol."))

(defmacro defreader (format &body body)
  "Define a reader for FORMAT that executes BODY. FORMAT should be a
pathname-type (i.e. file extension). Ensure that FORMAT is added to the list
of supported formats if necessary. BODY executes within a WITH-OPEN-FILE that
binds 'in' to a binary-stream of the ROM file. The BODY should return a list with
the rom's metadata as the first item and the rom's binary as the second."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       (pushnew ,(string-downcase format) *valid-formats* :test #'string=))
     (defmethod parse-rom ((rom (eql ,(ksymb (string-upcase format)))) pathname)
       (with-open-file (,(intern "IN") pathname :element-type '(unsigned-byte 8))
         (destructuring-bind (metadata binary) (progn ,@body)
           (make-instance 'rom :metadata metadata :binary binary
                          :format ',(intern (string-upcase format))))))))
