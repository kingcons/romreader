(in-package :romreader)

(defvar *valid-formats* '("nes")
  "A list of implemented ROM formats by file extension.")

(defun load-rom (path)
  "Check to see if PATH exists and is a supported ROM format. If so, call the
appropriate parse-rom method, otherwise error."
  (if (and (probe-file path)
           (member (pathname-type path) *valid-formats* :test #'equalp))
      (parse-rom (symb (string-upcase (pathname-type path))) path)
      (error 'unknown-format :filename path)))

(defgeneric parse-rom (format pathname)
  (:documentation "Parse the file located at PATHNAME as a ROM of the given
FORMAT."))

(defgeneric rom-binary (rom)
  (:documentation "Return a bytevector of the ROM data."))

(defgeneric rom-metadata (rom)
  (:documentation "Return a plist of the ROM metadata."))
