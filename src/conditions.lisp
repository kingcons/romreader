(in-package #:romreader)

;;;; Abstract Conditions

(define-condition romreader-error (error)
  ()
  (:documentation "The base condition for all errors in ROMREADER."))

;;;; Concrete Conditions

(define-condition unknown-format (romreader-error)
  ((filename :initarg :filename :reader filename))
  (:report (lambda (condition stream)
             (let ((filename (filename condition)))
               (format stream "No known ROM format matching ~s for ~a."
                       (pathname-type filename) filename))))
  (:documentation "Signalled when an appropriate parser method could not be
found for the extension of the given ROM pathname."))

(define-condition malformed-header (romreader-error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "Malformed ROM Header: ~a" (message condition))))
  (:documentation "Signalled when a corrupted or invalid header is encountered."))
