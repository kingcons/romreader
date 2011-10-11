(in-package :romreader)


;;;; General Utilities

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args) (load-time-value *package*))))

(defun ksymb (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defun print-byte (byte)
  (format nil "~8,'0b" byte))
