(in-package :cl-ganzhi)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro define-rrstruct (name-and-options &rest slot-descriptions)
  "Define a struct that all fields are required and readonly."
  (let ((slots (mapcar #'(lambda (options)
                           (destructuring-bind (field type) options
                             `(,field (error "Missing required field ~s" ',field)
                                      :type ,type :read-only t)))
                       slot-descriptions)))
    `(defstruct ,name-and-options
       ,@slots)))
