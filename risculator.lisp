;;; -*- Mode: Lisp -*-

(defpackage :blitz.risculator
  (:use :common-lisp :defclass-star)
  (:shadow "VARIABLE"))
(in-package :blitz.risculator)

;;; Globals

(defvar *unit-descriptions* (make-hash-table :test 'eq))

(defun get-unit-description (name)
  (or (gethash name *unit-descriptions*)
      (error "No unit named ~A." name)))

;;; Types and variables

(defclass* vtype ()
  ())

(defclass* vtype-integer (vtype)
  (width))

(defmethod print-object ((object vtype-integer) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "UINT~A" (width-of object))))

(defclass* variable ()
  (name
   initial
   vtype))

(defmethod print-object ((object variable) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A ~A" (name-of object) (vtype-of object))))

(defun make-integer-variable (name width initial)
  (make-instance 'variable
                 :name name
                 :initial initial
                 :vtype (make-instance 'vtype-integer :width width)))


(defclass* unit-description ()
  ((name)
   (input-vars)
   (output-vars)
   (state-vars)
   (transformation-form)))

(defmethod print-object ((object unit-description) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A ~A -> ~A" (name-of object) 
            (mapcar 'name-of (input-vars-of object))
            (mapcar 'name-of (output-vars-of object)))))

(defun register-unit-description (unit-description)
  (setf (gethash (name-of unit-description) *unit-descriptions*)
        unit-description))


(defun parse-variable (descr)
  (destructuring-bind (name width &key (initial 0))
      descr
    (make-integer-variable name width initial)))

(defun parse-variable-list (list)
  (mapcar #'parse-variable list))

(defun variable-declaration (var)
  `(type (unsigned-byte ,(width-of (vtype-of var))) ,(name-of var)))

(defmacro defunit ((name &key input output state) &body body)
  `(register-unit-description 
    (make-instance 'unit-description
                   :name ',name
                   :input-vars (parse-variable-list ',input)
                   :output-vars (parse-variable-list ',output)
                   :state-vars (parse-variable-list ',state)
                   :transformation-form '(progn ,@body))))


(defstruct unit
  input
  output
  state
  name-mapping
  description)

(defun parse-units (units)
  (let ((out (make-hash-table :test 'eq)))
    (loop 
       for (name type) in units
       for description = (get-unit-description type)
       do (setf (gethash name out)
                (let ((unit (make-unit :description description
                                       :name-mapping (make-hash-table :test 'eq))))
                  (flet ((unq-vars (vars)
                           (loop 
                              for var in vars
                              for new-name = (gensym (string (name-of var)))
                              do (setf (gethash (name-of var) (unit-name-mapping unit)) new-name
                                       (gethash new-name (unit-name-mapping unit)) (name-of var))
                              collect (make-integer-variable new-name
                                                             (width-of (vtype-of var)) 
                                                             (initial-of var)))))
                    (setf (unit-input unit) (unq-vars (input-vars-of description))
                          (unit-output unit) (unq-vars (output-vars-of description))
                          (unit-state unit) (unq-vars (state-vars-of description))))
                  unit)))
    out))

(defun unit-variables (unit)
  (append (unit-input unit)
          (unit-output unit)
          (unit-state unit)))

(defun unit-transformation (unit)
  `(let ,(loop for new-name in (unit-variables unit)
            collect `(,(gethash (name-of new-name) (unit-name-mapping unit))
                       ,(name-of new-name)))
     ,(transformation-form-of (unit-description unit))
     ,@(loop 
          for new-name in (unit-variables unit)
          collect `(setf ,(name-of new-name) ,(gethash (name-of new-name) (unit-name-mapping unit))))))


(defun parse-connections (units connections)
  (assert (and (listp connections)
               (eq (first connections) '<-)))
  (loop 
     for ((in-unit-name in-var-name) 
          (out-unit-name out-var-name) . rest) on (rest connections) by #'cddr
     for in-unit = (gethash in-unit-name units)
     for out-unit = (gethash out-unit-name units)
     for in-var = (gethash in-var-name (unit-name-mapping in-unit))
     for out-var = (gethash out-var-name (unit-name-mapping out-unit))
     collect `(,in-var ,out-var)))

(defmacro defpipe ((name &key units)
                   connections)
  (let* ((units (parse-units units))
         (all-vars (loop for unit being each hash-value in units
                      appending (unit-variables unit)))
         (connections (parse-connections units connections)))
    `(defun ,name ()
       (declare (optimize (speed 3)
                          (safety 0)
                          (debug 0)))
       (let ,(loop for var in all-vars
                collect `(,(name-of var) ,(initial-of var)))
         (declare ,@(mapcar 'variable-declaration all-vars))
         (loop
            (progn 
              ;; Update input values
              ,@(loop 
                   for (in out) in connections
                   collect `(setf ,in ,out))
              ;; Transform!
              ,@(loop 
                   for unit being each hash-value in units
                   collect (unit-transformation unit))
              ))))))

;;; EOF
