(in-package :cl-user)
(defpackage quickdocs-database.preference
  (:use :cl
        :sxql)
  (:import-from :datafly
                :retrieve-all
                :execute))
(in-package :quickdocs-database.preference)

(syntax:use-syntax :annot)

(defvar *preference* nil)

@export
(defun all-preferences (&key ignore-cache)
  (or (if ignore-cache nil *preference*)
      (setf *preference*
            (loop with preference = (make-hash-table :test 'equal)
                  for row in (retrieve-all
                              (select :*
                                (from :preference)))
                  do (setf (gethash (getf row :name) preference)
                           (getf row :value))
                  finally (return preference)))))

@export
(defun preference (name)
  (check-type name string)
  (gethash name (all-preferences)))

@export
(defun (setf preference) (value name)
  (check-type name string)
  (let ((all (all-preferences)))
    (if (nth-value 1 (gethash name all))
        (execute
         (update :preference
           (set= :value value)
           (where (:= :name name))
           (limit 1)))
        (execute
         (insert-into :preference
           (set= :name name
                 :value value))))
    (setf (gethash name all) value)))
