(in-package :cl-user)
(defpackage quickdocs-database.model.system
  (:use :cl
        :cl-annot.class
        :sxql)
  (:import-from :datafly
                :model
                :retrieve-all))
(in-package :quickdocs-database.model.system)

(syntax:use-syntax :annot)

@export-accessors
@model
(defstruct (system
            (:has-many (dependencies system)
             (select :system.*
               (from :system_dependencies)
               (left-join :system :on (:= :system_dependencies.depends_system_id :system.id))
               (where (:= :system_dependencies.system_id id)))))
  id
  project-id
  name
  version
  description
  long-description
  license)

@export
(defun system-authors (system)
  (check-type system system)
  (mapcar (lambda (row)
            (getf row :author-name))
          (retrieve-all
           (select :author_name
             (from :system_author)
             (where (:= :system_id (system-id system)))))))
