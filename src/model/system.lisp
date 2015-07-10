(in-package :cl-user)
(defpackage quickdocs-database.model.system
  (:use :cl
        :cl-annot.class
        :sxql)
  (:import-from :datafly
                :model
                :retrieve-all
                :retrieve-one
                :execute))
(in-package :quickdocs-database.model.system)

(syntax:use-syntax :annot)

@export-accessors
@export
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

(defun retrieve-system-authors (system-id &optional (type "author"))
  (retrieve-all
   (select :*
     (from :system_author)
     (where (:and (:= :system_id system-id)
                  (:= :type type))))))

@export
(defun system-authors (system)
  (check-type system system)
  (mapcar (lambda (row)
            (getf row :author-name))
          (retrieve-system-authors (system-id system) "author")))

@export
(defun system-maintainers (system)
  (check-type system system)
  (mapcar (lambda (row)
            (getf row :author-name))
          (retrieve-system-authors (system-id system) "maintainer")))

@export
(defun create-system (&key project-id
                        name
                        version
                        description
                        long-description
                        license
                        homepage-url

                        authors
                        maintainers)
  (execute
   (insert-into :system
     (set= :project_id project-id
           :name name
           :version version
           :description description
           :long_description long-description
           :license license
           :homepage_url homepage-url)))
  (let ((system (retrieve-one
                 (select :*
                   (from :system)
                   (where (:and (:= :project_id project-id)
                                (:= :name name)))
                   (limit 1))
                 :as 'system)))
    (dolist (author authors)
      (execute
       (insert-into :system_author
         (set= :system_id (system-id system)
               :author_name author
               :type "author"))))
    (dolist (maintainer maintainers)
      (execute
       (insert-into :system_author
         (set= :system_id (system-id system)
               :author_name maintainer
               :type "maintainer"))))
    system))

@export
(defun create-dependency (system-id depends-system-id)
  (execute
   (insert-into :system_dependencies
     (set= :system_id system-id
           :depends_system_id depends-system-id))))

@export
(defun create-system-packages (system-id packages &key failed (error-log ""))
  (execute
   (insert-into :system_packages
     (set= :system_id system-id
           :packages (prin1-to-string packages)
           :failed failed
           :error_log (or error-log "")))))
