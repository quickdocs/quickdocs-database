(in-package :cl-user)
(defpackage quickdocs-database.model.system
  (:use :cl
        :cl-annot.class
        :sxql
        :quickdocs-database.preference)
  (:import-from :datafly
                :model
                :retrieve-all
                :retrieve-one
                :execute)
  (:import-from :datafly.inflate
                :tinyint-to-boolean
                :string-to-keyword
                :octet-vector-to-string))
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
               (where (:= :system_dependencies.system_id id))))
            (:has-many (dependees system)
             (select :system.*
               (from :system_dependencies)
               (left-join :system :on (:= :system_dependencies.system_id :system.id))
               (where (:= :system_dependencies.depends_system_id id))))
            (:has-many (authors system-author)
             (select :*
               (from :system_author)
               (where (:= :system_id id))))
            (:has-a (extracted-info system-extracted-info)
             (where (:= :system_id id)))
            (:inflate (description long-description license homepage-url)
             #'octet-vector-to-string))
  id
  project-id
  name
  version
  description
  long-description
  homepage-url
  license)
@export 'system-dependencies
@export 'system-dependees
@export 'system-authors
@export 'system-extracted-info

@export
(defun retrieve-system (name &key (ql-dist-version (preference "ql-dist-version")))
  (retrieve-one
   (select :*
     (from :system)
     (left-join :project :on (:= :system.project_id :project.id))
     (where (:and (:= :ql_dist_version ql-dist-version)
                  (:= :system.name name)))
     (limit 1))
   :as 'system))

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

@export-accessors
@export
@model
(defstruct (system-author (:inflate type #'string-to-keyword))
  id
  system-id
  author-name
  type)

@export
(defun system-author= (author1 author2)
  (and (eq (system-author-type author1)
           (system-author-type author2))
       (string=
        (system-author-author-name author1)
        (system-author-author-name author2))))

@export
(defun create-dependency (system-id depends-system-id &key is-for-defsystem)
  (execute
   (insert-into :system_dependencies
     (set= :system_id system-id
           :depends_system_id depends-system-id
           :is_for_defsystem (if is-for-defsystem 1 0)))))

@export
(defun create-system-extracted-info (system-id packages &key failed (error-log ""))
  (execute
   (insert-into :system_extracted_info
     (set= :system_id system-id
           :packages (prin1-to-string packages)
           :failed (if failed 1 0)
           :error_log (or error-log "")))))

@export-accessors
@export
@model
(defstruct (system-extracted-info (:inflate packages
                                   (lambda (packages)
                                     (ignore-errors (read-from-string packages))))
                                  (:inflate failed #'tinyint-to-boolean))
  packages
  failed
  error-log)
