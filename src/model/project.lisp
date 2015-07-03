(in-package :cl-user)
(defpackage quickdocs-database.model.project
  (:use :cl
        :sxql
        :cl-annot.class)
  (:import-from :quickdocs-database.model.system
                :system)
  (:import-from :datafly
                :model
                :retrieve-one
                :retrieve-all
                :execute))
(in-package :quickdocs-database.model.project)

(syntax:use-syntax :annot)

@export-accessors
@model
(defstruct (project (:has-a (readme project-readme)
                     (where (:= :project_id id)))
                    (:has-many (systems system)
                     (select :*
                       (from :system)
                       (where (:= :project_id id)))))
  id
  ql-dist-version
  name
  release-version
  homepage-url
  repos-url
  archive-url)

@export
(defun project-cliki-description (project)
  (check-type project project)
  (getf (retrieve-one
         (select :description
           (from :project_cliki_description)
           (where (:= :project_id (project-id project)))
           (limit 1)))
        :description))

@export
(defun project-categories (project)
  (check-type project project)
  (mapcar (lambda (row)
            (getf row :category))
          (retrieve-all
           (select :category
             (from :project_category)
             (where (:= :project_name (project-name project)))))))

@export-accessors
@model
(defstruct project-readme
  project-id
  filename
  raw
  converted)

@export
(defun create-project-readme (project-id file converter)
  (check-type file pathname)
  (check-type converter function)
  (let* ((filename (file-namestring file))
         (raw (uiop:read-file-string file))
         (converted (funcall converter raw)))
    (execute
     (insert-into :project_readme
       (set= :project_id project-id
             :filename filename
             :raw raw
             :converted converted)))
    (make-project-readme
     :project-id project-id
     :filename filename
     :raw raw
     :converted converted)))

@export
(defun create-project (&key (ql-dist-version (ql-dist:version (ql-dist:dist "quicklisp")))
                         name
                         release-version
                         homepage-url
                         repos-url
                         archive-url

                         project-readme)
  (check-type project-readme project-readme)

  (execute
   (insert-into :project
     (set= :ql_dist_version ql-dist-version
           :name name
           :release_version release-version
           :homepage_url homepage-url
           :repos_url repos-url
           :archive_url archive-url)))
  (let ((project
          (retrieve-one
           (select :*
             (from :project)
             (where (:and (:= :ql_dist_version ql-dist-version)
                          (:= :name name)))
             (limit 1))
           :as 'project)))
    (when project-readme
      (execute
       (insert-into :project_readme
         (set= :project_id (project-id project)
               :filename (project-readme-filename project-readme)))))))
