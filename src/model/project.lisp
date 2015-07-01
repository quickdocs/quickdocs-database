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
                :retrieve-all))
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
