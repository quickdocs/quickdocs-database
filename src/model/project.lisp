(in-package :cl-user)
(defpackage quickdocs-database.model.project
  (:use :cl
        :sxql
        :cl-annot.class
        :quickdocs-database.preference)
  (:import-from :quickdocs-database.model.system
                :system
                :system-name
                :system-description
                :system-homepage-url)
  (:import-from :datafly
                :model
                :retrieve-one
                :retrieve-all
                :retrieve-one-value
                #+nil :retrieve-all-values
                :execute))
(in-package :quickdocs-database.model.project)

(syntax:use-syntax :annot)

@export-accessors
@export
@model
(defstruct (project (:has-a (readme project-readme)
                     (where (:= :project_id id)))
                    (:has-many (systems system)
                     (select :*
                       (from :system)
                       (where (:= :project_id id))))
                    (:inflate (repos-url archive-url) #'datafly.inflate:octet-vector-to-string))
  id
  ql-dist-version
  name
  release-version
  repos-url
  archive-url)
@export 'project-readme
@export 'project-systems

@export
(defun project-homepage-url (project)
  (loop for system in (project-systems project)
        when (system-homepage-url system)
          do (return (system-homepage-url system))))

@export
(defun project-primary-system (project)
  (flet ((remove-cl-prefix (str)
           (if (and (< 2 (length str))
                    (string= str "cl-" :end1 3))
               (subseq str 3)
               str)))
    (or (find (remove-cl-prefix (project-name project))
              (project-systems project)
              :key (lambda (system)
                     (remove-cl-prefix (system-name system)))
              :test #'string=)
        (first (project-systems project)))))

@export
(defun project-description (project)
  (let* ((system (project-primary-system project))
         (description (system-description system)))
    (when description
      (return-from project-description description))

    (let ((body (retrieve-one-value
                 (select :body
                   (from :cliki)
                   (where (:= :project_name (project-name project)))
                   (limit 1))
                 :body)))
      (when body
        (babel:octets-to-string body)))))

@export
(defun project-categories (project)
  (datafly.db:retrieve-all-values
   (select :category
     (from :cliki_project_category)
     (where (:= :project_name (project-name project))))
   :category))


@export
(defun retrieve-project (name &key (ql-dist-version (preference "ql-dist-version")))
  (retrieve-one
   (select :*
     (from :project)
     (where (:and (:= :ql_dist_version ql-dist-version)
                  (:= :name name)))
     (limit 1))
   :as 'project))

@export
(defun find-system-in-project (project system-name)
  (find system-name (project-systems project)
        :key #'system-name
        :test #'string=))

@export-constructors
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
                         repos-url
                         archive-url

                         project-readme)
  (check-type project-readme (or null project-readme))

  (execute
   (insert-into :project
     (set= :ql_dist_version ql-dist-version
           :name name
           :release_version release-version
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
               :filename (project-readme-filename project-readme)
               :raw (project-readme-raw project-readme)
               :converted (project-readme-converted project-readme)))))
    project))
