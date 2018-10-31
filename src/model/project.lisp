(in-package :cl-user)
(defpackage quickdocs-database.model.project
  (:use :cl
        :sxql
        :cl-annot.class
        :quickdocs-database.preference)
  (:import-from :quickdocs-database.model.system
                :system
                :system-name
                :system-license
                :system-authors
                :system-author-type
                :system-author-author-name
                :system-author=
                :system-description
                :system-dependencies
                :system-dependees
                :system-homepage-url)
  (:import-from :datafly
                :model
                :retrieve-one
                :retrieve-all
                :retrieve-one-value
                #+nil :retrieve-all-values
                :execute)
  (:import-from :datafly.inflate
                :octet-vector-to-string)
  (:import-from :function-cache
                :defcached))
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
                    (:inflate (homepage-url repos-url archive-url) #'octet-vector-to-string))
  id
  ql-dist-version
  name
  release-version
  homepage-url
  repos-url
  archive-url)
@export 'project-readme
@export 'project-systems

@export
(defun project-homepage-url* (project)
  (with-slots (homepage-url) project
    (when homepage-url
      (return-from project-homepage-url* homepage-url)))
  (let ((homepage (retrieve-one-value
                   (select :homepage_url
                     (from :repos_info)
                     (where (:= :project_name (project-name project)))
                     (limit 1))
                   :homepage-url)))
    (when homepage
      (let ((homepage (babel:octets-to-string homepage)))
        (if (and (<= 4 (length homepage))
                 (string= "http" homepage :end2 4))
            homepage
            (format nil "http://~A" homepage))))))

@export
(defun project-systems* (project)
  (let* ((primary-system (project-primary-system project))
         (systems (and primary-system
                       (remove (system-name primary-system)
                               (project-systems project)
                               :test #'string=
                               :key #'system-name))))
    (when primary-system
      (cons primary-system
            systems))))

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
         (description (and system (system-description system))))
    (when description
      (return-from project-description description)))

  (let ((description (retrieve-one-value
                      (select :description
                        (from :repos_info)
                        (where (:= :project_name (project-name project)))
                        (limit 1))
                      :description)))
    (when description
      (return-from project-description description)))

  (let ((body (retrieve-one-value
               (select :body
                 (from :cliki)
                 (where (:= :project_name (project-name project)))
                 (limit 1))
               :body)))
    (when body
      (babel:octets-to-string body))))

@export
(defcached project-authors-all (project)
  (delete-duplicates
   (mapcan #'system-authors (project-systems* project))
   :test #'system-author=
   :from-end t))

@export
(defun project-authors (project)
  (mapcar #'system-author-author-name
          (remove-if-not (lambda (author)
                           (eq (system-author-type author) :author))
                         (project-authors-all project))))

@export
(defun project-maintainers (project)
  (mapcar #'system-author-author-name
          (remove-if-not (lambda (author)
                           (eq (system-author-type author) :maintainer))
                         (project-authors-all project))))

@export
(defun project-licenses (project)
  (delete-duplicates
   (remove nil
           (mapcar #'system-license (project-systems* project)))
   :test #'equal
   :from-end t))

@export
(defun project-categories (project)
  (datafly.db:retrieve-all-values
   (select :category
     (from :cliki_project_category)
     (where (:= :project_name (project-name project))))
   :category))

@export
(defun project-dependencies (project)
  (retrieve-all
   (select :project.*
     (from :system_dependencies)
     (left-join :system :on (:= :system_dependencies.depends_system_id :system.id))
     (left-join :project :on (:= :system.project_id :project.id))
     (where (:and (:!= :project_id (project-id project))
                  (:in :system_dependencies.system_id
                       (select :id
                         (from :system)
                         (where (:= :project_id (project-id project)))))))
     (group-by :project.name))
   :as 'project))

@export
(defun project-dependees (project)
  (retrieve-all
   (select :project.*
     (from :system_dependencies)
     (left-join :system :on (:= :system_dependencies.system_id :system.id))
     (left-join :project :on (:= :system.project_id :project.id))
     (where (:and (:!= :project_id (project-id project))
                  (:in :system_dependencies.depends_system_id
                       (select :id
                         (from :system)
                         (where (:= :project_id (project-id project)))))))
     (group-by :project.name))
   :as 'project))

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
                         homepage-url
                         repos-url
                         archive-url

                         project-readme)
  (check-type project-readme (or null project-readme))

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
               :filename (project-readme-filename project-readme)
               :raw (project-readme-raw project-readme)
               :converted (project-readme-converted project-readme)))))
    project))
