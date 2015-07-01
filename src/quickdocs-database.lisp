(in-package :cl-user)
(defpackage quickdocs-database
  (:use :cl
        :sxql
        :quickdocs-database.model.project
        :quickdocs-database.model.system)
  (:import-from :datafly
                :retrieve-all))
(in-package :quickdocs-database)

(syntax:use-syntax :annot)

(cl-reexport:reexport-from :quickdocs-database.model.project)
(cl-reexport:reexport-from :quickdocs-database.model.system)

@export
(defun all-dist-projects (ql-dist-version)
  (retrieve-all
   (select :*
     (from :project)
     (where (:= :ql_dist_version ql-dist-version)))
   :as 'project))
