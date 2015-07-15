(in-package :cl-user)
(defpackage quickdocs-database
  (:nicknames qd-db qd.db)
  (:use :cl
        :sxql
        :quickdocs-database.model.project
        :quickdocs-database.model.system)
  (:import-from :datafly
                :retrieve-all
                :execute))
(in-package :quickdocs-database)

(syntax:use-syntax :annot)

(cl-reexport:reexport-from :quickdocs-database.model.project)
(cl-reexport:reexport-from :quickdocs-database.model.system)
(cl-reexport:reexport-from :quickdocs-database.preference)

@export
(defun all-dist-projects (ql-dist-version)
  (retrieve-all
   (select :*
     (from :project)
     (where (:= :ql_dist_version ql-dist-version)))
   :as 'project))
