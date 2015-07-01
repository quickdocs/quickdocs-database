#|
  This file is a part of quickdocs-database project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage quickdocs-database-asd
  (:use :cl :asdf))
(in-package :quickdocs-database-asd)

(defsystem quickdocs-database
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:datafly
               :sxql
               :cl-syntax-annot
               :cl-reexport)
  :components ((:module "src"
                :components
                ((:file "quickdocs-database" :depends-on ("model"))
                 (:module "model"
                  :components
                  ((:file "project" :depends-on ("system"))
                   (:file "system"))))))
  :description "Quickdocs database accessor collections"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
