(defsystem ceramic
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "http://ceramic.github.io/"
  :bug-tracker "https://github.com/ceramic/ceramic/issues"
  :source-control (:git "git@github.com:ceramic/ceramic.git")
  :depends-on (:trivial-download
               :trivial-extract
               :trivial-exe
               :trivial-build
               :uiop
               :archive
               :zip
               :jonathan
               :cl-json
               :external-program
               :bordeaux-threads
               :uuid
               :cl-fad
               :clack-handler-hunchentoot
               #-(or win32 mswindows)
               :osicat)
  :components ((:module "src"
                :serial t
                :components
                ((:file "util")
                 (:file "error")
                 (:file "os")
                 (:file "file")
                 (:file "logging")
                 (:file "runtime")
                 (:file "resource")
                 (:module "electron"
                  :serial t
                  :components
                  ((:file "tools")
                   (:static-file "main.js")
                   (:file "driver")))
                 (:file "setup")
                 #-quicklisp
                 (:file "ql-patch")
                 #+quicklisp
                 (:file "bundler")
                 (:file "ceramic"))))
  :description "Common Lisp web apps on the desktop"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ceramic-test))))
