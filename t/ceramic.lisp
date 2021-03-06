(in-package :cl-user)
(defpackage ceramic-test
  (:use :cl :fiveam)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :wipe-data)
  (:export :run-tests))
(in-package :ceramic-test)

(defun run-tests ()
  (let* ((*ceramic-directory* (asdf:system-relative-pathname :ceramic-test
                                                             #p"t/ceramic/"))
         (ceramic.log:*logging* t))
    (run! 'ceramic-test.electron.tools:electron-tools)
    (run! 'ceramic-test.electron:electron-driver)
    (run! 'ceramic-test.setup:setup)
    (run! 'ceramic-test.integration:integration)
    (run! 'ceramic-test.misc:misc)
    ;; Cleanup
    (ceramic.file:wipe-data)))
