(in-package :cl-user)
(defpackage ceramic.electron
  (:use :cl :parenscript)
  (:import-from :ceramic.util
                :tell)
  (:import-from :ceramic.file
                :*ceramic-directory*)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:import-from :ceramic.electron.tools
                :binary-pathname
                :get-release
                :prepare-release)
  ;; Interface
  (:export :*electron-version*
           :release-directory
           :global-binary-pathame
           :setup)
  ;; Classes
  (:export :process)
  ;; Functions
  (:export :*binary-pathname*
           :start-process
           :eval-in-process))
(in-package :ceramic.electron)

(defclass process ()
  ((external-process :initarg :external-process :reader external-process)
   (app :reader process-app)))

(defmethod initialize-instance :after ((p process) &key)
  (with-slots (app) p
    (setf app (make-instance 'app :process p))))

(defgeneric output-stream (process)
  (:method ((p process))
    (external-program:process-output-stream (external-process p))))

(defgeneric input-stream (process)
  (:method ((p process))
    (external-program:process-input-stream (external-process p))))

(defclass app ()
  ((process :initarg :process :reader app-process)
   (windows :initform nil :accessor app-windows)))

(defclass window ()
  ((app :initarg :app :reader window-app)
   (name :initarg :name :reader window-name)))

(defun find-window (app window-name)
  (or (find window-name
            (app-windows app)
            :test #'string=
            :key #'window-name)
      (error "Unable to find window named ~a" window-name)))

(defmethod handle-event (target channel &rest args &key &allow-other-keys)
  (format *debug-io*
          "Unhandled ~a event for ~a: ~a"
          channel target args))

(alexandria:define-constant +magic-prefix+ "[CERAMIC-EVENT] " :test #'string=)

(defun parse-and-handle-event (process line)
  (flet ((keywordify (name)
           (check-type name string)
           (alexandria:make-keyword (string-upcase name))))
    (let* ((event (with-input-from-string (s line :start (length +magic-prefix+))
                    (cl-json:decode-json s)))
           (emitter (keywordify (cdr (assoc :emitter event))))
           (channel (keywordify (cdr (assoc :channel event))))
           (window-name (cdr (assoc :window-name event)))
           (args (cdr (assoc :args event))))
      (apply #'handle-event
             (ecase emitter
               (:process process)
               (:app (process-app process))
               (:window (find-window (process-app process) window-name)))
             channel
             (alexandria:alist-plist args)))))

;; { emitter: 'app',     channel: 'ready',    args: [] }
;; { emitter: 'window',  channel: 'closed',   windowName: ?, args: [] }
;; { emitter: 'process', channel: 'whatever', args: [] }
(defun start-event-dispatcher (process)
  (bt:start-multiprocessing)
  (let ((stream (output-stream process)))
    (bt:make-thread
     (lambda ()
       (loop for line = (read-line stream nil nil)
             until (null line)
             when (alexandria:starts-with-subseq +magic-prefix+ line)
               do (restart-case
                      (parse-and-handle-event process line)
                    (ignore ()
                      :report "Proceed, ignoring this event."))))
     :name (format nil "Electron event dispacher~@[ (pid: ~a)~]"
                   ;; Alas, PROCESS-ID is not implemented for all Lisps.
                   (ignore-errors (external-program:process-id process))))))

(defun start-process (directory &key operating-system)
  "Start an Electron process, returning the process object."
  (let* ((binary-pathname (binary-pathname directory
                                           :operating-system operating-system))
         (external-process (external-program:start binary-pathname
                                                   (list)
                                                   :input :stream
                                                   :output :stream))
         (process (make-instance 'process :external-process external-process)))
    (start-event-dispatcher process)
    process))

;; figure out a way to do return values (maybe use stderr)
(defun send-request (stream target window-name code)
  (cl-json:encode-json
   `((:target . ,target)
     (:window-name . ,window-name)
     (:code . ,code))
   stream)
  (fresh-line stream)
  (finish-output stream))

(defun reval* (target code)
  (etypecase target
    (app
     (send-request (input-stream (app-process target))
                   :app nil code))
    (window
     ;; windows[request.windowName].webContents.executeJavascript(request.code)
     ;; require('crash-reporter').start();
     (send-request (input-stream (app-process (window-app target)))
                   :window (window-name target) code))))

(defmacro reval (target &body body)
  (let ((*ps-print-pretty* nil))
    `(reval* ,target ,(apply #'ps* body))))

;;; Commands

(defun create-window (app options)
  (reval app
    (let ((window (new (*browser-window (lisp options))))
          (id (lisp window-id)))
      (setf (@ windows id) window)
      ((@ window on) "closed" (lambda () (delete (@ windows id)))))))

(defun close-window (window)
  (reval app
    ((@ window (lisp (window-id window)) close))))

(defun destroy-window (window)
  (reval app
    ((@ window (lisp (window-id window)) destroy))))



(define-window-command close-window "close-window" ()
  ())

(define-window-command destroy-window "destroy-window" ()
  ())

(define-window-command send-message "send-message-to-window" (message)
  message)

(define-window-command show-window "show-window" ()
  ())

(define-window-command hide-window "hide-window" ()
  ())

(define-window-command resize-window "resize-window" (width height)
  (list (cons "width" width)
        (cons "height" height)))

(define-window-command focus-window "focus-window" ()
  ())

(define-window-command maximize-window "maximize-window" ()
  ())

(define-window-command unmaximize-window "unmaximize-window" ()
  ())

(define-window-command minimize-window "minimize-window" ()
  ())

(define-window-command unminimize-window "unminimize-window" ()
  ())

(define-window-command fullscreen-window "fullscreen-window" ()
  ())

(define-window-command unfullscreen-window "unfullscreen-window" ()
  ())

(define-window-command resizable-window "resizable-window" ()
  ())

(define-window-command unresizable-window "unresizable-window" ()
  ())

(define-window-command center-window "center-window" ()
  ())

(define-window-command set-window-position "set-window-position" (x y)
  (list (cons "x" x)
        (cons "y" y)))

(define-window-command set-window-title "set-window-title" (title)
  (list (cons "title" title)))

(define-window-command window-load-url "window-load-url" (url)
  (list (cons "url" url)))

(define-window-command window-reload "window-reload" ()
  ())

(define-window-command window-open-dev-tools "window-open-dev-tools" ()
  ())

(define-window-command window-close-dev-tools "window-close-dev-tools" ()
  ())

(define-window-command window-undo "window-undo" ()
  ())

(define-window-command window-redo "window-redo" ()
  ())

(define-window-command window-cut "window-cut" ()
  ())

(define-window-command window-copy "window-copy" ()
  ())

(define-window-command window-paste "window-paste" ()
  ())

(define-window-command window-delete "window-delete" ()
  ())

(define-window-command window-select-all "window-select-all" ()
  ())

(define-window-command window-eval "window-eval" (code)
  (list (cons "code" code)))

(defun quit (process)
  "End the Electron process."
  (send-command process "quit" nil))

;;; Interface

(defvar *electron-version* "0.36.8"
  "The version of Electron to use.")

(defun release-directory ()
  "Pathname to the local copy of the Electron release."
  (merge-pathnames #p"electron/" *ceramic-directory*))

(defun global-binary-pathname ()
  "The pathname to the downloaded Electron binary. Used for interactive
  testing."
  (binary-pathname (release-directory)
                   :operating-system *operating-system*))

(defun setup ()
  "Set up the Electron driver."
  (ensure-directories-exist (release-directory))
  (get-release (release-directory)
               :operating-system *operating-system*
               :architecture *architecture*
               :version *electron-version*)
  (prepare-release (release-directory) :operating-system *operating-system*))

(defun window-create ()
  (let ((stream (external-program:process-input-stream
                 (external-process ceramic::*process*))))
    (cl-json:encode-json
     `((:target . :app)
       (:code . ,(let ((*ps-print-pretty* nil))
                   (ps* `(let ((window (new (*browser-window #+nil options))))
                           (setf (getprop windows "foo") window)
                           (funcall (@ window on)
                                    "closed"
                                    (lambda ()
                                      (setf (getprop windows "foo") nil))))))))
     stream)
    (fresh-line stream)
    (finish-output stream)))

(defun window-load-url ()
  (let ((stream (external-program:process-input-stream
                 (external-process ceramic::*process*))))
    (cl-json:encode-json
     `((:target . :app)
       (:code . ,(let ((*ps-print-pretty* nil))
                  (ps* `(funcall (@ windows "foo" load-u-r-l)
                                 ,(format nil "data:text/html,~a"
                                          (quri:url-encode "<html><body>yow</body></html>")))))))
     stream)
    (fresh-line stream)
    (finish-output stream)))

(defun window-show ()
  (let ((stream (external-program:process-input-stream
                 (external-process ceramic::*process*))))
    (cl-json:encode-json
     `((:target . "app")
       (:code . ,(let ((*ps-print-pretty* nil))
                   (ps* `(funcall (@ windows "foo" show))))))
     stream)
    (fresh-line stream)
    (finish-output stream)))

(defun app-quit ()
  (let ((stream (external-program:process-input-stream
                 (external-process ceramic::*process*))))
    (cl-json:encode-json
     `((:target . "app")
       (:code . ,(let ((*ps-print-pretty* nil))
                   (ps* `(funcall (@ app quit))))))
     stream)
    (fresh-line stream)
    (finish-output stream)))

(defun throw-error ()
  (let ((stream (external-program:process-input-stream
                 (external-process ceramic::*process*))))
    (cl-json:encode-json
     `((:target . "app")
       (:code . ,(let ((*ps-print-pretty* nil))
                  (ps* `(throw (new (*error "wat")))))))
     stream)
    (finish-output stream)))

(defun window-close ()
  (let ((*PS-PRINT-PRETTY* nil))
    (ps* `(lambda (name) (funcall (getprop windows name 'close))))))

