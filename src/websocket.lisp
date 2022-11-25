(defpackage #:reblocks-websocket
  (:use #:cl)
  (:import-from #:reblocks/hooks
                #:call-next-hook)
  (:import-from #:ps
                #:symbol-to-js-string
                #:chain
                #:@)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:jonathan
                #:to-json)
  (:export
   #:websocket-widget
   #:*background*
   #:in-thread
   #:send-command
   #:send-script
   #:no-websocket-connection))
(in-package reblocks-websocket)


(defvar *uri* "/websocket")


;; TODO: Make a hook on webserver restart to reset this flag
(defvar *route-created* nil
  "This variable will be set to true when first WebSocket widget will be initialized.")


(defun on-message (ws message)
  (log:debug "Received websocket message" ws message)

  ;; (wsd:send ws (concatenate 'string
  ;;                           "pong "
  ;;                           message))
  )


(defun on-close (ws &key code reason)
  (log:debug "Websocket was closed" ws reason code))


(defun process-websocket (env)
  (log:debug "Processing websocket env")
  
  (handler-case (let ((ws (wsd:make-server env))
                      ;; Remember session to use it later in message processing
                      (session reblocks/session::*session*))

                  (log:debug "Created websocket server" ws)
                  ;; Bind websocket server to user's session.
                  ;; This way we'll be able to send him commands
                  ;; from other pieces of the server-side code.
                  (log:debug "Storing websocket server in the session")
                  (setf (reblocks/session:get-value :websocket)
                        ws)

                  (wsd:on :message ws (lambda (message)
                                        (on-message ws message)))
                  (wsd:on :close ws (lambda (&rest args)
                                      (apply 'on-close
                                             ws
                                             args)))
    
                  (lambda (responder)
                    (declare (ignore responder))
                    (log:info "Websocket responder was called" ws)
                    (wsd:start-connection ws)))
    (error ()
      (log:error "Unable to handle websocket.")
      (list 500
            (list :content-type "plain/text")
            (list "Unable to handle websocket")))))



(defclass websocket-route (routes:route)
  ())



(defun make-websocket-route (uri)
  "Makes a route for websocket handle.

Automatically adds a prefix depending on current webapp and widget."

  (let ((route (make-instance 'websocket-route
                              :template (routes:parse-template uri))))
    (reblocks/routes::add-route route)))


(reblocks/widget:defwidget websocket-widget ()
  ())


(defmethod initialize-instance ((widget websocket-widget) &rest initargs)
  (declare (ignorable initargs))

  (call-next-method)

  (unless *route-created*
    (make-websocket-route *uri*)
    (setf *route-created* t)))


(defun make-websocket-client-code ()
  (reblocks-parenscript:make-dependency*
   `(flet ((on-open ()
             (console.log "Connection was opened")
             (setf (@ window websocket_connected)
                   t)
             ((@ this send) "connected"))
           (on-close (event)
             (if (@ event was-clean)
                 (console.log "Connection closed cleanly")
                 (console.log "Connection was interrupted"))
             (console.log (+ "Code: " (ps:@ event code)
                             " reason: " (ps:@ event reason))))
           (on-message (message)
             (console.log "Message received: " message)
             (let* ((data ((@ -J-S-O-N parse)
                           (@ message data)))
                    (dirty-widgets (@ data widgets))
                    (method (@ data method)))
               (if method
                   (chain window
                          (process-command data))
                   (update-element-for-tree (widgets-json-to-tree dirty-widgets)))))
           (on-error (error)
             (console.log (+ "Error: " error)))
           (connect (url)
             (console.log "Connecting to: " url)
             (let* ((full-url (+ (if (equal window.location.protocol "http:")
                                     "ws://"
                                     "wss://")
                                 window.location.host
                                 url))
                    (socket (ps:new (-web-socket full-url))))
               (setf (ps:@ socket onopen)
                     on-open
                     (ps:@ socket onclose)
                     on-close
                     (ps:@ socket onmessage)
                     on-message
                     (ps:@ socket onerror)
                     on-error)
               socket)))
      (unless (ps:@ window websocket_connected)
        (connect ,*uri*)))))


(defvar *js-dependency* nil
  "Cache for js dependency.

We have to have a cached instance. Otherwise it will be slightly different
for each widget, because of symbols autogenerated by Parenscript.")


(defmethod reblocks/dependencies:get-dependencies ((widget websocket-widget))
  (log:debug "Returning dependencies for" widget)
  (unless *js-dependency*
    (setf *js-dependency*
          (make-websocket-client-code)))
  
  (append (list *js-dependency*)
          (call-next-method)))


(defvar *background* nil
  "This variable becomes t during background processing.")


(define-condition no-websocket-connection (error)
  ())


(defun send-script (script)
  "Sends JS script to frontend via Websocket."
  (let* ((script1 (etypecase script
                    (string script)
                    (list (ps:ps* script))))
         (script2 (reblocks/js/base:with-javascript-to-string script1))
         ;; (script (format nil
         ;;                 "<script type=text/javascript>\\n// <![CDATA[\\n~A;\\n// ]]>\\n</script>"
         ;;                 script))
         (payload (list :|jsonrpc| "2.0"
                        :|method| "executeCode"
                        :|params|
                        (list :|code| script2)))
         (json-payload (jonathan:to-json payload))
         (ws (reblocks/session:get-value :websocket)))

    (log:debug "Created" payload json-payload)
    ;; TODO: replace wsd:send with some sort of queue
    ;;       where data will be stored in case if connect
    ;;       was interrupted.
    (if ws
        (websocket-driver:send ws json-payload)
        (error 'no-websocket-connection))))


(defmethod reblocks/widget:update ((widget websocket-widget)
                                   &key
                                   inserted-after
                                   inserted-before)
  (declare (ignorable inserted-before inserted-after))
  (log:debug "Websocket widget update" *background*)

  (if *background*
      (reblocks/dependencies:with-collected-dependencies
        (let* ((rendered-widget (reblocks/html:with-html-string
                                  (reblocks/widget:render widget)))
               (dom-id (alexandria:make-keyword
                        (reblocks/widgets/dom:dom-id widget)))
               (payload (list :|widgets| (list dom-id
                                               rendered-widget)))
               (json-payload (jonathan:to-json payload))
               (ws (reblocks/session:get-value :websocket)))

          (log:debug "Created" payload json-payload)
          ;; TODO: replace wsd:send with some sort of queue
          ;;       where data will be stored in case if connect
          ;;       was interrupted.

          (if ws
              (progn
                (let ((payload-length (length json-payload)))
                  (log:debug "Payload length" payload-length))
                (websocket-driver:send ws json-payload))
              ;; (log:warn "No websocket connection")
              )))

      (call-next-method)))



(defun send-command (method-name &rest args)
  (let* ((ws (reblocks/session:get-value :websocket))
         ;; We need to preprocess command arguments and
         ;; to transfrom their names from :foo-bar to :|fooBar| form
         (prepared-args (loop for (key value) on args by #'cddr
                              appending (list (make-keyword (symbol-to-js-string key))
                                              value)))
         (payload (list :|method| (symbol-to-js-string method-name)
                        :|params| prepared-args))
         (json-payload (to-json payload)))
    (websocket-driver:send ws json-payload)))


(defmethod reblocks/routes:serve ((route websocket-route) env)
  (process-websocket env))


;; (defun let-bindings (&rest args)

;;   (remove-if #'null args))


;; (defmacro test-thread (&body body)
;;   (let* ((woo-package (find-package :woo))
;;          (ev-loop-symbol (when woo-package
;;                            (alexandria:ensure-symbol '*evloop*
;;                                                      :woo))))
;;     `(let* ,(let-bindings
;;              '(session weblocks.session::*session*)
;;              (when woo-package
;;                (list 'evloop ev-loop-symbol))
;;              'stop-thread)

;;        ,@body)))

;; (test-thread
;;   (princ "foo-bar"))


(defmacro in-thread ((thread-name) &body body)
  "Starts give piece of code in named thread, ensiring that reblocks/session::*session* and
reblocks/request:*request* will be bound during it's execution.

Also, it set weblocks.websocket:*backround* to true, to make `update' method distinguish
between usual request processing and background activity."
  
  (flet ((let-bindings (&rest args)
           "Returns a list or given arguments while removing nils.
            Suitable to form let's bind in macroses."
           (remove-if #'null args)))
    
    (let* ((woo-package (find-package :woo))
           (ev-loop-symbol (when woo-package
                             (alexandria:ensure-symbol '*evloop*
                                                       :woo))))
      `(let* ,(let-bindings
               '(session reblocks/session::*session*)
               '(request reblocks/request::*request*)
               (when woo-package
                 (list 'evloop ev-loop-symbol)))
         ;; Here we need to drop this header if it exists,
         ;; to make ajax-request-p return false for subsequent calls
         ;; in the thread.
         (when (reblocks/request:get-header "X-Requested-With"
                                            :request request)
           (setf request
                 (reblocks/request:remove-header "X-Requested-With"
                                                 :request request)))
     
         (bt:make-thread (lambda ()
                           (let ,(let-bindings
                                  '(reblocks/session::*session* session)
                                  '(reblocks/request::*request* request)
                                  ;; Hack
                                  (when woo-package
                                    (list ev-loop-symbol 'evloop))
                                  '(*background* t))
                             ,@body))
                         :name ,thread-name)))))


(defmacro in-thread-loop ((thread-name) &body body)
  "Starts give piece of code in named thread, ensiring that reblocks/session::*session* and
reblocks/request:*request* will be bound during it's execution.

Also, it set weblocks.websocket:*backround* to true, to make `update' method distinguish
between usual request processing and background activity."
  
  (flet ((let-bindings (&rest args)
           "Returns a list or given arguments while removing nils.
            Suitable to form let's bind in macroses."
           (remove-if #'null args)))
    
    (let* ((woo-package (find-package :woo))
           (ev-loop-symbol (when woo-package
                             (alexandria:ensure-symbol '*evloop*
                                                       :woo))))
      `(let* ,(let-bindings
               '(session reblocks/session::*session*)
               '(request reblocks/request::*request*)
               (when woo-package
                 (list 'evloop ev-loop-symbol))
               'stop-thread)
         ;; Here we need to drop this header if it exists,
         ;; to make ajax-request-p return false for subsequent calls
         ;; in the thread.
         (when (reblocks/request:get-header "X-Requested-With"
                                            :request request)
           (setf request
                 (reblocks/request:remove-header "X-Requested-With"
                                                 :request request)))
     
         (bt:make-thread (lambda ()
                           (loop
                             while (not stop-thread)
                             do (let ,(let-bindings
                                       '(reblocks/session::*session* session)
                                       '(reblocks/request::*request* request)
                                       ;; Hack
                                       (when woo-package
                                         (list ev-loop-symbol 'evloop))
                                       '(*background* t))
                                  (log:info "Yet another loop inside" ,thread-name)
                                  ,@body)))
                         :name ,thread-name)

         (reblocks/hooks:on-application-hook-stop-reblocks
           stop-thread ()
           (setf stop-thread t))
     
         (reblocks/hooks:on-application-hook-reset-session
           stop-thread (session)
           ;; TODO: make weblocks use this declaration.
           ;;       right now compiler issues a warning about unused
           ;;       session variable here.
           (declare (ignorable session))
           (setf stop-thread t))))))


(reblocks/hooks:on-application-hook-stop-reblocks
    reset-websocket-route ()
  (call-next-hook)
  ;; Resetting the flag to recreate a route on next start.
  (setf *route-created* nil))


(reblocks/hooks:on-application-hook-stop-reblocks
    reset-js-code-cache ()
  (reblocks/hooks:call-next-hook)
  (setf *js-dependency* nil))
