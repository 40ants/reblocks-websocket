(uiop:define-package #:reblocks-websocket
  (:use #:cl)
  (:import-from #:reblocks/hooks
                #:call-next-hook)
  (:import-from #:ps
                #:symbol-to-js-string
                #:chain
                #:@)
  (:import-from #:alexandria
                #:assoc-value
                #:make-keyword)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:reblocks/page
                #:*current-page*
                #:page-id
                #:page-metadata
                #:current-page)
  (:import-from #:reblocks/page-dependencies
                #:with-collected-dependencies)
  (:import-from #:reblocks/html
                #:with-html-string)
  (:import-from #:reblocks/widgets/dom
                #:dom-id)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:serapeum
                #:fmt)
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

  (when (string-equal message "ping")
    (wsd:send ws "pong")))


(defun (setf current-websocket) (ws)
  (log:debug "Storing websocket server in the session")
  (setf (page-metadata (current-page) :websocket)
        ws)
  (values))


(defun current-websocket ()
  (page-metadata (current-page) :websocket))


(defun on-close (ws &key code reason)
  (log:debug "Websocket was closed" ws reason code)
  (setf (current-websocket)
        nil))


(defun process-websocket (env)
  (log:debug "Processing websocket env")
  
  (handler-bind
      ((error (lambda (err)
                (log:error "Unable to handle websocket.")
                (if (reblocks/debug:status)
                    (invoke-debugger err)
                    (return-from process-websocket
                      (list 500
                            (list :content-type "plain/text")
                            (list "Unable to handle websocket")))))))
      (with-log-unhandled ()
        (let* ((ws (wsd:make-server env))
               ;; Remember session to use it later in message processing
               ;; (session reblocks/session::*session*)
               (request (lack.request:make-request env))
               (params (lack.request:request-parameters request))
               (page-id (assoc-value params "page-id"
                                     :test #'string-equal))
               (page (reblocks/page:get-page-by-id page-id))
               (*current-page* page))
          
          (flet ((on-message-handler (message)
                   (let ((*current-page* page))
                     (on-message ws message)))
                 (on-close-handler (&rest args)
                   (let ((*current-page* page))
                     (apply 'on-close
                            ws
                            args)))
                 (request-handler (responder)
                   (declare (ignore responder))
                   (log:info "Websocket responder was called" ws)
                   (wsd:start-connection ws)))
            (log:debug "Created websocket server" ws)
            ;; Bind websocket server to user's session.
            ;; This way we'll be able to send him commands
            ;; from other pieces of the server-side code.
            (setf (current-websocket)
                  ws)
            
            (wsd:on :message ws #'on-message-handler)
            (wsd:on :close ws #'on-close-handler)

            #'request-handler)))))



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


(defun make-websocket-client-code (&key (ping-interval 5))
  (reblocks-parenscript:make-dependency*
   `(let ((saved-page-id nil)
          (connected nil)
          (socket nil)
          (ping-timer nil)
          (ping-timeout-timer nil))
      (flet ((ping ()
               (when socket
                 (ps:chain socket
                           (send "ping"))
                 
                 (clear-ping-timeout-timer)

                 ;; If we don't receive a response in ping-interval - 1 seconds,
                 ;; then we'll reconnect
                 (setf ping-timeout-timer
                       (set-timeout on-ping-timeout ,(* (- ping-interval 1)
                                                        1000)))))
             (clear-ping-timeout-timer ()
               (when ping-timeout-timer
                 (clear-timeout ping-timeout-timer))
               (setf ping-timeout-timer nil))
             (on-ping-timeout ()
               (chain console
                      (log "Reconnecting because of ping timeout"))
               (connect-to-websocket saved-page-id))
             (on-open ()
               (chain console
                      (log "Connection was opened"))
               (setf connected t)
               (clear-ping-timeout-timer)
               
               ((@ this send) "connected")
               
               (unless ping-timer
                 (setf ping-timer
                       (set-interval ping ,(* ping-interval 1000)))))
             (on-close (event)
               (setf connected nil)
               (cond
                 ((@ event was-clean)
                  (chain console
                         (log "Connection closed cleanly")))
                 (t
                  (chain console
                         (log "Connection was interrupted, reconnecting"))
                  (connect-to-websocket saved-page-id)))
               (chain console
                      (log (+ "Code: " (ps:@ event code)
                              " reason: " (ps:@ event reason)))))
             (on-message (message)
               (chain console
                      (log "Message received: " message))
               (cond
                 ((= (@ message data) "pong")
                  (clear-ping-timeout-timer))
                 (t
                  (let* ((data ((@ -J-S-O-N parse)
                                (@ message data)))
                         (dirty-widgets (@ data widgets))
                         (method (@ data method)))
                    (if method
                        (chain window
                               (process-command data))
                        (update-element-for-tree (widgets-json-to-tree dirty-widgets)))))))
             (on-error (error)
               (chain console
                      (log (+ "Error: " error))))
             (connect-to-websocket (page-id)
               (unless connected
                 (setf saved-page-id page-id)
                 
                 (let* ((full-url (+ (if (equal (@ window location protocol) "http:")
                                         "ws://"
                                         "wss://")
                                     (@ window location host)
                                     ,*uri*
                                     "?page-id="
                                     page-id)))
                   (chain console
                          (log "Connecting to: " full-url))
                   
                   (setf socket
                         (ps:new (-web-socket full-url)))
                   
                   (setf (ps:@ socket onopen)
                         on-open
                         (ps:@ socket onclose)
                         on-close
                         (ps:@ socket onmessage)
                         on-message
                         (ps:@ socket onerror)
                         on-error)
                   socket))))
        (setf (ps:@ window connect-to-websocket)
              connect-to-websocket)))))


(defparameter *js-dependency* nil
  "Cache for js dependency.

We have to have a cached instance. Otherwise it will be slightly different
for each widget, because of symbols autogenerated by Parenscript.")


(defmethod reblocks/dependencies:get-dependencies ((widget websocket-widget))
  (log:debug "Returning dependencies for" widget)
  ;; TODO: enable this again
  ;; (unless *js-dependency*
  ;;   (setf *js-dependency*
  ;;         (make-websocket-client-code)))
  
  (list* ;; (list *js-dependency*)
   (make-websocket-client-code)
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
         (ws (current-websocket)))

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
      (with-collected-dependencies
        (let* ((rendered-widget (with-html-string
                                  (reblocks/widget:render widget)))
               (dom-id (alexandria:make-keyword
                        (dom-id widget)))
               (payload (list :|widgets| (list dom-id
                                               rendered-widget)))
               (json-payload (jonathan:to-json payload))
               (ws (current-websocket)))

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


(defmethod reblocks/widget:render :after ((widget websocket-widget))
  (unless *background*
    (reblocks/response:send-script
     `(ps:chain window
                (connect-to-websocket ,(fmt "~A" (page-id (current-page))))))))


(defun send-command (method-name &rest args)
  (let* ((ws (current-websocket))
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
               '(page reblocks/page::*current-page*)
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

         (log:debug "Creating a thread to update state via websocket")
         (bt:make-thread (lambda ()
                           (let ,(let-bindings
                                  '(reblocks/session::*session* session)
                                  '(reblocks/request::*request* request)
                                  '(reblocks/page::*current-page* page)
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
