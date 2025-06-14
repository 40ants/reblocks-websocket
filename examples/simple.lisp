(uiop:define-package #:reblocks-websocket-examples/simple
  (:use #:cl)
  (:import-from #:reblocks-websocket
                #:in-thread
                #:websocket
                #:websocket-widget)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/routes
                #:page))
(in-package #:reblocks-websocket-examples/simple)


(defwidget counter-box (websocket-widget)
  ((counter :initform 0
            :accessor counter)))


(defmethod render ((widget COUNTER-BOX))
  (in-thread ("Update counter")
    (sleep 1)
    (incf (counter widget))
    (update widget))
  
  (reblocks/html:with-html ()
    (:p (format nil "Counter: ~A" (counter widget)))))


(defapp simple-demo
  :prefix "/"
  :routes ((page ("/")
             (make-instance 'counter-box))
           (websocket ("/websocket"))))


(defun run (&key (port 8080))
  (reblocks/server:start :port port))
