(defsystem weblocks-websocket
  :version (:read-file-form "version.lisp-expr")
  :author ""
  :license ""
  :depends-on (
               (:version :weblocks "0.28.0")
               :weblocks-parenscript
               :websocket-driver)
  :components ((:module "src"
                :components
                ((:file "websocket"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.rst"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq)
                (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op weblocks-websocket-test))))

