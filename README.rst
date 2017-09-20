====================
 weblocks-websocket
====================

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/weblocks-websocket.svg?branch=master
    :target: https://travis-ci.org/40ants/weblocks-websocket

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

This module allows you to push some information from backend to frontend
and. In this case, updates of widgets's state on the client are
initiated by server. For example, you can have some sort of long running
process on the server and need to show it's status to the user.


Usage
=====

Define you widget and inherit it from the
``weblocks.websocket:websocket-widget``:

.. code:: common-lisp
          
   (websocket:defwidget counter-box (weblocks.websocket:websocket-widget)
     ((counter :initform 0
               :accessor counter)))


Define a code which will start some sort of background activity. In this
example we are doing it right when widget was created in the beginning
of the user session, but of cause, you can do it as a reaction on an
action.



.. code:: common-lisp

   (defmethod initialize-instance ((instance counter-box) &rest restargs)
     (declare (ignorable restargs))
     (call-next-method)

     (weblocks.websocket:in-thread ("Update counter")
       (sleep 3)
       ;; Updating counter
       (incf (counter instance))
       (weblocks:mark-dirty instance)))

That is it. Define a render method as usual and use the widget on the
page. Counter will be updated automatically. This works like a magic,
a framework makes all dirty work under the hood.


TODO
====

* Make a client-side code to reconnect when connection was broken.
* Add some queue on the backend to store data during reconnects.
