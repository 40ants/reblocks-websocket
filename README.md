<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# reblocks-websocket - Reblocks extension adding a bidirectional communication via Websocket.

<a id="reblocks-websocket-asdf-system-details"></a>

## REBLOCKS-WEBSOCKET ASDF System Details

* Version: 0.4.0

* Description: Reblocks extension allowing to add a bidirectional communication via Websocket between a backend and Reblocks widgets.

* Licence: Unlicense

* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>

* Homepage: [https://40ants.com/reblocks-websocket/][6f15]

* Bug tracker: [https://github.com/40ants/reblocks-websocket/issues][e6f2]

* Source control: [GIT][b1a1]

* Depends on: [alexandria][8236], [bordeaux-threads][3dbf], [jonathan][6dd8], [log4cl-extras][691c], [parenscript][7921], [reblocks][184b], [reblocks-parenscript][c07c], [serapeum][c41d], [websocket-driver][4f50]

[![](https://github-actions.40ants.com/40ants/reblocks-websocket/matrix.svg?only=ci.run-tests)][0497]

![](http://quickdocs.org/badge/reblocks-websocket.svg)

This module allows you to push some information from backend to frontend
and. In this case, updates of widgets's state on the client are
initiated by server. For example, you can have some sort of long running
process on the server and need to show it's status to the user.

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

This library depends on Reblocks (Weblocks fork) and a websocket-driver.
If you will use Woo server then probably you'll need this fork of the websocket-driver
([make-woo-work-from-separate-threads][ef5d] branch).
However, may be the recent version of websocket-driver will work just find, I don't know.

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :reblocks-websocket)
```
<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

Define you widget and inherit it from the
[`reblocks-websocket:websocket-widget`][c36a]:

```lisp
(reblocks:defwidget counter-box (reblocks-websocket:websocket-widget)
  ((counter :initform 0
            :accessor counter)))
```
Define a code which will start some sort of background activity. In this
example we are doing it right when widget was created in the beginning
of the user session, but of cause, you can do it as a reaction on an
action.

```lisp
(defmethod initialize-instance ((instance counter-box) &rest restargs)
  (declare (ignorable restargs))
  (call-next-method)

  (reblocks-websocket:in-thread ("Update counter")
    (sleep 3)
    ;; Updating counter
    (incf (counter instance))
    (reblocks:update instance)))
```
That is it. Define a render method as usual and use the widget on the
page. Counter will be updated automatically. This works like a magic,
a framework makes all dirty work under the hood.

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-40REBLOCKS-WEBSOCKET-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### REBLOCKS-WEBSOCKET

<a id="x-28-23A-28-2818-29-20BASE-CHAR-20-2E-20-22REBLOCKS-WEBSOCKET-22-29-20PACKAGE-29"></a>

#### [package](61dd) `reblocks-websocket`

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-WEBSOCKET-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-40REBLOCKS-WEBSOCKET-24NO-ACTIVE-WEBSOCKETS-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### NO-ACTIVE-WEBSOCKETS

<a id="x-28REBLOCKS-WEBSOCKET-3ANO-ACTIVE-WEBSOCKETS-20CONDITION-29"></a>

###### [condition](e294) `reblocks-websocket:no-active-websockets` (error)

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-40REBLOCKS-WEBSOCKET-24WEBSOCKET-WIDGET-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### WEBSOCKET-WIDGET

<a id="x-28REBLOCKS-WEBSOCKET-3AWEBSOCKET-WIDGET-20CLASS-29"></a>

###### [class](2ecb) `reblocks-websocket:websocket-widget` (widget)

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-WEBSOCKET-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28REBLOCKS-WEBSOCKET-3ASEND-COMMAND-20FUNCTION-29"></a>

##### [function](bcc1) `reblocks-websocket:send-command` method-name &rest args

<a id="x-28REBLOCKS-WEBSOCKET-3ASEND-SCRIPT-20FUNCTION-29"></a>

##### [function](d993) `reblocks-websocket:send-script` script

Sends `JS` script to frontend via Websocket.

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-WEBSOCKET-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28REBLOCKS-WEBSOCKET-3AIN-THREAD-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](b6b7) `reblocks-websocket:in-thread` (thread-name) &body body

Starts given piece of code in named thread, ensiring that reblocks/session::*session* and
reblocks/request:*request* will be bound during it's execution.

Also, it set reblocks.websocket:*backround* to true, to make `update' method distinguish
between usual request processing and background activity.

<a id="x-28REBLOCKS-WEBSOCKET-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-WEBSOCKET-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-28REBLOCKS-WEBSOCKET-3A-2ABACKGROUND-2A-20-28VARIABLE-29-29"></a>

##### [variable](4955) `reblocks-websocket:*background*` nil

This variable becomes t during background processing.


[6f15]: https://40ants.com/reblocks-websocket/
[c36a]: https://40ants.com/reblocks-websocket/#x-28REBLOCKS-WEBSOCKET-3AWEBSOCKET-WIDGET-20CLASS-29
[b1a1]: https://github.com/40ants/reblocks-websocket
[0497]: https://github.com/40ants/reblocks-websocket/actions
[61dd]: https://github.com/40ants/reblocks-websocket/blob/a8d48fc6f4785e080a3cb46f59cd684eb911cd9c/src/websocket.lisp#L1
[2ecb]: https://github.com/40ants/reblocks-websocket/blob/a8d48fc6f4785e080a3cb46f59cd684eb911cd9c/src/websocket.lisp#L227
[4955]: https://github.com/40ants/reblocks-websocket/blob/a8d48fc6f4785e080a3cb46f59cd684eb911cd9c/src/websocket.lisp#L388
[e294]: https://github.com/40ants/reblocks-websocket/blob/a8d48fc6f4785e080a3cb46f59cd684eb911cd9c/src/websocket.lisp#L392
[d993]: https://github.com/40ants/reblocks-websocket/blob/a8d48fc6f4785e080a3cb46f59cd684eb911cd9c/src/websocket.lisp#L427
[bcc1]: https://github.com/40ants/reblocks-websocket/blob/a8d48fc6f4785e080a3cb46f59cd684eb911cd9c/src/websocket.lisp#L447
[b6b7]: https://github.com/40ants/reblocks-websocket/blob/a8d48fc6f4785e080a3cb46f59cd684eb911cd9c/src/websocket.lisp#L519
[e6f2]: https://github.com/40ants/reblocks-websocket/issues
[ef5d]: https://github.com/svetlyak40wt/websocket-driver/tree/make-woo-work-from-separate-threads
[8236]: https://quickdocs.org/alexandria
[3dbf]: https://quickdocs.org/bordeaux-threads
[6dd8]: https://quickdocs.org/jonathan
[691c]: https://quickdocs.org/log4cl-extras
[7921]: https://quickdocs.org/parenscript
[184b]: https://quickdocs.org/reblocks
[c07c]: https://quickdocs.org/reblocks-parenscript
[c41d]: https://quickdocs.org/serapeum
[4f50]: https://quickdocs.org/websocket-driver

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
