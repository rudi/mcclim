;;; -*- Mode: Lisp; Package: CLIM-ABCL; -*-

;;;  (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :clim-abcl)

(defclass abcl-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass abcl-port (basic-port)
  ((id)
   (pointer :accessor port-pointer :initform (make-instance 'abcl-pointer))
   (window :initform nil :accessor abcl-port-window)))

(defun parse-abcl-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :abcl :port-type) 'abcl-port)
(setf (get :abcl :server-path-parser) 'parse-abcl-server-path)

(defmethod initialize-instance :after ((port abcl-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "ABCL-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'abcl-frame-manager :port port)
	(slot-value port 'climi::frame-managers)))

(defmethod print-object ((object abcl-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defmethod port-set-mirror-region ((port abcl-port) mirror mirror-region)
  ())

(defmethod port-set-mirror-transformation
    ((port abcl-port) mirror mirror-transformation)
  ())

(defmethod realize-mirror :around ((port abcl-port) sheet)
  (let* ((mirror (call-next-method))
         (parent (sheet-parent sheet))
         (parent-mirror (when parent (sheet-mirror parent))))
    (setf (sheet-direct-mirror sheet) mirror)
    (when (and mirror parent-mirror (java:java-object-p parent-mirror))
      ;; KLUDGE: I'm sure this will break horribly RSN - factor out into
      ;; its own gf once we discover parents with differently-named
      ;; methods for adding children.  Also, currently grafts are
      ;; mirrored to (gensym), which is why we test that the mirror of
      ;; the parent is actually a java object.
      (java:jcall "add" parent-mirror mirror))
    mirror))

(defmethod realize-mirror ((port abcl-port) (sheet mirrored-sheet-mixin))
  (error "Unimplemented realize-mirror for ~A" sheet))

(defmethod realize-mirror ((port abcl-port) (sheet vrack-pane))
  (let* ((pane (java:jnew "javax.swing.JTable"))
         (layout (java:jnew "javax.swing.BoxLayout" pane
                            (java:jfield "javax.swing.BoxLayout" "Y_AXIS"))))
    (java:jcall "setLayout" pane layout)
    pane))

(defmethod realize-mirror ((port abcl-port) (sheet label-pane))
  (java:jnew "javax.swing.JLabel" (climi::label-pane-label sheet)))

(defmethod realize-mirror ((port abcl-port) (sheet climi::menu-bar))
  nil)

(defmethod realize-mirror ((port abcl-port) (sheet top-level-sheet-pane))
  (java:jnew "javax.swing.JFrame" (frame-pretty-name (pane-frame sheet))))

(defmethod destroy-mirror ((port abcl-port) (sheet mirrored-sheet-mixin))
  ())

(defmethod mirror-transformation ((port abcl-port) mirror)
  ())


(defmethod port-set-sheet-region ((port abcl-port) (graft graft) region)
  ())

;; these don't exist
;;;(defmethod port-set-sheet-transformation
;;;    ((port abcl-port) (graft graft) transformation)
;;;  ())
;;;
;;;(defmethod port-set-sheet-transformation
;;;    ((port abcl-port) (sheet mirrored-sheet-mixin) transformation)
;;;  ())

(defmethod port-set-sheet-region
    ((port abcl-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region))
  nil)

(defmethod port-enable-sheet ((port abcl-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port abcl-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port abcl-port))
  nil)

(defmethod port-motion-hints ((port abcl-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod (setf port-motion-hints)
    (value (port abcl-port) (sheet mirrored-sheet-mixin))
  value)

(defmethod get-next-event
    ((port abcl-port) &key wait-function (timeout nil))
  (declare (ignore wait-function timeout))
  nil)

(defmethod make-graft ((port abcl-port)
                       &key (orientation :default) (units :device))
  (make-instance 'abcl-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port abcl-port) sheet)
  (make-instance 'abcl-medium :sheet sheet))

(defmethod text-style-mapping
    ((port abcl-port) text-style &optional character-set)
  (declare (ignore text-style character-set))
  nil)

(defmethod (setf text-style-mapping)
    (font-name (port abcl-port)
    (text-style text-style) &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

(defmethod port-character-width ((port abcl-port) text-style char)
  (declare (ignore text-style char))
  nil)

(defmethod port-string-width ((port abcl-port) text-style string &key (start 0) end)
  (declare (ignore text-style string start end))
  nil)

(defmethod port-mirror-width ((port abcl-port) sheet)
  (declare (ignore sheet))
  nil)

(defmethod port-mirror-height ((port abcl-port) sheet)
  (declare (ignore sheet))
  nil)

(defmethod graft ((port abcl-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port abcl-port) sheet width height)
  (declare (ignore sheet width height))
  ;; FIXME: this isn't actually good enough; it leads to errors in
  ;; WITH-OUTPUT-TO-PIXMAP
  nil)

(defmethod port-deallocate-pixmap ((port abcl-port) pixmap)
  nil)

(defmethod pointer-position ((pointer abcl-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer abcl-pointer))
  nil)

(defmethod port-modifier-state ((port abcl-port))
  nil)

(defmethod synthesize-pointer-motion-event ((pointer abcl-pointer))
  nil)

(defmethod port-frame-keyboard-input-focus ((port abcl-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus)
    (focus (port abcl-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod (setf port-keyboard-input-focus) (focus (port abcl-port))
  focus)

(defmethod port-keyboard-input-focus ((port abcl-port))
  nil)

(defmethod port-force-output ((port abcl-port))
  nil)

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?
(defmethod port-grab-pointer ((port abcl-port) pointer sheet)
  (declare (ignore pointer sheet))
  nil)

(defmethod port-ungrab-pointer ((port abcl-port) pointer sheet)
  (declare (ignore pointer sheet))
  nil)

(defmethod distribute-event :around ((port abcl-port) event)
  (declare (ignore event))
  nil)

(defmethod set-sheet-pointer-cursor ((port abcl-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)

(defmethod bind-selection ((port abcl-port) window &optional time)
  (declare (ignore window time))
  nil)

(defmethod release-selection ((port abcl-port) &optional time)
  (declare (ignore time))
  nil)

(defmethod request-selection ((port abcl-port) requestor time)
  (declare (ignore requestor time))
  nil)

(defmethod get-selection-from-event ((port abcl-port) event)
  (declare (ignore event))
  nil)

(defmethod send-selection ((port abcl-port) event string)
  (declare (ignore event string))
  nil)
