;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2, Chapter 32.2 Multi-processing
;;;            for single processing Lisps
;;;   Created: 2012-12-02
;;;    Author: Rudolf Schlatte <rudi@constantly.at>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2012 Rudolf Schlatte

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

(in-package :clim-internals)

(defconstant *multiprocessing-p* t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :clim-mp *features*))

(defun make-process (function &key name)
  (bt:make-thread function :name name))

(defun destroy-process (process)
  (bt:destroy-thread process))

(defun current-process ()
  (bt:current-thread))

(defun all-processes ()
  (bt:all-threads))

(defun processp (object)
  (bt:threadp object))

(defun process-name (process)
  (bt:thread-name process))

;;; What do these two do?
(defun process-state (process)
  (declare (ignore process))
  nil)

(defun process-whostate (process)
  (declare (ignore process))
  nil)

;;; process-wait[-with-timeout] shouldn't be used at all
(defun process-wait (reason predicate)
  (declare (ignore reason))
  (loop until (funcall predicate)
        do (bt:thread-yield)))

(defun process-wait-with-timeout (reason timeout predicate)
  (declare (ignore reason))
  (let ((end-time (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop until (or (funcall predicate)
                    (> (get-internal-real-time) end-time))
          do (bt:thread-yield))))

(defun process-yield ()
  (bt:thread-yield))

(defun process-interrupt (process function)
  (bt:interrupt-thread process function))

;;; Simulating disable/enable by synchronization on the thread object
;;; itself
(defun disable-process (process)
  (threads:interrupt-thread process (lambda () (threads:object-wait process))))

(defun enable-process (process)
  (threads:object-notify process))

;;;
(defun restart-process (process)
  (declare (ignore process))
  (error "Not implemented yet."))

(defmacro without-scheduling (&body body)
  `(progn ,@body))

;;; Punt on these - yes, there's
;;; java.util.concurrent.atomic.AtomicInteger but ...
(defmacro atomic-incf (place)
  `(incf (the fixnum ,place)))

(defmacro atomic-decf (place)
  `(decf (the fixnum ,place)))

;;; 32.3 Locks

(defun make-lock (&optional name)
  (declare (ignore name))
  (bt:make-lock name))

(defmacro with-lock-held ((place &optional state) &body body)
  (declare (ignore state))
  `(bt:with-lock-held (,place)
      ,@body))

(defun make-recursive-lock (&optional name)
  (bt:make-recursive-lock name))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  (declare (ignore state))
  `(bt:with-recursive-lock-held (place) ,@body))

;;; TODO: Java can wait and notify on any LispObject, but we should use
;;; a java.util.concurrent.locks.Condition implementation

(defun make-condition-variable ()
  (bt:make-condition-variable))

(defun condition-wait (cv lock &optional timeout)
  (when timeout (warn "condition-wait timeout not implemented"))
  (bt:condition-wait cv lock))

(defun condition-notify (cv)
  (bt:condition-notify cv))
