(in-package :goatee)

;;; A mixin for buffers that can be displayed in editable areas
(defclass displayable-buffer ()
  ((editable-areas :accessor editable-areas :initarg :editable-areas)))


(defclass editable-area ()
  (buffer :reader buffer :initarg buffer)
   (frame-begin-mark :accessor frame-begin-mark)
   (last-tick :accessor last-tick :initiarg :last-tick
		:documentation "buffer tick")
   (lines :accessor lines :initarg :lines
	  :initform (make-instance 'dbl-list-head))))

#+nil(progn
(defmethod (setf buffer) ((new-buf displayable-buffer) (win editable-area))
  (when (slot-boundp win 'buffer)
    (remove-mark frame-begin-mark))
  (setf (slot-value win 'buffer) new-buf)
  (pushnew win (editable-areas new-buf))
  (frame-window-to-point win)
  (add-new-lines win)
  new-buf)

(defconstant +point-frame-ratio+ 1/2)

(defgeneric frame-window-to-point (window))

;;;XXX How to deal with line wrap?
(defmethod frame-window-to-point ((win window))
  (with-accessors ((buffer buffer)
		   (frame-begin-mark frame-begin-mark))
    win
    (let* ((lines-to-start (floor (* (rows win) +point-frame-ratio+)))
	   (window-start (loop for lines from 0
			       for prev-eol = (point buffer)
			         then (position-backward buffer
							 #\newline
							 prev-eol )
			       while (and prev-eol
					  (<= lines lines-to-start))
			       finally (return (or prev-eol 0)))))
      (if (and (slot-boundp win 'frame-begin-mark) frame-begin-mark)
	  (progn
	    (remove-mark frame-begin-mark)
	    (setf frame-begin-mark
		  (insert-mark buffer frame-begin-mark window-start)))
	  (setf frame-begin-mark (insert-mark-using-class buffer
							  'fixed-mark
							  window-start))))))

(defclass line-mark (fixed-mark)
  ((last-update :accessor last-update :initarg :last-update)))

(defclass editable-area-line (dbl-list)
  ((buffer-line :accessor buffer-line :initarg :buffer-line)
   (last-tick :accessor last-tick :initiarg :last-tick)
   (editable-area :accessor editable-area :initiarg :editable-area
		  :documentation "backpointer")))

(defmethod add-new-lines ((win window))
  (with-accessors ((buf buffer)
		   (line-marks line-marks)
		   (lines lines))
    win
    (setf (dbl-head line-marks) nil)
    (setf (dbl-head lines) nil)
    (let ((start-line-pos (at (frame-begin-mark win))))
      (loop for line-count from 0 below (rows win)
	    for line-pos = start-line-pos then (position-forward
						buffer
						#\Newline
						(1+ prev-line-pos))
	    for prev-line-pos = 0 then line-pos
	    while line-pos
	    for line-mark = (insert-mark-using-class
			     'line-mark
			     (1+ line-pos)
			     :last-update (update-counter win))
	    for prev-line-mark-dbl = line-marks then line-mark-dbl
	    for line-mark-dbl = (insert-obj-after line-mark prev-line-mark-dbl)
	    for line = (make-instance 'line
				      :mark line-mark
				      :last-update (update-counter win))
	    for prev-line-dbl = lines then line-dbl
	    for line-dbl = (insert-obj-after line prev-line-dbl)))
    (loop for line-dbl = (dbl-head lines) then (next line-dbl)
	  while line-dbl
	  for line = (contents line-dbl)
	  do (let* ((start-line (at (mark line)))
		    (end-line (if (next line-dbl)
				  (1- (at (mark (contents (next line-dbl)))))
				  (position-forward buffer
						    #\Newline
						    (1+ start-line))))
		    (line-length (- start-line end-line))
		    (chars (make-array line-length
				       :element-type 'character
				       :adjustable t
				       :fill-pointer line-length)))
	       (buffer-string-into buffer chars
				   :start2 start-line :end2 end-line)
	       (setf (chars line) chars)))))
)