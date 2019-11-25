;; Functions that test the rendering engine
(in-package :clx-freetype2-renderer)
(export '(render-test))
(defun constituent(c)
  (and (graphic-char-p c)
       (not (char= c #\space))))

(defun white-space-split (string)
  (when (plusp (length string))
    (let ((cut-point (position-if
		      (complement #'constituent)
		      string)))
      (if cut-point
	  (cons (subseq string 0 cut-point)
		(white-space-split
		 (subseq string (1+ cut-point))))
	(list string)))))

(defun draw-words (drawable gcontext words font)
  (let* ((right-margin 10)
	 (left-margin 10)
	 (face (slot-value font 'ft-face))
	 (line-spacing (+ 4 (round (ft2:face-ascender-pixels face))))
	 (inter-word-space (round (ft2:string-pixel-width face " ")))
	 (actual-width (xlib:drawable-width drawable))
	 (line 1)
	 (x left-margin))
    (dolist (word words)
      (let ((width (round (ft2:string-pixel-width face word))))
	(when (> (+ x width right-margin) actual-width)
	  (incf line) 
	  (setf x left-margin)) 
	(draw-glyphs drawable gcontext x (* line line-spacing) word :font font)
	(incf x (+ width inter-word-space))))))


(defun handle-expose-event (count window gcontext font words)
  (when (zerop count)
    (let* ((width (xlib:drawable-width window))
	   (height (xlib:drawable-height window))
	   (x (round (/ width 2)))
	   (y (round (/ height 2)))
	   (gc-color (xlib:gcontext-foreground gcontext))
	   (pixmap (xlib:create-pixmap :width width
				       :height height
				       :depth (xlib:drawable-depth window)
				       :drawable window)))
      (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext))
	(xlib:draw-rectangle pixmap gcontext 0 0 
			     width height 'fill))
      (time (draw-words pixmap gcontext words font))
      (xlib:copy-area pixmap gcontext 0 0 width height window 0 0)
      ))
  nil)

(defun render-test (&optional (host "") (display-num 0))
  (let* ((display (xlib:open-display host :display display-num))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (dpi (multiple-value-list (screen-dpi screen)))
	 (font (make-instance 'font :family "DejaVu Sans" :size 16))
	 (gcontext (xlib:create-gcontext
	  	    :drawable root-window
	  	    :foreground white
	  	    :background black))
	 (window (xlib:create-window
		  :parent root-window
		  :x 0 :y 0
	  	  :width 500 :height 250
	  	  :background black
	  	  :event-mask (xlib:make-event-mask :exposure
	  					    :button-press
	  					    :structure-notify)))
	 (words (white-space-split "⚡ ⚕ Welcome to the academy. ⚛ ♬ CHAPTER I."))
	 (width nil)
	 (height nil))
    ;; setup the font
    (set-face-size font screen 14)
     (xlib:map-window window)
     (xlib:event-case (display :force-output-p t
     			      :discard-p t)
       (:configure-notify (w-w w-h) (setf width w-w
     					 height w-h) 
			  nil)
       (:exposure (count) (handle-expose-event count window gcontext font words))
       (:button-press () t)
       (:destroy-notify () t))
    (xlib:destroy-window window)
    (xlib:close-display display)
    ))

  
; 
