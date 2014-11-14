;; Functions that test the rendering engine
(in-package :clx-freetype2-renderer)
(export '(render-test))

(defun handle-expose-event (count window gcontext face)
  (when (zerop count)
    (let* ((width (xlib:drawable-width window))
	   (height (xlib:drawable-height window))
	   (x 20)
	   (y 50)
	   (pixmap (xlib:create-pixmap :width width
				       :height height
				       :depth (xlib:drawable-depth window)
				       :drawable window))
	   (gc-color (xlib:gcontext-foreground gcontext)))
      (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext))
	(xlib:draw-rectangle pixmap gcontext 0 0 
			     width height 'fill))
      ;(ft2:print-with-face face "Hello" :left-right)
      (draw-glyphs window gcontext 20 50 "Hello" :face face)
      (xlib:copy-area pixmap gcontext 0 0 width height window 0 0)))
  nil)

(defun render-test (&optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
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
	 (width nil)
	 (height nil))
    ;(describe (xlib:gcontext-font gcontext))
    ;; setup the font
    (set-face-size *face* 24 screen)
    (xlib:map-window window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:configure-notify (w-w w-h) (setf width w-w
					 height w-h) nil)
      (:exposure (count) (handle-expose-event count window gcontext *face*))
      (:button-press () t)
      (:destroy-notify () t))
    (xlib:destroy-window window)
    (xlib:close-display display)))
