;; The main rendering engine
(in-package :clx-freetype2-renderer)
(export '(draw-glyphs))


(defun print-alpha-data (array)
  (loop for i from 0 below (array-dimension array 0)
     do (loop for j from 0 below (array-dimension array 1)
	     do (format t "~4d" (aref array i j)))
     do (princ #\Newline)))

(defun make-alpha-picture (pixmap gc image display)
  (xlib:put-image pixmap gc image :x 0 :y 0)
  (xlib:render-create-picture pixmap
			      :format (display-alpha-picture-format display)))
(defun update-foreground (drawable gcontext)
  "Lazy updates foreground for drawable. @var{drawable} must be window or pixmap."
  (let ((pixmap (or (getf (xlib:drawable-plist drawable) :ft2-pen-surface)
                    (setf (getf (xlib:drawable-plist drawable) :ft2-pen-surface)
                          (xlib:create-pixmap 
                           :drawable drawable
                           :depth (xlib:drawable-depth drawable)
                           :width 1 :height 1)))))
    (let ((color (the xlib:card32 (xlib:gcontext-foreground gcontext))))
      (when (or (null (getf (xlib:drawable-plist drawable) :ft2-foreground))
                (/= (getf (xlib:drawable-plist drawable) :ft2-foreground)
                    color))
        (let ((previous-color (xlib:gcontext-foreground gcontext)))
          (setf (xlib:gcontext-foreground gcontext) color)
          (xlib:draw-point pixmap gcontext 0 0)
          (setf (xlib:gcontext-foreground gcontext) previous-color)
          (setf (getf (xlib:drawable-plist drawable) :ft2-foreground) color))))))
(defun update-background (drawable gcontext x y width height)
  "Lazy updates background for drawable. @var{drawable} must be window or pixmap."
  (let ((previous-color (xlib:gcontext-foreground gcontext))
        (color (the xlib:card32 (xlib:gcontext-background gcontext))))
    (setf (xlib:gcontext-foreground gcontext) color)
    (xlib:draw-rectangle drawable gcontext x y width height t)
    (setf (xlib:gcontext-foreground gcontext) previous-color)))

(defun string-to-array (face string direction width height)
  (let* ((flags (if (or (eq direction :up-down)
                        (eq direction :down-up))
                    '(:vertical-layout)
                    '(:default)))
         (array (make-array (list height width) :element-type '(unsigned-byte 8)
                                                :initial-element 0)))
    (ft2:do-string-render (face string bitmap x y
                       :direction direction)
      (let ((barray (ft2:bitmap-to-array bitmap)))
        (case direction
          (:left-right (ablit array barray :x x :y y))
          (:right-left (ablit array barray :x (+ width x) :y y))
          (:up-down    (ablit array barray :x x :y y))
          (:down-up    (ablit array barray :x x :y (+ height y))))))
    array))
(defun render-glyphs (drawable gcontext x y string face update-bg-p)
  "Actually handle the rendering"
  (let* ((display (xlib:drawable-display drawable))
	 (width (round (ft2:string-pixel-width face string)))
	 (height (round (ft2:string-pixel-height face string)))
	 (alpha-data (string-to-array face string :left-right width height))
	 (y-max (round (ft2:face-ascender-pixels face)))
	 (x-min (round (get-bearing-x #\t face)))
	 (x-pos (+ x x-min))
	 (y-pos (- y y-max))
	 (image (xlib:create-image :width width :height height 
	 			   :depth 8 :data alpha-data))
	 (alpha-pixmap (xlib:create-pixmap :width width :height height
					   :depth 8 :drawable drawable))
	 (alpha-gc (xlib:create-gcontext :drawable alpha-pixmap 
					 :foreground (xlib:gcontext-foreground gcontext)
					 :background (xlib:gcontext-background gcontext)))
	 (alpha-picture (make-alpha-picture alpha-pixmap alpha-gc image display))
	 (source-picture (get-drawable-pen-picture drawable))
	 (dest-picture (get-drawable-picture drawable)))
    (update-foreground drawable gcontext)
    (when update-bg-p
      (update-background drawable gcontext x-pos y-pos  width height))
    (setf  
     (xlib:picture-clip-x-origin dest-picture) (xlib:gcontext-clip-x gcontext)
     (xlib:picture-clip-y-origin dest-picture) (xlib:gcontext-clip-y gcontext)
     (xlib:picture-subwindow-mode dest-picture) (xlib:gcontext-subwindow-mode gcontext)
     (xlib::picture-clip-mask dest-picture) (xlib::gcontext-clip-mask gcontext))
    (xlib:render-composite :over source-picture alpha-picture dest-picture 0 0 0 0 
			   x-pos y-pos  width height))
  nil)

(defun draw-glyphs (drawable gcontext x y string &key (start 0) end face update-bg-p)
  "Draw glyphs to gcontext depending on whether or not face was provided"
  (if face
      (render-glyphs drawable gcontext x y (if (and start end (not (>= start end)))
					       (subseq string start end)
					       string) face update-bg-p)
      (xlib:draw-glyphs drawable gcontext x y string :start start :end end)))
