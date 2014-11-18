;; The main rendering engine
(in-package :clx-freetype2-renderer)
(export '(string-to-array
	  set-face-size))

;; Taken from clx-truetype.lisp
(defun screen-dpi (screen)
  "Returns dpi for @var{screen}. ((pixel width)/(mm width))*25.4 mm/inch"
  (values (floor (* (xlib:screen-width screen) 25.4)
		 (xlib:screen-width-in-millimeters screen))
	  (floor (* (xlib:screen-height screen) 25.4)
		 (xlib:screen-height-in-millimeters screen))))
(defun get-drawable-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ft2-surface)
      (setf (getf (xlib:drawable-plist drawable) :ft2-surface)
	    (xlib:render-create-picture
	     drawable
	     :format (first (xlib::find-matching-picture-formats (xlib:drawable-display drawable)
								 :depth (xlib:drawable-depth drawable)))))))
(defun get-drawable-pen-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ft2-pen)
      (setf (getf (xlib:drawable-plist drawable) :ft2-pen)
	    (xlib:render-create-picture
	     (or (getf (xlib:drawable-plist drawable) :ft2-pen-surface)
		 (setf (getf (xlib:drawable-plist drawable) :ft2-pen-surface)
		       (xlib:create-pixmap
			:drawable drawable
			:depth (xlib:drawable-depth drawable)
			:width 1 :height 1)))
	     :format (first (xlib::find-matching-picture-formats (xlib:drawable-display drawable)
								 :depth (xlib:drawable-depth drawable)))
	     :repeat :on))))
(defun display-alpha-picture-format (display)
  (or (getf (xlib:display-plist display) :ft2-alpha-format)
      (setf (getf (xlib:display-plist display) :ft2-alpha-format)
	    (first
	     (xlib:find-matching-picture-formats
	      display
	      :depth 8 :alpha 8 :red 0 :blue 0 :green 0)))))

;; cf src/toy.lisp in cl-freetype2
(defun flat-array (arr)
  (make-array (apply #'* (array-dimensions arr))
	      :element-type (cadr (type-of arr))
              :displaced-to arr ))

(defun row-width (arr)
  (let ((dim (array-dimensions arr)))
    (if (> (array-rank arr) 2)
        (* (car dim) (caddr dim))
        (car dim))))

(defun ablit (arr1 arr2 &key (x 0) (y 0))
  "Destructivly copy arr2 into arr1 for 2- and 3-dimensional (Y:X, Y:X:RGB(A))
arrays.  X and Y may be specified as a 2D offset into ARR1."
  (assert (= (array-rank arr1) (array-rank arr2)))
  (let ((flat1 (flat-array arr1))
        (flat2 (flat-array arr2))
        (height1 (row-width arr1))
        (height2 (row-width arr2))
        (width1 (array-dimension arr1 1))
        (width2 (array-dimension arr2 1))
        (xoff (* x (if (= (array-rank arr1) 3)
                       (array-dimension arr1 2)
                       1))))
    (loop for y2 from 0 below height2
          for y1 from y below height1
          do (let ((x1 (+ (* y1 width1) xoff))
                   (x2 (* y2 width2)))
               (replace flat1 flat2
                        :start1 x1
                        :end1 (* (1+ y1) width1)
                        :start2 x2
                        :end2 (+ x2 width2)))))
  arr1)
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

(defun set-face-size (face size screen)
  (multiple-value-bind (dpi-x dpi-y)
	(screen-dpi screen)
    (ft2:set-char-size face (* size 64) 0 dpi-x dpi-y)))
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
(defun get-bearing-x (char face)
  "Returns the bearing-x of char. TODO: make sure this does what I want"
  (ft2:load-char face char '(:no-bitmap))
  (ft2::ft-glyph-metrics-hori-bearing-x 
   (ft2::ft-glyphslot-metrics 
    (ft2::ft-face-glyph face))))
(defun render-glyphs (drawable gcontext x y string face update-bg-p)
  "Actually handle the rendering"
  (let* ((display (xlib:drawable-display drawable))
	 (width (round (ft2:string-pixel-width face string)))
	 (height (round (ft2:string-pixel-height face string)))
	 (alpha-data (string-to-array face string :left-right width height))
	 (y-max (round (ft2:face-ascender-pixels face)))
	 (x-min (round (get-bearing-x (aref string 0) face)))
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
      (update-background drawable gcontext (+ x x-min) (- y y-max) width height))
    (setf  
	  (xlib:picture-clip-x-origin dest-picture) (xlib:gcontext-clip-x gcontext)
    	  (xlib:picture-clip-y-origin dest-picture) (xlib:gcontext-clip-y gcontext)
    	  (xlib:picture-subwindow-mode dest-picture) (xlib:gcontext-subwindow-mode gcontext)
    	  (xlib::picture-clip-mask dest-picture) (xlib::gcontext-clip-mask gcontext))
    (xlib:render-composite :over source-picture alpha-picture dest-picture 0 0 0 0 
			   (+ x x-min) (- y y-max)  width height))
  nil)

(defun draw-glyphs (drawable gcontext x y string &key (start 0) end face update-bg-p)
  "Draw glyphs to gcontext depending on whether or not face was provided"
  (if face
      (render-glyphs drawable gcontext x y (if (and start end (not (>= start end)))
					       (subseq string start end)
					       string) face update-bg-p)
      (xlib:draw-glyphs drawable gcontext x y string :start start :end end)))
