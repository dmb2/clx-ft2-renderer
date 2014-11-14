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
;; cf src/toy.lisp in cl-freetype2
(defun string-to-array (face string)
  (let* ((height (round (ft2:string-pixel-height face string)))
	 (width (round (ft2:string-pixel-width face string)))
	 (array (make-array (list width height) :element-type 'unsigned-byte :initial-element 0)))
    (ft2:do-string-render (face string bitmap x y)
      (let ((barray (bitmap-to-array bitmap)))
	(ft2::ablit array barray :x x :y y))) 
    array))

(defun set-face-size (face size screen)
  (multiple-value-bind (dpi-x dpi-y)
	(screen-dpi screen)
    (ft2:set-char-size face (* size 64) 0 dpi-x dpi-y)))
;; cf draw-text in clx-truetype.lisp
(defun render-glyphs (drawable gcontext x y string face)
  "Actually handle the rendering"
  (when (and start end)
    (when (>= start end)
      (return-from draw-text))
    (setf string (subseq string start end)))
  (let* ((display (xlib:drawable-display drawable))
	 (width nil)
	 (height nil)
	 (image (xlib:create-image :width width :height height 
				   :depth 8 :drawable drawable))
	 (alpha-pixmap (xlib:create-pixmap :width width :height height
					   :depth 8 :drawable drawable))
	 (alpha-gc (xlib:create-gcontext :drawable alpha-pixmap))
	 (source-picture (get-drawable-pen-picture drawable))
	 (dest-picture (get-drawable-picture drawable))
	 ;)))

(defun draw-glyphs (drawable gcontext x y string &key (start 0) end face)
  "Draw glyphs to gcontext depending on whether or not face was provided"
  (if face
      (render-glyphs drawable gcontext x y (subseq string start end) face)
      (xlib:draw-glyphs drawable gcontext x y string :start start :end end)))
