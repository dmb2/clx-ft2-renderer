;;;; clx-freetype2-renderer.asd

(asdf:defsystem #:clx-freetype2-renderer
  :description "Describe clx-freetype2-renderer here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:clx
               #:cl-freetype2)
  :serial t
  :components ((:file "package")
	       (:file "render")
               (:file "clx-freetype2-renderer")
	       (:file "tests")))

