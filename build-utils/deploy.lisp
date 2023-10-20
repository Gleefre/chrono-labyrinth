(require :uiop)
(require :asdf)

(ql:quickload :deploy)
(load "chrono-labyrinth.asd")
(push :deploy *features*)
(ql:quickload :chrono-labyrinth)

(deploy:define-resource-directory data "res/")

(defmacro dont-deploy (&rest libraries)
  `(progn
     ,@(loop for library in (alexandria:flatten libraries)
             collect `(deploy:define-library ,library :dont-deploy T))))

(defmacro deploy (&rest names)
  `(progn
     ,@(loop for name in (alexandria:flatten names)
             for library = (gensym)
             collect `(cffi:define-foreign-library ,library (:linux ,name)) ; Why :linux? FIXME
             collect `(deploy:define-library ,library :dont-deploy NIL))))

(dont-deploy
 cl-opengl-bindings::opengl
 ;; Can't ship SDL2 - too many dependencies
 ;; FIXME
 #+linux (sdl2::libsdl2
          sdl2-image::libsdl2-image
          sdl2-ttf::libsdl2-ttf))

(asdf:make :chrono-labyrinth)
