(asdf:defsystem "chrono-labyrinth"
  :description "Chrono Labyrinth"
  :version "0.0.0"
  :author ("Gleefre <varedif.a.s@gmail.com>"
           "...")
  :licence "Apache 2.0"

  :depends-on ("uiop" "asdf"
               "sdl2"
               "sketch" "sketch-utils" "sketch-examples"
               "stopclock"
               "alexandria" "serapeum"
               "deploy"
	       ;; "cl-cffi-gtk"
	       "bordeaux-threads"
	       "cl-gtk4"
	       "swank"
	       ;; "gtk-demo"
	       )

  :pathname "src"
  :serial T
  :components ((:file "packages")
               (:file "specials")
               (:file "gtk-sketch-area")
	       (:file "utils")
               (:file "gtk")
               (:file "music")
               (:file "gameplay")
               (:file "draw")
               (:file "game"))

  :defsystem-depends-on ("deploy")
  :build-operation #-darwin "deploy-op" #+darwin "osx-app-deploy-op"
  :build-pathname "ChronoLabyrinth"
  :entry-point "chrono-labyrinth:start-toplevel")
