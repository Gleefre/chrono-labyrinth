(asdf:defsystem "chrono-labyrinth"
  :description "Chrono Labyrinth"
  :version "0.0.0"
  :author ("Gleefre <varedif.a.s@gmail.com>"
           "Vyacheslav Mikushev")
  :licence "Apache 2.0"

  :depends-on ("uiop" "asdf"
               "sdl2"
               "sketch" "sketch-utils" "sketch-examples"
               "stopclock"
               "alexandria" "serapeum"
               "deploy"
               "file-select"
               "harmony" "cl-mixed-vorbis"
               #+(and linux (not android)) "cl-mixed-pulse"
               #+android "cl-mixed-aaudio"
               #+darwin "cl-mixed-coreaudio"
               #+windows "cl-mixed-wasapi"
               #+bsd "cl-mixed-oss")

  :pathname "src"
  :serial T
  :components ((:file "packages")
               (:file "specials")
               (:file "utils")
               (:file "tilesets")
               (:file "caching-methods")
               (:file "camera")
               (:file "character")
               (:file "music")
               (:file "game-objects")
               (:file "world")
               (:file "save-world")
               (:file "game-physics")
               (:file "gameplay")
               (:file "draw")
               (:file "game"))

  :defsystem-depends-on ("deploy")
  :build-operation #-darwin "deploy-op" #+darwin "osx-app-deploy-op"
  :build-pathname "ChronoLabyrinth"
  :entry-point "chrono-labyrinth:start-toplevel")

(asdf:defsystem "chrono-labyrinth/editor"
  :description "Chrono Labyrinth Editor"
  :depends-on ("chrono-labyrinth"
               "sketch"
               "cl-gtk4"
               "cl-gdk4")
  :pathname "src"
  :serial T
  :components ((:file "gtk-sketch-area")
               (:file "gtk")))
