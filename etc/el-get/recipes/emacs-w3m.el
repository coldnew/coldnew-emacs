(name :emacs-w3m
      :type cvs
      :module "emacs-w3m"
      :options "login"
      :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot"
     ;;;; :url "pserver:anonymous@w3m.cvs.sourceforge.net:/cvsroot/w3m"
      :build ("autoconf" "./configure" "make")
      :build/darwin ("autoconf" "./configure --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs" "make"))
:info "doc"
:features "w3m-load")
