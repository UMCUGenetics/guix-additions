;;;
;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (umcu packages rstudio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

;;
;; RStudio was imported from:
;; https://github.com/BIMSBbioinfo/guix-bimsb/blob/master/bimsb/packages/staging.scm
;;

(define-public rstudio-server
  (package
   (name "rstudio-server")
   (version "1.3.1093")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/rstudio/rstudio.git")
                   (commit (string-append "v" version))))
             (sha256
              (base32
               "0zarhsm3cghz4fn4c53y2zy05z3cxqzp87h16ya8v7hyxapaqfy6"))
             (file-name (string-append name "-" version "-checkout"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags '("-DRSTUDIO_TARGET=Server")
      #:tests? #f ; no tests
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'set-java-home
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
            #t))
        (add-after 'unpack 'fix-dependencies
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Disable checks for bundled dependencies.  We take care of them by other means.
            (substitute* "src/cpp/session/CMakeLists.txt"
              (("if\\(NOT EXISTS \"\\$\\{RSTUDIO_DEPENDENCIES_DIR\\}/common/rmarkdown\"\\)") "if (FALSE)")
              (("if\\(NOT EXISTS \"\\$\\{RSTUDIO_DEPENDENCIES_DIR\\}/common/rsconnect\"\\)") "if (FALSE)"))

            ;; Force using Boost.Signals2 because our Boost package has
            ;; removed Boost.Signals (it was deprecated in 1.69.0).
            (substitute* "src/cpp/CMakeLists.txt"
              (("set\\(RSTUDIO_BOOST_SIGNALS_VERSION 1\\)")
               "set(RSTUDIO_BOOST_SIGNALS_VERSION 2)"))
            #t))
        (add-after 'unpack 'copy-clang
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "dependencies/common"
                                      (let ((clang   (assoc-ref inputs "clang"))
                                            (dir     "libclang")
                                            (lib     "libclang/3.5")
                                            (headers "libclang/builtin-headers"))
                                        (mkdir-p dir)
                                        (mkdir-p lib)
                                        (mkdir-p headers)
                                        (for-each (lambda (file)
                                                    (install-file file lib))
                                                  (find-files (string-append clang "/lib") ".*"))
                                        (install-file (string-append clang "/include") dir)
                                        #t))))
        (add-after 'unpack 'unpack-dictionaries
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "dependencies/common"
               (mkdir "dictionaries")
               (mkdir "pandoc") ; TODO: only to appease the cmake stuff
               (zero? (system* "unzip" "-qd" "dictionaries"
                               (assoc-ref inputs "dictionaries"))))))
        (add-after 'unpack 'unpack-mathjax
          (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "dependencies/common"
               (mkdir "mathjax-27")
               (zero? (system* "unzip" "-qd" "mathjax-27"
                               (assoc-ref inputs "mathjax"))))))
        (add-after 'unpack 'unpack-gin
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "src/gwt"
              (install-file (assoc-ref inputs "junit") "lib")
              (mkdir-p "lib/gin/1.5")
              (zero? (system* "unzip" "-qd" "lib/gin/1.5"
                              (assoc-ref inputs "gin"))))))
        (add-after 'unpack 'unpack-gwt
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "src/gwt"
              (mkdir-p "lib/gwt")
	      (system* "unzip" "-qd" "lib/gwt"
		       (assoc-ref inputs "gwt"))
               (rename-file "lib/gwt/gwt-2.7.0" "lib/gwt/2.7.0"))
	    #t)))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("unzip" ,unzip)
      ("ant" ,ant)
      ("jdk" ,icedtea "jdk")
      ("gin"
       ,(origin
         (method url-fetch)
         (uri "https://s3.amazonaws.com/rstudio-buildtools/gin-1.5.zip")
         (sha256
          (base32 "155bjrgkf046b8ln6a55x06ryvm8agnnl7l8bkwwzqazbpmz8qgm"))))
      ("gwt"
       ,(origin
         (method url-fetch)
         (uri "https://s3.amazonaws.com/rstudio-buildtools/gwt-2.7.0.zip")
         (sha256
          (base32 "1cs78z9a1jg698j2n35wsy07cy4fxcia9gi00x0r0qc3fcdhcrda"))))
      ("junit"
       ,(origin
         (method url-fetch)
         (uri "https://s3.amazonaws.com/rstudio-buildtools/junit-4.9b3.jar")
         (sha256
          (base32 "0l850yfbq0cgycp8n0r0a1b7xznd37pgfd656vzdwim4blznqmnw"))))
      ("mathjax"
       ,(origin
         (method url-fetch)
         (uri "https://s3.amazonaws.com/rstudio-buildtools/mathjax-27.zip")
         (sha256
          (base32 "0xj143xqijybf13jaq534rvgplhjqfimwazbpbyc20yfqjkblv65"))))
      ("dictionaries"
       ,(origin
         (method url-fetch)
         (uri "https://s3.amazonaws.com/rstudio-dictionaries/core-dictionaries.zip")
         (sha256
          (base32 "153lg3ai97qzbqp6zjg10dh3sfvz80v42cjw45zwz7gv1risjha3"))))))
   (inputs
    `(("r" ,r)
      ("r-rmarkdown" ,r-rmarkdown) ; TODO: must be linked to another location
      ;;("r-rsconnect" ,r-rsconnect) ; TODO: must be linked to another location
      ("clang" ,clang-3.7)
      ("boost" ,boost)
      ("boost-signals2" ,boost-signals2)
      ("libuuid" ,util-linux "lib")
      ("pandoc" ,ghc-pandoc)
      ("openssl" ,openssl)
      ("pam" ,linux-pam)
      ("zlib" ,zlib)))
   (home-page "http://www.rstudio.org/")
   (synopsis "Integrated development environment (IDE) for R")
   (description
    "RStudio is an integrated development environment (IDE) for the R
programming language. Some of its features include: Customizable workbench
with all of the tools required to work with R in one place (console, source,
plots, workspace, help, history, etc.); syntax highlighting editor with code
completion; execute code directly from the source editor (line, selection, or
file); full support for authoring Sweave and TeX documents.  RStudio can also
be run as a server, enabling multiple users to access the RStudio IDE using a
web browser.")
   (license license:agpl3+)))

(define-public rstudio
  (package (inherit rstudio-server)
    (name "rstudio")
    (arguments
     (substitute-keyword-arguments (package-arguments rstudio-server)
       ((#:configure-flags flags)
        '(list "-DRSTUDIO_TARGET=Desktop"
               (string-append "-DQT_QMAKE_EXECUTABLE="
                              (assoc-ref %build-inputs "qtbase")
                              "/bin/qmake")))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'relax-qt-version
             (lambda _
               (substitute* "src/cpp/desktop/CMakeLists.txt"
                 (("5\\.4") "5.9"))
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (qtwebengine-path (string-append
                                         (assoc-ref inputs "qtwebengine")
                                         "/lib/qt5/libexec/QtWebEngineProcess")))
                 (wrap-program (string-append bin "/rstudio")
                   `("QTWEBENGINEPROCESS_PATH" ":" = (,qtwebengine-path))))))))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtlocation" ,qtlocation)
       ("qtsvg" ,qtsvg)
       ("qtsensors" ,qtsensors)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)
       ,@(package-inputs rstudio-server)))
    (synopsis "Integrated development environment (IDE) for R (desktop version)")))
