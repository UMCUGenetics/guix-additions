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
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages java)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (umcu packages mysql))

;;
;; When RStudio upgraded to Qt 5.11, they use Qt5WebEngine, which isn't
;; available in Guix.  We resurrected the Qt 5.9.4 packages exclusively for
;; RStudio.
;;

(define qtbase-for-rstudio
  (package
    (name "qtbase")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1kq422vb2zaic099pgzwk7c0qzgc3xap6qahw5vklrq0mgivvrk9"))
             ;; Use TZDIR to avoid depending on package "tzdata".
             (patches (search-patches "qtbase-use-TZDIR.patch"))
             (modules '((guix build utils)))
             (snippet
               ;; corelib uses bundled harfbuzz, md4, md5, sha3
              '(begin
                (for-each
                  (lambda (dir)
                    (delete-file-recursively (string-append "src/3rdparty/" dir)))
                  (list "double-conversion" "freetype" "harfbuzz-ng"
                        "libpng" "libjpeg" "pcre2" "sqlite" "xcb"
                        "xkbcommon" "zlib"))
                #t))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa)
       ("which" ,(@ (gnu packages base) which))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cups" ,cups)
       ("dbus" ,dbus)
       ("double-conversion" ,double-conversion)
       ("eudev" ,eudev)
       ("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("libinput" ,libinput-minimal)
       ("libjpeg" ,libjpeg)
       ("libmng" ,libmng)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxtst" ,libxtst)
       ("mtdev" ,mtdev)
       ("mysql" ,mysql-5.6.25)
       ("nss" ,nss)
       ("openssl" ,openssl)
       ("pcre2" ,pcre2)
       ("postgresql" ,postgresql)
       ("pulseaudio" ,pulseaudio)
       ("sqlite" ,sqlite)
       ("unixodbc" ,unixodbc)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-wm" ,xcb-util-wm)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("ruby" ,ruby)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-bin-sh
           (lambda _
             (substitute* '("config.status"
                            "configure"
                            "mkspecs/features/qt_functions.prf"
                            "qmake/library/qmakebuiltins.cpp")
                          (("/bin/sh") (which "sh")))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "configure"
                 (("/bin/pwd") (which "pwd")))
               (substitute* "src/corelib/global/global.pri"
                 (("/bin/ls") (which "ls")))
               ;; The configuration files for other Qt5 packages are searched
               ;; through a call to "find_package" in Qt5Config.cmake, which
               ;; disables the use of CMAKE_PREFIX_PATH via the parameter
               ;; "NO_DEFAULT_PATH". Re-enable it so that the different
               ;; components can be installed in different places.
               (substitute* (find-files "." ".*\\.cmake")
                 (("NO_DEFAULT_PATH") ""))
               ;; do not pass "--enable-fast-install", which makes the
               ;; configure process fail
               (zero? (system*
                       "./configure"
                       "-verbose"
                       "-prefix" out
                       "-docdir" (string-append out "/share/doc/qt5")
                       "-headerdir" (string-append out "/include/qt5")
                       "-archdatadir" (string-append out "/lib/qt5")
                       "-datadir" (string-append out "/share/qt5")
                       "-examplesdir" (string-append
                                       out "/share/doc/qt5/examples")
                       "-opensource"
                       "-confirm-license"
                       ;; Do not build examples; if desired, these could go
                       ;; into a separate output, but for the time being, we
                       ;; prefer to save the space and build time.
                       "-no-compile-examples"
                       ;; Most "-system-..." are automatic, but some use
                       ;; the bundled copy by default.
                       "-system-sqlite"
                       "-system-harfbuzz"
                       "-system-pcre"
                       ;; explicitly link with openssl instead of dlopening it
                       "-openssl-linked"
                       ;; explicitly link with dbus instead of dlopening it
                       "-dbus-linked"
                       ;; don't use the precompiled headers
                       "-no-pch"
                       ;; drop special machine instructions that do not have
                       ;; runtime detection
                       ,@(if (string-prefix? "x86_64"
                                             (or (%current-target-system)
                                                 (%current-system)))
                             '()
                             '("-no-sse2"))
                       "-no-mips_dsp"
                       "-no-mips_dspr2")))))
         (add-after 'install 'patch-mkspecs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (archdata (string-append out "/lib/qt5"))
                    (mkspecs (string-append archdata "/mkspecs"))
                    (qt_config.prf (string-append
                                    mkspecs "/features/qt_config.prf")))
               ;; For each Qt module, let `qmake' uses search paths in the
               ;; module directory instead of all in QT_INSTALL_PREFIX.
               (substitute* qt_config.prf
                 (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../include/qt5))")
                 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                 (("\\$\\$\\[QT_HOST_LIBS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                 (("\\$\\$\\[QT_INSTALL_BINS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../bin))"))

               ;; Searches Qt tools in the current PATH instead of QT_HOST_BINS.
               (substitute* (string-append mkspecs "/features/qt_functions.prf")
                 (("cmd = \\$\\$\\[QT_HOST_BINS\\]/\\$\\$2")
                  "cmd = $$system(which $${2}.pl 2>/dev/null || which $${2})"))

               ;; Resolve qmake spec files within qtbase by absolute paths.
               (substitute*
                   (map (lambda (file)
                          (string-append mkspecs "/features/" file))
                        '("device_config.prf" "moc.prf" "qt_build_config.prf"
                          "qt_config.prf" "winrt/package_manifest.prf"))
                 (("\\$\\$\\[QT_HOST_DATA/get\\]") archdata)
                 (("\\$\\$\\[QT_HOST_DATA/src\\]") archdata))
               #t)))
         (add-after 'unpack 'patch-paths
           ;; Use the absolute paths for dynamically loaded libs, otherwise
           ;; the lib will be searched in LD_LIBRARY_PATH which typically is
           ;; not set in guix.
           (lambda* (#:key inputs #:allow-other-keys)
             ;; libresolve
             (let ((glibc (assoc-ref inputs ,(if (%current-target-system)
                                                 "cross-libc" "libc"))))
               (substitute* '("src/network/kernel/qdnslookup_unix.cpp"
                              "src/network/kernel/qhostinfo_unix.cpp")
                 (("^\\s*(lib.setFileName\\(QLatin1String\\(\")(resolv\"\\)\\);)" _ a b)
                (string-append a glibc "/lib/lib" b))))
             ;; X11/locale (compose path)
             (substitute* "src/plugins/platforminputcontexts/compose/generator/qtablegenerator.cpp"
               ;; Don't search in /usr/…/X11/locale, …
               (("^\\s*m_possibleLocations.append\\(QStringLiteral\\(\"/usr/.*/X11/locale\"\\)\\);" line)
                (string-append "// " line))
               ;; … but use libx11's path
               (("^\\s*(m_possibleLocations.append\\(QStringLiteral\\()X11_PREFIX \"(/.*/X11/locale\"\\)\\);)" _ a b)
                (string-append a "\"" (assoc-ref inputs "libx11") b)))
             ;; libGL
             (substitute* "src/plugins/platforms/xcb/gl_integrations/xcb_glx/qglxintegration.cpp"
               (("^\\s*(QLibrary lib\\(QLatin1String\\(\")(GL\"\\)\\);)" _ a b)
                (string-append a (assoc-ref inputs "mesa") "/lib/lib" b)))
             ;; libXcursor
             (substitute* "src/plugins/platforms/xcb/qxcbcursor.cpp"
               (("^\\s*(QLibrary xcursorLib\\(QLatin1String\\(\")(Xcursor\"\\), 1\\);)" _ a b)
                (string-append a (assoc-ref inputs "libxcursor") "/lib/lib" b))
               (("^\\s*(xcursorLib.setFileName\\(QLatin1String\\(\")(Xcursor\"\\)\\);)" _ a b)
                (string-append a (assoc-ref inputs "libxcursor") "/lib/lib" b)))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "QMAKEPATH")
            (files '("lib/qt5")))
           (search-path-specification
            (variable "QML2_IMPORT_PATH")
            (files '("lib/qt5/qml")))
           (search-path-specification
            (variable "QT_PLUGIN_PATH")
            (files '("lib/qt5/plugins")))
           (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc/xdg")))))
    (home-page "https://www.qt.io/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define qtsvg-for-rstudio
  (package (inherit qtbase-for-rstudio)
    (name "qtsvg")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0yh3an9rc7fh013cw3bm318ap6428icsmnj38hhg1w6lpwr2gwm2"))))
    (propagated-inputs `())
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase-for-rstudio)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'configure-qmake
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (qtbase (assoc-ref inputs "qtbase"))
                    (tmpdir (string-append (getenv "TMPDIR")))
                    (qmake (string-append tmpdir "/qmake"))
                    (qt.conf (string-append tmpdir "/qt.conf")))
               ;; Use qmake with a customized qt.conf to override install
               ;; paths to $out.
               (symlink (which "qmake") qmake)
               (setenv "PATH" (string-append tmpdir ":" (getenv "PATH")))
               (with-output-to-file qt.conf
                 (lambda ()
                   (format #t "[Paths]
Prefix=~a
ArchData=lib/qt5
Data=share/qt5
Documentation=share/doc/qt5
Headers=include/qt5
Libraries=lib
LibraryExecutables=lib/qt5/libexec
Binaries=bin
Tests=tests
Plugins=lib/qt5/plugins
Imports=lib/qt5/imports
Qml2Imports=lib/qt5/qml
Translations=share/qt5/translations
Settings=etc/xdg
Examples=share/doc/qt5/examples
HostPrefix=~a
HostData=lib/qt5
HostBinaries=bin
HostLibraries=lib

[EffectiveSourcePaths]
HostPrefix=~a
HostData=lib/qt5
" out out qtbase)))
               #t)))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Valid QT_BUILD_PARTS variables are:
             ;; libs tools tests examples demos docs translations
             (zero? (system* "qmake" "QT_BUILD_PARTS = libs tools tests"))))
         (add-before 'check 'set-display
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (synopsis "Qt module for displaying SVGs")
    (description "The QtSvg module provides classes for displaying the
 contents of SVG files.")))

(define qtimageformats-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtimageformats")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1nfxvf96wh1smdmcsk4m9f7zg69fgp844f8772qpv6v4m20p1qb9"))
             (modules '((guix build utils)))
             (snippet
              '(delete-file-recursively "src/3rdparty"))))
    (native-inputs `())
    (inputs
     `(("jasper" ,jasper)
       ("libmng" ,libmng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("mesa" ,mesa)
       ("qtbase" ,qtbase-for-rstudio)
       ("zlib" ,zlib)))
    (synopsis "Additional Image Format plugins for Qt")
    (description "The QtImageFormats module contains plugins for adding
support for MNG, TGA, TIFF and WBMP image formats.")))

(define qtx11extras-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtx11extras")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1a125fi7lbxfps207i12jammm4cjbiawmp4sqa3bxqah8p21i6w7"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt Extras for X11")
    (description "The QtX11Extras module includes the library to access X11
from within Qt 5.")))

(define qtxmlpatterns-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtxmlpatterns")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0ybz0i3wblvrm958s9ykp3a79bakjbb7k74q71mqaaswkv9imxgs"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'disable-network-tests
             (lambda _ (substitute* "tests/auto/auto.pro"
                         (("qxmlquery") "# qxmlquery")
                         (("xmlpatterns ") "# xmlpatterns"))
               #t))))))
    (native-inputs `(("perl" ,perl)))
    (inputs `(("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt XML patterns module")
    (description "The QtXmlPatterns module is a XQuery and XPath engine for
XML and custom data models.  It contains programs such as xmlpatterns and
xmlpatternsvalidator.")))

(define qtdeclarative-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtdeclarative")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0r9dhfc6qmxlzn2v9r6z6n2mcq6pv1nmyh91g9hcdlkx40xqlqyw"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("qtsvg" ,qtsvg-for-rstudio)
       ("qtxmlpatterns" ,qtxmlpatterns-for-rstudio)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt QML module (Quick 2)")
    (description "The Qt QML module provides a framework for developing
applications and libraries with the QML language.  It defines and implements the
language and engine infrastructure, and provides an API to enable application
developers to extend the QML language with custom types and integrate QML code
with JavaScript and C++.")))

(define qtconnectivity-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtconnectivity")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "12qckqz6ldvn1czkkigadmgl07yk4gs74hy4ifh4hmpm7cv519yv"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'disable-failing-tests
             ;; this test fails on armhf and aarch64
             (lambda _
               (substitute* "tests/auto/qndefnfcsmartposterrecord/tst_qndefnfcsmartposterrecord.cpp"
                 (("QCOMPARE\\(record.action\\(\\), QNdefNfcSmartPosterRecord::UnspecifiedAction")
                 "//QCOMPARE(record.action(), QNdefNfcSmartPosterRecord::UnspecifiedAction"))
               #t))))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (inputs
     `(("bluez" ,bluez)
       ("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt Connectivity module")
    (description "The Qt Connectivity modules provides modules for interacting
with Bluetooth and NFC.")))

(define qtwebsockets-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtwebsockets")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "00786d9m8skj68n5x9d8151zmmskx7ckhgcdd08hs9nly04h55vj"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (inputs `(("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt Web Sockets module")
    (description "WebSocket is a web-based protocol designed to enable two-way
communication between a client application and a remote host.  The Qt
WebSockets module provides C++ and QML interfaces that enable Qt applications
to act as a server that can process WebSocket requests, or a client that can
consume data received from the server, or both.")))

(define qtsensors-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtsensors")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0n6lkkn7c9x8vcplmfvkx7jq6najh2mrwnfb3blrmkmpash3lgvr"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:parallel-tests? _ #f) #f) ; can lead to race condition
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "tests/auto/qsensorgestures_gestures/tst_sensorgestures_gestures.cpp"
                 (("2000") "5000")      ;lengthen test timeout
                 (("QTest::newRow(\"twist\") << \"twist\"") "")) ;failing test
               #t))))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (inputs `(("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt Sensors module")
    (description "The Qt Sensors API provides access to sensor hardware via QML
and C++ interfaces.  The Qt Sensors API also provides a motion gesture
recognition API for devices.")))

(define qtmultimedia-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtmultimedia")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0x2f3vpax7rq0lxnncbp5b248bxdicrwn8hv4hsas2g2283s0lj9"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively
                   "examples/multimedia/spectrum/3rdparty")
                 ;; We also prevent the spectrum example from being built.
                 (substitute* "examples/multimedia/multimedia.pro"
                   (("spectrum") "#"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (zero? (system* "qmake" "QT_BUILD_PARTS = libs tools tests"
                                 (string-append "QMAKE_LFLAGS_RPATH=-Wl,-rpath," out "/lib -Wl,-rpath,")
                                 (string-append "PREFIX=" out))))))))
       ((#:tests? _ #f) #f)))           ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("mesa" ,mesa)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase-for-rstudio)
       ;; Gstreamer is needed for the mediaplayer plugin
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "Qt Multimedia module")
    (description "The Qt Multimedia module provides set of APIs to play and
record media, and manage a collection of media content.  It also contains a
set of plugins for interacting with pulseaudio and GStreamer.")))

(define qtwayland-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtwayland")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0x4q17k23akf14i3pyllr96s8lvprk1x006wp0mi5rhk4199cx1z"))
             (modules '((guix build utils)))
             (snippet
               ;; The examples try to build and cause the build to fail
              '(delete-file-recursively "examples"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'check 'set-ld-library-path
             ;; <https://lists.gnu.org/archive/html/guix-devel/2017-09/msg00019.html>
             ;;
             ;; Make the uninstalled libQt5WaylandClient.so.5 available to the
             ;; wayland platform plugin.
             (lambda _
               (setenv "LD_LIBRARY_PATH" (string-append (getcwd) "/lib"))
               #t))))))
    (native-inputs
     `(("glib" ,glib)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxext" ,libxext)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrender" ,libxrender)
       ("mesa" ,mesa)
       ("mtdev" ,mtdev)
       ("qtbase" ,qtbase-for-rstudio)
       ("wayland" ,wayland)))
    (synopsis "Qt Wayland module")
    (description "The Qt Wayland module provides the QtWayland client and
compositor libraries.")))

(define qtserialport-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtserialport")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "172i5cpqnk0c3m0hg08hgj15qvsyd1xvw9yf2dqicg3l10lqwg8c"))))
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("eudev" ,eudev)))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-dlopen-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/serialport/qtudev_p.h"
               ;; Use the absolute paths for dynamically loaded libs,
               ;; otherwise the lib will be searched in LD_LIBRARY_PATH which
               ;; typically is not set in guix.
               (("^\\s*(udevLibrary->setFileNameAndVersion\\(QStringLiteral\\(\")(udev\"\\),\\s*[0-9]+\\);)" _ a b)
                (string-append a (assoc-ref inputs "eudev") "/lib/lib" b)))
             #t))))))
    (synopsis "Qt Serial Port module")
    (description "The Qt Serial Port module provides the library for
interacting with serial ports from within Qt.")))

(define qtserialbus-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtserialbus")
    (version "5.9.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0gz5xsskv02yy078yffxyn8rdlklf4rsgnqrziyz5ywxwdh96gn5"))))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtserialport" ,qtserialport-for-rstudio)))
    (synopsis "Qt Serial Bus module")
    (description "The Qt Serial Bus API provides classes and functions to
access the various industrial serial buses and protocols, such as CAN, ModBus,
and others.")))

(define qtwebchannel-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtwebchannel")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1acs0fa5rxm3cir0lydc9a8685qagf1786vkssv51wk3v9r3lc4h"))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)
       ("qtwebsockets" ,qtwebsockets-for-rstudio)))
    (inputs `(("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Web communication library for Qt")
    (description "The Qt WebChannel module enables peer-to-peer communication
between the host (QML/C++ application) and the client (HTML/JavaScript
application).  The transport mechanism is supported out of the box by the two
popular web engines, Qt WebKit 2 and Qt WebEngine.")))

(define qtlocation-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtlocation")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "186jzv19v674n8jmm13v5xwv211lygih5657rlvbhc1s4jq6iv9p"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)
       ("qtquickcontrols" ,qtquickcontrols-for-rstudio)
       ("qtserialport" ,qtserialport-for-rstudio)))
    (inputs
     `(("icu4c" ,icu4c)
       ("openssl" ,openssl)
       ("qtbase" ,qtbase-for-rstudio)
       ("zlib" ,zlib)))
    (synopsis "Qt Location and Positioning modules")
    (description "The Qt Location module provides an interface for location,
positioning and geolocation plugins.")))

(define qttools-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qttools")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "11vfk6c8snsqwqj1xk53c0h2mkqr4gfa9kinp8py56x7sn15galm"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt Tools and Designer modules")
    (description "The Qt Tools module provides a set of applications to browse
the documentation, translate applications, generate help files and other stuff
that helps in Qt development.")))

(define qtscript-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtscript")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0lz0iv1baah7cxrpyiqzqp4fxxf75i21qd06ha7r5d80hq3xlia0"))
             (patches (search-patches "qtscript-disable-tests.patch"))))
    (native-inputs
     `(("perl" ,perl)
       ("qttools" ,qttools-for-rstudio)))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt Script module")
    (description "Qt provides support for application scripting with ECMAScript.
The following guides and references cover aspects of programming with
ECMAScript and Qt.")))

(define qtquickcontrols-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtquickcontrols")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "12yrmv6afjbd1fw3r8zjdrbq5l7cy7k5bxcyiv1m97gykfh0b8hn"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Quick Controls and other Quick modules")
    (description "The QtScript module provides classes for making Qt
applications scriptable.  This module provides a set of extra components that
can be used to build complete interfaces in Qt Quick.")))

(define qtquickcontrols2-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtquickcontrols2")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0334ayansm743kf113rs3k9hi9qb6giscfx9xig3y1z7asisfa0m"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Quick Controls 2 and other Quick 2 modules")
    (description "The Qt Quick Controls 2 module contains the Qt Labs Platform
module that provides platform integration: native dialogs, menus and menu bars,
and tray icons.  It falls back to Qt Widgets when a native implementation is
not available.")))

(define qtgraphicaleffects-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtgraphicaleffects")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1vxq4j7cb5cya1g234rxhfb361n45gp8c70gj8pc03njswkm7xwp"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Graphical Effects module")
    (description "The Qt Graphical Effects module provides a set of QML types
for adding visually impressive and configurable effects to user interfaces.
Effects are visual items that can be added to Qt Quick user interface as UI
components.  The API consists of over 20 effects provided as separate QML
types.  The effects cover functional areas such as blending, masking, blurring,
coloring, and many more.")))

(define qtdeclarative-render2d-for-rstudio
  ;; As of Qt-5.8.0 this module has been merged into qtdeclarative
  (package (inherit qtsvg-for-rstudio)
    (name "qtdeclarative-render2d")
    (version "5.7.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0zwch9vn17f3bpy300jcfxx6cx9qymk5j7khx0x9k1xqid4166c3"))
             (modules '((guix build utils)))
             (snippet
              '(delete-file-recursively "tools/opengldummy/3rdparty"))))
    (native-inputs `())
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Declarative Render module")
    (description "The Qt Declarative Render 2D module provides a Raster
backend for QtQuick scene graph.")
    (properties `((superseded . ,qtdeclarative-for-rstudio)))))

(define qtgamepad-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtgamepad")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1ci6aapq0i8qbzkn9xxvxn1n81z3y28yrlyzw0anqzj9qp97cl6f"))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libxrender" ,libxrender)
       ("sdl2" ,sdl2)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Gamepad module")
    (description "The Qt Gamepad module is an add-on library that enables Qt
applications to support the use of gamepad hardware and in some cases remote
control equipment.  The module provides both QML and C++ interfaces.  The
primary target audience are embedded devices with fullscreen user interfaces,
and mobile applications targeting TV-like form factors.")))

(define qtscxml-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtscxml")
    (version "5.9.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0knp328cinawz6xbhf9wd6h6gbwp74rb5cpmlr8gv3g5a7fjlsh1"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively "tests/3rdparty")
                 ;; the scion test refers to the bundled 3rd party test code.
                 (substitute* "tests/auto/auto.pro"
                   (("scion") "#"))))))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt SCXML module")
    (description "The Qt SCXML module provides functionality to create state
machines from SCXML files.  This includes both dynamically creating state
machines (loading the SCXML file and instantiating states and transitions) and
generating a C++ file that has a class implementing the state machine.  It
also contains functionality to support data models and executable content.")))

(define qtpurchasing-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtpurchasing")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "08sk8vw16pa1qv36rfr9dsbzlwlv6kznfpsq8wfabhkgbfl6awqs"))))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Purchasing module")
    (description "The Qt Purchasing module provides and in-app API for
purchasing goods and services.")))

(define qtcanvas3d-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtcanvas3d")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0agdxgk7knf6zkjdi6316y2k9zq72wcg5zn3cbhw4hzjw81qadgg"))
             (modules '((guix build utils)))
             (snippet
              '(delete-file-recursively "examples/canvas3d/3rdparty"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
      ;; Building the tests depends on the bundled 3rd party javascript files,
      ;; and the test phase fails to import QtCanvas3D, causing the phase to
      ;; fail, so we skip building them for now.
      ((#:phases phases)
       `(modify-phases ,phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "qmake" "QT_BUILD_PARTS = libs tools"
                               (string-append "PREFIX=" out))))))))
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs `())
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Canvas 3D module")
    (description "The Qt Canvas 3D module provides a way to make WebGL-like 3D
drawing calls from Qt Quick JavaScript.")))

(define qtcharts-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtcharts")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1rykb72gr95rxd0rvbl846ys8xvyyhrms1jz7l4hlwp6zn1jkxvm"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Charts module")
    (description "The Qt Charts module provides a set of easy to use chart
components.  It uses the Qt Graphics View Framework, therefore charts can be
easily integrated to modern user interfaces.  Qt Charts can be used as QWidgets,
QGraphicsWidget, or QML types. Users can easily create impressive graphs by
selecting one of the charts themes.")
    (license license:gpl3)))

(define qtdatavis3d-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtdatavis3d")
    (version "5.9.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0i1zd7lcakhicfpqj7dlw8hzk8x5i4ddk1427jhxcpja48l4jxy5"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Data Visualization module")
    (description "The Qt Data Visualization module provides a way to visualize
data in 3D as bar, scatter, and surface graphs. It is especially useful for
visualizing depth maps and large quantities of rapidly changing data, such as
data received from multiple sensors. The look and feel of graphs can be
customized by using themes or by adding custom items and labels to them.")
    (license license:gpl3)))

(define qtnetworkauth-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtnetworkauth")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0mqcqkp9h5bgzb3wfy239wh1c9s9zxd7mww11c0jyp56wk5balcx"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg-for-rstudio)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'remove-failing-test
             (lambda _
               ;; These tests can't find their test data.
               (substitute* "tests/auto/auto.pro"
                 (("oauth1 ") "# oauth1 "))
               #t))))))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)))
    (synopsis "Qt Network Authorization module")
    (description "The Qt Network Authorization module provides an
implementation of OAuth and OAuth2 authenticathon methods for Qt.")))

(define qtremoteobjects-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtremoteobjects")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1wb50dapv0l45c0rfmpiaddvwv9na50lmd5zmm052q9d1xb15f6b"))))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)))
    (synopsis "Qt Remote Objects module")
    (description "The Qt Remote Objects module is an @dfn{inter-process
communication} (IPC) module developed for Qt.  The idea is to extend existing
Qt's functionalities to enable an easy exchange of information between
processes or computers.")))

(define qtspeech-for-rstudio
  (package (inherit qtsvg-for-rstudio)
    (name "qtspeech")
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "17h8hrixxcsn7pd5iipbj2hxpp5m2dhfq3w04wkamambb49qs80x"))))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)
       ("qtmultimedia" ,qtmultimedia-for-rstudio)
       ("qtxmlpatterns" ,qtxmlpatterns-for-rstudio)))
    (synopsis "Qt Speech module")
    (description "The Qt Speech module enables a Qt application to support
accessibility features such as text-to-speech, which is useful for end-users
who are visually challenged or cannot access the application for whatever
reason.  The most common use case where text-to-speech comes in handy is when
the end-user is driving and cannot attend the incoming messages on the phone.
In such a scenario, the messaging application can read out the incoming
message.")))

(define qtwebkit-for-rstudio
  (package
    (name "qtwebkit")
    (version "5.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.qt.io/official_releases/qt/"
                            (version-major+minor version) "/" version
                            "/submodules/" name "-opensource-src-"
                            version ".tar.xz"))
        (sha256
         (base32
          "1ksjn1vjbfhdm4y4rg08ag4krk87ahp7qcdcpwll42l0rnz61998"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2.7)
       ("ruby" ,ruby)
       ("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("icu" ,icu4c)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libwebp" ,libwebp)
       ("sqlite" ,sqlite)
       ("fontconfig" ,fontconfig)
       ("libxrender" ,libxrender)
       ("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)
       ("qtmultimedia" ,qtmultimedia-for-rstudio)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-qmlwebkit-plugins-rpath
           (lambda _
             (substitute* "Source/WebKit/qt/declarative/experimental/experimental.pri"
               (("RPATHDIR_RELATIVE_TO_DESTDIR = \\.\\./\\.\\./lib")
                "RPATHDIR_RELATIVE_TO_DESTDIR = ../../../../../lib"))
             (substitute* "Source/WebKit/qt/declarative/public.pri"
               (("RPATHDIR_RELATIVE_TO_DESTDIR = \\.\\./\\.\\./lib")
                "RPATHDIR_RELATIVE_TO_DESTDIR = ../../../../lib"))
             #t))
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (setenv "QMAKEPATH"
                              (string-append (getcwd) "/Tools/qmake:"
                                             (getenv "QMAKEPATH")))
                      (system* "qmake"))))
         ;; prevent webkit from trying to install into the qtbase store directory,
         ;; and replace references to the build directory in linker options:
         (add-before 'build 'patch-installpaths
                     (lambda* (#:key outputs inputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (qtbase (assoc-ref inputs "qtbase"))
                              (builddir (getcwd))
                              (linkbuild (string-append "-L" builddir))
                              (linkout (string-append "-L" out))
                              (makefiles
                               (map-in-order
                                (lambda (i)
                                  (let* ((in (car i))
                                         (mf (string-append (dirname in) "/"
                                                            (cdr i))))
                                    ;; by default, these Makefiles are
                                    ;; generated during install, but we need
                                    ;; to generate them now
                                    (system* "qmake" in "-o" mf)
                                    mf))
                                '(("Source/api.pri" . "Makefile.api")
                                  ("Source/widgetsapi.pri"
                                   . "Makefile.widgetsapi")
                                  ("Source/WebKit2/WebProcess.pro"
                                   . "Makefile.WebProcess")
                                  ("Source/WebKit2/PluginProcess.pro"
                                   . "Makefile.PluginProcess")
                                  ("Source/WebKit/qt/declarative/public.pri"
                                   . "Makefile.declarative.public")
                                  ("Source/WebKit/qt/declarative/experimental/experimental.pri"
                                   . "Makefile.declarative.experimental")
                                  ("Source/WebKit/qt/examples/platformplugin/platformplugin.pro"
                                   . "Makefile")))))
                         ;; Order of qmake calls and substitutions matters here.
                         (system* "qmake" "-prl" "Source/widgetsapi.pri"
                                  "-o" "Source/Makefile")
                         (substitute* (find-files "lib" "libQt5.*\\.prl")
                           ((linkbuild) linkout))
                         (substitute* (find-files "lib"
                                                  "libQt5WebKit.*\\.la")
                           (("libdir='.*'")
                            (string-append "libdir='" out "/lib'"))
                           ((linkbuild) linkout))
                         (substitute* (find-files "lib/pkgconfig"
                                                  "Qt5WebKit.*\\.pc")
                           (((string-append "prefix=" qtbase))
                            (string-append "prefix=" out))
                           ((linkbuild) linkout))
                         ;; Makefiles must be modified after .prl/.la/.pc
                         ;; files, lest they get rebuilt:
                         (substitute* makefiles
                           (((string-append "\\$\\(INSTALL_ROOT\\)" qtbase))
                            out )
                           (((string-append "-Wl,-rpath," builddir))
                            (string-append "-Wl,-rpath," out)))))))))
    (home-page "https://www.webkit.org")
    (synopsis "Web browser engine and classes to render and interact with web
content")
    (description "QtWebKit provides a Web browser engine that makes it easy to
embed content from the World Wide Web into your Qt application.  At the same
time Web content can be enhanced with native controls.")

    (license license:lgpl2.1+)))

;;
;; RStudio was imported from:
;; https://github.com/BIMSBbioinfo/guix-bimsb/blob/master/bimsb/packages/staging.scm
;;

(define-public rstudio-server
  (package
   (name "rstudio-server")
   (version "1.1.453")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/rstudio/rstudio.git")
                   (commit (string-append "v" version))))
             (sha256
              (base32
               "0caz8c0p7kgz0s524r37jycsv7clpry4k54xg02jbwzw37imag30"))
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
          (lambda _
            ;; Disable checks for bundled dependencies.  We take care of them by other means.
            (substitute* "src/cpp/session/CMakeLists.txt"
                         (("if\\(NOT EXISTS \"\\$\\{RSTUDIO_DEPENDENCIES_DIR\\}/common/rmarkdown\"\\)") "if (FALSE)")
                         (("if\\(NOT EXISTS \"\\$\\{RSTUDIO_DEPENDENCIES_DIR\\}/common/rsconnect\"\\)") "if (FALSE)"))
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
               (mkdir "mathjax-26")
               (zero? (system* "unzip" "-qd" "mathjax-26"
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
         (uri "https://s3.amazonaws.com/rstudio-buildtools/mathjax-26.zip")
         (sha256
          (base32 "0wbcqb9rbfqqvvhqr1pbqax75wp8ydqdyhp91fbqfqp26xzjv6lk"))))
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
      ("clang" ,clang-3.5)
      ("boost" ,boost)
      ("libuuid" ,util-linux)
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
               #t))))))
    (inputs
     `(("qtbase" ,qtbase-for-rstudio)
       ("qtdeclarative" ,qtdeclarative-for-rstudio)
       ("qtlocation" ,qtlocation-for-rstudio)
       ("qtsvg" ,qtsvg-for-rstudio)
       ("qtsensors" ,qtsensors-for-rstudio)
       ("qtxmlpatterns" ,qtxmlpatterns-for-rstudio)
       ("qtwebkit" ,qtwebkit-for-rstudio)
       ("qtwebchannel" ,qtwebchannel-for-rstudio)
       ,@(package-inputs rstudio-server)))
    (synopsis "Integrated development environment (IDE) for R (desktop version)")))
