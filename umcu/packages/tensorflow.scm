;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Roel Janssen <roel@gnu.org>
;;;
;;; This file is not officially part of GNU Guix.
;;;
;;; This software is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This software is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software.  If not, see <http://www.gnu.org/licenses/>.

(define-module (umcu packages tensorflow)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages image)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

(define-public gemmlowp-for-tensorflow
  ;; The commit hash is taken from "tensorflow/workspace.bzl".
  (let ((commit "38ebac7b059e84692f53e5938f97a9943c120d98")
        (revision "2"))
    (package
     (name "gemmlowp")
     (version (git-version "0" revision commit))
     (source (origin
              (method url-fetch)
              (uri (string-append "https://mirror.bazel.build/"
                                  "github.com/google/gemmlowp/archive/"
                                  commit ".zip"))
              (file-name (string-append "gemmlowp-" version ".zip"))
              (sha256
               (base32
                "0n56s2g8hrssm4w8qj1v58gfm56a04n9v992ixkmvk6zjiralzxq"))))
     (build-system cmake-build-system)
     (arguments
      `(#:configure-flags
        (list ,@(match (%current-system)
                  ((or "x86_64-linux" "i686-linux")
                   '("-DCMAKE_CXX_FLAGS=-msse4.1"))
                  (_ '())))
        #:phases
        (modify-phases %standard-phases
          ;; This directory contains the CMakeLists.txt.
          (add-after 'unpack 'chdir
            (lambda _ (chdir "contrib") #t))
          ;; There is no install target
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib/"))
                     (inc (string-append out "/include/")))
                (install-file "../build/libeight_bit_int_gemm.so" lib)
                (for-each (lambda (dir)
                            (let ((target (string-append inc "/" dir)))
                              (mkdir-p target)
                              (for-each (lambda (h)
                                          (install-file h target))
                                        (find-files (string-append "../" dir)
                                                    "\\.h$"))))
                          '("meta" "profiling" "public" "fixedpoint"
                            "eight_bit_int_gemm" "internal"))
                #t))))))
     (native-inputs
      `(("unzip" ,unzip)))
     (home-page "https://github.com/google/gemmlowp")
     (synopsis "Small self-contained low-precision GEMM library")
     (description
      "This is a small self-contained low-precision @dfn{general matrix
multiplication} (GEMM) library.  It is not a full linear algebra library.
Low-precision means that the input and output matrix entries are integers on
at most 8 bits.  To avoid overflow, results are internally accumulated on more
than 8 bits, and at the end only some significant 8 bits are kept.")
     (license license:asl2.0))))

(define-public tensorflow-core
  (package
    (name "tensorflow-core")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tensorflow/tensorflow.git")
             (commit (string-append "v" version))))
       (file-name (string-append "tensorflow-" version "-checkout"))
       (sha256
        (base32
         "0jljzbwhmxi8crbivwachcmlfrrv279qrsvwc62cnnbyw0n1g0kp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       (list "-f" "tensorflow/contrib/makefile/Makefile"
             "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-version
           (lambda _
             (substitute* "tensorflow/tools/git/gen_git_source.sh"
               (("^GIT_VERSION=.*")
                (string-append "GIT_VERSION=" ,version "\n")))
             #t))
         (add-after 'unpack 'unpack-third-party
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "tensorflow/contrib/makefile/"
               (let ((fft2d "downloads/fft2d")
                     (nsync "downloads/nsync"))
                 (mkdir-p fft2d)
                 (invoke "tar" "xf" (assoc-ref inputs "fft2d")
                         "-C" fft2d "--strip-components=1")
                 (mkdir-p nsync)
                 (invoke "tar" "xf" (assoc-ref inputs "nsync")
                         "-C" nsync "--strip-components=1")))))
         ;; FIXME: it would be nice to build a separate package for nsync and
         ;; use it here.  Unfortunately, I could not build Tensorflow with a
         ;; separately built nsync.
         (add-before 'build 'build-nsync
           (lambda _
             (with-directory-excursion "tensorflow/contrib/makefile/"
               (invoke "bash" "compile_nsync.sh")
               (setenv "TARGET_NSYNC_LIB"
                       "tensorflow/contrib/makefile/downloads/nsync/builds/default.linux.c++11/nsync.a")
               (setenv "HOST_NSYNC_LIB"
                       "tensorflow/contrib/makefile/downloads/nsync/builds/default.linux.c++11/nsync.a")
               #t)))
         (add-after 'unpack 'find-eigen-headers
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ensure that Eigen headers can be found
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (getenv "CPLUS_INCLUDE_PATH")
                                    ":"
                                    (assoc-ref inputs "eigen")
                                    "/include/eigen3"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include")))
               (install-file "tensorflow/contrib/makefile/gen/lib/libtensorflow-core.a" lib)
               (for-each (lambda (file)
                           (let ((target (string-append inc "/"
                                                        (dirname file))))
                             (mkdir-p target)
                             (install-file file target)))
                         (find-files "tensorflow/core" ".*\\.h$"))
               #t))))))
    (native-inputs
     `(("protobuf" ,protobuf)           ; protoc
       ;; "You may use, copy, modify this code for any purpose and without
       ;; fee. You may distribute this ORIGINAL package."
       ("fft2d"
        ,(origin
           (method url-fetch)
           (uri "https://mirror.bazel.build/www.kurims.kyoto-u.ac.jp/~ooura/fft.tgz")
           (sha256
            (base32
             "15jjkfvhqvl2c0753d2di8hz0pyzn598g74wqy79awdrf1y67fsj"))))
       ("nsync"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://mirror.bazel.build/"
                               "github.com/google/nsync/archive/"
                               "0559ce013feac8db639ee1bf776aca0325d28777.tar.gz"))
           (sha256
            (base32
             "0qdkyqym34x739mmzv97ah5r7ph462v5xkxqxvidmcfqbi64b132"))))
       ("googletest" ,googletest)))
    (inputs
     `(("eigen" ,eigen)
       ("gemmlowp" ,gemmlowp-for-tensorflow)
       ("protobuf" ,protobuf)
       ("zlib" ,zlib)))
    (home-page "https://tensorflow.org")
    (synopsis "Machine learning framework")
    (description
     "TensorFlow is a software library for high performance numerical
computation.  Its flexible architecture allows easy deployment of computation
across a variety of platforms, and from desktops to clusters of servers to
mobile and edge devices.

This package provides only the core library.")
    (license license:asl2.0)))

;; XXX: temporary package for tensorflow / grpc
(define-public c-ares-next
  (package
    (name "c-ares")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://c-ares.haxx.se/download/" name "-" version
                    ".tar.gz"))
              (sha256
               (base32
                "0lk8knip4xk6qzksdkn7085mmgm4ixfczdyyjw656c193y3rgnvc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; some tests seem to require Internet connection
       #:configure-flags
       (list "-DCARES_BUILD_TESTS=ON")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://c-ares.haxx.se/")
    (synopsis "C library for asynchronous DNS requests")
    (description
      "C-ares is a C library that performs DNS requests and name resolution
asynchronously.  It is intended for applications which need to perform DNS
queries without blocking, or need to perform multiple DNS queries in parallel.
The primary examples of such applications are servers which communicate with
multiple clients and programs with graphical user interfaces.")
    (license (license:x11-style "https://c-ares.haxx.se/license.html"))))

(define-public protobuf-next
  (package (inherit protobuf)
    (name "protobuf")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-cpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0a955bz59ihrb5wg7dwi12xajdi5pmz4bl0g147rbdwv393jwwxk"))))))

(define-public grpc
  (package
    (name "grpc")
    (version "1.16.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jimqz3115f9pli5w6ik9wi7mjc7ix6y7yrq4a1ab9fc3dalj7p2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:configure-flags
       (list "-DgRPC_ZLIB_PROVIDER=package"
             "-DgRPC_CARES_PROVIDER=package"
             "-DgRPC_SSL_PROVIDER=package"
             "-DgRPC_PROTOBUF_PROVIDER=package")))
    (inputs
     `(("c-ares" ,c-ares-next)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (native-inputs
     `(("protobuf" ,protobuf-next)
       ("python" ,python-wrapper)))
    (home-page "https://grpc.io")
    (synopsis "High performance universal RPC framework")
    (description "gRPC is a modern open source high performance @dfn{Remote
Procedure Call} (RPC) framework that can run in any environment.  It can
efficiently connect services in and across data centers with pluggable support
for load balancing, tracing, health checking and authentication.  It is also
applicable in last mile of distributed computing to connect devices, mobile
applications and browsers to backend services.")
    (license license:asl2.0)))

;; Older Graphviz needed for pygraphviz.  See
;; https://github.com/pygraphviz/pygraphviz/issues/175
(define-public graphviz-2.38
  ;; This commit corresponds to the changelog change for version 2.38.0.
  ;; There are no tags.
  (let ((commit "f54ac2c9313ae80ccf76ef4ac6aa9be820a23126")
        (revision "1"))
    (package (inherit graphviz)
      (name "graphviz")
      (version (git-version "2.38.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/graphviz/graphviz.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vjg308gflmi1khgjmcj431cnkrlv12bg4cqah39mwhny92jy92x"))))
      (arguments
       (substitute-keyword-arguments (package-arguments graphviz)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'prepare-bootstrap
               (lambda _
                 (substitute* "autogen.sh"
                   (("/bin/sh") (which "sh"))
                   (("\\$GRAPHVIZ_VERSION_DATE") "0"))
                 (setenv "CONFIG_SHELL" (which "sh"))
                 (setenv "SHELL" (which "sh"))

                 (map make-file-writable (find-files "." ".*"))
                 #t))
             (replace 'bootstrap
               (lambda _ (invoke (which "sh") "autogen.sh" "NOCONFIG") #t))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("flex" ,flex)
         ("perl" ,perl)
         ("tcl" ,tcl)
         ,@(package-native-inputs graphviz))))))

(define-public python-doctest-ignore-unicode
  (package
    (name "python-doctest-ignore-unicode")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "doctest-ignore-unicode" version))
       (sha256
        (base32
         "1m9aa4qnyj21lbq4sbvmv1vcz7zksss4rz37ddf2hxv4hk8b547w"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/gnublade/doctest-ignore-unicode")
    (synopsis "Ignore Unicode literal prefixes in doctests")
    (description
     "This package adds support for a flag to ignore Unicode literal prefixes
in doctests.")
    (license license:asl2.0)))

(define-public python-pygraphviz
  (package
    (name "python-pygraphviz")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pygraphviz/pygraphviz.git")
             (commit (string-append "pygraphviz-" version))))
       (file-name (string-append "pygraphviz-" version "-checkout"))
       (sha256
        (base32
         "1yldym38m8ckgflln83i88143pd9fjj1vfp23sq39fs6np5g0nzp"))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags
       (let ((graphviz (assoc-ref %build-inputs "graphviz")))
         (list (string-append "--include-path=" graphviz "/include")
               (string-append "--library-path=" graphviz "/lib")))))
    (inputs
     `(("graphviz" ,graphviz-2.38)))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-mock" ,python-mock)
       ("python-doctest-ignore-unicode" ,python-doctest-ignore-unicode)))
    (home-page "http://pygraphviz.github.io")
    (synopsis "Python interface to Graphviz")
    (description "PyGraphviz is a Python interface to the Graphviz graph
layout and visualization package.  With PyGraphviz you can create, edit, read,
write, and draw graphs using Python to access the Graphviz graph data
structure and layout algorithms.")
    (license license:bsd-3)))

(define-public python-absl-py
  (package
    (name "python-absl-py")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "absl-py" version))
       (sha256
        (base32
         "1mp9lk0b2qa37b7y6ak4lvf6ifw2ylyy6bkf9ik77md3j4xrwlc7"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/abseil/abseil-py")
    (synopsis "Abseil Python common libraries")
    (description
     "This package provides the Abseil Python Common Libraries, a collection
of Python libraries for building Python applications.")
    (license license:asl2.0)))

(define-public python-astor
  (package
    (name "python-astor")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astor" version))
       (sha256
        (base32
         "13gv6f2xz9i564byp21gcpc0l3w4cs23k1wbcam8kky2ls3hvhwm"))))
    (build-system python-build-system)
    ;; FIXME: There are two errors and two test failures.
    (arguments `(#:tests? #f))
    (home-page "https://github.com/berkerpeksag/astor")
    (synopsis "Read/rewrite/write Python ASTs")
    (description "Astor is designed to allow easy manipulation of Python
source via the AST.")
    (license license:bsd-3)))

(define-public python-astunparse
  (package
    (name "python-astunparse")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astunparse" version))
       (sha256
        (base32
         "1jhidwyrqn17avqh9xnnm3wd7q7aahaq009cba67g86y6gxicyyj"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; there are none
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-wheel" ,python-wheel)))
    (home-page "https://github.com/simonpercivall/astunparse")
    (synopsis "AST unparser for Python")
    (description "This package provides an AST unparser for Python.  It is a
factored out version of @code{unparse} found in the Python source
distribution.")
    (license license:bsd-3)))

(define-public python-gast
  (package
    (name "python-gast")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gast" version))
       (sha256
        (base32
         "0c296xm1vz9x4w4inmdl0k8mnc0i9arw94si2i7pglpc461r0s3h"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-astunparse" ,python-astunparse)))
    (home-page "https://pypi.org/project/gast/")
    (synopsis "Generic Python AST that abstracts the underlying Python version")
    (description
     "GAST provides a compatibility layer between the AST of various Python
versions, as produced by @code{ast.parse} from the standard @code{ast}
module.")
    (license license:bsd-3)))

(define-public python-grpcio
  (package
    (name "python-grpcio")
    (version "1.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grpcio" version))
       (sha256
        (base32
         "0qb9y6j83nxa6d4kc60i8yfgdm7a8ms7b54kncjzf5y7nsxp8rzx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://grpc.io")
    (synopsis "HTTP/2-based RPC framework")
    (description "This package provides a Python library for communicating
with the HTTP/2-based RPC framework gRPC.")
    (license license:asl2.0)))

(define-public tensorflow
  (package
    (name "tensorflow")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tensorflow/tensorflow.git")
             (commit (string-append "v" version))))
       (file-name (string-append "tensorflow-" version "-checkout"))
       (sha256
        (base32
         "0a9kwha395g3wgxfwln5j8vn9nkspmd75xldrlqdq540w996g8xa"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:build-type "Release"
       #:configure-flags
       (let ((protobuf (assoc-ref %build-inputs "protobuf"))
             (snappy (assoc-ref %build-inputs "snappy"))
             (sqlite (assoc-ref %build-inputs "sqlite")))
         (list ;; TODO: Use protobuf from Guix
          ;; (string-append "-Dprotobuf_STATIC_LIBRARIES="
          ;;                protobuf "/lib/libprotobuf.a")
          (string-append "-DPROTOBUF_PROTOC_EXECUTABLE="
                         protobuf "/bin/protoc")
          ;; TODO: Use snappy from Guix
          ;; (string-append "-Dsnappy_STATIC_LIBRARIES="
          ;;                snappy "/lib/libsnappy.a")
          ;; (string-append "-Dsnappy_HEADERS="
          ;;                snappy "/include/snappy.h")
          ;; TODO: this is ignored.  Should be a definition in code.
          ;;"-DTF_USE_SNAPPY"
          ;; Use sqlite from Guix
          (string-append "-Dsqlite_STATIC_LIBRARIES="
                         sqlite "/lib/libsqlite.a")
          (string-append "-Dsqlite_HEADERS="
                         sqlite "/include/sqlite3.h "
                         sqlite "/include/sqlite3ext.h")
          ;; Use system libraries wherever possible.  Currently, this
          ;; only affects zlib.
          "-Dsystemlib_ALL=ON"

          ;;"-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=true"
          "-Dtensorflow_ENABLE_POSITION_INDEPENDENT_CODE=ON"
          "-Dtensorflow_BUILD_SHARED_LIB=ON"
          "-Dtensorflow_OPTIMIZE_FOR_NATIVE_ARCH=OFF"
          "-Dtensorflow_ENABLE_SSL_SUPPORT=OFF"
          "-Dtensorflow_BUILD_CONTRIB_KERNELS=OFF"))
       #:make-flags
       (list "CC=gcc")
       #:modules ((ice-9 ftw)
                  (guix build utils)
                  (guix build cmake-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-source-file-times-to-1980
           ;; At the end of the tf_python_build_pip_package target, a ZIP
           ;; archive should be generated via bdist_wheel, but it fails with
           ;; "ZIP does not support timestamps before 1980".  Luckily,
           ;; SOURCE_DATE_EPOCH is respected, which we set to some time in
           ;; 1980.
          (lambda _ (setenv "SOURCE_DATE_EPOCH" "315532800") #t))
         ;; See https://github.com/tensorflow/tensorflow/issues/20517#issuecomment-406373913
         (add-after 'unpack 'python3.7-compatibility
           (lambda _
             (substitute* '("tensorflow/python/eager/pywrap_tfe_src.cc"
                            "tensorflow/python/lib/core/ndarray_tensor.cc"
                            "tensorflow/python/lib/core/py_func.cc")
               (("PyUnicode_AsUTF8") "(char *)PyUnicode_AsUTF8"))
             (substitute* "tensorflow/c/eager/c_api.h"
               (("unsigned char async")
                "unsigned char is_async"))

             ;; Remove dependency on tensorboard, a complicated but probably
             ;; optional package.
             (substitute* "tensorflow/tools/pip_package/setup.py"
               ((".*'tensorboard >.*") ""))
             #t))
         (add-after 'python3.7-compatibility 'chdir
           (lambda _ (chdir "tensorflow/contrib/cmake") #t))
         (add-after 'chdir 'disable-downloads
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "external" "\\.cmake$")
               (("GIT_REPOSITORY.*") "")
               (("GIT_TAG.*") "")
               (("PREFIX ")
                "DOWNLOAD_COMMAND \"\"\nPREFIX "))

             ;; Use packages from Guix
             (let ((grpc (assoc-ref inputs "grpc")))
               (substitute* "CMakeLists.txt"
                 ;; Sqlite
                 (("include\\(sqlite\\)") "")
                 (("\\$\\{sqlite_STATIC_LIBRARIES\\}")
                  (string-append (assoc-ref inputs "sqlite")
                                 "/lib/libsqlite3.so"))
                 (("sqlite_copy_headers_to_destination") "")

                 ;; PNG
                 (("include\\(png\\)") "")
                 (("\\$\\{png_STATIC_LIBRARIES\\}")
                  (string-append (assoc-ref inputs "libpng")
                                 "/lib/libpng16.so"))
                 (("png_copy_headers_to_destination") "")

                 ;; JPEG
                 (("include\\(jpeg\\)") "")
                 (("\\$\\{jpeg_STATIC_LIBRARIES\\}")
                  (string-append (assoc-ref inputs "libjpeg")
                                 "/lib/libjpeg.so"))
                 (("jpeg_copy_headers_to_destination") "")

                 ;; GIF
                 (("include\\(gif\\)") "")
                 (("\\$\\{gif_STATIC_LIBRARIES\\}")
                  (string-append (assoc-ref inputs "giflib")
                                 "/lib/libgif.so"))
                 (("gif_copy_headers_to_destination") "")

                 ;; lmdb
                 (("include\\(lmdb\\)") "")
                 (("\\$\\{lmdb_STATIC_LIBRARIES\\}")
                  (string-append (assoc-ref inputs "lmdb")
                                 "/lib/liblmdb.so"))
                 (("lmdb_copy_headers_to_destination") "")

                 ;; TODO: the "protobuf" project is depended on by other
                 ;; projects in the cmake files, so removing it is a little
                 ;; tricky.
                 ;;(("include\\(protobuf\\)") "")
                 ;; (("\\$\\{protobuf_STATIC_LIBRARIES\\}")
                 ;;  (string-append (assoc-ref inputs "protobuf:static")
                 ;;                 "/lib/libprotobuf.a"))
                 ;;(("protobuf_copy_headers_to_destination") "")
                 ;;(("^ +protobuf$") "")

                 ;; gRPC
                 ;; TODO: the gRPC sources are needed by protobuf
                 ;; (("include\\(grpc\\)") "")
                 ;; (("\\$\\{grpc_STATIC_LIBRARIES\\}")
                 ;;  (string-append grpc "/lib/libaddress_sorting.a "
                 ;;                 grpc "/lib/libgpr.a "
                 ;;                 grpc "/lib/libgrpc++.a "
                 ;;                 grpc "/lib/libgrpc.a "
                 ;;                 grpc "/lib/libgrpc++_cronet.a "
                 ;;                 grpc "/lib/libgrpc_cronet.a "
                 ;;                 grpc "/lib/libgrpc++_error_details.a "
                 ;;                 grpc "/lib/libgrpc_plugin_support.a "
                 ;;                 grpc "/lib/libgrpcpp_channelz.a "
                 ;;                 grpc "/lib/libgrpc++_reflection.a "
                 ;;                 grpc "/lib/libgrpc++_unsecure.a "
                 ;;                 grpc "/lib/libgrpc_unsecure.a "))
                 ;; (("list\\(APPEND tensorflow_EXTERNAL_DEPENDENCIES grpc\\)") "")
                 ))

             ;; ;; Remove dependency on bundled grpc
             ;; (substitute* "tf_core_distributed_runtime.cmake"
             ;;   (("tf_core_cpu grpc") "tf_core_cpu"))
             ;; (substitute* "tf_tools.cmake"
             ;;   (("add_dependencies\\(\\$\\{proto_text\\} grpc\\)") ""))

             (substitute* "external/grpc.cmake"
               (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/grpc/src/grpc/third_party/cares/cares/lib/libcares.a")
                (string-append (assoc-ref inputs "c-ares")
                               "/lib/libcares.so")))
             #t))
         (add-after 'configure 'unpack-third-party
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This is needed to configure bundled packages properly.
             (setenv "CONFIG_SHELL" (which "bash"))
             (for-each
              (lambda (name)
                (let* ((what  (assoc-ref inputs (string-append name "-src")))
                       (name* (string-map (lambda (c)
                                            (if (char=? c #\-)
                                                #\_ c)) name))
                       (where (string-append "../build/" name* "/src/" name*)))
                  (cond
                   ((string-suffix? ".zip" what)
                    (mkdir-p where)
                    (with-directory-excursion where
                      (invoke "unzip" what)))
                   ((string-suffix? ".tar.gz" what)
                    (mkdir-p where)
                    (invoke "tar" "xf" what
                            "-C" where "--strip-components=1"))
                   (else
                    ;; TODO: merge with "where"
                    (let ((parent (string-append "../build/" name* "/src/")))
                      (mkdir-p parent)
                      (with-directory-excursion parent
                        (when (file-exists? name*)
                          (delete-file-recursively name*))
                        (copy-recursively what name*)
                        (map make-file-writable
                             (find-files name* ".*"))))))))
              (list "boringssl"
                    "cub"
                    "double-conversion"
                    "eigen"
                    "farmhash"
                    "fft2d"
                    "grpc" ; TODO: the sources need to be available for protobuf magic
                    "highwayhash"
                    "jsoncpp"
                    "nsync"
                    "protobuf"
                    "re2"
                    "snappy"))

             (rename-file "../build/cub/src/cub/cub-1.8.0/"
                          "../build/cub/src/cub/cub/")

             ;; gRPC dependencies: use installed packages instead of bundled
             ;; sources.
             (substitute* "../build/grpc/src/grpc/CMakeLists.txt"
               (("set\\(gRPC_ZLIB_PROVIDER \"module\"")
                "set(gRPC_ZLIB_PROVIDER \"package\"")
               (("set\\(gRPC_CARES_PROVIDER \"module\"")
                "set(gRPC_CARES_PROVIDER \"package\"")
               (("set\\(gRPC_SSL_PROVIDER \"module\"")
                "set(gRPC_SSL_PROVIDER \"package\"")
               (("set\\(gRPC_PROTOBUF_PROVIDER \"module\"")
                "set(gRPC_PROTOBUF_PROVIDER \"package\""))
             #t))
         (add-after 'unpack 'fix-python-build
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Ensure that all Python dependencies can be found at build time.
             (substitute* "tensorflow/contrib/cmake/tf_python.cmake"
               (("PYTHONPATH=\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/tf_python" m)
                (string-append m ":" (getenv "PYTHONPATH"))))

             ;; Correct the RUNPATH of ops libraries generated for Python.
             ;; TODO: this doesn't work :(
             ;; /gnu/store/...-tensorflow-1.9.0/lib/python3.7/site-packages/tensorflow/contrib/seq2seq/python/ops/lib_beam_search_ops.so:
             ;; warning: RUNPATH contains bogus entries: ("/tmp/guix-build-tensorflow-1.9.0.drv-0/source/tensorflow/contrib/build")
             ;; /gnu/store/...-tensorflow-1.9.0/lib/python3.7/site-packages/tensorflow/contrib/seq2seq/python/ops/lib_beam_search_ops.so:
             ;; error: depends on 'libpywrap_tensorflow_internal.so', which
             ;; cannot be found in RUNPATH ...
             (substitute* "tensorflow/contrib/cmake/tf_cc_ops.cmake"
               (("set_target_properties.*")
                (string-append "set_target_properties(${_AT_TARGET} PROPERTIES \
COMPILE_FLAGS ${target_compile_flags} \
INSTALL_RPATH_USE_LINK_PATH TRUE \
INSTALL_RPATH " (assoc-ref outputs "out") "/lib)\n")))
             #t))
         (add-after 'unpack 'find-eigen-headers
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ensure that Eigen headers can be found
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (getenv "CPLUS_INCLUDE_PATH")
                                    ":"
                                    (assoc-ref inputs "eigen")
                                    "/include/eigen3"))
             #t))
         (add-after 'build 'build-pip-package
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "LDFLAGS"
                     (string-append "-Wl,-rpath="
                                    (assoc-ref outputs "out") "/lib"))
             (invoke "make" "tf_python_build_pip_package")
             #t))
         (add-after 'build-pip-package 'install-python
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (wheel (car (find-files "../build/tf_python/dist/" "\\.whl$"))))
               (invoke "python" "-m" "pip" "install" wheel
                       (string-append "--prefix=" out))

               ;; XXX: broken RUNPATH, see fix-python-build phase.
               (delete-file (string-append out "/lib/python3.7/site-packages/tensorflow/contrib/seq2seq/python/ops/lib_beam_search_ops.so"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("protobuf" ,protobuf-next)      ; protoc
       ;; The commit hashes and URLs for third-party source code are taken
       ;; from "tensorflow/workspace.bzl".
       ("boringssl-src"
        ,(let ((commit "ee7aa02")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://boringssl.googlesource.com/boringssl")
                   (commit commit)))
             (file-name (string-append "boringssl-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "1jf693q0nw0adsic6cgmbdx6g7wr4rj4vxa8j1hpn792fqhd8wgw")))))
       ("cub-src"
        ,(let ((version "1.8.0"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/github.com/NVlabs/"
                                 "cub/archive/" version ".zip"))
             (file-name (string-append "cub-" version ".zip"))
             (sha256
              (base32
               "1hsqikqridb90dkxkjr2918dcry6pfh46ccnwrzawl56aamhdykb")))))
       ("double-conversion-src"
        ,(let ((commit "5664746")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/double-conversion.git")
                   (commit commit)))
             (file-name (string-append "double-conversion-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "1h5lppqqxcvdg5jq42i5msgwx20ryij3apvmndflngrgdpc04gn1")))))
       ("eigen-src"
        ,(let ((version "fd6845384b86"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/bitbucket.org/"
                                 "eigen/eigen/get/" version ".tar.gz"))
             (file-name (string-append "eigen-" version ".tar.gz"))
             (sha256
              (base32
               "1aan4mvan8i5xsx4ivyqlg8ckbnnaqjw6i9ad8my992gg1fl2mnr")))))
       ("farmhash-src"
        ,(let ((commit "816a4ae622e964763ca0862d9dbd19324a1eaf45"))
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://mirror.bazel.build/github.com/google/farmhash/archive/"
                   commit ".tar.gz"))
             (file-name (string-append "farmhash-0-"
                                       (string-take commit 7)
                                       ".tar.gz"))
             (sha256
              (base32
               "185b2xdxl4d4cnsnv6abg8s22gxvx8673jq2yaq85bz4cdy58q35")))))
       ;; The license notice on the home page at
       ;; http://www.kurims.kyoto-u.ac.jp/~ooura/fft.html says:
       ;;   Copyright Takuya OOURA, 1996-2001
       ;;
       ;;   You may use, copy, modify and distribute this code for any purpose
       ;;   (include commercial use) and without fee. Please refer to this
       ;;   package when you modify this code.
       ;;
       ;; We take the identical tarball from the Bazel mirror, because the URL
       ;; at the home page is not versioned and might change.
       ("fft2d-src"
        ,(origin
           (method url-fetch)
           (uri "https://mirror.bazel.build/www.kurims.kyoto-u.ac.jp/~ooura/fft.tgz")
           (file-name "fft2d.tar.gz")
           (sha256
            (base32
             "15jjkfvhqvl2c0753d2di8hz0pyzn598g74wqy79awdrf1y67fsj"))))
       ("grpc-src"
        ,(let ((version "d184fa229d75d336aedea0041bd59cb93e7e267f"))
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://mirror.bazel.build/"
                   "github.com/grpc/grpc/archive/"
                   version".tar.gz"))
             (file-name (string-append "grpc-" version ".tar.gz"))
             (sha256
              (base32
               "0wsn0yvwnc08i9cq76083kgfv9k8q1wafnap6gvn32ki1qqk2nw9")))))
       ("highwayhash-src"
        ,(let ((commit "be5edafc2e1a455768e260ccd68ae7317b6690ee")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/highwayhash.git")
                   (commit commit)))
             (file-name (string-append "highwayhash-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "154jwf98cyy54hldr94pgjn85zynly3abpnc1avmb8a18lzwjyb6")))))
       ("jsoncpp-src"
        ,(let ((commit "4356d9b")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/open-source-parsers/jsoncpp.git")
                   (commit commit)))
             (file-name (string-append "jsoncpp-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "1anixxs5nwqknmcdxjd9zii5x0z7jx5qy011narjp2vxid59dzqa")))))
       ("nsync-src"
        ,(let ((version "0559ce013feac8db639ee1bf776aca0325d28777")
               (revision "1"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/"
                                 "github.com/google/nsync/archive/"
                                 version ".tar.gz"))
             (file-name (string-append "nsync-0." revision
                                       "-" (string-take version 7)
                                       ".tar.gz"))
             (sha256
              (base32
               "0qdkyqym34x739mmzv97ah5r7ph462v5xkxqxvidmcfqbi64b132")))))
       ("protobuf-src"
        ,(let ((version "396336eb961b75f03b25824fe86cf6490fb75e3a"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/"
                                 "github.com/google/protobuf/archive/"
                                 version ".tar.gz"))
             (file-name (string-append "protobuf-" version ".tar.gz"))
             (sha256
              (base32
               "1qsr5hgmmikshrg3035q6klbpv945lxfz0h8xhry4aj7rxx90vc4")))))
       ("re2-src"
        ,(let ((commit "e7efc48")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/re2")
                   (commit commit)))
             (file-name (string-append "re2-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "161g9841rjfsy5pn52fcis0s9hdr7rxvb06pad38j5rppfihvign")))))
       ("snappy-src"
        ,(let ((version "1.1.7"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/"
                                 "github.com/google/snappy/archive/"
                                 version ".tar.gz"))
             (file-name (string-append "snappy-" version ".tar.gz"))
             (sha256
              (base32
               "1m7rcdqzkys5lspj8jcsaah8w33zh28s771bw0ga2lgzfgl05yix")))))
       ("googletest" ,googletest)
       ("swig" ,swig)
       ("unzip" ,unzip)))
    (propagated-inputs
     `(("python-absl-py" ,python-absl-py)
       ("python-astor" ,python-astor)
       ("python-gast" ,python-gast)
       ("python-grpcio" ,python-grpcio)
       ("python-numpy" ,python-numpy)
       ("python-protobuf" ,python-protobuf)
       ("python-six" ,python-six)
       ("python-termcolo" ,python-termcolor)
       ("python-wheel" ,python-wheel)))
    (inputs
     `(("c-ares" ,c-ares-next)          ; for grpc
       ("eigen" ,eigen)
       ("gemmlowp" ,gemmlowp-for-tensorflow)
       ("lmdb" ,lmdb)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("giflib" ,giflib)
       ("sqlite" ,sqlite)
       ("python" ,python-wrapper)
       ("zlib" ,zlib)))
    (home-page "https://tensorflow.org")
    (synopsis "Machine learning framework")
    (description
     "TensorFlow is a software library for high performance numerical
computation.  Its flexible architecture allows easy deployment of computation
across a variety of platforms, and from desktops to clusters of servers to
mobile and edge devices.")
    (license license:asl2.0)))
