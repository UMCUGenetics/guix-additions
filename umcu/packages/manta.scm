;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;;
;;; This file is not officially part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (umcu packages manta)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages python)
  #:use-module (umcu packages pyflow))

;;
;;  WARNING: This package is a work-in-progess.  It does not work.
;;

(define-public manta-0.29.6
  (package
   (name "manta")
   (version "0.29.6")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/manta/releases/download/v"
                  version "/manta-" version ".release_src.tar.bz2"))
            (file-name (string-append name "-" version ".tar.bz2"))
            (sha256
             (base32 "05nwr57nhjcshviqy8bf4hlfhl01nzis1rc71m694y0g54rwnvzh"))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f ; TODO: Temporarily disabled.
      #:phases
      (modify-phases %standard-phases
        ;; This configure script is not an autotools configure script.
        ;; Therefore, the default options applied by the standard configure
        ;; phase result in a configuration failure.
        ;; The configure script should be run before running cmake, therefore,
        ;; we replace the original configure phase.
        (replace 'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((source-dir (getcwd))
                  (boost-path (assoc-ref inputs "boost"))
                  (cmake-path (string-append (assoc-ref inputs "cmake")
                                             "/bin/cmake")))
              (mkdir "../build")
              (chdir "../build")
              (setenv "BOOST_ROOT" (string-append boost-path))
              (zero? (system* (string-append source-dir "/configure")
                              (string-append "--with-cmake=" cmake-path)
                              (format #f "--jobs=~a" (parallel-job-count))
                              (string-append "--prefix=" (assoc-ref %outputs "out"))
                              "--build-type=Release")))))
        (add-before 'configure 'use-dynamic-boost
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; By default, it looks for static libraries.  This substitution
            ;; makes sure it looks for dynamically linked versions of Boost.
            (substitute* "src/cmake/boost.cmake"
              (("Boost_USE_STATIC_LIBS ON")
               "Boost_USE_STATIC_LIBS OFF"))
            ;;(substitute* "redist/CMakeLists.txt"
            ;;  (("superset(ZLIB_DIR")
            ;;   (format #f "superset(ZLIB_DIR ~s) #" (assoc-ref inputs "zlib"))))
            ))
        (replace 'build
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((zlib-path (assoc-ref inputs "zlib")))
              (system* "cmake"
               (string-append "-DZLIB_INCLUDE_DIR=" zlib-path "/include")
               (string-append "-DZLIB_LIBRARY=" zlib-path "/lib"))
              (system* "make" (format #f "-j~a" (parallel-job-count)))))))))
    (inputs
     `(("cmake" ,cmake)
       ("boost" ,boost)
       ("pyflow" ,pyflow)
       ("python" ,python-2)
       ("cppcheck" ,cppcheck)
       ("doxygen" ,doxygen)
       ("zlib" ,zlib)))
    (home-page "https://github.com/Illumina/manta")
   (synopsis "Structural variant and indel caller for mapped sequencing data")
   (description "Manta calls structural variants (SVs) and indels from mapped
paired-end sequencing reads.  It is optimized for analysis of germline variation
in small sets of individuals and somatic variation in tumor/normal sample pairs.
Manta discovers, assembles and scores large-scale SVs, medium-sized indels and
large insertions within a single efficient workflow.")
   (license license:gpl3)))

(define-public manta-0.29.5
  (package (inherit  manta-0.29.6)
   (name "manta")
   (version "0.29.5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Illumina/manta/releases/download/v"
                  version "/manta-" version ".release_src.tar.bz2"))
            (file-name (string-append name "-" version ".tar.bz2"))
            (sha256
             (base32 "1svansz8m5z481454mh206b6f60qimvgwm17nqnazbr5s3x9yb66")
            )))))
