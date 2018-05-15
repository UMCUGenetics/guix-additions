;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages guile)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile)
  #:use-module (umcu packages guix))

(define-public guile-sparql
  (package
   (name "guile-sparql")
   (version "0.0.6")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://www.roelj.com/guile-sparql-" version ".tar.gz"))
            (sha256
             (base32 "0nnpwq91s4zm7nihwvv0rkf2h4dz306rbnr1kp796hw623ikrxb8"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
       (add-before 'configure 'autoreconf
        (lambda _
          (system* "autoreconf" "-vfi"))))))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("guile" ,guile-2.2)))
   (home-page "https://github.com/roelj/guile-sparql")
   (synopsis "SPARQL module for Guile")
   (description "This package provides an interface to query SPARQL
endpoints from Guile.")
   (license license:gpl3+)))

(define-public sesame
  (package
   (name "sesame")
   (version "0.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://www.roelj.com/sesame-" version ".tar.gz"))
            (sha256
             (base32 "1qapjjlkwwsk1cich8m467y7b73b17p3g39pkxvcplymwad6pvix"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-executable
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out            (assoc-ref outputs "out"))
                  (load-path      (string-append
                                   (assoc-ref outputs "out") "/share/guile/site/2.2:"
                                   (getenv "GUILE_LOAD_PATH")))
                  (compiled-path  (string-append
                                   (assoc-ref outputs "out") "/lib/guile/2.2/site-ccache:"
                                   (getenv "GUILE_LOAD_COMPILED_PATH"))))
              (wrap-program (string-append out "/bin/sesame")
               `("PATH" ":" = (,(getenv "PATH")))
               `("GUILE_LOAD_PATH" ":" = (,load-path))
               `("GUILE_LOAD_COMPILED_PATH" ":" = (,compiled-path))))
            #t)))))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("guile" ,guile-2.2)
      ("guile-sparql" ,guile-sparql)
      ("guile-json" ,guile-json)
      ("curl" ,curl)
      ("tar" ,tar)
      ("gzip" ,gzip)))
   (home-page #f)
   (synopsis "")
   (description "")
   (license license:gpl3+)))

(define-public graph-cnv-analysis
  (package
   (name "graph-cnv-analysis")
   (version "0.0.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/UMCUGenetics/" name "/releases/download/"
                  version "/" name "-" version ".tar.gz"))
            (sha256
             (base32 "1s4ycsj2b06gspjf3zcj0xg150kd1a29swmgalbvbnw1hbc83ni8"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-executable
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out            (assoc-ref outputs "out"))
                  (load-path      (string-append
                                   (assoc-ref outputs "out") "/share/guile/site/2.2:"
                                   (getenv "GUILE_LOAD_PATH")))
                  (compiled-path  (string-append
                                   (assoc-ref outputs "out") "/lib/guile/2.2/site-ccache:"
                                   (getenv "GUILE_LOAD_COMPILED_PATH"))))
              (wrap-program (string-append out "/bin/plot-cnv-regions")
               `("PATH" ":" = (,(getenv "PATH")))
               `("GUILE_LOAD_PATH" ":" = (,load-path))
               `("GUILE_LOAD_COMPILED_PATH" ":" = (,compiled-path))))
            #t)))))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("guile" ,guile-2.2)
      ("guile-sparql" ,guile-sparql)))
   (home-page "https://github.com/UMCUGenetics/graph-cnv-analysis")
   (synopsis "")
   (description "")
   (license license:gpl3+)))
