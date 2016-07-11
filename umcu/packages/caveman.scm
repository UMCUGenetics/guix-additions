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

(define-module (umcu packages caveman)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics))

(define-public caveman
  (package
   (name "caveman")
   (version "1.9.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/cancerit/CaVEMan/archive/"
                                version ".tar.gz"))
            (sha256
             (base32 "19h631avgknidbchk6997ckgd60072nlbg9hv2zxn6vvq3fpsyb8"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags (list (string-append
                          "HTSLOC=" (assoc-ref %build-inputs "htslib")))
      #:tests? #f ; Tests require a network connection.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'patch-out-tests
          (lambda _
            (substitute* "Makefile"
             (("copyscript test") "copyscript"))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "bin/caveman" bin)
              (install-file "bin/generateCavemanUMNormVCF" bin)
              (install-file "bin/mergeCavemanResults" bin)))))))
   (inputs
    `(("htslib" ,htslib)
      ("zlib" ,zlib)))
   (home-page "http://cancerit.github.io/CaVEMan/")
   (synopsis "Implementation of an SNV expectation maximisation algorithm for
calling single base substitutions in paired data")
   (description "A C implementation of the CaVEMan program.  Uses an expectation
maximisation approach to calling single base substitutions in paired data.  It
is designed for use with a compute cluster.  Most steps in the program make use
of an index parameter.  The split step is designed to divide the genome into
chunks of adjustable size to optimise for runtime/memory usage requirements.")
   (license license:agpl3+)))
