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

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

(define-module (umcu packages snpeff)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages zip))

(define-public snpeff-bin-4.1
  (package
   (name "snpeff")
   (version "4.1")
   (source (origin
             (method url-fetch)
            (uri "mirror://sourceforge/snpeff/snpEff_v4_1_core.zip")
            (sha256
             (base32 "1vjgj6aacjsw6iczy09h18q5kx8ppxrrcq8w38g159zq7y3732kb"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("icedtea" ,icedtea-7)
      ("perl" ,perl)
      ("python" ,python)
      ("bash" ,bash)
      ("r" ,r)))
   (native-inputs
    `(("unzip" ,unzip)
      ("perl" ,perl)
      ("python" ,python-2)
      ("bash" ,bash)
      ("r" ,r)))
   (arguments
    `(#:tests? #f ; This is a binary package only, so no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; This is a binary package only.
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/share/java/" ,name))
                    (create-and-copy
                     (lambda (dir)
                       (mkdir (string-append bin "/" dir))
                       (copy-recursively dir (string-append bin "/" dir)))))
               (install-file "snpEff.config" bin)
               (install-file "snpEff.jar" bin)
               (install-file "SnpSift.jar" bin)
               (map create-and-copy '("scripts" "galaxy"))))))))
   (home-page "http://snpeff.sourceforge.net/")
   (synopsis "Genetic variant annotation and effect prediction toolbox.")
   (description "Genetic variant annotation and effect prediction toolbox.
It annotates and predicts the effects of variants on genes (such as amino
acid changes).")
   ;; No license specified.
   (license license:non-copyleft)))

(define-public snpeff-bin-4.1h
 (package (inherit snpeff-bin-4.1)
  (name "snpeff")
  (version "4.1h")
  (source (origin
      (method url-fetch)
      (uri "mirror://sourceforge/snpeff/snpEff_v4_1h_core.zip")
      (sha256
        (base32 "1j45jp4y8wj0q01clxsx46w1f4jm2wh85yl1mbrha7qbqs8c1qn3"))))
  ))
