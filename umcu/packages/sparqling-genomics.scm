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

(define-module (umcu packages sparqling-genomics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim))

(define-public sparqling-genomics
  (package
   (name "sparqling-genomics")
   (version "0.99.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/UMCUGenetics/sparqling-genomics/"
                  "releases/download/" version "/sparqling-genomics-"
                  version ".tar.gz"))
            (sha256
             (base32
              "0n1gxcn62zmz6rvqj0nybn08l3k49p8204g347zf0a8965p9c6vw"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'install 'setup-static-resources
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out        (assoc-ref outputs "out"))
                   (web-root   (string-append
                                out "/share/sparqing-genomics/sg-web"))
                   (static-dir (string-append web-root "/static")))
              (mkdir-p static-dir)
              (copy-recursively "web/static" static-dir))))
        (add-after 'install 'wrap-executable
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out  (assoc-ref outputs "out"))
                   (guile-load-path
                    (string-append out "/share/guile/site/2.2"))
                   (guile-load-compiled-path
                    (string-append out "/lib/guile/2.2/site-ccache"))
                   (web-root (string-append
                              out "/share/sparqing-genomics/sg-web")))
              (wrap-program (string-append out "/bin/sg-web")
                `("GUILE_LOAD_PATH" ":" prefix (,guile-load-path))
                `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                  (,guile-load-compiled-path))
                `("SG_WEB_ROOT" ":" prefix (,web-root)))))))))
   (native-inputs
    `(("texlive" ,texlive)))
   (inputs
    `(("guile" ,guile-2.2)
      ("htslib" ,htslib)
      ("libgcrypt" ,libgcrypt)
      ("pkg-config" ,pkg-config)
      ("raptor2" ,raptor2)
      ("xz" ,xz)
      ("zlib" ,zlib)))
   (propagated-inputs
    `(("gnutls" ,gnutls))) ; Needed to query HTTPS endpoints.
   (home-page "https://github.com/UMCUGenetics/sparqling-genomics")
   (synopsis "Tools to use SPARQL to analyze genomics data")
   (description "This package provides various tools to extract RDF triples
from genomic data formats, and a web interface to query SPARQL endpoints.")
   (license license:gpl3+)))
