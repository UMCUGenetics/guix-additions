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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (umcu packages guix))

(define-public shallowcp
  (package
   (name "shallowcp")
   (version "0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/roelj/shallowcp/releases/download/"
                  version "/shallowcp-" version ".tar.gz"))
            (sha256
             (base32
              "0mivgpc1bnyl1jg5rajg569s4zpjzz8a2y4afmb4szli94444w1r"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        ;; There's no compiler involved.
        (delete 'build))))
   (inputs
    `(("guile" ,guile-2.2)
      ("pkg-config" ,pkg-config)))
   (home-page "https://github.com/roelj/shallowcp")
   (synopsis "Tool for making shallow copies of a directory")
   (description "This package provides @code{shallowcp}, which can create
shallow copies of a directory (symlinks to files instead of a full copy).")
   (license license:gpl3+)))

(define-public sparqling-genomics
  (package
   (name "sparqling-genomics")
   (version "0.99.11")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/UMCUGenetics/sparqling-genomics/"
                  "releases/download/" version "/sparqling-genomics-"
                  version ".tar.gz"))
            (sha256
             (base32
              "15mi3mvypcdsz6ysczwxismr2iizckji18qnckpbdrm6x4c34pad"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags (list (string-append
                               "--with-libldap-prefix="
                               (assoc-ref %build-inputs "openldap")))
      #:modules ((guix build gnu-build-system)
                 ((guix build guile-build-system)
                  #:select (target-guile-effective-version))
                 (guix build utils))
      #:imported-modules ((guix build guile-build-system)
                          ,@%gnu-build-system-modules)
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'wrap-executable
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out  (assoc-ref outputs "out"))
                   (guile-version (target-guile-effective-version))
                   (guile-load-path
                    (string-append out "/share/guile/site/"
                                   guile-version ":"
                                   (getenv "GUILE_LOAD_PATH")))
                   (guile-load-compiled-path
                    (string-append out "/lib/guile/"
                                   guile-version "/site-ccache:"
                                   (getenv "GUILE_LOAD_COMPILED_PATH")))
                   (web-root (string-append
                              out "/share/sparqling-genomics/web"))
                   (certs (assoc-ref inputs "nss-certs"))
                   (certs-dir (string-append certs "/etc/ssl/certs")))
              (wrap-program (string-append out "/bin/sg-web")
                `("GUILE_LOAD_PATH" ":" prefix (,guile-load-path))
                `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                  (,guile-load-compiled-path))
                `("SG_WEB_ROOT" ":" = (,web-root))
                `("SSL_CERT_DIR" ":" = (,certs-dir)))
              (wrap-program (string-append out "/bin/sg-auth-manager")
                `("GUILE_LOAD_PATH" ":" prefix (,guile-load-path))
                `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                  (,guile-load-compiled-path))
                `("SSL_CERT_DIR" ":" = (,certs-dir)))))))))
   (native-inputs
    `(("texlive" ,texlive)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("guile" ,guile-3.0)
      ("gnutls" ,gnutls) ; Needed for HTTPS.
      ("htslib" ,htslib)
      ("libxml2" ,libxml2)
      ("nss-certs" ,nss-certs)
      ("openldap" ,openldap)
      ("raptor2" ,raptor2)
      ("xz" ,xz)
      ("zlib" ,zlib)))
   (home-page "https://github.com/UMCUGenetics/sparqling-genomics")
   (synopsis "Tools to use SPARQL to analyze genomics data")
   (description "This package provides various tools to extract RDF triples
from genomic data formats, and a web interface to query SPARQL endpoints.")
   ;; All programs except the web interface is licensed GPLv3+.  The web
   ;; interface is licensed AGPLv3+.
   (license (list license:gpl3+ license:agpl3+))))
