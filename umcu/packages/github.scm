(define-module (umcu packages github)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tls)
  #:use-module (umcu packages boost)
  #:use-module (umcu packages python))

(define-public xqilla
  (package
   (name "xqilla")
   (version "2.3.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/xqilla/XQilla-"
                                version ".tar.gz"))
            (sha256
             (base32 "1mjgcyar3qyizpnb0h9lxaj6p9yq4vj09qd8qan1bwv6z6sbjxlg"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags (list (string-append
                               "--with-xerces="
                               (assoc-ref %build-inputs "xerces-c")))))
   (inputs
    `(("xerces-c" ,xerces-c)))
   (home-page "http://xqilla.sourceforge.net/")
   (synopsis "XQuery and XPath utility")
   (description "XQilla is an XQuery and XPath 2 library and command line
utility written in C++ implemented on top of the Xerces-C library.")
   (license license:asl2.0)))
