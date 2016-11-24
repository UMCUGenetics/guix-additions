(define-module (umcu packages nanosvc)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression))

(define-public libinfra
  (package
   (name "libinfra")
   (version "1.0.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/roelj/libinfra/releases/download/v"
                  version "/libinfra-" version ".tar.gz"))
            (sha256
             (base32 "03yzab5jjaz1r2mfc2i790cwlldhxrvbr30slp9bcb0sbzkjn4c0"))))
   (build-system gnu-build-system)
   (home-page "https://github.com/roelj/libinfra")
   (synopsis "Logging and timing library for C programs")
   (description "This library provides a program and library-wide
infrastructure for functionality that is needed in various places.
Concretely, libinfra provides a file logging interface, and a timer
interface which is integrated with the logging interface.")
   (license license:gpl3+)))
