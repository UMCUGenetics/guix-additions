(define-module (umcu packages bitarray)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages bioinformatics))

(define-public python-bitarray
 (package
   (name "python-bitarray")
   (version "1.1.0")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bitarray" version))
       (sha256
         (base32
           "0nv1283qcfilhnb4q6znlijply6lfxwpvp10cr0v33l0qwa86mwz"))))
   (build-system python-build-system)
   (home-page
     "https://github.com/ilanschnell/bitarray")
   (synopsis
     "Efficient arrays of booleans")
   (description
     "Efficient arrays of booleans")
   (license license:expat)))
