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
   (version "1.2.1")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bitarray" version))
       (sha256
         (base32
           "0nv1283qcfilhnb4q6znlijply6lfxwpvp10cr0v33l0qwa86mwz"))))
   (build-system python-build-system)
   (home-page "https://github.com/ilanschnell/bitarray")
   (synopsis "Efficient arrays of booleans")
   (description
     "This package provides an object type which efficiently represents an
array of booleans.  Bitarrays are sequence types and behave very much like
usual lists.  Eight bits are represented by one byte in a contiguous block
of memory.  The user can select between two representations: little-endian
and big-endian.  All of the functionality is implemented in C.  Methods for
accessing the machine representation are provided.  This can be useful when
bit level access to binary files is required, such as portable bitmap image
files.  Also, when dealing with compressed data which uses variable bit
length encoding, you may find this module useful.")
   (license license:psfl)))
