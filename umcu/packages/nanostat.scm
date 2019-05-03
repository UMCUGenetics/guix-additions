(define-module (umcu packages nanostat)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages bioinformatics))

(define-public python-nanomath
 (package
   (name "python-nanomath")
   (version "0.23.1")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nanomath" version))
       (sha256
         (base32
           "04b0n1qqyqq0id55zxp2dl3zj367gf59c8jilca406aqnjryv9sl"))))
   (build-system python-build-system)
   (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)))
   (home-page
     "https://github.com/wdecoster/nanomath")
   (synopsis
     "A few simple math function for other Oxford Nanopore processing scripts")
   (description
     "A few simple math function for other Oxford Nanopore processing scripts")
   (license license:expat)))

(define-public python-nanoget
 (package
   (name "python-nanoget")
   (version "1.8.0")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nanoget" version))
       (sha256
         (base32
           "0cs5sc2i7mfbikgssfaia28bagvka2a8qpmdzbf6i27piv2c7kyz"))))
   (build-system python-build-system)
   (propagated-inputs
     `(("python-biopython" ,python-biopython)
       ("python-nanomath" ,python-nanomath)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-pysam" ,python-pysam)))
   (home-page
     "https://github.com/wdecoster/nanoget")
   (synopsis
     "Functions to extract information from Oxford Nanopore sequencing data and alignments.")
   (description
     "Functions to extract information from Oxford Nanopore sequencing data and alignments.")
   (license license:expat)))

(define-public nanostat
 (package
   (name "python-nanostat")
   (version "1.1.2")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "NanoStat" version))
       (sha256
         (base32
           "1mr81xl08qw1vyl552snnxafzmbg9rv9lskyzvzqg8dhm8baslya"))))
   (build-system python-build-system)
   (propagated-inputs
     `(("python-nanoget" ,python-nanoget)
       ("python-nanomath" ,python-nanomath)))
   (home-page
     "https://github.com/wdecoster/nanostat")
   (synopsis
     "Calculate statistics for Oxford Nanopore sequencing data and alignments")
   (description
     "Calculate statistics for Oxford Nanopore sequencing data and alignments")
   (license license:expat)))
