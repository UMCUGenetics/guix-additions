(define-module (umcu packages r-qqman)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix licenses)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages gawk))
 
(define-public r-qqman
 (package
  (name "r-qqman")
  (version "0.1.4")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "qqman" version))
      (sha256
        (base32
          "1v9s9ag1hfb47py87wb2nad4mbsfx35832hdmrh5kxrb2f11zl1s"))))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-calibrate" ,r-calibrate)))
  (home-page
    "http://cran.r-project.org/web/packages/qqman")
  (synopsis
    "Q-Q and Manhattan Plots for GWAS Data")
  (description
    "Create Q-Q and manhattan plots for GWAS data from PLINK results.")
  (license gpl3)))
