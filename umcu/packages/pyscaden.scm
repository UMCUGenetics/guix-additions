(define-module (umcu packages pyscaden)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python))

(define-public python-scaden
(package
  (name "python-scaden")
  (version "0.9.2")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "scaden" version))
      (sha256
        (base32
          "1favsmpq8wgdfbrr426zmla7i73vpp5gs1na1f38ijpvv9p3qnij"))))
  (build-system python-build-system)
  (arguments `(#:tests? #f)) ; ; There are no tests.))
  (propagated-inputs
    `(("python-click" ,python-click)
      ("python-matplotlib" ,python-matplotlib)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-scanpy" ,python-scanpy)
      ("python-scikit-learn" ,python-scikit-learn)
      ("python-scipy" ,python-scipy)
      ("python-seaborn" ,python-seaborn)
      ("tensorflow" ,tensorflow)
      ("python-anndata" ,python-anndata)
      ("python-tqdm" ,python-tqdm)))
  (home-page
    "https://github.com/KevinMenden/scaden")
  (synopsis
    "Cell type deconvolution using single cell data")
  (description
    "Cell type deconvolution using single cell data")
     (license license:gpl3+)))
