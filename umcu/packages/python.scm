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

(define-module (umcu packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (umcu packages mysql))

(define-public python-py-make
 (package
   (name "python-py-make")
   (version "0.1.1")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py-make" version))
       (sha256
         (base32
           "1sg848j1v65i636qr8d9p4b29ps4zpb1p7382cdyav5bglcm259j"))))
   (build-system python-build-system)
   (propagated-inputs
     `(("python-coverage" ,python-coverage)
       ("python-flake8" ,python-flake8)
       ("python-nose" ,python-nose)
       ("python-docopt" ,python-docopt)))
   (home-page "https://github.com/tqdm/pymake")
   (synopsis
     "Makefile execution powered by pure Python")
   (description
     "Makefile execution powered by pure Python")
   (license #f)))

(define-public python-pydoc-markdown
 (package
   (name "python-pydoc-markdown")
   (version "2.0.5")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydoc-markdown" version))
       (sha256
         (base32
           "07yfafkibpb0lpn8garnrxxvbswxiv8m21h1s8nsacyalvaillgi"))))
   (build-system python-build-system)
   (propagated-inputs
     `(("python-six" ,python-six)
       ("python-pyyaml" ,python-pyyaml)
       ("python-mkdocs" ,python-mkdocs)
       ("python-markdown" ,python-markdown)))
   (home-page
     "https://github.com/NiklasRosenstein/pydoc-markdown")
   (synopsis
     "Create Python API documentation in Markdown format")
   (description
     "Create Python API documentation in Markdown format")
   (license license:expat)))

(define-public python-scaden
  (package
    (name "python-scaden")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "scaden" version))
              (sha256
               (base32
                "1favsmpq8wgdfbrr426zmla7i73vpp5gs1na1f38ijpvv9p3qnij"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; There are no tests.
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
    (home-page "https://github.com/KevinMenden/scaden")
    (synopsis "Cell type deconvolution using single cell data")
    (description "Cell type deconvolution using single cell data")
    (license license:gpl3+)))
