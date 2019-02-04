;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages pyclone)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages bioinformatics))

(define-public python2-pydp
  (package
    (name "python2-pydp")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://bitbucket.org/aroth85/pydp/downloads/PyDP-"
                    version ".tar.gz"))
              (sha256
               (base32
                "03f56vh0yi3l8s7vpfsvp3ac8d1acf36jg31amknnarxfrg4cdir"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "")
    (synopsis "")
    (description "")
    ;; Custom license, which is probably non-free
    (license #f)))

(define-public python2-pyclone
  (package
    (name "python2-pyclone")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://bitbucket.org/aroth85/pyclone/downloads/PyClone-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1bzvrhsya4s9akcsyyafhqyn5cpl4b5hfk43q5ky4rdj9spybimm"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("python2-pyyaml" ,python2-pyyaml)
       ("python2-pydp" ,python2-pydp)
       ("python2-pandas" ,python2-pandas)
       ("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-matplotlib" ,python2-matplotlib)
       ("python2-seaborn" ,python2-seaborn)))
    (home-page "http://compbio.bccrc.ca/software/pyclone/")
    (synopsis "Tool for inferring cellular prevalence of point mutations.")
    (description "PyClone is a tool for inferring the cellular prevalence of
point mutations from deeply sequenced data.  The model supports simultaneous
analysis of multiple related samples and infers clusters of mutations whose
cellular prevalences shift together.  Such clusters of mutations can be
inferred as mutational genotypes of distinct clonal populations.")
    ;; Custom license, which is probably non-free
    (license #f)))
