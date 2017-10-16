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

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

;; WARNING: This is non-free software. It will NEVER and SHOULD NEVER be
;; mainlined in GNU Guix.  You should avoid using this package, and if you
;; can, please write a free replacement for it.

(define-module (umcu packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics))

(define-public python-py2bit
  (package
   (name "python-py2bit")
   (version "0.2.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "py2bit" version))
            (sha256
             (base32
              "1cdf4qlmgwsh1f4k0wdv2sr8x9qn4366p0k3614vbd0fpqiarxrl"))))
   (build-system python-build-system)
   (home-page "https://github.com/dpryan79/py2bit")
   (synopsis "A package for accessing 2bit files using lib2bit")
   (description "A package for accessing 2bit files using lib2bit")
   (license license:expat)))

(define-public python-deeptools
  (package
   (name "python-deeptools")
   (version "2.5.0.1")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "deepTools" version))
     (sha256
      (base32
       "1ybnbz4y1aql2fgdf0s18cf4arsd8myv18rd3nna261sppjfxlfs"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-matplotlib" ,python-matplotlib)
      ("python-numpy" ,python-numpy)
      ("python-numpydoc" ,python-numpydoc)
      ("python-py2bit" ,python-py2bit)
      ("python-pybigwig" ,python-pybigwig)
      ("python-pysam" ,python-pysam)
      ("python-scipy" ,python-scipy)))
   (home-page "http://pypi.python.org/pypi/deepTools/")
   (synopsis "Useful tools for exploring deep sequencing data")
   (description "Useful tools for exploring deep sequencing data")
   (license #f)))

(define-public python-macs2
  (package
    (name "python-macs2")
    (version "2.1.1.20160309")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MACS2" version))
       (sha256
        (base32
         "09ixspd1vcqmz1c81ih70xs4m7qml2iy5vyx1y74zww3iy1vl210"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)))
    (home-page "http://github.com/taoliu/MACS/")
    (synopsis "Model Based Analysis for ChIP-Seq data")
    (description "Model Based Analysis for ChIP-Seq data")
    (license #f)))

(define-public python-pytabix
  (package
    (name "python-pytabix")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             (pypi-uri "pytabix" version)))
       (sha256
        (base32
         "1ldp5r4ggskji6qx4bp2qxy2vrvb3fam03ksn0gq2hdxgrlg2x07"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/slowkow/pytabix")
    (synopsis "Python interface for tabix")
    (description "Python interface for tabix")
    (license license:expat)))

(define-public python2-pytabix
  (package-with-python2 python-pytabix))

(define-public python-logutils
  (package
  (name "python-logutils")
  (version "0.3.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "logutils" version))
      (sha256
        (base32
          "0ayycc1988cjdzr3i085gxwrj6l1scamjl79mv7cw4cj6sbn47qh"))))
  (build-system python-build-system)
  (home-page "http://code.google.com/p/logutils/")
  (synopsis "Logging utilities")
  (description "Logging utilities")
  (license #f)))

(define-public python2-logutils
  (package-with-python2 python-logutils))

(define-public python-helper
  (package
    (name "python-helper")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "helper" version))
       (sha256
        (base32
         "0p56dvjpaz9wnr0ik2wmvgqjf9ji180bhjky7q272l5dan94lgd6"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-logutils" ,python-logutils)
       ("python-pyyaml" ,python-pyyaml)
       ("python-mock" ,python-mock)))
    (home-page "https://github.com/gmr/helper")
    (synopsis
     "Development library for quickly writing configurable applications and daemons")
    (description
     "Development library for quickly writing configurable applications and daemons")
    (license license:bsd-3)))

(define-public python2-helper
  (package-with-python2 python-helper))

(define-public beta
  (package
    (name "beta")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri "http://cistrome.org/BETA/src/BETA_1.0.7.zip")
            (sha256
             (base32 "0axnq7mjj8b7v3fkyh7vp4gq1h252czxakc914zvii10i6kmjkh1"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((unzip (string-append (assoc-ref inputs "unzip")
                                         "/bin/unzip")))
               (system* unzip (assoc-ref %build-inputs "source") "-C" "BETA_1.0.7/*")
               (chdir "BETA_1.0.7"))))
         (add-after 'unpack 'fix-compiler-invocation
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "BETA/misp/Makefile"
                          (("CC = cc") (string-append "CC = " (assoc-ref inputs "gcc") "/bin/gcc")))))
         (add-after 'unpack 'fix-rscript-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "BETA/Up_Down_score.py"
               (("Rscript") (string-append (assoc-ref inputs "r") "/bin/Rscript")))
             (substitute* "BETA/Up_Down_distance.py"
               (("Rscript") (string-append (assoc-ref inputs "r") "/bin/Rscript"))))))))
    (native-inputs
     `(("unzip" ,unzip)
       ("gcc" ,gcc)))
    (inputs
     `(("python2-numpy" ,python2-numpy)
       ("zlib" ,zlib)
       ("r" ,r-minimal)))
    (home-page "http://cistrome.org/BETA")
    (synopsis "Binding and Expression Target Analysis")
    (description "Binding and Expression Target Analysis (BETA) is a software
package that integrates ChIP-seq of transcription factors or chromatin
regulators with differential gene expression data to infer direct target
genes.  BETA has three functions: (1) to predict whether the factor has
activating or repressive function; (2) to infer the factor’s target genes;
and (3) to identify the motif of the factor and its collaborators which might
modulate the factor’s activating or repressive function. Here we describe the
implementation and features of BETA to demonstrate its application to several
datasets.  BETA requires ~2GB RAM and 1h for the whole procedure.")
    (license #f)))

(define-public python-easywebdav
  (package
    (name "python-easywebdav")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "easywebdav" version))
       (sha256
        (base32
         "11azylc4b8gp6nrrpbn096qymn4rzvd43nryn7p59wv0w45y5k5i"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (home-page "http://github.com/amnong/easywebdav")
    (synopsis "Straight-forward WebDAV client, implemented using Requests")
    (description "This package provides a straight-forward WebDAV client,
implemented using Requests")
    (license #f)))

(define-public python2-easywebdav
  (package-with-python2 python-easywebdav))

(define-public python2-wsgiref
  (package
    (name "python2-wsgiref")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/41/9e/309259ce8dff8c596"
             "e8c26df86dbc4e848b9249fd36797fd60be456f03fc/wsgiref-0.1.2.zip"))
       (sha256
        (base32
         "0y8fyjmpq7vwwm4x732w97qbkw78rjwal5409k04cw4m03411rn7"))))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((unzip (string-append (assoc-ref inputs "unzip")
                                         "/bin/unzip")))
               (when (zero? (system* unzip (assoc-ref %build-inputs "source")))
                 (chdir "wsgiref-0.1.2"))))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (build-system python-build-system)
    (home-page "http://cheeseshop.python.org/pypi/wsgiref")
    (synopsis "WSGI (PEP 333) Reference Library")
    (description "WSGI (PEP 333) Reference Library")
    (license #f)))

(define-public clarity-utils
  (let ((commit "108942f7d1f951f03ea4ec0631a2f22d7a97ba2c"))
    (package
      (name "clarity-utils")
      (version "0.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/UMCUGenetics/clarity_utils.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1km6v7zl39zk2hl8b14dllsz867n4dyclx71p97pf7n9sjwl4p9a"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((scripts-location (string-append
                                        (assoc-ref outputs "out")
                                        "/share/clarity-utils")))
                 (mkdir-p scripts-location)
                 ;; This file is a duplicate of the one in the root directory.
                 (delete-file "epp_scripts/glsapiutil.py")
                 (for-each (lambda (file)
                             (install-file file scripts-location))
                           (find-files "." "\\.py$" #:directories? #f)))
               #t)))))
      (propagated-inputs
       `(("python2-easywebdav" ,python2-easywebdav)
         ("python2-wsgiref" ,python2-wsgiref)))
      (home-page "https://github.com/UMCUGenetics/clarity_utils")
      (synopsis "Utility scripts to work with Genologics Clarity LIMS")
      (description "Utility scripts to work with Genologics Clarity LIMS")
      (license #f))))


(define-public python-scikit-rebate
  (package
    (name "python-scikit-rebate")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skrebate" version))
       (sha256
        (base32
         "0xa3smiah1q0wmmq8krfvc3mgk5fq0cb968xydsgh75ykrpp5zij"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-scikit-learn" ,python-scikit-learn)))
    (home-page "https://epistasislab.github.io/scikit-rebate/")
    (synopsis "Relief-based feature selection algorithms for Python")
    (description
     "Scikit-rebate is a scikit-learn-compatible Python implementation of
ReBATE, a suite of Relief-based feature selection algorithms for Machine
Learning.  These algorithms excel at identifying features that are predictive
of the outcome in supervised learning problems, and are especially good at
identifying feature interactions that are normally overlooked by standard
feature selection algorithms.")
    (license license:bsd-3)))

(define-public python2-scikit-rebate
  (package-with-python2 python-scikit-rebate))

(define-public python-logging
  (package
    (name "python-logging")
    (version "0.4.9.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "logging" version))
              (sha256
               (base32
                "05r4iz1qb2vdrv0kg7yv0ibp7wyryldvv18h60nh91ghfc3vbxi6"))))
    (build-system python-build-system)
    (home-page "http://www.red-dove.com/python_logging.html")
    (synopsis "A logging module for Python")
    (description "A logging module for Python")
    (license #f)))

(define-public python2-logging
  (package-with-python2 python-logging))

(define-public icgc-get
  (let ((commit "656e48704a6dd5fd0e03572d3027983a0fd40fe9"))
    (package
      (name "icgc-get")
      (version (string-append "0.0.0-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/icgc/icgc-get.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "16dm0a9nfhcw26mkjw7f0vv2cv05caj6rf40g9yhkknfc3ypnkr8"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2
         #:tests? #f))
      (inputs
       `(("python2-pytest" ,python2-pytest)
         ("python2-pytest-runner" ,python2-pytest-runner)
         ("python2-certifi" ,python2-certifi)
         ("python2-subprocess32" ,python2-subprocess32)
         ("python2-tabulate" ,python2-tabulate)
         ("python2-psutil" ,python2-psutil)
         ("python2-requests" ,python2-requests)
         ("python2-click" ,python2-click)
         ("python2-logging" ,python2-logging)
         ("python2-pyyaml" ,python2-pyyaml)
         ("python2-idna" ,python2-idna)
         ("python2-cryptography" ,python2-cryptography)
         ("python2-pyopenssl" ,python2-pyopenssl)
         ("python2-jinja2" ,python2-jinja2)))
      (home-page "https://github.com/icgc/icgc-get")
      (synopsis "Universal download client for ICGC data")
      (description "The data for ICGC resides in many data repositories around
the world.  These repositories each have their own environment (public cloud,
private cloud, on-premise file systems, etc.), access controls (DACO, OAuth,
asymmetric keys, IP filtering), download clients and configuration mechanisms.
Thus, there is much for a user to learn and perform before actually acquiring
the data.  This is compounded by the fact that the number of environments are
increasing over time and their characteristics are frequently changing.  A
coordinated mechanism to bootstrap and streamline this process is highly
desirable.  This is the problem the icgc-get tool helps to solve.")
      (license license:gpl3))))
