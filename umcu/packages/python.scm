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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages qt)
  #:use-module (umcu packages vcf-explorer))

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

(define-public python2-pyopenssl-17.1.0
  (package (inherit python2-pyopenssl)
    (version "17.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyOpenSSL" version))
              (sha256
               (base32
                "0qwmqhfsq84ydir9dz273ypmlcvs7v71m1jns0sd4k0h6lfsa82s"))))))

(define-public python2-pyyaml-3.11
  (package (inherit python2-pyyaml)
    (version "3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pyyaml.org/download/pyyaml/PyYAML-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1s26125vfnskng58ym37xhwv8v0mm95b2cwbjfag8prfhy596v63"))))))

(define-public python2-lxml-3.5.0b1
  (package (inherit python2-lxml)
    (version "3.5.0b1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lxml" version))
              (sha256
               (base32
                "12z24m5gkn8i0fszzdixbdkwvyq6ysf9ajhsqfdj8ibchggzli12"))))))

(define-public python-pyperclip
  (package
    (name "python-pyperclip")
    (version "1.5.27")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/asweigart/pyperclip.git")
                    (commit "7deec0b6464bbda18ce2f700980edba2d6c51e10")))
              (sha256
               (base32
                "1w17szqv1dlfhwwfss66afld4s7xz8yv65q9xaspfdarqrxyp19f"))))
    (arguments `(#:tests? #f))
    (build-system python-build-system)
    (home-page "https://github.com/asweigart/pyperclip")
    (synopsis "Cross-platform clipboard module for Python.")
    (description "This package provides a cross-platform clipboard module for
Python. It currently only handles plain text.")
    (license license:bsd-3)))

(define-public python2-pyperclip
  (package-with-python2 python-pyperclip))

(define-public python-cmd2
  (package
    (name "python-cmd2")
    (version "0.7.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cmd2" version))
              (sha256
               (base32
                "0widbir8ay1fd4zm8l0rjq78j1cvbammbz8xs32crbanqsgzpqml"))))
    (build-system python-build-system)
    (inputs
     `(("python-pytest" ,python-pytest)
       ("python-mock" ,python-mock)))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-pyperclip" ,python-pyperclip)
       ("python-pyparsing" ,python-pyparsing)))
    (home-page "https://github.com/python-cmd2/cmd2")
    (synopsis "Building interactive command line applications in Python")
    (description "This package provides a tool for building interactive command
line applications in Python")
    (license license:expat)))

(define-public python2-cmd2
  (package-with-python2 python-cmd2))

(define-public python-cmd2-0.6.8
  (package (inherit python-cmd2)
    (version "0.6.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cmd2" version))
              (sha256
               (base32
                "1a346zcd46c8gwbbp2cxsmvgfkyy26kwxjzdnkv7n47w6660sy5c"))))
    (arguments `(#:tests? #f))))

(define-public python2-cmd2-0.6.8
  (package-with-python2 python-cmd2-0.6.8))

(define-public python-sortedcontainers
  (package
    (name "python-sortedcontainers")
    (version "1.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sortedcontainers" version))
       (sha256
        (base32
         "1sjh8lccbmvwna91mlhl5m3z4320p07h063b8x8br4p4cll49w0g"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-tox" ,python-tox)))
    (home-page "http://www.grantjenks.com/docs/sortedcontainers/")
    (synopsis "Python Sorted Container Types: SortedList, SortedDict, and SortedSet")
    (description "Python Sorted Container Types: SortedList, SortedDict, and SortedSet")
    (license license:asl2.0)))

(define-public python-intervaltree
  (package
    (name "python-intervaltree")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "intervaltree" version))
              (sha256
               (base32
                "02w191m9zxkcjqr1kv2slxvhymwhj3jnsyy3a28b837pi15q19dc"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-sortedcontainers" ,python-sortedcontainers)))
    (home-page "https://github.com/chaimleib/intervaltree")
    (synopsis "Editable interval tree data structure for Python 2 and 3")
    (description "Editable interval tree data structure for Python 2 and 3")
    (license license:asl2.0)))

(define-public python-intervaltree-2.0.4
  (package (inherit python-intervaltree)
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/chaimleib/intervaltree/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "1vrsnqfj2japh8p9pmh0g7q4a2d1pj3khy8fjbhjrzbapnhqilb9"))))))

(define-public python2-intervaltree-2.0.4
  (package-with-python2 python-intervaltree-2.0.4))

(define-public python-flask-0.10.1
  (package (inherit python-flask)
    (version "0.10.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/pallets/flask/archive/"
                   version ".tar.gz"))
             (sha256
              (base32
               "15mndapc57jj8pq9g8x2p5vs488s0974c1wz8xyrkmnj9cdk5qdm"))))))

(define-public python2-flask-0.10.1
  (package-with-python2 python-flask-0.10.1))

(define-public python2-progressbar
  (package
    (name "python-progressbar")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "progressbar" version))
              (sha256
               (base32
                "0m0j93yfvbd8pw8cz2vdb9hyk9d0zkkd509k69jrw545jxr8mlxj"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (home-page "http://code.google.com/p/python-progressbar")
    (synopsis "Text progress bar library for Python.")
    (description "Text progress bar library for Python.")
    (license #f)))

(define-public python2-requests-2.5.1
  (package (inherit python2-requests)
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/requests/requests/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "135maxpvkajvak715r9akzyfrcl10h99haxfvj8bwwwjsh1s4phy"))))))

(define-public python2-parcel
  (package
    (name "python2-parcel")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LabAdvComp/parcel.git")
                    (commit "50d6124a3e3fcd2a234b3373831075390b886a15")))
              (sha256
               (base32
                "05cpd6kzky60gwfn40vw5r8spgfa9ycr5x7v8q159pv6szxqy5yg"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-version-number
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("Flask==0.10.1") "Flask==0.10.1.post20171020"))
             #t))
         (add-before 'build 'set-home
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "HOME" (getcwd))))
         (add-before 'install 'set-pythonpath
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (python-version "2.7")
                    (libdir (string-append out "/lib/python"
                                           python-version "/site-packages"))
                    (bindir (string-append out "/bin")))
               (mkdir-p bindir)
               (mkdir-p libdir)
               (setenv "PYTHONPATH"
                       (string-append (getenv "PYTHONPATH") ":" libdir))))))))
    (propagated-inputs
     `(("fabric" ,fabric)
       ("python2-termcolor" ,python2-termcolor)
       ("python2-intervaltree" ,python2-intervaltree-2.0.4)
       ("python2-flask" ,python2-flask-0.10.1)
       ("python2-cmd2" ,python2-cmd2-0.6.8)
       ("python2-progressbar" ,python2-progressbar)
       ("python2-requests-2.5.1" ,python2-requests-2.5.1)))
    (home-page "https://bitbucket.org/andrewmacgregor/parcel")
    (synopsis "Python Webapp Deployment Made Easier.")
    (description "Python Webapp Deployment Made Easier.")
    (license #f)))

(define-public gdc-client
  (package
   (name "gdc-client")
   (version "1.3.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/NCI-GDC/gdc-client/archive/"
                  version ".tar.gz"))
            (sha256
             (base32 "07vmhim36yn1m0fpj5sz1xs2wxdsqsnvjqzihsgyc7s9k1s7z11a"))))
   (build-system python-build-system)
   (arguments `(#:python ,python-2))
   ;; python2-ndg-httpsclient uses pyOpenSSL 17.3.0.  We can't propagate
   ;; both.
   (inputs
    `(("python2-pyopenssl" ,python2-pyopenssl-17.1.0)
      ("python2-pytest" ,python2-pytest)))
   (propagated-inputs
    `(("python2-cryptography" ,python2-cryptography)
      ("python2-jsonschema" ,python2-jsonschema)
      ("python2-lxml" ,python2-lxml-3.5.0b1)
      ("python2-pyyaml" ,python2-pyyaml-3.11)
      ("python2-parcel" ,python2-parcel)
      ("python2-pyasn1" ,python2-pyasn1)
      ("python2-ndg-httpsclient" ,python2-ndg-httpsclient)
      ,@(package-propagated-inputs python2-parcel)))
   (home-page "https://gdc.nci.nih.gov/access-data/gdc-data-transfer-tool")
   (synopsis "GDC Data Transfer Tool")
   (description "This package provides several convenience functions over the
GDC API which provides general download/upload via HTTPS.")
   (license license:asl2.0)))

(define-public epigwas
  (package
   (name "epigwas")
   (version "01")
   (source (origin
            (method url-fetch)
            (uri "http://archive.broadinstitute.org/mpg/epigwas/soft.tar.gz")
            (sha256
             (base32 "1v3b0xdccck3h7fydmd6rvy3ksln44a7a7nqlbwklcwkz1fb4pvq"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; Nothing to configure.
        (delete 'build) ; Nothing to build.
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((scripts (string-append (assoc-ref outputs "out")
                                          "/share/epigwas/scripts/")))
              (mkdir-p scripts)
              (copy-recursively "." scripts)
              (substitute* (string-append scripts "phenoCellSpec_v01/markCellSpecif.py")
                (("parseData") "data"))))))))
   (home-page "http://archive.broadinstitute.org/mpg/epigwas/")
   (synopsis "Scripts for phenotypic cell type specificity")
   (description "Phenotypic cell-type specificity identifies chromatin
 marks that overlap phenotypically associated SNPs in cell type specific
way.  That is, if SNPs associated to a phenotype regulate gene expression
in a cell type specific manner, they would overlap chromatin mark peaks
that highlight active gene regulation in the cell type(s) relevant to this
phenotype.  Presumably, identification of critical cell types and marks can
be used to fine-map loci to their best causal variant. Here we provide
scripts that allow users to test:
* Chromatin marks for their phenotypic cell type specificity;
* Specific cell-types for overlap of selected chromatin marks with SNPs
  associated to particular pehnotypes.")
   (license #f)))

(define-public python-sv2
  (package
    (name "python-sv2")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dantaki/SV2/releases/download/sv2"
                    version "/sv2-" version ".tar.gz"))
              (sha256
               (base32
                "0gdms14rda4n7mr84mv4sn5i93l8jvhc7nrk6qj93zjw4bv2d1n5"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:python ,python-2))
    (native-inputs
     `(("python2-cython" ,python2-cython)))
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-pandas" ,python2-pandas)
       ("python2-pybedtools" ,python2-pybedtools)
       ("python2-pysam" ,python2-pysam)
       ("python2-scikit-learn" ,python2-scikit-learn)
       ("bedtools" ,bedtools)))
    (home-page "https://github.com/dantaki/SV2")
    (synopsis "Support vector structural variation genotyper")
    (description "Support vector structural variation genotyper.")
    ;; MIT license.
    (license license:expat)))

(define-public python2-ete2
  (package
    (name "python2-ete2")
    (version "2.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ete2" version))
       (sha256
        (base32
         "18g25prsq3v6zlz9zn8d2nb3fxvld51ci31k21k1v30jfrls8j7w"))
       (patches (list (search-patch "python-ete2-disable-homecalling.patch")))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:python ,python-2))
    (propagated-inputs
     `(("python2-numpy" ,python2-numpy)
       ("python2-pyqt" ,python2-pyqt-4)
       ("python2-lxml" ,python2-lxml)))
    (home-page "http://etetoolkit.org")
    (synopsis "Python environment for phylogenetic tree exploration")
    (description "This package provides a Python environment for phylogenetic
tree exploration")
    (license #f)))

(define-public phylowgs
  (package
   (name "phylowgs")
   (version "smchet5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/morrislab/phylowgs/archive/"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "0c7lpn0fsrlmyfwhx6mgj0gka9lqdis945sfzhcg5kyi42y4m39a"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; No tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build ; These guys didn't even care to use a build system.
          (lambda _
            (system "g++ -o mh.o -O3 mh.cpp util.cpp $(gsl-config --cflags --libs)")))
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (string-append (assoc-ref outputs "out")
                                      "/share/phylowgs")))
              (for-each (lambda (file) (install-file file out))
                        (find-files "." "\\.py"))
              (install-file "mh.o" out)))))))
   (inputs
    `(("gsl" ,gsl)
      ("python" ,python-2)))
   (propagated-inputs
    `(("python2-numpy" ,python2-numpy)
      ("python2-scipy" ,python2-scipy)
      ("python2-ete2" ,python2-ete2)
      ("python2-pyvcf" ,python2-pyvcf)))
   (native-search-paths
     (list (search-path-specification
            (variable "GUIX_PHYLOWGS")
            (files (list "share/phylowgs")))))
   (home-page "https://github.com/morrislab/phylowgs")
   (synopsis "")
   (description "")
   (license license:gpl3+)))
