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

(define-public python2-logging
  (package
    (name "python2-logging")
    (version "0.4.9.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "logging" version))
              (sha256
               (base32
                "05r4iz1qb2vdrv0kg7yv0ibp7wyryldvv18h60nh91ghfc3vbxi6"))))
    (arguments
     `(#:python ,python-2))
    (build-system python-build-system)
    (home-page "http://www.red-dove.com/python_logging.html")
    (synopsis "A logging module for Python")
    (description "A logging module for Python")
    (license #f)))

(define-public python-sv2
  (package
    (name "python-sv2")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dantaki/SV2/releases/download/v"
                    version "/sv2-" version ".tar.gz"))
              (sha256
               (base32
                "0shm0vcsads0a7g1v0h4vx7lk3scfjdk8qrqzpkpmyrv01fix3bb"))))
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
  (let ((commit "18636df520e483d9a4c001ee3f9b3963d75c2bbc"))
    (package
     (name "phylowgs")
     (version (string-append "smchet5-" (string-take commit 9)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/morrislab/phylowgs.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32 "02p6hsnw6k8xcpzryqrpmd6m76xgpv3lkr8jixlqcj1xfz4bgyrj"))))
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
     (license license:gpl3+))))

(define-public python-mappy
  (package
    (name "python-mappy")
    (version "2.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mappy" version))
       (sha256
        (base32
         "0cihy3lvk5q3cdg80886mq4s4khb9s8azfi2igwvx9zzy56myah1"))))
    (build-system python-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/lh3/minimap2")
    (synopsis "Minimap2 python binding")
    (description "Minimap2 python binding")
    (license license:expat)))

(define-public python2-mappy
  (package-with-python2 python-mappy))

(define-public python-ont-tombo
  (package
    (name "python-ont-tombo")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ont-tombo" version))
       (sha256
        (base32
         "1023hadgcsgi53kz53ql45207hfizf9sw57z0qij3ay1bx68zbpm"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose2" ,python-nose2)
       ("python-tqdm" ,python-tqdm)))
    (propagated-inputs
     `(("python-cython" ,python-cython)
       ("python-future" ,python-future)
       ("python-h5py" ,python-h5py)
       ("python-mappy" ,python-mappy)
       ("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/nanoporetech/tombo")
    (synopsis "Analysis of raw nanopore sequencing data.")
    (description "Analysis of raw nanopore sequencing data.")
    ;; In addition, some of the source code form may contain derivative works of
    ;; nanoraw licensed under the licence from The Regents of the University of
    ;; California, terms of which are included below.
    (license license:mpl2.0)))

(define-public python2-ont-tombo
  (package-with-python2 python-ont-tombo))

(define-public python-sparqlwrapper
  (package
    (name "python-sparqlwrapper")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/RDFLib/sparqlwrapper/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "1qdbvnh6vl5s2xa0cgg21ywwv7gscsf2xqxwj1jm7fx9ykk757kw"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-rdflib" ,python-rdflib)))
    (home-page "http://rdflib.github.io/sparqlwrapper")
    (synopsis "SPARQL Endpoint interface to Python")
    (description "SPARQL Endpoint interface to Python")
    (license license:w3c)))

(define-public python2-sparqlwrapper
  (package-with-python2 python-sparqlwrapper))

(define-public python-sparqlkernel
  (package
    (name "python-sparqlkernel")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sparqlkernel" version))
              (sha256
               (base32
                "1d3c1byqq5bvnazvmiqilwnjwx8a1wf0mwivm0gxnrkg6siqxsk3"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-traitlets" ,python-traitlets)
       ("python-notebook" ,python-notebook)
       ("python-ipykernel" ,python-ipykernel)
       ("python-html5lib" ,python-html5lib-0.9)))
    (propagated-inputs
     `(("python-sparqlwrapper" ,python-sparqlwrapper)
       ("python-pygments" ,python-pygments)))
    (home-page "https://github.com/paulovn/sparql-kernel")
    (synopsis "Jupyter kernel for SPARQL")
    (description "This package provides a Jupyter kernel for running SPARQL
queries.")
    (license license:bsd-3)))

(define-public python2-sparqlkernel
  (package-with-python2 python-sparqlkernel))

(define-public python-pyega3
  (package
   (name "python-pyega3")
   (version "3.0.19")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "pyega3" version))
            (sha256
             (base32
              "1vky0my8n2ys92km8731y4bhlxb2gf9kzgjjxbpjzj012bbzkz1v"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-requests" ,python-requests)
      ("python-tqdm" ,python-tqdm)
      ("python-urllib3" ,python-urllib3)))
   (home-page "https://github.com/EGA-archive/ega-download-client")
   (synopsis "EGA python client")
   (description "EGA python client")
   (license license:asl2.0)))

(define-public python2-htseq-0.6.0
  (package (inherit python2-htseq)
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri "https://files.pythonhosted.org/packages/6f/65/17615c8703a5b66654365db3a7a5d3cec598b968809f1e14f227b63589c4/HTSeq-0.6.0.tar.gz")
              (sha256
               (base32
                "1v90sjpbkj7wpzn52yxpasb84fhkw1v85nw2nwxz2bgzfxn93byp"))
              (patches (search-patches "0001-htseq-increase-memory-limit.patch"))))))

(define-public python-jupyterhub
  (package
   (name "python-jupyterhub")
   (version "1.0.0")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "jupyterhub" version))
            (sha256
             (base32
              "0zx6gw9yhgki05j21p6x1x2sf5a2mg2c2mx0ii8rl6q4b98ilm1k"))))
   (build-system python-build-system)
   (arguments `(#:tests? #f))
   (propagated-inputs
    `(("python-alembic" ,python-alembic)
      ("python-async-generator" ,python-async-generator)
      ("python-certipy" ,python-certipy)
      ("python-dateutil" ,python-dateutil)
      ("python-entrypoints" ,python-entrypoints)
      ("python-jinja2" ,python-jinja2)
      ("python-oauthlib" ,python-oauthlib)
      ("python-pamela" ,python-pamela)
      ("python-prometheus-client" ,python-prometheus-client)
      ("python-requests" ,python-requests)
      ("python-sqlalchemy" ,python-sqlalchemy)
      ("python-tornado" ,python-tornado)
      ("python-traitlets" ,python-traitlets)))
   (home-page "https://jupyter.org")
   (synopsis "JupyterHub: A multi-user server for Jupyter notebooks")
   (description "JupyterHub: A multi-user server for Jupyter notebooks")
   (license license:bsd-3)))

(define-public python-bash-kernel
  (package
   (name "python-bash-kernel")
   (version "0.7.2")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "bash_kernel" version))
            (sha256
             (base32
              "0w0nbr3iqqsgpk83rgd0f5b02462bkyj2n0h6i9dwyc1vpnq9350"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'install-kernelspec
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (setenv "HOME" "/tmp")
              (invoke "python" "-m" "bash_kernel.install" "--prefix" out)
              #t))))))
   (inputs
    `(("jupyter" ,jupyter)))
   (home-page "https://github.com/takluyver/bash_kernel")
   (synopsis "A bash kernel for Jupyter")
   (description "A bash kernel for Jupyter")
   (license license:expat)))

(define-public python-jupyterlab-server
  (package
    (name "python-jupyterlab-server")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyterlab_server" version))
        (sha256
          (base32
            "1bax8iqwcc5p02h5ysdc48zvx7ll5jfzfsybhb3lfvyfpwkpb5yh"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("python-jinja2" ,python-jinja2)
        ("python-json5" ,python-json5)
        ("python-jsonschema" ,python-jsonschema)
        ("python-notebook" ,python-notebook)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-requests" ,python-requests)))
    (home-page "https://jupyter.org")
    (synopsis "JupyterLab Server")
    (description "JupyterLab Server")
    (license license:bsd-3)))

(define-public python-jupyterlab
  (package
    (name "python-jupyterlab")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyterlab" version))
        (sha256
          (base32
            "06n4idmxbbr364hfv8yr3m1b06f4hwv504rbhplm7882y1dyxb17"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("python-jinja2" ,python-jinja2)
        ("python-jupyterlab-server"
         ,python-jupyterlab-server)
        ("python-notebook" ,python-notebook)
        ("python-tornado" ,python-tornado)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-check-links"
         ,python-pytest-check-links)
        ("python-requests" ,python-requests)))
    (home-page "http://jupyter.org")
    (synopsis
      "The JupyterLab notebook server extension.")
    (description
      "The JupyterLab notebook server extension.")
    (license license:bsd-3)))

(define-public python-boto
  (package
    (name "python-boto")
    (version "2.49.0")
    (source
      (origin
        (method url-fetch)
	(uri (string-append
              "https://github.com/boto/boto/archive/" version ".tar.gz"))
	(sha256
	  (base32 "051ka4lm1a7469gj8ibrr0pkckgd64gn0m2g7lr5my228m7zvgix"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "http://docs.pythonboto.org/")
    (synopsis "Old version of Boto 3")
    (description "Old version of Boto 3")
  (license license:asl2.0)))

(define-public python-s3transfer-0.3.3
  (package
    (name "python-s3transfer")
    (version "0.3.3")
    (source 
      (origin
        (method url-fetch)
	(uri (string-append
	      "https://github.com/boto/s3transfer/archive/"
	      version ".tar.gz"))
	(sha256
	  (base32 "0lgwhqwyz4552n21sp51gqhfhyy3zlxla2nzfla3s5rjmxx0z2qc"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (inputs
       `(("python-botocore", python-botocore)
	 ("python-urllib3", python-urllib3)
	 ("python-dateutil", python-dateutil)
	 ("python-docutils", python-docutils)
	 ("python-jmespath", python-jmespath)
	 ("python-mock", python-mock)))
    (home-page "https://pypi.org/project/s3transfer/")
    (synopsis "An Amazon S3 Transfer Manager for Python")
    (description "S3transfer is a Python library for managing Amazon S3 transfers.

Note

This project is not currently GA. If you are planning to use this code in production, make sure to lock to a minor version as interfaces may break from minor version to minor version. For a basic, stable interface of s3transfer, try the interfaces exposed in boto3")
    (license license:asl2.0)))

(define-public python-boto3
  (package
    (name "python-boto3")
    (version "1.12.1")
    (source
      (origin
        (method url-fetch)
	(uri (string-append
	      "https://github.com/boto/boto3/archive/"
	      version ".tar.gz"))
	(sha256
	  (base32 "1yznwdvr1ijfm7flbrd4pblpdcqyvr3wdnspsmzbzsp54a3jwflk"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (inputs
       `(("python-s3transfer", python-s3transfer)
	 ("python-jmespath", python-jmespath)
	 ("python-botocore", python-botocore)
	 ("python-urllib3", python-urllib3)
	 ("python-dateutil", python-dateutil)
	 ("python-docutils", python-docutils)
	 ("python-mock", python-mock)
	 ("python-nose", python-nose)))
    (home-page "https://boto3.amazonaws.com/v1/documentation/api/latest/index.html")
    (synopsis "AWS SDK for Python http://aws.amazon.com/sdk-for-python/")
    (description "Boto3 is the Amazon Web Services (AWS) Software Development Kit (SDK) for Python, which allows Python developers to write software that makes use of services like Amazon S3 and Amazon EC2. You can find the latest, most up to date, documentation at our doc site, including a list of services that are supported.")
    (license license:asl2.0)))

(define-public python-smart-open
  (package
    (name "python-smart-open")
    (version "1.9.0")
    (source
      (origin
        (method url-fetch)
	(uri "https://github.com/RaRe-Technologies/smart_open/archive/2feb910a647e50069f960623fa38bd34b9846d69.tar.gz")
	(sha256
	  (base32 "0znq1389jscv66ivga1f4l6098arwrgfambizir8sygikp6rg49c"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (inputs
       `(("python-boto3", python-boto3)))
    (home-page "https://github.com/RaRe-Technologies/smart_open")
    (synopsis "Utils for streaming large files (S3, HDFS, gzip, bz2...)")
    (description "smart_open is a Python 2 & Python 3 library for efficient streaming of very large files from/to storages such as S3, GCS, HDFS, WebHDFS, HTTP, HTTPS, SFTP, or local filesystem. It supports transparent, on-the-fly (de-)compression for a variety of different formats.

smart_open is a drop-in replacement for Python's built-in open(): it can do anything open can (100% compatible, falls back to native open wherever possible), plus lots of nifty extra stuff on top.")
    (license license:gpl2)))

(define-public python-google
  (package
    (name "python-google")
    (version "2.0.3")
    (source
      (origin
        (method url-fetch)
	(uri (pypi-uri
	      "google"
	      version))
	(sha256
	  (base32 "0j5v4sg4c8s9k5nrnmcrq5zc40yiynmwbf6ysmp6dgikpxyrjzgz"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://breakingcode.wordpress.com/")
    (synopsis "Python bindings to the Google search engine.")
    (description "The author of this package has not provided a project description")
    (license license:bsd-3)))

(define-public python-google-cloud-core
  (package
    (name "python-google-cloud-core")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
	(uri (pypi-uri
	      "google-cloud-core"
	      version))
	(sha256
	  (base32 "1n19q57y4d89cjgmrg0f2a7yp7l1np2448mrhpndq354h389m3w7"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://github.com/googleapis/google-cloud-python")
    (synopsis "Google Cloud API client core library")
    (description "This library is not meant to stand-alone. Instead it defines common helpers (e.g. base Client classes) used by all of the google-cloud-* packages.")
    (license license:asl2.0)))

(define-public python-google-cloud-storage
  (package
    (name "python-google-cloud-storage")
    (version "1.26.0")
    (source
      (origin
        (method url-fetch)
	(uri (pypi-uri
	      "google-cloud-storage"
	      version))
      (sha256
        (base32 "0caxqf6vda89cmc81fxhmfk3n61aypqz2sswnbsylzf436rsxpzz"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://github.com/googleapis/python-storage")
    (synopsis "Google Cloud Storage API client library")
    (description "Google Cloud Storage allows you to store data on Google infrastructure with very high reliability, performance and availability, and can be used to distribute large data objects to users via direct download.")
    (license license:asl2.0)))

(define-public python-google-api-core
  (package
    (name "python-google-api-core")
    (version "1.16.0")
    (source
      (origin
        (method url-fetch)
	(uri (pypi-uri
	      "google-api-core"
	      version))
	(sha256
	  (base32 "1qh30ji399gngv2j1czzvi3h0mgx3lfdx2n8qp8vii7ihyh65scj"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://github.com/googleapis/google-cloud-python")
    (synopsis "Google API client core library")
    (description "This library is not meant to stand-alone. Instead it defines common helpers used by all Google API clients. For more information, see the documentation.")
    (license license:asl2.0)))

(define-public python-google-resumable-media
  (package
    (name "python-google-resumable-media")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
	(uri (pypi-uri
	      "google-resumable-media"
	      version))
	(sha256
	  (base32 "0aldswz9lsw05a2gx26yjal6lcxhfqpn085zk1czvjz1my4d33ra"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cachetools", python-cachetools)))
    (arguments `(#:tests? #f))
    (home-page "https://github.com/googleapis/google-resumable-media-python")
    (synopsis "Utilities for Google Media Downloads and Resumable Uploads")
    (description "Utilities for Google Media Downloads and Resumable Uploads. See the docs for examples and usage.")
    (license license:expat)))

(define-public python-cachetools
  (package
    (name "python-cachetools")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
	(uri (pypi-uri
	      "cachetools"
	      version))
	(sha256
	  (base32 "1601kakkw5y1zj8lbcr1y6cph30swwc5iw978577y9azm2bxslls"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://github.com/tkem/cachetools/")
    (synopsis "Extensible memoizing collections and decorators")
    (description "This module provides various memoizing collections and decorators, including variants of the Python Standard Library’s @lru_cache function decorator.")
    (license license:asl2.0)))

(define-public python-google-auth
  (package
    (name "python-google-auth")
    (version "1.11.2")
    (source
      (origin
        (method url-fetch)
	(uri (pypi-uri
	      "google-auth"
	      version))
	(sha256
	  (base32 "1mh7i4ybillnd2m8bm6b1mfwnkp25jdrkcypd3q00vjxyci2xqhy"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://github.com/googleapis/google-auth-library-python")
    (synopsis "Google Authentication Library")
    (description "This library simplifies using Google’s various server-to-server authentication mechanisms to access Google APIs.")
    (license license:asl2.0)))

(define-public python-gensim
  (package
    (name "python-gensim")
    (version "3.8.1")
    (source
      (origin
        (method url-fetch)
	(uri (pypi-uri
              "gensim"
              version))
	(sha256
	  (base32 "1p6zn59gasz8qrb4hcwv016997yk5aw4pizwf37cgc6pm307y9rk"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (inputs
       `(("python-numpy" ,python-numpy)))
    (home-page "https://radimrehurek.com/gensim/")
    (synopsis "Topic modelling for humans")
    (description "Gensim = \"Generate Similar\"
Gensim started off as a collection of various Python scripts for the Czech Digital Mathematics Library dml.cz in 2008, where it served to generate a short list of the most similar articles to a given article.

I also wanted to try these fancy \"Latent Semantic Methods\", but the libraries that realized the necessary computation were not much fun to work with.

Naturally, I set out to reinvent the wheel. Our 2010 LREC publication describes the initial design decisions behind Gensim: clarity, efficiency and scalability. It is fairly representative of how Gensim works even today.

Later versions of gensim improved this efficiency and scalability tremendously. In fact, I made algorithmic scalability of distributional semantics the topic of my PhD thesis.

By now, Gensim is—to my knowledge—the most robust, efficient and hassle-free piece of software to realize unsupervised semantic modelling from plain text. It stands in contrast to brittle homework-assignment-implementations that do not scale on one hand, and robust java-esque projects that take forever just to run \"hello world\".

In 2011, I started using Github for source code hosting and the Gensim website moved to its present domain. In 2013, Gensim got its current logo and website design.")
    (license license:gpl2)))

(define-public python-batchspawner
  (package
    (name "python-batchspawner")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append 
              "https://github.com/jupyterhub/batchspawner/archive/"
              version ".tar.gz"))
        (sha256
          (base32
            "0rz1aq5b5vh3hy6q2yj7f6wrvfkzvm5rgqjigggy01xqxm7y0j08"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("python-jupyterhub" ,python-jupyterhub)
       ("python-pamela" ,python-pamela)))
    (home-page "http://jupyter.org")
    (synopsis
      "Batchspawner: A spawner for Jupyterhub to spawn notebooks using batch resource managers.")
    (description
      "Batchspawner: A spawner for Jupyterhub to spawn notebooks using batch resource managers.")
    (license license:bsd-3)))

(define-public python-jupyterhub-ldapauthenticator
  (package
    (name "python-jupyterhub-ldapauthenticator")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyterhub-ldapauthenticator" version))
        (sha256
          (base32
            "083yvnb6csxjmhxa0kw17db23bxihnax64vcz34k0hc38vi2xfjv"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jupyterhub" ,python-jupyterhub)
        ("python-ldap3" ,python-ldap3)
        ("python-tornado" ,python-tornado)
        ("python-traitlets" ,python-traitlets)))
    (home-page
      "https://github.com/yuvipanda/ldapauthenticator")
    (synopsis "LDAP Authenticator for JupyterHub")
    (description "LDAP Authenticator for JupyterHub")
    (license #f)))

(define-public python-checkm-genome
  (package
    (name "python-checkm-genome")
    (version "1.0.18")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "checkm-genome" version))
              (sha256
               (base32
                "1dx214mbsz6dmn4zgf4df2q6k4kdsj8m0ykxygzs0h73npm33kqd"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:python ,python-2))
    (propagated-inputs
     `(("python2-dendropy" ,python-dendropy)
       ("python2-matplotlib" ,python-matplotlib)
       ("python2-numpy" ,python-numpy)
       ("python2-pysam" ,python-pysam)
       ("python2-scipy" ,python-scipy)
       ("python2-setuptools" ,python-setuptools)))
    (home-page "http://pypi.python.org/pypi/checkm/")
    (synopsis "Assess the quality of putative genome bins.")
    (description "Assess the quality of putative genome bins.")
    (license #f)))

(define-public python-wrapspawner
  (package
    (name "python-wrapspawner")
    (version "0-9c51368")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jupyterhub/wrapspawner.git")
                    (commit "9c51368710bf52eab874aeef4cf1c5738d506430")))
              (sha256
               (base32
                "15yhjav76d8ns5hjvsdbxl9a8733qqwxskdr11k1a8h6k1xm3snc"))))
    (arguments `(#:tests? #f))
    (build-system python-build-system)
    (home-page "https://github.com/jupyterhub/wrapspawner")
    (synopsis "Wrapspawner for Jupyterhub.")
    (description "This package includes WrapSpawner and ProfilesSpawner, which
provide mechanisms for runtime configuration of spawners. The inspiration for
their development was to allow users to select from a range of pre-defined
batch job profiles, but their operation is completely generic.")
    (license license:bsd-3)))

(define-public python-pyvcf
  (package
    (name "python-pyvcf")
    (version "0.6.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyVCF" version))
              (sha256
               (base32
                "1ngryr12d3izmhmwplc46xhyj9i7yhrpm90xnsd2578p7m8p5n79"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; The tests cannot find its files.
    (propagated-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-psutil" ,python-psutil)))
    (home-page "https://github.com/jamescasbon/PyVCF")
    (synopsis "Variant Call Format (VCF) parser for Python")
    (description "Variant Call Format (VCF) parser for Python")
    (license #f)))

(define-public python2-pyvcf
  (package-with-python2 python-pyvcf))

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

(define-public python-argopt
 (package
   (name "python-argopt")
   (version "0.5.0")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "argopt" version))
       (sha256
         (base32
           "0r7xc9c5hs6jz0zja1z44x7inciw9lk7ya6q24sryf1l88pmprd4"))))
   (build-system python-build-system)
   (propagated-inputs
     `(("python-coverage" ,python-coverage)
       ("python-flake8" ,python-flake8)
       ("python-nose" ,python-nose)))
   (home-page "https://github.com/casperdcl/argopt")
   (synopsis "doc to argparse driven by docopt")
   (description "doc to argparse driven by docopt")
   (license #f)))

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

(define-public python-mkdocs
 (package
   (name "python-mkdocs")
   (version "1.0.4")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mkdocs" version))
       (sha256
         (base32
           "0fg9w6rdskwnn7knri7xzrd26k9svwqlxvdr0kk5spfpm8ll7lqp"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs
     `(("python-click" ,python-click)
       ("python-jinja2" ,python-jinja2)
       ("python-livereload" ,python-livereload)
       ("python-markdown" ,python-markdown)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pytest" ,python-pytest)
       ("python-tornado" ,python-tornado)))
   (home-page "https://www.mkdocs.org")
   (synopsis "Project documentation with Markdown.")
   (description
     "Project documentation with Markdown.")
   (license license:bsd-3)))

(define-public python-livereload
 (package
   (name "python-livereload")
   (version "2.6.1")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "livereload" version))
       (sha256
         (base32
           "0rhggz185bxc3zjnfpmhcvibyzi86i624za1lfh7x7ajsxw4y9c9"))))
   (build-system python-build-system)
   (propagated-inputs
     `(("python-six" ,python-six)
       ("python-tornado" ,python-tornado)))
   (home-page
     "https://github.com/lepture/python-livereload")
   (synopsis
     "Python LiveReload is an awesome tool for web developers")
   (description
     "Python LiveReload is an awesome tool for web developers")
   (license license:bsd-3)))

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

(define-public python-retrying
 (package
   (name "python-retrying")
   (version "1.3.3")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "retrying" version))
       (sha256
         (base32
           "0fwp86xv0rvkncjdvy2mwcvbglw4w9k0fva25i7zx8kd19b3kh08"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs
     `(("python-six" ,python-six)
       ("python-pytest" ,python-pytest)
       ("python-tqdm" ,python-tqdm)))
   (home-page "https://github.com/rholder/retrying")
   (synopsis "Retrying")
   (description "Retrying")
   (license license:asl2.0)))

(define-public python-plotly-3.9.0
 (package
   (name "python-plotly")
   (version "3.9.0")
   (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "plotly" version))
       (sha256
         (base32
           "0zhnrls44xvb99shxr11vn8h2fk5xhgniwy2gy9wgxw2lji3b329"))))
   (build-system python-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs
     `(("python-decorator" ,python-decorator)
       ("python-nbformat" ,python-nbformat)
       ("python-pytz" ,python-pytz)
       ("python-requests" ,python-requests)
       ("python-retrying" ,python-retrying)
       ("python-six" ,python-six)))
   (home-page "https://plot.ly/python/")
   (synopsis
     "An open-source, interactive graphing library for Python")
   (description
     "An open-source, interactive graphing library for Python")
   (license license:expat)))

(define-public python-pycoqc
 (package
  (name "python-pycoqc")
  (version "2.5.0.21")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pycoQC" version))
      (sha256
       (base32
        "02lqck381nk8bvczxjc8inr5ihhxziwwp7zdp1l43h8q2wix67k9"))))
  (build-system python-build-system)
  (arguments
   `(#:tests? #f
     #:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'downgrade-tqdm
         (lambda* (#:key inputs #:allow-other-keys)
           (substitute* "setup.py"
             (("tqdm==4.35") "tqdm>=4.19.6")
             (("pysam==0.15.3") "pysam>=0.15.0")
             (("jinja2==2.10.1") "jinja2>=2.10.1")
             (("h5py==2.9.0") "h5py>=2.8.0")
             (("plotly==4.1.0") "plotly>=3.9.0")
             (("pandas==0.25.1") "pandas>=0.24.2")
             (("scipy==1.3.1") "scipy>=1.3.1")
             (("numpy==1.17.1") "numpy>=1.15.4")))))))
  (propagated-inputs
    `(("python-h5py" ,python-h5py)
      ("python-jinja2" ,python-jinja2)
      ("python-numpy" ,python-numpy)
      ("python-pandas" ,python-pandas)
      ("python-plotly" ,python-plotly-3.9.0)
      ("python-scipy" ,python-scipy)
      ("python-tqdm" ,python-tqdm)
      ("python-pysam" ,python-pysam)))
  (home-page "https://github.com/a-slide/pycoQC")
  (synopsis "")
  (description "")
  (license #f)))

(define-public pyflow
  (package
    (name "pyflow")
    (version "1.1.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Illumina/pyflow/releases/download/v"
                    version "/pyflow-" version ".tar.gz"))
              (sha256
               (base32
                "14zw8kf24c7xiwxg0q98s2dlifc4fzrjwzx1dhb99zvdihnx5bg7"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; There is no test suite.
    (home-page "https://illumina.github.io/pyflow")
    (synopsis "Tool to manage tasks in the context of a task dependency graph")
    (description "This package is a Python module to manage tasks in the context
of a task dependency graph.  It has some similarities to make.")
    (license license:bsd-2)))

(define-public pyflow-2
  (package-with-python2 pyflow))

(define-public python2-parabam
(package
  (name "python2-parabam")
  (version "2.2.5")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "parabam" version))
      (sha256
        (base32
          "1a4pq7lligzg636qixx67c3kxcrpsyvxhakrfkxvhww8084b9rrj"))))
  (build-system python-build-system)
  (arguments
    `(#:python ,python-2
      #:tests? #f ))
  (propagated-inputs
    `(("python2-numpy" ,python2-numpy)
      ("python2-pysam" ,python2-pysam)))
  (home-page "")
  (synopsis "Parallel BAM File Analysis")
  (description "Parallel BAM File Analysis")
  (license license:gpl3)))

(define-public telomecat
(package
  (name "telomerecat" )
  (version "3.2")
  (source (origin
    (method url-fetch)
      (uri "https://files.pythonhosted.org/packages/ae/9c/08288b2a8ccd7d8092a8bd8198d014a0ccbafa1e5e77e872347a6424725e/telomerecat-3.2.tar.gz")
        (sha256
          (base32
            "0m71w1s52rishfy9jbn76c7qh6jzga4xj1jxx7m5gq690q4m13fm"))))
  (build-system python-build-system)
  (arguments
    `(#:python ,python-2
      #:tests? #f ))
  (propagated-inputs
    `(("python2-pypdf2" ,python2-pypdf2)
     ("python2-numpy" ,python2-numpy)
     ("python2-pysam" ,python2-pysam)
     ("python2-pandas" ,python2-pandas)
     ("r-argparser" ,r-argparser)
     ("python2" ,python-2.7)
     ("python2-parabam" ,python2-parabam)))
  (inputs
    `(("unzip" ,unzip)))
  (home-page "http://pypi.python.org/pypi/telomerehunter/")
  (synopsis "Estimation of Telomere Content from WGS Data")
  (description "TelomereHunter extracts, sorts and analyses telomeric reads
                from WGS Data. It is designed to take BAM files from a tumor and/or a control
                sample as input. The tool was developed at the German Cancer Research Center (DKFZ).")
  (license #f)))

(define-public python-telomerehunter
(package
  (name "telomerehunter")
  (version "1.1.0")
  (source (origin
  (method url-fetch)
  (uri "https://files.pythonhosted.org/packages/e5/67/ce6ac292a88a078a733dc3d9adb3f153834692effbf0851b93a6f3e49b7a/telomerehunter-1.1.0-py2-none-any.whl")
   (sha256
    (base32
     "1055z4hs2hhsfwqnjm0kffkhh6ag041mp6l13i2gs5454xk02nwi"))))
  (build-system python-build-system)
  (arguments
   `(#:python ,python-2
     #:tests? #f
     #:phases
     (modify-phases %standard-phases
       (replace 'unpack
         (lambda* (#:key inputs #:allow-other-keys)
           (let ((unzip (string-append (assoc-ref inputs "unzip")
                                       "/bin/unzip")))
             (system* unzip (assoc-ref %build-inputs "source")))))
       (replace 'build
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((python-libdir (string-append (assoc-ref outputs "out") 
                                                "/lib/python2.7/site-packages"))
                  (site-dir (string-append python-libdir "/telomerehunter"))
                  (bindir (string-append (assoc-ref outputs "out") "/bin")))
         (substitute* "telomerehunter/run_plot.sh"
           (("R --no-save") (string-append (assoc-ref inputs "r") "/bin/R --no-save")))
         (substitute* (list "telomerehunter/filter_telomere_reads.py"
                            "telomerehunter/normalize_TVR_counts.R")
           (("samtools ") (string-append (assoc-ref inputs "samtools") "/bin/samtools ")))
         (mkdir-p python-libdir)
         (copy-recursively "telomerehunter" site-dir)
         (mkdir-p bindir)
         (mkdir-p (string-append (assoc-ref outputs "out") "/site-library"))
         (install-file "telomerehunter-1.1.0.data/scripts/telomerehunter" bindir)
         (wrap-program (string-append bindir "/telomerehunter")
          `("PYTHONPATH" ":" prefix (,bindir ,(getenv "PYTHONPATH")
                                             ,site-dir))))))
       (delete 'install))))
  (inputs
   `(("unzip" ,unzip)
     ("samtools" ,samtools)))
  (propagated-inputs
    `(("r" ,r)
     ("python2-pypdf2", python2-pypdf2)
     ("python2-numpy", python2-numpy)
     ("python2-pysam", python2-pysam)
     ("r-ggplot2", r-ggplot2)
     ("r-reshape2", r-reshape2)
     ("r-gridextra", r-gridextra)
     ("r-rcolorbrewer" ,r-rcolorbrewer)
     ("r-cowplot", r-cowplot)
     ("r-svglite", r-svglite)
     ("r-dplyr", r-dplyr)))
  (native-search-paths
    (list (search-path-specification
      (variable "R_LIBS_SITE")
        (files (list "site-library/")))))
  (home-page "http://pypi.python.org/pypi/telomerehunter/")
  (synopsis "Estimation of Telomere Content from WGS Data")
  (description "TelomereHunter extracts, sorts and analyses telomeric reads
                from WGS Data. It is designed to take BAM files from a tumor and/or a control
                sample as input. The tool was developed at the German Cancer Research Center (DKFZ).")
  (license license:gpl3+)))

(define-public python-smmap2
  (package
    (name "python-smmap2")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "smmap2" version))
              (sha256
               (base32
                "06s6d09qzfrns0mn2xgayby6dbk1vi92v97zg6l6xj6chm555jz6"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; no test target
    (home-page "https://github.com/gitpython-developers/gitdb")
    (synopsis "Pure python implementation of a sliding window memory map
manager")
    (description "Smmap wraps an interface around mmap and tracks the mapped
files as well as the amount of clients who use it.  If the system runs out of
resources, or if a memory limit is reached, it will automatically unload
unused maps to allow continued operation.")
    (license license:bsd-3)))

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

(define-public python-nanostat
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
