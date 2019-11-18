(define-module (umcu packages pycoqc)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bioinformatics))

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
  (version "2.5.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "pycoQC" version))
      (sha256
       (base32
        "10skrk9ws7zqfg51c3d4nhia2va3m88p7kcasmh7n9gb3y470z1w"))))
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
