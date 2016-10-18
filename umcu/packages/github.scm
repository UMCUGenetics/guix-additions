(define-module (umcu packages github)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression))

(define-public python-pygithub
  (package
    (name "python-pygithub")
    (version "1.29")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyGithub" version))
              (sha256
               (base32
                "0zqaqnk847bdw6wvsllgmg71ngqfiyd41kwxcgrwdz9gfkwzjxji"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; no test target
    (home-page "http://pygithub.github.io/PyGithub/v1/index.html")
    (synopsis "Python wrapper to the Github API v3")
    (description "PyGithub is a Python library to use the Github API (v3).
With it, you can manage your Github resources (repositories, user profiles,
organizations, etc.) from Python scripts.")
    (license license:lgpl2.1)))

(define-public python-gitpython
  (package
    (name "python-gitpython")
    (version "2.0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/gitpython-developers/GitPython/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "1pxf1ijlnyhbp0bifm2lxzrxhn1v34ylf4klqxawsjnc81yphf57"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; no test target
    (propagated-inputs
     `(("python-gitdb2" ,python-gitdb2)))
    (home-page "http://pygithub.github.io/PyGithub/v1/index.html")
    (synopsis "Python library used to interact with Git repositories")
    (description "")
    (license license:bsd-3)))

(define-public python-gitdb2
  (package
    (name "python-gitdb2")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gitdb2" version))
              (sha256
               (base32
                "0rmblgxx30yrwpfx4p7ffis6prjhfv0wjrj9z6jlv2qv82dj1wxr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; no test target
    (propagated-inputs
     `(("python-smmap2" ,python-smmap2)))
    (home-page "https://github.com/gitpython-developers/gitdb")
    (synopsis "GitDB is a pure-Python git object database")
    (description "")
    (license license:bsd-3)))

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

