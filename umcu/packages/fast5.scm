(define-module (umcu packages fast5)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time))

(define-public fast5
  (package
   (name "fast5")
   (version "0.6.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/mateidavid/fast5/archive/v"
                                version ".tar.gz"))
            (sha256
             (base32 "06pfg2ldra5g6d14xrxprn35y994w44g0zik2d7npddd0wncxcgq"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (setenv "HDF5_INCLUDE_DIR" (string-append (assoc-ref inputs "hdf5") "/include"))
            (setenv "HDF5_LIB_DIR" (string-append (assoc-ref inputs "hdf5") "/lib"))
            ;; TODO: Check for return value.
            (substitute* "python/Makefile"
              (("install: check_virtualenv") "install:"))
            (system* "make" "-C" "python" "install")))
        (replace 'install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (python (assoc-ref inputs "python"))
                   (site-dir (string-append out "/lib/python3.6"
                                            "/site-packages/fast5/"))
                   (bin (string-append out "/bin"))
                   (include (string-append out "/include")))
              (mkdir-p site-dir)
              (mkdir-p bin)
              (install-file "python/fast5/fast5.pyx" site-dir)
              (install-file "python/bin/f5ls" bin)
              (install-file "python/bin/f5pack" bin)

              ;; Patch /usr/bin/env.
              (substitute* (list (string-append bin "/f5ls")
                                 (string-append bin "/f5pack"))
                (("/usr/bin/env python") (string-append python "/bin/python3")))

              ;; Wrap the environments of main programs so that
              ;; these work as expected.
              (wrap-program (string-append bin "/f5ls")
                `("PYTHONPATH" ":" prefix (,bin ,(getenv "PYTHONPATH")
                                                ,site-dir)))
              (wrap-program (string-append bin "/f5pack")
                `("PYTHONPATH" ":" prefix (,bin ,(getenv "PYTHONPATH")
                                                ,site-dir)))

              (for-each (lambda (file)
                          (install-file file include))
                        (find-files "src")))
            #t)))))
   (native-inputs
    `(("which" ,which)))
   (inputs
    `(("python" ,python-3)
      ("python-setuptools" ,python-setuptools)
      ("python-cython" ,python-cython)
      ("hdf5" ,hdf5)
      ("gcc" ,gcc-9)))
   (propagated-inputs
    `(("python-dateutil" ,python-dateutil)))
   (home-page "https://github.com/mateidavid/fast5")
   (synopsis "Library for accessing Oxford Nanopore sequencing data")
   (description "This package provides a lightweight C++ library for accessing
Oxford Nanopore Technologies sequencing data.")
   (license license:expat)))
