(define-module (umcu packages snp-fingerprinting)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python))

;; This package is only for Python 2.  We therefore don't expose
;; the python3 version to the user.
(define python-mysql-python
  (package
    (name "python-mysql-python")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              ;;(uri (pypi-uri "MySQL-python" version))
              (uri (string-append
                    "https://pypi.python.org/packages/a5/e9/51b544da85a36a68de"
                    "de7a7091f068d802fc515a3a202652828c73453cad/MySQL-python-"
                    version ".zip"))
              (sha256
               (base32
                "0x0c2jg0bb3pp84njaqiic050qkyd7ymwhfvhipnimg58yv40441"))))
    (build-system python-build-system)
    (inputs
     `(("python-configparser" ,python-configparser)
       ("mysql" ,mysql)
       ("zlib" ,zlib)
       ("openssl" ,openssl)
       ("python-nose" ,python-nose))) ; Needed for tests.
    (native-inputs
     `(("unzip" ,unzip)))
    (propagated-inputs
     `(("python-setuptools" ,python-setuptools)
       ("python-configparser" ,python-configparser)))
    (home-page "https://github.com/farcepest/MySQLdb1")
    (synopsis "Python interface to MySQL")
    (description "Python interface to MySQL")
    (license license:gpl3)))

(define-public python2-mysql-python
  (package-with-python2 python-mysql-python))
