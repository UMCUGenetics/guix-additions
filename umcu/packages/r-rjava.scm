(define-module (umcu packages r-rjava)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix licenses)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages java)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk))

(define-public r-rjava
  (package
    (name "r-rjava")
    (version "0.9-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rJava" version))
       (sha256
        (base32
         "0s9cjy1wh7snmbqwznh8f1r4ipylr7mgda4a979z963a8lqy32n2"))))
    (properties `((upstream-name . "rJava")))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-JAVA_HOME
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jdk (assoc-ref inputs "jdk")))
               (setenv "JAVA_HOME" jdk)
               (setenv "JAVA" (which "java"))
               (setenv "JAR" (which "jar"))
               (setenv "JAVAC" (which "javac"))
               (setenv "JAVAH" (which "javah"))
               (setenv "JAVA_CPPFLAGS"
                       (string-append "-I" jdk "/include "
                                      "-I" jdk "/include/linux"))
               (match (find-files (string-append jdk "/jre/lib/") "libjvm.so")
                 ((lib) (setenv "JAVA_LIBS" lib))
                 (_ (error "Could not find libjvm.so"))))
             #t)))))
    (inputs
     `(("icu4c" ,icu4c)
       ("jdk" ,icedtea-8 "jdk")
       ("pcre" ,pcre)
       ("zlib" ,zlib)))
    (home-page "http://www.rforge.net/rJava/")
    (synopsis "Low-Level R to Java interface")
    (description
     "This package provides a low-level interface to the Java VM very much
      like .C/.Call and friends.  It allows the creation of objects, calling methods
      and accessing fields.")
    (license license:gpl2)))
