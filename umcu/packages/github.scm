(define-module (umcu packages github)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tls)
  #:use-module (umcu packages boost)
  #:use-module (umcu packages python))

(define-public xqilla
  (package
   (name "xqilla")
   (version "2.3.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/xqilla/XQilla-"
                                version ".tar.gz"))
            (sha256
             (base32 "1mjgcyar3qyizpnb0h9lxaj6p9yq4vj09qd8qan1bwv6z6sbjxlg"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags (list (string-append
                               "--with-xerces="
                               (assoc-ref %build-inputs "xerces-c")))))
   (inputs
    `(("xerces-c" ,xerces-c)))
   (home-page "http://xqilla.sourceforge.net/")
   (synopsis "XQuery and XPath utility")
   (description "XQilla is an XQuery and XPath 2 library and command line
utility written in C++ implemented on top of the Xerces-C library.")
   (license license:asl2.0)))

(define-public genetorrent
  (let ((commit "476d7bd449851a8fbdd82263ce675a70e6a5b37f"))
    (package
      (name "genetorrent")
      (version "3.8.7")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://bitbucket.org/cghub/genetorrent.git")
                      (commit commit)))
                (sha256
                 (base32
                  "130awzgxwxffqn6ng348196dbx5zx086ljblw6b6vyb6wrhiw43n"))))
      (arguments
       `(#:tests? #f ; Tests fail to create SSL certificates.
         #:configure-flags (list (string-append
                                  "--with-xqilla="
                                  (assoc-ref %build-inputs "xqilla"))
                                 (string-append
                                  "--with-boost-libdir="
                                  (assoc-ref %build-inputs "boost") "/lib")
                                 (string-append
                                  "--with-xerces="
                                  (assoc-ref %build-inputs "xerces-c")))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'autoreconf
             (lambda _ (zero? (system* "autoreconf" "-vif"))))
           ;; There seems to be a broken shebang in the configure script of
           ;; libtorrent after generating it.  The 'fix-shebang phase fixes
           ;; that.
           (add-after 'autoreconf 'fix-shebang
             (lambda _
               (substitute* "libtorrent/configure"
                 (("#! /bin/sh")
                  (string-append "#!" (assoc-ref %build-inputs "bash")
                                 "/bin/sh")))
               #t))
           ;; Building libtorrent fails with:
           ;; gtBaseOpts.cpp:41:24: fatal error: gt_scm_rev.h: No such file or
           ;; directorycompilation terminated.
           ;;
           ;; The following page describes this problem too, and includes a fix:
           ;; http://iokevins.blogspot.nl/2015/02/genetorrent-building-release-387-on.html
           ;; Which is simply creating a '.git' file in the toplevel directory.
           (add-after 'autoreconf 'fix-libtorrent-build
             (lambda _
               (zero? (system* "touch" ".git")))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)))
      (inputs
       `(("pkg-config" ,pkg-config)
         ("boost" ,boost-1.58)
         ("xerces-c" ,xerces-c)
         ("xqilla" ,xqilla)
         ("curl" ,curl)
         ("openssl" ,openssl)
         ("python" ,python-2)
         ("bash" ,bash)))
      (build-system gnu-build-system)
      (home-page "https://bitbucket.org/cghub/genetorrent")
      (synopsis "")
      (description "")
      (license #f))))
