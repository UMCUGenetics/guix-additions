;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
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

(define-module (umcu packages guix)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages version-control))

(define (patch-url seqno)
  "Return the URL of Bash patch number SEQNO."
  (format #f "mirror://gnu/bash/bash-4.4-patches/bash44-~3,'0d" seqno))

(define (bash-patch seqno sha256)
  "Return the origin of Bash patch SEQNO, with expected hash SHA256"
  (origin
    (method url-fetch)
    (uri (patch-url seqno))
    (sha256 sha256)))

(define-syntax-rule (patch-series (seqno hash) ...)
  (list (bash-patch seqno (base32 hash))
        ...))

(define %patch-series-4.4
  ;; This is the current patches series for 4.4, generated using
  ;; 'download-patches' below.
  (patch-series
   (1 "03vzy7qwjdd5qvl3ydg99naazas2qmyd0yhnrflgjbbm64axja1y")
   (2 "0lrwq6vyqism3yqv9s7kzaf3dsl4q5w9r5svcqz279qp7qca083h")
   (3 "1chqww2rj6g42b8s60q5zlzy0jzp684jkpsbrbfy1vzxja8mmpsi")
   (4 "1cy8abf96hkrjhw921ndr0shlcnc52bg45rn6xri4v5clhq0l25d")
   (5 "0a8515kyk4zsgmvlqvlganjfr7pq0j6kzpr4d6xx02kpbdr4n7i2")
   (6 "1f24wgqngmj2mrj9yibwvc2zvlmn5xi53mnw777g3l40c4m2x3ka")
   (7 "1bzdsnqaf05gdbqpsixhan8vygjxpcxlz1dd8d9f5jdznw3wq76y") ;CVE-2017-5932
   (8 "1firw915mjm03hbbw9a70ch3cpgrgnvqjpllgdnn6csr8q04f546")
   (9 "0g1l56kvw61rpw7dqa9fcl9llkl693h73g631hrhxlm030ddssqb")
   (10 "01lfhrkdsdkdz8ypzapr614ras23x7ckjnr60aa5bzkaqprccrc4")
   (11 "038p7mhnq9m65g505hi3827jkf9f35nd1cy00w8mwafpyxp44mnx")
   (12 "0gh6lbb1rwpk44pvbamm6vzdfi50xnwkqd9v7s8cjwk3pz973hps")))

(define-public bash-custom
  (let* ((cppflags (string-join '("-DNON_INTERACTIVE_LOGIN_SHELLS"
                                  "-DSSH_SOURCE_BASHRC")
                                " "))
         (configure-flags
          ``("--with-installed-readline"
             ,,(string-append "CPPFLAGS=" cppflags)
             ,(string-append
               "LDFLAGS=-Wl,-rpath -Wl,"
               (assoc-ref %build-inputs "readline")
               "/lib"
               " -Wl,-rpath -Wl,"
               (assoc-ref %build-inputs "ncurses")
               "/lib")))
         (version "4.4"))
    (package
     (name "bash")
     (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/bash/bash-" version ".tar.gz"))
              (sha256
               (base32
                "1jyz6snd63xjn6skk7za6psgidsd53k05cr3lksqybi0q6936syq"))
              (patch-flags '("-p0"))
              (patches %patch-series-4.4)))
     (version (string-append version "."
                             (number->string (length %patch-series-4.4))))
     (build-system gnu-build-system)

     (outputs '("out"
                "doc"                         ;1.7 MiB of HTML and extra files
                "include"))                   ;headers used by extensions
     (inputs `(("readline" ,readline)
               ("ncurses" ,ncurses)))             ;TODO: add texinfo
     (arguments
      `(;; When cross-compiling, `configure' incorrectly guesses that job
        ;; control is missing.
        #:configure-flags ,(if (%current-target-system)
                               `(cons* "bash_cv_job_control_missing=no"
                                       ,configure-flags)
                               configure-flags)

        ;; Bash is reportedly not parallel-safe.  See, for instance,
        ;; <http://patches.openembedded.org/patch/32745/> and
        ;; <http://git.buildroot.net/buildroot/commit/?h=79e2d802a>.
        #:parallel-build? #f
        #:parallel-tests? #f

        ;; XXX: The tests have a lot of hard-coded paths, so disable them
        ;; for now.
        #:tests? #f

        #:modules ((srfi srfi-26)
                   (guix build utils)
                   (guix build gnu-build-system))

        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'install-sh-symlink
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Add a `sh' -> `bash' link.
              (let ((out (assoc-ref outputs "out")))
                (with-directory-excursion (string-append out "/bin")
                  (symlink "bash" "sh")))))

          (add-after 'install 'move-development-files
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Move 'Makefile.inc' and 'bash.pc' to "include" to avoid
              ;; circular references among the outputs.
              (let ((out     (assoc-ref outputs "out"))
                    (include (assoc-ref outputs "include"))
                    (lib     (cut string-append <> "/lib/bash")))
                (mkdir-p (lib include))
                (rename-file (string-append (lib out)
                                            "/Makefile.inc")
                             (string-append (lib include)
                                            "/Makefile.inc"))
                (rename-file (string-append out "/lib/pkgconfig")
                             (string-append include
                                            "/lib/pkgconfig"))

                ;; Don't capture the absolute file name of 'install' to avoid
                ;; retaining a dependency on Coreutils.
                (substitute* (string-append (lib include)
                                            "/Makefile.inc")
                  (("^INSTALL =.*")
                   "INSTALL = install -c\n"))
                #t))))))

     (native-search-paths
      (list (search-path-specification            ;new in 4.4
             (variable "BASH_LOADABLES_PATH")
             (files '("lib/bash")))))

     (synopsis "The GNU Bourne-Again SHell")
     (description
      "Bash is the shell, or command-line interpreter, of the GNU system.  It
is compatible with the Bourne Shell, but it also integrates useful features
from the Korn Shell and the C Shell and new improvements of its own.  It
allows command-line editing, unlimited command history, shell functions and
aliases, and job control while still allowing most sh scripts to be run
without modification.")
     (license license:gpl3+)
     (home-page "https://www.gnu.org/software/bash/"))))

(define-public guixr
  (package
    (name "guixr")
    (version "1.4.4-next")
    (source #f)
    (build-system gnu-build-system)
    (propagated-inputs
     `(("guix" ,guix)
       ("gwl" ,gwl)))
    (inputs
     `(("bash-full" ,bash-custom)
       ("git" ,git)
       ("gawk" ,gawk)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (with-output-to-file "guixr"
               (lambda _
                 (format #t "#!/bin/bash

set -u
set -e

#Configuration
guix_root=\"/gnu\"
guix_additional=\"/gnu/repositories/guix-additions\"
guix_pin=\"/gnu/repositories/guix\"
guix_profile=\"/gnu/profiles/base\"
guix=\"~a/bin/guix\"
git=\"~a/bin/git\"
gawk=\"~a/bin/gawk\"
coreutils=\"~a\"
readlink=\"${coreutils}/bin/readlink\"
cut=\"${coreutils}/bin/cut\"
grep=\"~a/bin/grep\"

# Avoid locale warnings.
export GUIX_LOCPATH=\"${guix_profile}/lib/locale\"

# Use /gnu as state directory.
export NIX_STATE_DIR=$guix_root

# Ensure the latest Guix packages are used.  Do not override
# the user's customizations (if any).
if [ -v HOME ]; then
  if [ ! -L $HOME/.config/guix/latest ]; then
    mkdir -p $HOME/.config/guix
    ln -s /gnu/repositories/guix $HOME/.config/guix/latest > /dev/null 2>&1 ||:
  # Renew the link as repository updates are managed centrally.
  # This will avoid the warning of an outdated version of Guix.
  elif [ \"$(${readlink} -f $HOME/.config/guix/latest)\" = \"/gnu/repositories/guix\" ]; then
    rm -f $HOME/.config/guix/latest
    ln -s /gnu/repositories/guix $HOME/.config/guix/latest > /dev/null 2>&1 ||:
  fi
fi

# Include our non-standard package repository
export GUIX_PACKAGE_PATH=\"$guix_additional${GUIX_PACKAGE_PATH:+:$GUIX_PACKAGE_PATH}\"

# Set the Guile environment for GWL
export GUILE_LOAD_PATH=\"${guix_profile}/share/guile/site/2.2${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}\"
export GUILE_LOAD_COMPILED_PATH=\"${guix_profile}/lib/guile/2.2/ccache${GUILE_LOAD_COMPILED_PATH:+:$GUILE_LOAD_COMPILED_PATH}\"

# Set the X.509 certificates
export SSL_CERT_DIR=\"${guix_profile}/etc/ssl/certs\"
export SSL_CERT_FILE=\"${SSL_CERT_DIR}/ca-certificates.crt\"

# Use guix with the given arguments
export GUIX_DAEMON_SOCKET=guix://10.100.7.235:9999
if [ $# -lt 1 ]; then
  ${guix}
elif [ \"$1\" == \"package\" ] && [ $# -ge 2 ] && ([ \"$2\" == \"--install\" ] || [ \"$2\" == \"--upgrade\" ] ||
         [ \"$2\" == \"-i\" ] || [ \"$2\" == \"-u\" ]); then
  ${guix} $@
  echo \"\"
  echo \"The following repositories and versions were used:\";
  echo -n \" * GNU Guix upstream:         \";
  ${git} -C /gnu/repositories/guix rev-parse HEAD;
  echo -n \" * UMCU additional packages:  \";
  ${git} -C /gnu/repositories/guix-additions rev-parse HEAD;
elif [ \"$1\" == \"pull\" ]; then
  echo \"This feature has been disabled.\";
elif [ \"$1\" == \"gc\" ]; then
  echo \"This feature has been disabled.\";
elif [ \"$1\" == \"load-profile\" ]; then
  if [ $# -gt 1 ]; then
    if [ \"$2\" != \"--help\" ] && [ \"$2\" != \"-h\" ]; then
      arguments=(\"$@\")
      profile_arguments=(\"${arguments[@]:1}\")
      profile_arguments=(\"${profile_arguments[@]/--}\")
      profiles=${profile_arguments[@]/%/\"/etc/profile\"}
      sge_variables=$(${coreutils}/bin/env | ${grep} \"^SGE\")
      ${coreutils}/bin/env - ~a/bin/bash --init-file <(echo \"export $sge_variables\"; echo \"PS1=\\\"\\u@\\h \\W [env]\\\\$ \\\"\") -i \"${@:$(($# + 1))}\"
    else
      printf \"Usage:\\n  $0 $1 /path/to/profile\\n\"
    fi
  else
    printf \"Usage:\\n  $0 $1 /path/to/profile\\n\"
  fi
else
  ${guix} $@
fi~%"
                         (assoc-ref inputs "guix")
                         (assoc-ref inputs "git")
                         (assoc-ref inputs "gawk")
                         (assoc-ref inputs "coreutils")
                         (assoc-ref inputs "grep")
                         (assoc-ref inputs "bash-full"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p out)
               (install-file "guixr" out)
               (chmod (string-append out "/guixr") #o555)
               ;; It seems that Dutch people expect a 'q' before a 'u', which
               ;; leads to a typo of the 'guixr' command.  For the maximum
               ;; convenience level for users, we provide an alias.
               (system* "ln" "--symbolic"
                        (string-append out "/guixr")
                        (string-append out "/quixr"))))))))
    (home-page #f)
    (synopsis "GNU Guix remote")
    (description "This wrapper around GNU Guix enables remote guix-daemon
communication.  It can be used for cluster deployments with a single build
node.  This script was originally developed by Ricardo Wurmus.  This version
has been slightly modified to work on the UMC Utrecht cluster set-up.")
    (license #f)))

(define-public iotop-logger
  (package
    (name "iotop-logger")
    (version "1.0")
    (source #f)
    (build-system gnu-build-system)
    (propagated-inputs
     `(("iotop" ,iotop)
       ("bash" ,bash)
       ("grep" ,grep)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (with-output-to-file "iotop-logger"
               (lambda _
                 (format #t "#!/bin/bash~%")
                 (format #t "~a/sbin/iotop -Pbot --delay=1 | ~a/bin/egrep -v \"DISK READ|DISK WRITE\"~%"
                         (assoc-ref inputs "iotop")
                         (assoc-ref inputs "grep"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p out)
               (install-file "iotop-logger" out)
               (chmod (string-append out "/iotop-logger") #o555)))))))
    (home-page #f)
    (synopsis "Script to gather disk reads/writes information")
    (description "This script is a wrapper around iotop, to gather reads and
writes to the disk for various processes.")
    (license #f)))

(define-public collectl
  (package
   (name "collectl")
   (version "4.1.2")
   (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/collectl/collectl/collectl-" version
                   "/collectl-" version ".src.tar.gz"))
             (sha256
              (base32
               "0cf44kwazxfh98yshyi1jcw34lzfy5ahqxpgqqsxa6ps7zlm89lp"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:phases
      (modify-phases %standard-phases
        (delete 'build) ; There's nothing to build.
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "INSTALL"
              (("DESTDIR:=\"/\"") (format #f "DESTDIR:=~s"
                                          (assoc-ref outputs "out")))
              (("DESTDIR/usr") "DESTDIR"))))
        (replace 'install
                 (lambda _
                   (system* "./INSTALL"))))))
   (inputs
    `(("perl" ,perl)))
   (home-page "http://collectl.sourceforge.net")
   (synopsis "Performance data collector")
   (description "This package provides a program that collects various
performance measurement data like CPU, memory, disk and network performance
numbers.")
   (license license:artistic2.0)))

(define-public vmtouch
  (package
   (name "vmtouch")
   (version "1.3.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/hoytech/vmtouch/archive/v"
                  version ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32 "1src2byjwgjsbq3sd29r9qgmjwfb1f4c03p5cjqqwk42iw5rh5a6"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
      #:make-flags `("CC=gcc"
                     ,(string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (inputs
    `(("perl" ,perl)))
   (home-page "https://github.com/hoytech/vmtouch")
   (synopsis "Virtual memory toucher")
   (description "vmtouch is a tool for learning about and controlling the file
system cache of unix and unix-like systems.")
   (license license:bsd-3)))
