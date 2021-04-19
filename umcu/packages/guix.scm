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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web))

(define-public guixr
  (package
    (name "guixr")
    (version "1.15.0")
    (source #f)
    (build-system gnu-build-system)
    (inputs
     `(("bash-full" ,bash)
       ("git" ,git)))
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
guix=\"/gnu/profiles/base/bin/guix\"
git=\"~a/bin/git\"
coreutils=\"~a\"
readlink=\"${coreutils}/bin/readlink\"
cut=\"${coreutils}/bin/cut\"
grep=\"~a/bin/grep\"

# Avoid locale warnings.
export GUIX_LOCPATH=\"${guix_profile}/lib/locale\"

# Use /gnu as state directory.
export GUIX_STATE_DIRECTORY=$guix_root

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
export GUIX_PACKAGE_PATH=\"${GUIX_PACKAGE_PATH:+$GUIX_PACKAGE_PATH:}$guix_additional\"

# Set the Guile environment for Guix
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
elif [ \"$1\" == \"load-profile\" ]; then
  if [ $# -gt 1 ]; then
    if [ \"$2\" != \"--help\" ] && [ \"$2\" != \"-h\" ]; then
      arguments=(\"$@\")
      profile_arguments=(\"${arguments[@]:1}\")
      profile_arguments=(\"${profile_arguments[@]/--}\")
      profiles=${profile_arguments[@]/%/\"/etc/profile\"}
      # When no variables are set in the Guix profile file, at least set PATH
      # to avoid leaking to hardcoded '/usr/bin'.
      set_output=$(${grep} -h \"^export\" $profiles || echo \"export PATH=\\\"\\\"\")
      sge_variables=$(export -p | ${grep} \"^declare -x SGE\" || echo \"# No SGE variables found.\")
      slurm_variables=$(export -p | ${grep} \"^declare -x SLURM\" || echo \"# No SLURM variables found.\")
      tmp_variables=$(export -p | ${grep} \"^declare -x TMP\" || echo \"# No TMP variables found.\")
      job_id_variables=$(export -p | ${grep} \"^declare -x JOB_ID\" || echo \"# No JOB_ID variable found.\")
      hostname_variables=$(export -p | ${grep} \"^declare -x HOSTNAME\" || echo \"# No HOSTNAME variable found.\")
      logname_variables=$(export -p | ${grep} \"^declare -x LOGNAME\" || echo \"# No LOGNAME variable found.\")
      malloc_variables=$(export -p | ${grep} \"^declare -x MALLOC_ARENA_MAX\" || echo \"# No MALLOC_ARENA_MAX variable found.\")
      home_variables=$(export -p | ${grep} \"^declare -x HOME\" || echo \"# No HOME variable found.\")
      locale_variables=$(export -p | ${grep} \"^declare -x LANG\" || echo \"# No LANG variable found.\")
      locpath_variables=$(export -p | ${grep} \"^declare -x GUIX_LOCPATH\" || echo \"# No GUIX_LOCPATH variable found.\")
      display_variables=$(export -p | ${grep} \"^declare -x DISPLAY\" || echo \"# No DISPLAY variable found.\")
      xauth_variables=$(export -p | ${grep} \"^declare -x XAUTHORITY\" || echo \"# No XAUTHORITY variable found.\")
      last_profile=\"${profile_arguments[-1]}\"
      profile_paths=\"$(echo ${profile_arguments[@]} | ${coreutils}/bin/tr ' ' ':')\"
      ${coreutils}/bin/env - ~a/bin/bash --init-file <(echo \"$locale_variables\";
                                                       echo \"export TERM=xterm\";
                                                       echo \"$display_variables\";
                                                       echo \"export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin\";
                                                       echo \"$xauth_variables\";
                                                       echo \"$locpath_variables\";
                                                       echo \"$sge_variables\";
                                                       echo \"$slurm_variables\";
                                                       echo \"$malloc_variables\";
                                                       echo \"$hostname_variables\";
                                                       echo \"$logname_variables\";
                                                       echo \"$tmp_variables\";
                                                       echo \"$job_id_variables\";
                                                       echo \"$home_variables\";
                                                       echo \"$set_output\";
                                                       echo \"declare -x GUIX_PROFILE_PATH=\\\"$last_profile\\\"\";
                                                       echo \"declare -x GUIX_PROFILES=\\\"$profile_paths\\\"\";
                                                       echo \"PS1=\\\"\\u@\\h \\W [env]\\\\$ \\\"\") -i \"${@:$(($# + 1))}\"
    else
      printf \"Usage:\\n  $0 $1 /path/to/profile\\n\"
    fi
  else
    printf \"Usage:\\n  $0 $1 /path/to/profile\\n\"
  fi
else
  ${guix} $@
fi~%"
                         (assoc-ref inputs "git")
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
