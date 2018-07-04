#!/bin/bash
# JdL

# Based on a list of pip packages
# Use guix import pypi xx
# check if package available?
#   yes -> install into profile
#   no ->
#     recurse this process for each dependency
#     make package
#     check build package
#     add package

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.

# Initialize variables:
packages=""
verbose=0



export SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs"
export SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
export GIT_SSL_CAINFO="$SSL_CERT_FILE"

export CURL_CA_BUNDLE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
export REQUESTS_CA_BUNDLE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"

# PIP REQUIRES PEM
export CA_BUNDLE="$HOME/.guix-profile/etc/ssl/certs/DigiCert_Global_Root_CA:2.16.8.59.224.86.144.66.70.177.161.117.106.201.89.145.199.74.pem"
export PIP_CERT="$HOME/.guix-profile/etc/ssl/certs/DigiCert_High_Assurance_EV_Root_CA:2.16.2.172.92.38.106.11.64.155.143.11.121.242.174.70.37.119.pem"

read -d '' preamble << EOF
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 UMCU
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
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages qt))
EOF


show_help() {                                                   # Function to print a usage string.
  echo "Usage: $0 [ -p package1,package2 ] [ -v verbose ]" 1>&2 # Echo usage string to standard error
  exit 1                                                        # Exit with error status 1
}

while getopts "h?vp:" opt; do
    case "$opt" in
    h|\?)
        show_help
        ;;
    v)  verbose=1
        ;;
    p)  packages=$OPTARG
        ;;
    esac
done

shift $((OPTIND-1))

[ "${1:-}" = "--" ] && shift

check_avail() {
  guixr package -i $1 | grep 'unknown package' &> /dev/null
  if [ $? == 0 ]; then
    # found a error message, package is not available
    false
  else
    true
  fi
}

check_build() {
  guixr build -i $1 | grep '?? error ??' &> /dev/null
  if [ $? == 0 ]; then
    # found a error message, package did not build
    # add missing dependencies
    false
  else
    true
  fi
}


add_recipie() {
  git add $1
  git commit -m "Adding pip package $1"
  git push
}



make_recipie() {
  if [ check_avail $1 ];
  then
    guixr package -i $1
  else
    # recurse through all dependecies
    guix import pypi $1 | ...""... | while read -r pack; do
      make_recipie $pack
    done

    # ASSUMES TO BE RUN FROM THE SCRIPTS FOLDER
    recipie_file = "../../guix-additions/umcu/packages/$1.scm"
    echo $preamble > $recipie_file
    echo "(define public $1" >> $recipie_file
    guix import pypi $1 >> $recipie_file
    echo ")" >> $recipie_file

    if [ check_build $1 ];
      add_recipie $recipie_file
      # git pull on hpcguix
      # guix package -i $1
    else
      echo "[ERROR] a problem occurred while building $1"
      exit 1
    fi
  fi
}

#echo "verbose=$verbose, output_file='$output_file', Leftovers: $@"
for package in ${packages//,/ }
do
    if [ $verbose == 1 ]; then echo make_recipie "python-$package"; fi
    make_recipie "python-$package"
done

# End of file

#guix package --list-available | cut -f1 | while read -r package ; do
#    PACKAGE_URL=`guix build -S $package`
#    sudo mv $PACKAGE_URL .
#done
