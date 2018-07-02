#!/bin/bash
# JdL

# Based on a list of pip packages
# For each package get dependencies
#   get/create recipes for dependencies
# create recipie for package
# Add all GUIX packages to guix-addiotions
# Make a request to upstream to include the recipies

# Use guix import pypi xx where possible

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.

# Initialize our own variables:
packages=""
verbose=0


read -d '' preamble << EOF
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
  guix package -i $1 | grep 'unknown package' &> /dev/null
  if [ $? == 0 ]; then
   return false
  fi
  return true
}

add_recipie() {
  git add $1
  git commit -m "Adding python package $1"
  git push
}

make_recipie() {
  if check_avail $1
  then
    guix package -i $1
  else
    guix import pypi $1 | | while read -r pack; do
      make_recipie $pack
    done

    # CHECK BEST PRACTICES PATHS
    echo $preamble > "$1.scm"
    echo "(define public $1">>"$1.scm"
    guix import pypi $1 >> "$1.scm"
    echo ")" >> "$1.scm"

    if guix build $1

      add_recipie "$1.scm"
      # git pull on hpcguix
      # guix package -i $1
    else
      exit 1
    fi
  fi
}

#echo "verbose=$verbose, output_file='$output_file', Leftovers: $@"
for package in ${packages//,/ }
do
    if [ $verbose == 1 ]; then
        echo "guix import pypi $package"
    fi
    guix import pypi $package
    # check if available if so -> install into profile
    # are dependecies in the store?
    #   yes -> make recipie and add to git
    #   no -> get missing dependecies and recurse
    #
done

# End of file

#guix package --list-available | cut -f1 | while read -r package ; do
#    PACKAGE_URL=`guix build -S $package`
#    sudo mv $PACKAGE_URL .
#done
