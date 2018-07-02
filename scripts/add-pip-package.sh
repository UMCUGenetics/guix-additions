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

#echo "verbose=$verbose, output_file='$output_file', Leftovers: $@"
for package in ${packages//,/ }
do
    if [ $verbose == 1 ]; then
        echo "guix import pypi $package"
    fi
    guix import pypi $package
done

# End of file

#guix package --list-available | cut -f1 | while read -r package ; do
#    PACKAGE_URL=`guix build -S $package`
#    sudo mv $PACKAGE_URL .
#done
