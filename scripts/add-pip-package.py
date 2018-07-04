#!/bin/python
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

import argparse
import os
import re

parser = argparse.ArgumentParser(description='Generate GUIX recipies for PIP package(s)')
parser.add_argument("-v", "--verbose", help="Toggle verbose output")
parser.add_argument("-p", "--packages", nargs="+", type=str, help="List of pip packages to add to GUIX")
args = parser.parse_args()


GUIX_UNKNOWN_PACKAGE_MESSAGE="unknown package"
GUIX_PACKAGE_NOTFOUND_MESSAGE="package not found"

#error: Could not find suitable distribution for Requirement.parse('xxxx<=0.0.0')
# present in error output
GUIX_DEPENDENCY_NOTFOUND_PATTERN="(?<=Could not find suitable distribution for Requirement.parse\(\')(.+)\<\="
GUIX_PACKAGE_PATH="GUIX_PACKAGE_PATH"

# ASSUMES TO BE RUN FROM THE SCRIPTS FOLDER
GUIX_ADDITIONS_PATH="../../guix-additions/umcu/packages/"


PREAMLBE="""
;;; GNU Guix --- Functional package management for GNU
;;; Copyright 2018 UMCU
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
"""


def add_recipe(package, packagefile):
  os.popen("git add {}".format(packagefile))
  os.popen("git commit -m \"Automatically adding pip package {}\"".format(package))
  os.popen("git push")

#    (build-system python-build-system)
#    (inputs
#     `(("python-pysocks" ,python-pysocks)
#       ("python-pytest" ,python-pytest)
#       .......
#       ("python-pytest-xdist" ,python-pytest-xdist)))

# MAYBE HANDY AT SOME POINT
# DIASBLE TESTS
# (arguments
# '(#:tests? #f))

def add_dependency(recipe, dependency):
    # Add inputs field if not yet present
    if not "(inputs" in recipe:
        recipe = recipe.replace("(build-system python-build-system)","(build-system python-build-system)\n(inputs\n\'())")

    # Add dependency
    recipe = recipe.replace("(inputs\n\'(", "(inputs\n\'((\"{0}\" ,{0})\n".format(dependency))



def check_avail(package):
    packagestate = os.popen("guixr package -i {}".format(package), stderr=subprocess.STDOUT).read()

    if GUIX_UNKNOWN_PACKAGE_MESSAGE in packagestate:
        if args.verbose: print("Package [{}] not found, trying to create a package for it".format(package))
        return(False)
    else:
        return(True)

def make_recipe(package):
    recipefile = "{0}{1}.scm".format(GUIX_ADDITIONS_PATH, package)

    if check_avail(package):
        print("Package [{0}] is available in GUIX".format(package))
        return(True)

    else:
        # 1st iteration is special due to the PREAMLBE etc...
        # TODO think about how we can clean this up once we have a working prototype

        packagerecipe = os.popen("guixr import pypi {0}".format(package.replace("python-",""))).read()
        if args.verbose: print("Package [{0}] - Current recipe {1}".format(package, packagerecipe))

        # Make GUIX recipe
        packagerecipe = "{0}\n(define-public {1}\n{2}\n) (define-public python2-{1}\n (package-with-python2 {1}))".format(PREAMLBE, package, packagerecipe)
        if args.verbose: print("Package [{0}] - Current recipe {1}".format(package, packagerecipe))

        # write to file
        with open(recipefile, 'w') as outf:
            outf.write(packagerecipe)

        buildstate = os.popen("guixr build {0}".format(package)).read()
        if args.verbose: print("Package [{0}] - Current buildstate {1}".format(package, buildstate))


        while "Could not find suitable distribution" in buildstate:
            # parse missing dependency from buildstate
            missing_dep = re.match(GUIX_DEPENDENCY_NOTFOUND_PATTERN, buildstate)
            if missing_dep is None:
                if args.verbose: print("Package [{0}] - No more missing dependecies".format(package))
                break
            if args.verbose: print("Package [{0}] - Missing dependency {1}".format(package, missing_dep.groups()))

            # recurse for this dependency
            make_recipe(missing_dep.group(1))

            # when dependency is done add it and continue
            add_dependency(packagerecipe)
            if args.verbose: print("Package [{0}] - Current recipe {1}".format(package, packagerecipe))

            with open(recipefile, 'w') as outf:
                outf.write(packagerecipe)

            buildstate = os.popen("guixr build {0}".format(package)).read()
            if args.verbose: print("Package [{0}] - Current buildstate {1}".format(package, buildstate))

        # We have a complete and working packagerecipe
        add_recipe(package, packagefile)
        # Check if it installs
        packagestate = os.popen("guixr package -i {}".format(package)).read()
        if args.verbose: print("Package [{0}] - Current packagestate {1}".format(package, packagestate))

        # Report on succes or failure
        if "nothing to be done" in packagestate:
            print("DONE")
        if not GUIX_UNKNOWN_PACKAGE_MESSAGE in packagestate:
            if not GUIX_PACKAGE_NOTFOUND_MESSAGE in packagestate:
                print("DONE")

# SET GUIX PATH TO LOCAL PACKAGES
os.system("export {0}={1}".format(GUIX_PACKAGE_PATH, GUIX_ADDITIONS_PATH))
if args.verbose: os.system("echo ${}".format(GUIX_PACKAGE_PATH))
for pack in args.packages:
    if args.verbose: print("Package [{0}] - Starting to generate recipe".format(pack))
    packagename = "python-{0}".format(pack)
    make_recipe(packagename)
