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
from subprocess import Popen, PIPE
import re

parser = argparse.ArgumentParser(description='Generate GUIX recipies for PIP package(s)')
parser.add_argument("-v", "--verbose", help="Toggle verbose output")
parser.add_argument("-p", "--packages", nargs="+", type=str, help="List of pip packages to add to GUIX")
args = parser.parse_args()


GUIX_UNKNOWN_PACKAGE_MESSAGE="unknown package"
GUIX_PACKAGE_NOTFOUND_MESSAGE="package not found"

#error: Could not find suitable distribution for Requirement.parse('xxxx<=0.0.0')
# present in error output
GUIX_DEPENDENCY_NOTFOUND_PATTERN="(?<=Could not find suitable distribution for Requirement.parse\(\')(.+)\'"
PYTHON_MODULE_NOTFOUND_PATTERN="(?<=ModuleNotFoundError: No module named \')(.+)\'"
GUIX_PACKAGE_PATH="GUIX_PACKAGE_PATH"

# ASSUMES TO BE RUN FROM THE SCRIPTS FOLDER
GUIX_ADDITIONS_PATH="../../guix-additions/umcu/packages/"
GUIX_GNU_ADDITIONS_PATH=":/gnu/repositories/guix-additions/"


PREAMLBE_GUIX="""
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
"""

PREAMLBE_MODULES="""
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
  Popen("git add {}".format(packagefile), stdout=PIPE, stderr=PIPE, shell=True)
  Popen("git commit -m \"Automatically adding pip package {}\"".format(package), stdout=PIPE, stderr=PIPE, shell=True)
  Popen("git push", stdout=PIPE, stderr=PIPE, shell=True)

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

def add_native(recipe, dependency):
    # Add inputs field if not yet present
    if not "(native-inputs" in recipe:
        recipe = recipe.replace("(build-system python-build-system)","(build-system python-build-system)\n(native-inputs\n\'())")

    # Add dependency
    recipe = recipe.replace("(native-inputs\n\'(", "(native-inputs\n\'((\"{0}\" ,{0})\n".format(dependency))


def add_dependency(recipe, dependency):
    # Add inputs field if not yet present
    if not "(inputs" in recipe:
        recipe = recipe.replace("(build-system python-build-system)","(build-system python-build-system)\n(inputs\n\'())")

    # Add dependency
    recipe = recipe.replace("(inputs\n\'(", "(inputs\n\'((\"{0}\" ,{0})\n".format(dependency))

# MAKE A RECIPE CLEANER
# Use estat as an example
#     ("python-tqdm\r" ,#{python-tqdm\xd;}#)))

def check_avail(package):
    p_pack = Popen("guixr package -i {}".format(package), stdout=PIPE, stderr=PIPE, shell=True)
    stdout, stderr = p_pack.communicate()
    if args.verbose: print("O: "+stdout)
    if args.verbose: print("E: "+stderr)

    if GUIX_UNKNOWN_PACKAGE_MESSAGE in stderr:
        if args.verbose: print("Package [{}] not found, trying to create a package for it".format(package))
        return(False)
    if "command not found" in stderr:
        return(False)
    return(True)

def make_recipe(package):
    recipefile = "{0}{1}.scm".format(GUIX_ADDITIONS_PATH, package)

    if check_avail(package):
        print("Package [{0}] is available in GUIX".format(package))
        return(True)

    else:
        # 1st iteration is special due to the PREAMLBE etc...
        # TODO think about how we can clean this up once we have a working prototype

        p_imp = Popen("guixr import pypi {0}".format(package.replace("python-","")), stdout=PIPE, stderr=PIPE, shell=True)
        packagerecipe, stderr = p_imp.communicate()
        if args.verbose: print("Package [{0}] - Current recipe {1}".format(package, packagerecipe))

        # Make GUIX recipe
        packagerecipe = "{0}\n(define-module (umcu packages {1})\n{2}\n(define-public {1}\n {3}) (define-public python2-{4}\n (package-with-python2 {1}))".format(PREAMLBE_GUIX, package, PREAMLBE_MODULES, packagerecipe, package.replace("python-",""))
        if args.verbose: print("Package [{0}] - Current recipe {1}".format(package, packagerecipe))

        # write to file
        with open(recipefile, 'w') as outf:
            outf.write(packagerecipe)

        p_build = Popen("guixr build {0}".format(package), stdout=PIPE, stderr=PIPE, shell=True)
        stdout, stderr = p_build.communicate()
        if args.verbose: print("Package [{0}] - Current buildstate\n O:{1}\n E:{2}".format(package, stdout, stderr))

        while "Could not find suitable distribution" in stderr:
            # parse missing dependency from buildstate
            missing_dep = re.findall(GUIX_DEPENDENCY_NOTFOUND_PATTERN, stderr)
            missing_nat = re.findall(PYTHON_MODULE_NOTFOUND_PATTERN, stderr)

            if len(missing_dep) is 0 and len(missing_nat) is 0:
                if args.verbose: print("Package [{0}] - No more missing dependecies".format(package))
                break

            # For alll missing dependencies
            for mis_dep in missing_dep:
                if args.verbose: print("Package [{0}] - Missing dependency {1}".format(package, mis_dep))
                # recurse for this dependency
                missing_dep = "python-"+mis_dep.split("=")[0].replace("<","").replace(">","")
                make_recipe(missing_dep)
                add_dependency(packagerecipe, missing_dep)

            for mis_nat in missing_nat:
                if args.verbose: print("Package [{0}] - Missing native input {1}".format(package, mis_nat))
                missing_nat = "python-"+mis_nat
                make_recipe(missing_nat)
                add_native(packagerecipe, missing_nat)


            if args.verbose: print("Package [{0}] - Current recipe {1}".format(package, packagerecipe))
            with open(recipefile, 'w') as outf:
                outf.write(packagerecipe)

            p_build = Popen("guixr build {0}".format(package), stdout=PIPE, stderr=PIPE, shell=True)
            stdout, stderr = p_build.communicate()
            if args.verbose: print("Package [{0}] - Current buildstate\n O:{1}\n E:{2}".format(package, stdout, stderr))

        exit(1)
        # We have a complete and working packagerecipe
        add_recipe(package, recipefile)

        # Check if it installs
        p_pack = Popen("guixr package -i {}".format(package), stdout=PIPE, stderr=PIPE, shell=True)
        stdout, stderr = p_pack.communicate()
        if args.verbose: print("Package [{0}] - Current packagestate\n O:{1}\n E:{2}".format(package, stdout, stderr))

        # Report on succes or failure
        if "nothing to be done" in stdout:
            print("DONE")
        if not GUIX_UNKNOWN_PACKAGE_MESSAGE in stderr:
            if not GUIX_PACKAGE_NOTFOUND_MESSAGE in stderr:
                print("DONE")

# SET GUIX PATH TO LOCAL PACKAGES
#print("export {0}={1}".format(GUIX_PACKAGE_PATH, GUIX_ADDITIONS_PATH))
os.environ[GUIX_PACKAGE_PATH]=GUIX_ADDITIONS_PATH+GUIX_GNU_ADDITIONS_PATH
if args.verbose: print(os.environ[GUIX_PACKAGE_PATH])

for pack in args.packages:
    if args.verbose: print("Package [{0}] - Starting to generate recipe".format(pack))
    packagename = "python-{0}".format(pack)
    make_recipe(packagename)
