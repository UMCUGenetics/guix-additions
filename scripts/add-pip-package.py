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
