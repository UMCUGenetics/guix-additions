#!/bin/bash

guix package --list-available | grep "umcu/packages" | awk '{ print $1 "@" $2 }' | while read -r package ; do
    guix build $package
done
