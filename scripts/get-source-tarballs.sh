#!/bin/bash

guix package --list-available | cut -f1 | while read -r package ; do
    PACKAGE_URL=`guix build -S $package`
    sudo mv $PACKAGE_URL .
done
