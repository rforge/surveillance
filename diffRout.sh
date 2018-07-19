#!/bin/bash
# Compare surveillance-Ex.Rout file from R CMD check with saved file
#
# Copyright (C) 2018 Sebastian Meyer

set -e   # enables shell option 'errexit', i.e. exit immediately on error

if [ ! -d surveillance.Rcheck ]; then
    echo "Error: need to run R CMD check first"
    exit 1
fi

## define patterns to ignore
SCRIPT="/<environment: 0x/d; /<bytecode: 0x/d; /^Time elapsed: /d; /^time needed /d; /^% /d; /^Time at beginning/d; /^Runtime/d; /^Estimated finishing time:/d; /^Done /d;"

sed -e "$SCRIPT" develop/surveillance-Ex.Rout.save > /tmp/old.Rout
sed -e "$SCRIPT" surveillance.Rcheck/surveillance-Ex.Rout > /tmp/new.Rout

if [ -x "$(command -v meld)" ]; then
    meld /tmp/old.Rout /tmp/new.Rout &
else
    diff -b /tmp/old.Rout /tmp/new.Rout
fi
