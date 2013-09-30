#!/bin/bash

# This shell scripts generates the top-level Markdown structure of the
# cl-6502 book.
#
# The authors list is automatically generated from Git history,
# ordered from most to least commits.
# TODO: Add jit.lisp later?

cat <<EOF
% romreader
% $(git log --pretty=format:%an | \
        grep -v -e '^root$' | \
        sort | uniq -c | sort -nr | sed 's/^[0-9 ]*//' | \
        awk 'NR > 1 { printf("; ") } { printf("%s", $0) } END { print("") }')

# Introduction
$(cat ../intro.md)

# The Core Design
$(cat ../romreader.md)
$(cat obj/romreader.lisp.md)

# No One Expects Binary Formats
$(cat ../conditions.md)
$(cat obj/conditions.lisp.md)

# The iNES File Format
$(cat ../nes.md)
$(cat obj/nes.lisp.md)

# All Packaged Up
$(cat ../package.md)
$(cat obj/package.lisp.md)

# Conclusion
$(cat ../outro.md)

EOF
