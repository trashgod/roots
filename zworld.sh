#! /bin/sh
# Build the world; run examples; clean up.
gprbuild
./hash/examples.sh
./roots/examples.sh | grep error
./stream/examples.sh
./words/examples.sh
gprclean -r
rmdir obj