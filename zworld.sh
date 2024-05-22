#! /bin/sh
# Build the world; run examples; clean up.
gprbuild -Xmode=release
./hash/examples.sh
./roots/examples.sh | grep error
./shared/examples.sh
./stream/examples.sh
./words/examples.sh
gprclean -r
rmdir obj shared/lib