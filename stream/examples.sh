#!/bin/sh
echo Building…
gprbuild -Xmode=release hd
./obj/hd -h
echo Testing…
./obj/hd default.gpr > test1.txt
./obj/hd < default.gpr > test2.txt
diff test1.txt test2.txt
./obj/hd ./obj/hd.o > test3.txt
./obj/hd < ./obj/hd.o > test4.txt
diff test3.txt test4.txt
echo Cleaning up.
rm test*.txt
