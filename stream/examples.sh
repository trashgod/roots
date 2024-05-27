#!/bin/sh
printf "Building…"
gprbuild hd
printf "Testing…\n"
./obj/hd -h
./obj/hd default.gpr > test1.txt
./obj/hd < default.gpr > test2.txt
diff test1.txt test2.txt
./obj/hd ./obj/hd.o > test3.txt
./obj/hd < ./obj/hd.o > test4.txt
diff test3.txt test4.txt
printf "Cleaning up.\n"
rm test*.txt
