#!/bin/sh
gprbuild -Xmode=develop jumble
./obj/jumble -h
./obj/jumble zzxxzz acert eerst
./obj/jumble | sort -n | tail -8
