#!/bin/sh
gprbuild jumble
./obj/jumble -h
./obj/jumble 7 aghhsowxyz
./obj/jumble zzxxzz acert eerst
./obj/jumble | sort -n | tail -8
