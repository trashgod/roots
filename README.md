# Ada Demonstration Programs

Ada demonstration programs, including hash collisions, complex polynomial roots, shared libraries, stream hex dump, simulation, and word jumbles.

## Collisions in a Hashed Table

This program examines collisions in a hashed table. In the program's output, the rows labeled 0..7 represent occupancy classes. Row zero is unused table entries; row one is table entries occupied by a single word; row two is table entries occupied by two words; etc. Next to each count is the percentage of words hashed to that occupancy class. In the example shown, 55% of the 235,886 words have unique hashes; in the worst case, seven words hash to the same table entry. See also [hash/examples](hash/examples.sh) and [_Hash Table_](https://en.wikipedia.org/wiki/Hash_table).

```
$ gprbuild collisions && ./obj/collisions
…
/usr/share/dict/words
Hash count: 235886
Table size: 393241
Load factor: 59.99%
 0: 215725 (0.00%)
 1: 129710 (54.99%)
 2: 38727 (32.84%)
 3: 7768 (9.88%)
 4: 1153 (1.96%)
 5: 143 (0.30%)
 6: 14 (0.04%)
 7: 1 (0.00%)
```

## Roots of Polynomials with Complex Coefficients

Complex polynomial roots using the [Durand-Kerner-Weierstrass](http://en.wikipedia.org/wiki/Durand-Kerner_method) method in Ada. An example of instantiating [`Generic_Roots`](roots/generic_roots.ads) and invoking it may be found [here](roots/croot.adb). See [roots/examples](roots/examples.sh); complete output is shown [here](roots/roots.md).

```
$ gprbuild croot && ./obj/croot
…
Roots of assorted monic polynomials:
Poly:  1.00x^2 - 3.00x^1 + 2.00
Real:  1.00000000000000000E+00
Real:  2.00000000000000000E+00
Largest error: < 1.00000000000000000E-18

Poly:  1.00x^2 - 2.00x^1 + 1.00
Real:  1.00000000000000000E+00
Real:  1.00000000000000000E+00
Largest error: < 1.00000000000000000E-18

Poly:  1.00x^2 + 0.00x^1 + 4.00
Comp:  0.00000000000000000E+00 +- 2.00000000000000000E+00i
Largest error: < 1.00000000000000000E-18
…
```

## Shared Library

A simple, shared library that mixes Ada and C. Executing [shared/examples](shared/examples.sh) runs a simple client that uses the library. A more complex, aggregate library may be found in [_Samples GPR Aggregate Libs_](https://github.com/LoneWanderer-GH/Samples-GPR-Aggregate-Libs). While the examples are nominally cross-platform, some MacOS specific issues are examined [here](https://stackoverflow.com/a/78306770/230513).

```
$ ./shared/examples.sh
Hello from Ada!
Hello from C!
```

## Stream Hex Dump

A hexadecimal dump utility that reads from either

- A named file using `Ada.Streams.Stream_IO`, or
- Standard input using `Ada.Text_IO.Text_Streams`.

See [`stream/examples`](stream/examples.sh) for more examples.

```
$ gprbuild hd && ./obj/hd -h
HexDump: hd [file | stdin]
$ echo "Hello, world." | ./obj/hd
000000: 48 65 6c 6c 6f 2c 20 77 6f 72 6c 64 2e 0a        Hello, world..
```
## War Card Game Simulation

An Ada program that repeatedly plays the children's card game of war. After playing a number of games, the program tallies up some statistics on wins and losses, followed by an ASCII plot of how long the games took.

```
$ gprbuild war && ./obj/war 
Working...
Of 10000 games, 9902 ended before 1200 plays.
West won:       5043
East won:       4859
Average length: 315
Standard dev.:  215
Longest game:   1174
Shortest game:  30
W: A J K A 9 K 5 6 Q 5 7 5 9 T 4 A J 6 K 3 Q Q 3 A Q 6 
E: K 8 9 7 7 2 2 2 8 J 3 5 9 8 4 T T J 4 7 2 6 T 8 3 4 
Distribution of lengths:
0000-0100 |*********
0100-0200 |******************************
0200-0300 |**********************
0300-0400 |***************
0400-0500 |**********
0500-0600 |*******
0600-0700 |*****
0700-0800 |***
0800-0900 |**
0900-1000 |**
1000-1100 |*
1100-1200 |*
```

## Word Jumble

Jumble is an Ada program intended to unscramble jumbled words. You can use it to solve word puzzles; or, when making your own puzzles, you can check that a jumbled word doesn't unscramble to more than one word. It implements the first algorithm outlined [here](https://en.wikipedia.org/wiki/Jumble). The output of several [examples](words/examples.sh) is shown below.

```
$ ./words/examples.sh 
…
Checking 215843 entries.
jumble [-h] [word …]
Checking 215843 entries.
zzxxzz: no match.
acert: caret carte cater crate creat creta react recta trace 
eerst: ester estre reest reset steer stere stree terse tsere 
9 Antu Tuan Tuna antu aunt naut taun tuan tuna 
9 Canari Carian Crania acinar arnica canari carina crania narica 
9 Merat Trema armet mater metra ramet tamer terma trame 
9 caret carte cater crate creat creta react recta trace 
9 ester estre reest reset steer stere stree terse tsere 
10 Easter Eastre Teresa asteer easter reseat saeter seater staree teaser 
10 Elaps Lepas Pales lapse salep saple sepal slape spale speal 
11 Orang Ronga angor argon goran grano groan nagor orang organ rogan 
```
