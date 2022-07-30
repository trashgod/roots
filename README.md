# Ada Demonstration Programs

Ada demonstration programs, including hash collisions and complex polynomial roots.

## Collisions in a Hashed Table

This program examines collisions in a hashed table. In the program's output, the rows labeled 0..7 represent occupancy classes. Row zero is unused table entries; row one is table entries occupied by a single word; row two is table entries occupied by two words; etc. Next to each count is the percentage of words hashed to that occupancy class. In the example shown, 55% of the 235,886 words have unique hashes; in the worst case, seven words hash to the same table entry. See also [_Hash Table_](https://en.wikipedia.org/wiki/Hash_table).

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

## Complex Polynomial Roots 

Complex polynomial roots using the [Durand-Kerner-Weierstrass](http://en.wikipedia.org/wiki/Durand-Kerner_method) method in Ada. An example of instantiating [`Generic_Roots`](roots/generic_roots.ads) and invoking it may be found [here](roots/croot.adb). Complete output is shown [here](roots/roots.md):

```
$ gprbuild croot && ./obj/croot
…
Roots of assorted monic polynomials:
Poly:  1.00x^2 - 3.00x^1 + 2.00
Real:  1.00000000000000E+00
Real:  2.00000000000000E+00
Largest error: < 1.00000000000000E-15

Poly:  1.00x^2 - 2.00x^1 + 1.00
Real:  1.00000000000000E+00
Real:  1.00000000000000E+00
Largest error: < 1.00000000000000E-15

Poly:  1.00x^2 + 0.00x^1 + 4.00
Comp:  0.00000000000000E+00 +- 2.00000000000000E+00i
Largest error: < 1.00000000000000E-15
…
```
