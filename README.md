# Modular Project

Ada demonstration programs, including complex polynomial roots.

## Complex Polynomial Roots 

Complex polynomial roots using the [Durand-Kerner-Weierstrass](http://en.wikipedia.org/wiki/Durand-Kerner_method) method in Ada. An example of instantiating [`Generic_Roots`](roots/generic_roots.ads) and invoking it may be found [here](roots/croot.adb). Complete output is shown [here](roots/roots.md)]:

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
