Polynomial roots using the [Durand-Kerner-Weierstrass](http://en.wikipedia.org/wiki/Durand-Kerner_method) method in Ada. An example of instantiating []`Generic_Roots`](generic_roots.ads) and invoking it may be found [here](croot.adb). Typical output is shown below:

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

Poly:  4.00x^2 + 0.00x^1 + 1.00
Comp:  0.00000000000000E+00 +- 5.00000000000000E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^3 - 3.00x^2 + 2.00x^1 + 0.00
Real:  0.00000000000000E+00
Real:  1.00000000000000E+00
Real:  2.00000000000000E+00
Largest error: < 1.00000000000000E-15

Poly:  1.00x^4 + 0.00x^3 - 13.00x^2 + 0.00x^1 + 36.00
Real: -3.00000000000000E+00
Real: -2.00000000000000E+00
Real:  2.00000000000000E+00
Real:  3.00000000000000E+00
Largest error: < 1.00000000000000E-15

Poly:  1.00x^5 + 0.00x^4 - 13.00x^3 + 0.00x^2 + 36.00x^1 + 0.00
Real: -3.00000000000000E+00
Real: -2.00000000000000E+00
Real:  0.00000000000000E+00
Real:  2.00000000000000E+00
Real:  3.00000000000000E+00
Largest error: < 1.00000000000000E-15

Poly:  1.00x^2 + 1.00x^1 + 1.00
Comp: -5.00000000000000E-01 +- 8.66025403784439E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Real: -1.00000000000000E+00
Comp:  0.00000000000000E+00 +- 1.00000000000000E+00i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Comp: -8.09016994374947E-01 +- 5.87785252292473E-01i
Comp:  3.09016994374947E-01 +- 9.51056516295154E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Real: -1.00000000000000E+00
Comp: -5.00000000000000E-01 +- 8.66025403784439E-01i
Comp:  5.00000000000000E-01 +- 8.66025403784439E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Comp: -9.00968867902419E-01 +- 4.33883739117558E-01i
Comp: -2.22520933956314E-01 +- 9.74927912181824E-01i
Comp:  6.23489801858733E-01 +- 7.81831482468030E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Real: -1.00000000000000E+00
Comp: -7.07106781186547E-01 +- 7.07106781186547E-01i
Comp:  0.00000000000000E+00 +- 1.00000000000000E+00i
Comp:  7.07106781186547E-01 +- 7.07106781186548E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Comp: -9.39692620785908E-01 +- 3.42020143325669E-01i
Comp: -5.00000000000000E-01 +- 8.66025403784439E-01i
Comp:  1.73648177666930E-01 +- 9.84807753012208E-01i
Comp:  7.66044443118978E-01 +- 6.42787609686539E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^9 + 1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Real: -1.00000000000000E+00
Comp: -8.09016994374947E-01 +- 5.87785252292473E-01i
Comp: -3.09016994374947E-01 +- 9.51056516295154E-01i
Comp:  3.09016994374947E-01 +- 9.51056516295154E-01i
Comp:  8.09016994374947E-01 +- 5.87785252292473E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^10 + 1.00x^9 + 1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Comp: -9.59492973614497E-01 +- 2.81732556841430E-01i
Comp: -6.54860733945285E-01 +- 7.55749574354258E-01i
Comp: -1.42314838273285E-01 +- 9.89821441880933E-01i
Comp:  4.15415013001886E-01 +- 9.09631995354518E-01i
Comp:  8.41253532831181E-01 +- 5.40640817455598E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^11 + 1.00x^10 + 1.00x^9 + 1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Real: -1.00000000000000E+00
Comp: -8.66025403784439E-01 +- 5.00000000000000E-01i
Comp: -5.00000000000000E-01 +- 8.66025403784439E-01i
Comp:  0.00000000000000E+00 +- 1.00000000000000E+00i
Comp:  5.00000000000000E-01 +- 8.66025403784439E-01i
Comp:  8.66025403784439E-01 +- 5.00000000000000E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^12 + 1.00x^11 + 1.00x^10 + 1.00x^9 + 1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Comp: -9.70941817426052E-01 +- 2.39315664287558E-01i
Comp: -7.48510748171101E-01 +- 6.63122658240795E-01i
Comp: -3.54604887042536E-01 +- 9.35016242685415E-01i
Comp:  1.20536680255323E-01 +- 9.92708874098054E-01i
Comp:  5.68064746731156E-01 +- 8.22983865893656E-01i
Comp:  8.85456025653210E-01 +- 4.64723172043769E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^13 + 1.00x^12 + 1.00x^11 + 1.00x^10 + 1.00x^9 + 1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Real: -1.00000000000000E+00
Comp: -9.00968867902419E-01 +- 4.33883739117558E-01i
Comp: -6.23489801858733E-01 +- 7.81831482468030E-01i
Comp: -2.22520933956314E-01 +- 9.74927912181824E-01i
Comp:  2.22520933956314E-01 +- 9.74927912181824E-01i
Comp:  6.23489801858733E-01 +- 7.81831482468030E-01i
Comp:  9.00968867902419E-01 +- 4.33883739117558E-01i
Largest error: < 1.00000000000000E-15

Poly:  1.00x^14 + 1.00x^13 + 1.00x^12 + 1.00x^11 + 1.00x^10 + 1.00x^9 + 1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Comp: -9.78147600733806E-01 +- 2.07911690817759E-01i
Comp: -8.09016994374947E-01 +- 5.87785252292473E-01i
Comp: -5.00000000000000E-01 +- 8.66025403784439E-01i
Comp: -1.04528463267653E-01 +- 9.94521895368273E-01i
Comp:  3.09016994374947E-01 +- 9.51056516295154E-01i
Comp:  6.69130606358858E-01 +- 7.43144825477394E-01i
Comp:  9.13545457642601E-01 +- 4.06736643075800E-01i
Largest error: = 1.60982338570648E-15

Poly:  1.00x^15 + 1.00x^14 + 1.00x^13 + 1.00x^12 + 1.00x^11 + 1.00x^10 + 1.00x^9 + 1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Real: -1.00000000000000E+00
Comp: -9.23879532511287E-01 +- 3.82683432365090E-01i
Comp: -7.07106781186548E-01 +- 7.07106781186548E-01i
Comp: -3.82683432365090E-01 +- 9.23879532511287E-01i
Comp:  0.00000000000000E+00 +- 1.00000000000000E+00i
Comp:  3.82683432365090E-01 +- 9.23879532511287E-01i
Comp:  7.07106781186548E-01 +- 7.07106781186548E-01i
Comp:  9.23879532511287E-01 +- 3.82683432365090E-01i
Largest error: = 2.22044604925031E-15

Poly:  1.00x^37 + 1.00x^36 + 1.00x^35 + 1.00x^34 + 1.00x^33 + 1.00x^32 + 1.00x^31 + 1.00x^30 + 1.00x^29 + 1.00x^28 + 1.00x^27 + 1.00x^26 + 1.00x^25 + 1.00x^24 + 1.00x^23 + 1.00x^22 + 1.00x^21 + 1.00x^20 + 1.00x^19 + 1.00x^18 + 1.00x^17 + 1.00x^16 + 1.00x^15 + 1.00x^14 + 1.00x^13 + 1.00x^12 + 1.00x^11 + 1.00x^10 + 1.00x^9 + 1.00x^8 + 1.00x^7 + 1.00x^6 + 1.00x^5 + 1.00x^4 + 1.00x^3 + 1.00x^2 + 1.00x^1 + 1.00
Real: -1.00000000000000E+00
Comp: -9.86361303402722E-01 +- 1.64594590280734E-01i
Comp: -9.45817241700635E-01 +- 3.24699469204684E-01i
Comp: -8.79473751206489E-01 +- 4.75947393037074E-01i
Comp: -7.89140509396394E-01 +- 6.14212712689668E-01i
Comp: -6.77281571625741E-01 +- 7.35723910673132E-01i
Comp: -5.46948158122427E-01 +- 8.37166478262529E-01i
Comp: -4.01695424652969E-01 +- 9.15773326655057E-01i
Comp: -2.45485487140799E-01 +- 9.69400265939330E-01i
Comp: -8.25793454723323E-02 +- 9.96584493006670E-01i
Comp:  8.25793454723323E-02 +- 9.96584493006670E-01i
Comp:  2.45485487140799E-01 +- 9.69400265939330E-01i
Comp:  4.01695424652969E-01 +- 9.15773326655058E-01i
Comp:  5.46948158122427E-01 +- 8.37166478262529E-01i
Comp:  6.77281571625741E-01 +- 7.35723910673132E-01i
Comp:  7.89140509396394E-01 +- 6.14212712689668E-01i
Comp:  8.79473751206489E-01 +- 4.75947393037074E-01i
Comp:  9.45817241700635E-01 +- 3.24699469204683E-01i
Comp:  9.86361303402722E-01 +- 1.64594590280734E-01i
Largest error: = 1.61259894326804E-14

Poly: (1 + 3i)x^2 + (2 + 2i)x + (3 + i)
Comp: -8.00000000000000E-01 - 6.00000000000000E-01i
Comp:  0.00000000000000E+00 + 1.00000000000000E+00i
Largest error: < 1.00000000000000E-15

Poly: (1 + i)x^3 + (2 + i)x^2 + (3 + i)x + (4 + i)
Comp: -1.40135938330275E+00 + 2.88269653138075E-01i
Comp: -2.84985631785343E-01 - 1.30378640290474E+00i
Comp:  1.86345015088091E-01 + 1.51551674976666E+00i
Largest error: < 1.00000000000000E-15

Poly: (1 + i)x^3 + (2 + 2i)x^2 + (3 + 3i)x + (4 + 4i)
Real: -1.65062919143939E+00
Comp: -1.74685404280306E-01 - 1.54686888723140E+00i
Comp: -1.74685404280306E-01 + 1.54686888723140E+00i
Largest error: < 1.000000
```
