# Aristarchus

## 

A Mass is a Number

-----------------------------------------------------------------------------------------------------------------------------------

A Radius is a Number

-----------------------------------------------------------------------------------------------------------------------------------

A Name is an unbound identifier

----------------------------------------------------------------------------------------------------------------------------------------------------------------

A Posn is a (posn Number Number)

Represents a coordinate pair.

-----------------------------------------------------------------------------------------------------------------------------------

## Definitional Forms:

(body Name Mass Radius):

​		Binds a Name to an internal Structure containing the mass and radius.

ex: `(body earth 5.972e+24 6.3781e+6)`

ex: `(body sun 1.989e+30 6.957e+8)`

----------------------------------------------------------------------------------------------------------------------------------------------------------------

A Distance is a Number, represents the radius between the centers of two things

-----------------------------------------------------------------------------------------------------------------------------------

(connection Name Identifier Identifier Distance) :

​		Binds a Name to an internal Structure containing two identifiers and the distance between their centers.

ex: `(connection earth-sun sun earth 1.496e+11)`

----------------------------------------------------------------------------------------------------------------------------------------------------------------

(system Name (Connection ...)):

​		 binds a Name to an internal Structure containing a list of connections

ex: `(system Pluto-moons (pluto-charon pluto-hydra pluto-nix pluto-styx pluto-kerberos))`

-----------------------------------------------------------------------------------------------------------------------------------

## Expressions:

A Formula is one of :

- surface-gravity
- escape-velocity
- kepler3-period
- L1
- L2
- L3
- L4
- L5

-----------------------------------------------------------------------------------------------------------------------------------

A Mode is either `'answer` or `'steps`

`'answer` computes the answer to the formula with the given input.

`'steps` produces a webpage with step-by-step solutions.

An Expression is (Formula Structure ... [#:mode Mode])

-----------------------------------------------------------------------------------------------------------------------------------

`(surface-gravity Body)`:

Finds the surface gravity of a body. The output is in `m/s^2`.

ex: `(surface-gravity earth) -> ~9.79`

-----------------------------------------------------------------------------------------------------------------------------------

`(escape-velocity body)`

Finds the escape velocity from a body. The output is in `m/s`

ex: `(escape-velocity earth) -> ~11179.56`

-----------------------------------------------------------------------------------------------------------------------------------

`(kepler3-period connection)`

Finds the period of an orbital system. The output is in `s`

ex: `(kepler3-period earth-sun) -> ~31554700.48`

31554700.4807 seconds is approximately 365.2164 days.

-----------------------------------------------------------------------------------------------------------------------------------

`(L1 connection)`

Finds the L1 lagrange point of an orbital system. The output is a `posn`.

ex: `(L1 earth-sun) -> (posn 148103583760.57556 0)`

-----------------------------------------------------------------------------------------------------------------------------------

`(L2 connection)`

Finds the L2 lagrange point of an orbital system. The output is a `posn`.

ex: `(L2 earth-sun) -> (posn 151096416239.42444 0)`

-----------------------------------------------------------------------------------------------------------------------------------

`(L3 connection)`

Finds the L3 lagrange point of an orbital system. The output is a `posn`.

ex: `(L3 earth-sun) -> (posn -149600187156.13324 0)`

-----------------------------------------------------------------------------------------------------------------------------------

`(L4 connection)`

Finds the L4 lagrange point of an orbital system. The output is a `posn`.

ex: `(L4 earth-sun) -> (posn 74799550825.28027 129557400406.15201)`

-----------------------------------------------------------------------------------------------------------------------------------

`(L5 connection)`

Finds the L5 lagrange point of an orbital system. The output is a `posn`.

ex: `(L5 earth-sun) -> (posn 74799550825.28027 129557400406.15201)`

-----------------------------------------------------------------------------------------------------------------------------------

## Formula:

An arg is an identifier.

----

An Association list is a [(identifier any) ...].

------

An MSE (Math-s-expression) is one of:

- Identifier
- Symbol
- Number
- String
- (+ MSE MSE ...)
- (- MSE MSE ...)
- (* MSE MSE ...)
- (/ MSE MSE)
- (sqrt MSE)
- (expt MSE MSE)
- (posn MSE MSE)

----

(formula (Name arg ...) Association-List MSE)`

ex: 

```Racket
(formula (surface-gravity body)
         ((G G) (M_b (body-mass body)) (r (body-radius body)))
         (/ (* G M_b) (expt r 2))))
```