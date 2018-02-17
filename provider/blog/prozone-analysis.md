---
title: Prozone effect analysis
tags: kappa, chemistry
math: kappa
date: 2017-02-08
---

## Problem

Let's consider the following Kappa model:
```kappa
'A.B' A(b), B(a) <-> A(b!1), B(a!1) @ 'k+', 'k-'
'B.C' B(c), C(b) <-> B(c!1), C(b!1) @ 'k+', 'k-'
```

We want to compute the concentratin of $ABC$ complex at equilibrium. 

## Solution

Let's suppose
that the initial quantities of $A$ and $C$ are equal. By symmetry, we have:
$$ [A] = [C]     [AB] = [BC] $$
Therefore, the variables describing the state of the system are the following:
$$ [A]     [B]     [AB]     [ABC]$$
The law of mass action gives the following equations:
```align
(d[A])/(dt)  &=  k^-([AB] + [ABC]) - k^+[A]*([B] + [BC])     \\ \\
(d[B])/(dt)  &=  k^-([AB] + [BC]) - k^+[B]*([A]+[C])         \\ \\
(d[AB])/(dt)  &=  k^+[A][B] + k^-[ABC] - k^+[AB][C] - k^-[AB]  \\ \\
(d[ABC])/(dt)  &=  k^+([AB][C] + [A][BC]) - 2k^-[ABC]
```


At equilibrium, we have:
$$k^-([AB] + [ABC])  =  k^+[A]*([B] + [AB])$$
$$k^-[AB]  =  k^+[A][B]$$
$$k^+[A][B] + k^-[ABC]  =  k^+[A][AB] + k^-[AB]$$
$$k^+[A][AB] = k^-[ABC]$$
The third equation is redundant, as it can be derived from the second one and the fourth one.
Let's write $K = k^+//k^-$. Then, we have $$ [ABC] = K[A][AB]     [AB] = K[A][B] $$
Remind that we want to express $[ABC]$ as a function of $a$ and $b$ where:
```align
a &= [A] + [AB] + [ABC] \\ \\
b &= [B] + [AB] + [BC] + [ABC] = [B] + 2[AB] + [ABC]
```
By isolating $[ABC]$ in these two equations, we get
$$ a - [A]  =  b - [B] - [AB]  $$
Now, let's look again at the first equilibrium equality:
$$k^-([AB] + [ABC])  =  k^+[A]*([B] + [AB])$$
We have $[AB] + [ABC] = a - [A]$ and $[B] + [AB] = b - a + [A]$ Thus,
$$ a - [A]  =  K[A](b - a + [A])$$
which we can rewrite
$$ K[A]^2 + (1 + K(b-a))[A] - a = 0$$

Besides, from the definition of $a$, we have $[AB] = a - [A] - [ABC]$ and so
$$ [ABC] = K[A][AB] = K[A](a - [A] - [ABC])$$
which yield $$ [ABC] = (K[A])/(1+K[A]) * (a - [A])$$

Note that we could have expressed it as a function of [AB] too:
$$ [ABC] = (K[AB])/(1+K[AB]) * (a - [AB]) $$

## Approximations
### At the end
If $b >> a$, and so $1 + K(b-a) ~= Kb$. Then, $$[A] ~= 1/Ka/b$$
and so $$[ABC] ~= a^2/b$$

### On the beginning
We have $b ~= 0$. There are two cases to consider:

+ The binding reactions are fast, in which case $1 + K(b-a) ~= -Ka$
+ The binding reactions are slow, in which case $1 + K(b-a) ~= 1$