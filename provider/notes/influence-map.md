---
title: Influence map and causal independence
tags: kappa, chemistry
math: kappa
date: 2017-02-01
---

## A simplified model

Let's consider a finite set of species $mc V$.
A rule is given by a triple $(p, delta, k)$ where
$p : mc V -> NN$, $delta : mc V -> ZZ$ and $k in RR^+$. 
Such a triple correspond to the chemical equation:
$$ sum_(A in mc V) p(A) * A  ->  sum_(A in mc V) (p(A) + delta(A)) * A     (k)$$
Let $S = (p_i, delta_i, k_i)_i$ a system of rules. Then, 
$S$ yields the following system of differential equations:
$$(d[A])/(dt) = sum_i  k_i delta_i(A) prod_(B in mc V) [B]^(p_i(B))$$

## Definition of the influence map

The *rules influence map* is a graph whose nodes are rules. Besides, 
there is an arrow from a rule $r_i$ to a rule $r_j$ if and only if 
$delta_i(A) != 0$ and $p_j(A) != 0$ for some species $A in mc V$.

The *species influence map* is a graph whose nodes are species. Besides,
there is an arrow from $A$ to $B$ if and only if there exists a rule $r_i$
such that $p_i(A) != 0$ and $delta_i(A) != 0$.

Finally, we say that a species $A$ affects a species $B$ in the state $I$
if $$exists t,   [B]_(I uu {A})(t)  !=  [B]_I(t)$$
Then, the following theorem holds:

### Theorem

+ If there is no path between $A$ and $B$ in the *species influence map*,
then $A$ does not affect $B$.
+ If there is no path between the rule $eps -> A$ and $B -> B$ in the
*rules* influence map, then $A$ does not affect $B$.

*Proof.*
Here is a proof.

## Discussion

There is a duality between rules and species.

## Stochastic semantics

### Definition

A state of the system is given by a map $s : mc V -> NN$. Let's write $mc S$
the set of every state. To make things easier, let's assume first that 
$mc S$ is a finite subset of $NN^(mc V)$.
We have a transition $trans s (r_i) (s')$ when:

+ $p_i <= s$
+ $s' = s + delta_i$

Then, we define the transition weight between states $s$ and $s'$ as 
$$w(s, s') := sum_(trans s (r_i) (s')) k_i prod_(A in mc V) (p_i(A)) choose (s(A))$$
for $s != s'$. The total activity of a state $s$ is defined as $a(s) := sum_(s' != s) w(s, s')$.
Finally, we set $w(s, s) := -a(s)$ for all $s$. Then, the stochastic semantics of the system is 
given by the master equation $$ (d pi(t))/(dt) = pi(t) W$$
where $W$ is the matrix representation of $w$ and $pi(t)$ is a vector of size $abs (mc S)$ 
giving the marginal probability of every possible state.

### Proof of the main theorem

We have to redefine what it means for a species to affect another one in the 
context of the stochastic semantics: we say that $A$ affects $B$ in initial state I
if $$forall c,  exists t,   PP(s(t)(B) > c |  I)  !=  PP(s(t)(B) > c | I uu {A})$$

### Proof

The *Kolmogorov forward equations give* 
$$ (d pi(s))/(dt) = -a(s) pi(s) + sum_(i : s - delta_i in mc S) pi(s - delta_i) lambda_i(s-delta_i)$$

Let's consider a subset of species $X sube mc V$ and $Y = mc V - X$. We write $pi {s(X) = x} = sum_(s  :  s(X) = x) pi(s)$.
Then, we have $$ (d pi {s(X) = x})/(dt) = - sum_(s(X) = x) a(s) pi(s) + sum_(s(X) = x) sum_(i  :  s - delta_i in mc S) pi(s - delta_i) lambda_i(s-delta_i)$$
Let's simplify this expression:
```align
& sum_(s(X) = x) sum_(i  :  s - delta_i in mc S) pi(s - delta_i) lambda_i(s-delta_i) \\ \\
&= sum_s pi(s) [sum_(i  :  s(X) = x - delta_i(X)) lambda_i(s)] \\ \\
&= sum_(s(X) != x) pi(s) [sum_(i  :  s(X) = x - delta_i(X)) lambda_i(s)]  +  sum_(s(X) = x) pi(s) [sum_(i  :  delta_i(X) = 0) lambda_i(s)]  \\
```

Let's look at the term $sum_(i  :  s(X) = x - delta_i(X)) lambda_i(s)$. If $s(X) != x$, then
$s(X) = x - delta_i(X)$ implies $delta_i(X) != 0$. Thus, every rule whose activity is summed
affects one of the species of $X$. Therefore, it is a function of $s(D)$: let's write it $F(s(D))$.
Similarly, we have 
$$sum_(i  :  delta_i(X) = 0) lambda_i(s)  =  a(s)  - sum_(i  :  delta_i(X) != 0) lambda_i(s)  =  a(s) - G(s(D))$$
for some function $G$. Thus,
```align
(d pi {s(X) = x})/(dt)  &=  sum_(s(X) != x) pi(s) F(s(D))  - sum_(s(X) = x) pi(s) G(s(D)) \\ \\
&= sum_d pi {s(D) = d ^^ s(X) != x} * F(d)  -  sum_d pi {s(D) = d ^^ s(X) = x} * G(d)
```

Intuitively, $F$ represents the total activity of the rules that, when applied in the current state, would
bring $X$ to value $x$. Besides, $G$ represents the total activity of the rules that modify $X$.

In the particular case where $X = D$ (if $X$ is closed for dependencies), then
$$ (d pi {s(X) = x})/(dt) = sum_(x_0 != x) pi {s(X) = x_0} * F_x(x_0)  -  pi(x)*G(x) $$
where
$$ F_(x_0)(x') = sum_(i  :  x_0 + delta_i = x) lambda_i(x_0)      
G(x) = sum_(i  :  delta_i(X) != 0) lambda_i(x) $$

## A finer result

Let's consider a differential system $$(dx_i)/(dt) = F_(i)(x(t))$$ 
and suppose that for any $x$ and any $i$, 
$v |-> F_i(x[j:=v])$ is nondecreasing for all $j != i$. Then, if $x$ and
$y$ are two functions respecting this ODE such that $x(0) <= y(0)$,
then $forall t>=0, \  x(t) <= y(t)$.

*Proof.*  let's write $delta = y - x$ and suppose that there exists some 
$t > 0$ such that $neg (delta(t) >= 0)$. Then, let's consider 
$t_0 = min_i [inf {t > 0 : delta_i(t) < 0}]$. As $delta$ is continuous, we have $delta(t_0) >= 0$ and $delta_i(t_0) = 0$ for some $i$. Besides,
$delta_i$ takes negative values in every neihborhood of $t_0$.
However, $$delta_i'(t_0) = F_i(y(t_0)) - F_i(x(t_0))$$
As $y_i(t_0) = x_i(t_0)$ and $F_i$ is increasing on every other
variable, we have $delta_i'(t_0) >= 0$ and so $delta_i$ is nonnegative on a 
neigborhood of $t_0$, which is a contradiction.


## An even finer result

Let's consider the following differential system, where variables are
split in three disjoint sets $P$, $N$ and $U$:
$$ (dp_i)/(dt) = F_i(p(t);n(t);u(t))      (dn_i)/(dt) = G_i(p(t);n(t);u(t))    (du_i)/(dt) = H_i(u(t)) $$
Besides, we make the following hypotheses:

+ The function $v |-> F_i(p[j:=v];n;u)$ is nondecreasing for all $i, j != i$
+ The function $v |-> F_i(p;n[j:=v];u)$ is nonincreasing for all $i,j$
+ The function $v |-> G_i(p[j:=v];n;u)$ is nonincreasing for all $i, j$
+ The function $v |-> G_i(p;n[j:=v];u)$ is nondecreasing for all $i,j != i$

Let $x = (p, n, u)$ and $hat x = (hat p, hat n, hat u)$ two solutions of the system above and a time-predicate $Pi_x^(x')$ defined as follows:

$$Pi_x^(x')(t) := hat p(t) >= p(t)  ^^  hat n(t) <= n(t)  ^^  hat u(t) = u(t)$$
Then, $Pi_x^(x')(0)$ implies $Pi_x^(x')(t)$ for all $t >= 0$.


### Back to Kappa
We can make the following statements

+ If there is no odd loop-free path between $A$ and $B$ in the influence 
map, then $A$ does not inhibit $B$.
+ If there is no even loop-free path between $A$ and $B$ in the influence 
map, then $A$ does not activate $B$.

Actually, the contraposed statement may be clearer:

+ If $A$ activates $B$, then there is an even loop-free path between 
$A$ and $B$ in the influence map.
+ The same hold for inhibition and odd paths.

*Remark:* by *loop*, we did not mean *cycle*. Cycles are allowed in paths.

*Proof.* Let's suppose there is no odd loop-free path between $A$ and $B$.
Let $D$ the set of variables $B$ depends on. We write $D = X |_| U$ where
$X$ is the set of variables that are influenced by $A$. Then, we define
the following subsets of $X$:
```align
P &= { C  : "there is an even path from " A " to " C } \\ \\
N &= { C  : "there is an odd path from " A " to " C }
```
We can prove using the hypothesis that $X = P |_| N$. Then, we can
check that there is a system of differential equations describing 
the evolution of $B$ for which the premises of the previous theorem hold.


## Loops and stochastic semantics

The theorem above is wrong for stochastic semantics. Indeed, let's consider
a system with only one rule $$r  :  A, A -> eps$$
Then, if we start in a mixture with a single $A$, adding one $A$ will
create an instance of $r$ and make both $A$ disappear. Thus, $A$ may inhibit
itself, although there is no loop-free path between $A$ and $A$ in the 
influence map.


### Remarks

+ The results we get for the *rules influence map* are not as good.
Take the example of a reversible reaction $A <-> B$:
```{.graph width=300}
graph [rankdir=LR]
"=>A" -> "A=>B"
"A=>B" -> "B=>A"
"B=>A" -> "obs(B)" [arrowhead=tee]
```
+ Is there an example where we have to use a loop or a path between
a rule and the reverse rule to prove inhibition ? Yes: prozone.

+ Explanation for prozone: $B$ increases, so $A$ decreases. But $A$ has a
positive influence on $ABC$.
+ Structural simplification of chemical reaction networks preserving deterministic semantics

### A species inhibits itself

+ $A$ needs to bind to $B$ and $C$. If you put too much $A$, it is not good
+ When you increase $A$

