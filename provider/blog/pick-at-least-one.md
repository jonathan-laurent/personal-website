---
title: A nice urn problem
tags: probability, math, kappa
date: 2017-06-02
---

While implementing counterfactual execution of Kappa traces,
I came upon the following urn problem:

> Let's consider $n$ urns, each of them containing both white and 
black balls. What is an efficient way to pick uniformly one ball in every urn
conditionned to the fact that one ball at least has to be black ?

One obvious algorithm would be to make a series of draws and to 
reject everyone but the first one featuring at least one black ball 
([rejection sampling](https://en.wikipedia.org/wiki/Rejection_sampling)).
However, this can get very inefficient when black balls are rare in
every box, which is typically the case in the application I had in mind.

## Formal statement

The problem can be reduced to a simpler form. Indeed, 
instead of directly picking one ball per urn, one can draw a
vector $x$ of size $n$ whose coordinates are either $0$ for _white_ and
$1$ for _black_ and then pick either a white or black ball uniformly 
in each urn depending on the corresponding coordinate of $x$.
If we write $p_i$ the ratio of black balls in urn $i$, then the
probability that vector $x$ is drawn is equal to $F(x)$ where:

$$ F(x) = prod_i p_i^(x_i)(1-p_i)^((1-x_i)) $$

Then, the probability of drawing $x$ conditioned to the fact that
$x$ contains at least one $1$ coordinate is equal to $0$ if $x = 0$
and $$ (F(x))/(1 - prod_i (1-p_i))$$ otherwise.

## Solution

### Implementation in OCaml

```ocaml
(*  This function solves the following problem:
    Given `n` boxes containing white and black balls, pick one ball
    from each box conditionned to the fact that at least one ball has 
    to be black.
    Input: [prob] is an array of size `n` whose ith element
    is the probability to draw a black ball from the ith box.Agent
    Returns: an array of `n` booleans, [true] representing a black ball
    and [false] a white ball. *)

let draw_at_least_one probs = 
    let draw p =
        let x = Random.float 1.0 in
        int_of_float (ceil (log1p (-. x) /. log1p (-. p))) in
    let array_min t = 
        Array.fold_left min t.(0) t in
    let r = Array.map draw probs in
    let m = array_min r in
    Array.map (fun e -> e = m) r
```