---
title: Deriving Leibniz's rule from Newton's identity
tags: math
description: A funny mathematical finding.
use: asciimath
date: 2016-01-09
---

## Introduction

The Leibiniz's rule provides a way to compute the $n$-th derivative of the product of two functions. 
For $f$ and $g$ two infinitly differentiable functions and $n$ an integer, it states that: 
$$ (fg)^((n)) = sum_(k=0)^n k choose n f^((k))g^((n-k)) $$
Such a formula can only remind Newton's identity, which says that:
$$ (a+b)^n = sum_(k=0)^n k choose n a^kb^(n-k) $$
for $a$ and $b$ two elements of any ring $(A, +, *)$ such that $ab=ba$. 
Surprisingly, though, deriving the former from the latter is quite tricky. A solution to this problem is proposed in this post.

## A space of formal sums
The trick here is to consider _differentiation_ as a purely algebraic operator that operates on expressions of the form: 
$$ sum_(i,j) phi(i,j) f^((i))g^((j)) $$
where $phi : NN^2 -> RR$ is a function with finite support. 
We write $[phi]_(f,g)$ the quantity above. Then, we look for an operator $Del$ on $E = {phi : NN^2 -> RR | phi " has finite support"}$ such that:
$$ ([phi]_(f, g))' = [Del phi]_(f, g) $$

In order to describe such a $Del$, let's consider the basis $(e_(i,j))$ of $E$ where $e_(i,j)$ maps $(i,j)$ to $1$ and any other couple to $0$. 
It is easy to figure out that: $$ Del (e_(i,j)) = e_(i+1,j) + e_(i,j+1)$$

## Using Newton's identity
As a consequence, $Del = Del_1 + Del_2$ where $Del_1$ and $Del_2$ are defined by $Del_1(e_(i,j)) = e_(i+1,j)$ and $Del_2(e_(i,j)) = e_(i,j+1)$. 
Besides, it is easy to see that $Del_1@Del_2=Del_2@Del_1$. As a consequence, by using Newton's identity in the ring of $E$'s endomorphisms:
$$(Del_1 + Del_2)^n = sum_(k=0)^n k choose n Del_1^k @ Del_2^(n-k)$$
and so $$(Del_1 + Del_2)^n(e_(0,0)) = sum_(k=0)^n k choose n e_(k, n-k).$$
Finally: $$(f+g)^((n)) = [Del^n e_(0,0)]_(f,g) = [(Del_1+Del_2)^n e_(0, 0)]_(f, g) = sum_(k=0)^n k choose n f^((k))g^((n-k))$$
which is exactly what we wanted to show.