---
title: Influences, activation and inhibition
tags: meta
math: kappa
date: 2017-02-07
---

## Prerequisites

### Events and traces
Let $Q$ a set of states. In Kappa, $Q$ is usually the set of all possible mixtures.
An event $e$ is defined by:

+ A name $"label"(e)$
+ A precondition $"pre"(e) sube Q$, which is the set of every state from which 
$e$ can be triggered
+ An effect $"eff"(e) : "pre"(e) -> Q$, which describes how the event modifies
the state of the world

A *context* is defined as a set of states and a *trace* as a sequence of events. 
Besides, for any trace $t$, we write $c |- t$ if $t$ can be executed from any 
state of $c$. It is easy to define the $"pre"$ and $"eff"$ operators inductively
on traces.

### Concurrent events, equivalent traces
 Two events $e_1$ and $e_2$ are said to be *concurrent* in context
$c$ if the following conditions hold:

+ If $c |- e_1$ and $c |- e_2$, then $c |- e_1e_2$ and $c |- e_2e_1$
+ $e_1$ and $e_2$ commute in the sense that 
$"eff"(e_1) @ "eff"(e_2) = "eff"(e_2) @ "eff"(e_1)$

Two events are said to be *concurrent* if they are concurrent in any context. 
Finally, two traces $t$ and $t'$ are said to be *equivalent*, which we write 
$t sim t'$, if $t$ can be obtained from $t$ by permuting concurrent events.


## Definitions

### Immediate causality and inhibition
We define immediate causality and inhibition as follows:

+ We say that $e_1$ is an *immediate cause* of $e_2$ in context $c$
if $c |- e_1e_2$ but $c |/- e_2$.
+ We say that $e_1$ is an *immediate inhibitor* of $e_2$ if
there is a context $c$ such that $c |- e_1$, $c |- e_2$ and $c |/- e_1e_2$.

Then, an event is said to be an immediate cause (resp. an
immediate inhibitor) of another one if it is for some context.
Finally, if $t$ a trace, $e_1, e_2 in t$:

+ We say that $e_1$ is a cause of $e_2$ in $t$ if $t sim t_1 * e_1 * e_2 * t_2$
for some traces $t_1, t_2$ and $e_1$ is a cause of $e_2$ in context $"post"(t_1)$.
+ We define immediate inhibition relative to a trace the same way.

### Dependencies and anti-dependencies

As opposed to the previous notions that were defined in a very generic
context, the notions of dependencies and anti-dependencies are specific
to Kappa. Let $t$ a trace and $e_1, e_2$ two events of $t$, $e_1 < e_2$.

+ $e_1$ is said to be a *dependency* of $e_2$ in $t$ if there exists a site that is
tested by $e_2$ and lastly modified by $e_1$ before $e_2$.
+ $e_1$ is said to be an *anti-dependency*

### Influences

Influences are abstractions at the level of rules of the notions of immediate
causality, immediate inhibition, and contribution that are defined at the
level of events.

## Problem
An inhibition pattern has the form: event $e$ of story $s$ inhibits event $e'$
of story $s'$. This pattern appears in a trace if:

+ There is an instance of story $s$ in $t$.
+ The events that precede $e'$ in $s'$ appear before $e$ in $t$.
+ $e'$ could have happened instead of $e$.

An explanation is a sequence of stories with inhibition patterns.
We can check it dynamically.


We need a large definition of what a story is and what a matching is.