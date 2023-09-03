---
title: Counterfactuals in Kappa
tags: research
math: kappa
date: 2017-04-26
---

## Introduction

What does it mean for an event to be an actual cause of another one in a
given simulation trajectory ? A lot of attempts exist in the philosophy
and CS litterature to give a formal account of causation. Although none
of these proposals has managed to gather consensus, most of them rely
on _counterfactual_ statements, which are statements of the form:

> _Event $e_2$ would not have happend had $e_1$ not happened._

In his seminal work, Lewis gives a semantics to counterfactual
statements that is usually referred as the _closest-word semantics_.
In his view, $e_2$ counterfactually depends on $e_1$ if:

+ both of them happen in the actual world
+ $e_2$ does not happen in the closest possible world to the actual one
in which $e_1$ does not happen

In the particular case of Kappa, this heuristic raises the followng question:
given a trajectory and some event $e$ in it, what would be the closest
possible trajectory in which $e$ is blocked ? We make an attempt to
answer this question in this note.

## Counterfactual statements

### A new perspective on Gillespie's algorithm

In order to do so, we have to change our perspective
on how Kappa models are simulated slightly, 
by giving an alternate (and equivalent)
description of the Gillespie algorithm. Let's consider a Kappa model.
An _event kind_ is defined as a pair $(r, phi)$, where $r$ is a rule
and $phi$ a one-to-one mapping from local agents that appear in
the left hand side of $r$ and global agent identifiers. An _event_ 
is defined as an instance of a given event kind that triggers at a 
specific time in a simulation trajectory.

Then, simulation can be thought as follows. For every possible event
kind $e = (r, phi)$, there is an associated _bell_ that rings randomly and 
independently
of the state of the reactional mixture, according to a Poisson
process whose parameter is the rate of $r$. Every time this bell rings,
an instance of $e$ triggers in case its precondition is met in the
current mixture.

For example, there may be a bell labelled 
_"kinase 23 phosphorylates substrates 67"_. Every time this bell rings,
a phosphorylation event happens given that:

* kinase 23 and substrate 67 are indeed in the mixture
* they are bound to each other through the right sites
* the targeted site in the substrate is not already phosphorylated

This simulation process can be proven equivalent to the 
Gilespie algorithm.
It features an infinite number of bells, but 
this is not a problem as its only function is to be used in the
definition of counterfactual statements and not to be implemented 
directly in any way.

### Definition

The advantage of the previous reformulation of Gillespie's 
algorithm is that it factors out randomness. Indeed, if
we specify the ringing times of every bell in advance, then
simulation becomes completely deterministic. Indeed, given some
initial mixture, we can map every ringing history $omega$ to
a unique infinite trajectory $T(omega)$.

We can easily generalize our definition of $T$
to account for external interventions. We define an intervention
$iota$ by:

+ A predicate $"blocked"_iota[e, t]$ that specifies whether or not
an instance of event kind $e$ should be blocked at time $t$.
+ A set of events $"new"_iota$, an event being given by an event kind
and a time. It is possible to allow events featuring rules that do not
belong to the model.

Then, we can map a ringing history $omega$ along
with an intervention $iota$ to a trajectory
$T(omega ; iota)$ that can be computed from initial conditions
as follows:

+ When a bell for event kind $e$ rings at time $t$, we only attempt to
trigger $e$ if $"blocked"_iota[e, t]$ is false.
+ We additionally attempt to trigger events from the set $"new"_iota$
at the appropriate times.

We can now give a rigorous semantics to counterfactual statements as
follows: given a trajectory $tau$ and a predicate $psi$ on trajectories,
we say that intervention $iota$ would have resulted in $psi$ being true
with probability $p$, which we write $iota |=_p psi$ if:

$$ PP( psi[T(omega ; iota)] | T(omega) = tau ) = p $$

We write $iota |= psi$ if $iota |=_p psi$ for some $p != 0$. Note that
this definition is more general than what we initially aimed for.
If we are interested in knowing whether $e_2$ counterfactually depends
on $e_1$ in trajectory $tau$, we can check whether or not $iota |= psi$,
where:

+ $"blocked"_iota(e) = (e = e_1)$
+ $psi(tau) ==$ "event $e_2$ happens in $tau$"

That being said, this simple account may be problematic for reasons 
we will explore later.


### Evaluating counterfactuals

In order to estimate counterfactual statements trough sampling,
we need to find a _"resimulation"_ algorithm to draw 
_counterfactual trajectories_ from 
the distribution of $T(omega ; iota) | T(omega) = tau$ for
a given $tau$.

Let $tau$ a given simulation trajectory and $iota$ an intervention.
We want to _resimulate_ $tau$ modulo intervention $iota$. Before we
give the algorithm, we need the following definitions:

+ A simulation state is given by a pair $(M, t)$ where $M$ is the
current reactional mixture and $t$ is the current time.
We also define $M_tau(t)$ as the reactional mixture at time $t$ for trajectory
$tau$.
+ For a simulation state $(M, t)$ of the counterfactual
trajectory:
    *  We say that a logical site is _divergent_ if it has different
values in $M$ and in $M_tau(t)$.
    *  An embedding from the left hand side of 
$r$ into $M$ is said to be _divergent_ if it features a divergent
site.
    * The total activity $alpha_"d"$ of divergent embeddings is defined as
the sum for every rule $r$ of the reaction rate of $r$ multiplied by the number
of divergent embeddings of $r$ in the reactional mixture.

**Resimulation algorithm**

Start with $t = 0$ and the initial mixture. Then, loop as follows:

1. Draw $delta sim "Exp"(alpha_"d")$ where $alpha_"d"$ is the total activity
of divergent embeddings. Then, define the following times:
    + Let $t_"r"$ the time of the next event triggering in $tau$.
(after time $t$)
    + Let $t_"i"$ the time of the next event in $"new"_iota$.
    + Let $t_"d" = t + delta$.
2. Consider which one of ${t_"d", t_"r", t_"i"}$ is smallest:
    + If $t_"r"$ is smallest, then try to execute the next event in 
the reference trace $tau$.
If it fails (its preconditions are not met in the counterfactual mixture),
do nothing.
    + If $t_"i"$ is the smallest, then execute the corresponding events in
    $"new"_iota$ if possible.
    + If $t_"d"$ is the smallest, then draw a rule and a divergent embedding
    and apply the corresponding event.

3. Set $t = "min"{t_"d", t_"r", t_"i"}$ and go back to step 1.

**Theorem:** trajectories that are drawn using this algorithm for
a reference trajectory $tau$ and an intervention $iota$ effectively
follow the distribution $T(omega ; iota) | T(omega) = tau$.


## Discussion

### An example

Let's illustrate counterfactuals on a simple example.
Consider a model with the following rules:

```kappa
%agent: K(d, x~u~p)
%agent: S(d, x~u~p)

'pK' K(x~u) -> K(x~p) @ 'slow_mod_rate' 
'b'  K(d), S(d) -> K(d!1), S(d!1) @ 'on_rate'
'u'  K(d!1, x~u), S(d!1) -> K(d, x~u), S(d) @ 'off_rate_fast'
'u*' K(d!1, x~p), S(d!1) -> K(d, x~p), S(d) @ 'off_rate_slow'
'p'  K(d!1), S(d!1, x~u) -> K(d!1), S(d!1, x~p) @ 'mod_rate'

%init: 10 K(x~u), S(x~u)
```


Let's suppose we simulate such a model and get the following trajectory, 
the event of interest being the phosphorylation of some substrate 
(rule embeddings are implicit):

```
init, b, u, pK, b, p, u*
```

Here, there is a unique story (or causal-core)
to explain `p`, namely:

```graph
graph [rankdir="LR"]
init -> b -> p
```

However, this story misses an intuitive cause of `p` which is `pK`.
Indeed, `pK` plays an important role in making `p` happen because
it makes the bond between the substrate and kinase involved strong enough
for the phosphorylation to take place before unbinding occurs.
Indeed, in the
extreme case where `'off_rate_fast'` is equal to $oo$, the trajectory
`init, b, p` has a zero likelihood of happening.


In contrast to this, `pK` can be easily identified as a cause of `p`
using counterfactual reasoning. Indeed, if we resimulate our trajectory
to the time the event of interest had triggered and
blocking `pK`, we are likely to find
`init, b, u, b, u`, which does not feature `p` anymore. 
Note that here, the last `u` event is proper to the counterfactual trajectory.

**Remark.** To be totally honest here, blocking a single instance of `pK`
may not be enough to block `p` with high probability as the same kinase 
may still get phosphorylated
with a small delay before the binding event occurs. 
A possible fix would be to block every phosphorylation of the kinase involved
in the original trajectory from the time `pK` happened to the time `p` happened.
This small example illustrates how defining causality from
counterfactual statements can be tricky.

### Extending stories with inhibition arrows

In our example, counterfactual reasoning enabled us
to highlight `pK` as a cause of the event of interest. However, it did
not make it obvious **why** it is so. Stories could provide such explanations
if augmented with inibition arrows. In our example, we would get the following
_augmented story_:


```graph
u [ style=dotted]
init -> b -> p
init -> pK
b -> u
pK -> u [arrowhead=tee, color=red]
u -> p [arrowhead=tee, color=red]
```


Here, dotted nodes correspond to events that did **not** happen.
In the next section, we give a rigorous meaning to diagrams like
the one above by introducing the notion of a counterfactual experiment.

## Counterfactual experiments

### Definition

We define a counterfactual experiment as a triple 
$(T(omega), iota, \ T(omega ; iota))$
for some ringing history $omega$ and intervention $iota$.
Given such a triple $(t, iota, t')$, we can distinguish
between three kinds of events:

+ events that are common to $t$ and $t'$
+ events that are proper to $t$ (we call them *factual* events)
+ events that are proper to $t'$ (we call them *counterfactual* events)

Then, we can define direct causal influences and inhibitions.

+ Within $t$ or $t'$, an event $e_1$ is said to have a _direct
causal influence_ on $e_2$ if there exists a logical site $s$ such
that $e_2$ tests $s$ and $e_1$ is the last event before $e_2$ that 
modifies $s$.
+ An event $e_1$ that is proper to $t$ is said to have a _direct
inhibitory influence_ on an event $e_2$ that is proper to $t'$ if
    * $e_2$ tests some logical site $s$ to some value $v$
    * $e_1$ is the last event in $t$ that modifies $s$ to a value different than $v$
    * there is no event in $t'$ between $e_1$ and $e_2$ that modifies $s$
+ The definition above also holds when the roles of $t$ and $t'$ are reversed

For conciseness, a direct causal influence between two events is also called
a _positive arrow_. Similarly, a direct inhibitory influence is also called
_negative arrow_. We call _path_ a sequence of positive and negative arrows 
connecting two events.

### Example

Let's consider the following experiment:

+ Reference trajectory: `init, pK, b, p`
+ Intervention: blocking `pK`
+ Counterfactual trajectory: `init, b, u`

It can be represented as follows:

```graph
u [ style=dotted]
pK [ penwidth=1.5  ]
p [ penwidth=1.5 ]

init -> b -> p
init -> pK
b -> u
pK -> u [arrowhead=tee, color=red]
u -> p [arrowhead=tee, color=red]
```

Dotted nodes refer to events that are proper to the counterfactual trajectory,
thick nodes refer to events that are proper to the reference trajectory
and normal nodes represent events that are common to both. As we can see,
direct inhibitory influences are always directed from dotted nodes to
thick nodes or the other way around. 

In this example, we can see that the intervention of blocking `pK` is 
related to the non-triggering of `p` by a path consisting in two negative 
arrows. Such an _explanatory_ always exists, as stated in the following 
theorem.

### Theorem

The following result connects counterfactual reasoning 
with the preexisting notions of inhibition and direct causal influence.

**Completeness theorem**

Consider a trajectory $tau$, $e$ an event in $tau$ and $iota$ an intervention.
If $iota$ results in $e$ not happening with nonzero probability, 
then there exists a counterfactual experiment ($tau$, $iota$, $tau'$) 
such that:

+ $e$ does not appear in the counterfactual trajectory $tau'$
+ one of the following is true:
    * there is a path between an event blocked by $iota$ and $e$
    with an even number of negative arrows
    * there is a path between an event of $"new"_iota$ and $e$ with an
    odd number of negative arrows


**Corollary**
If an instance of rule $s$ counterfactually depends on an instance of rule
$r$, then there is a path between $r$ and $s$ in the influence map
with an even number of negative arrows.