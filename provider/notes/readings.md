---
title: Readings
tags: research
math: kappa
date: 2017-02-09
---

## On CLF

+ [FP Lecture notes](http://www.cs.cmu.edu/~fp/courses/15816-f16/lectures/23-clf.pdf)
+ [System Description: Celf – A Logical Framework
for Deductive and Concurrent Systems](https://www.itu.dk/~carsten/papers/ijcar08.pdf)
+ [A concurrent logical framework I:
Judgments and properties](https://www.cs.cmu.edu/~fp/papers/CMU-CS-02-101.pdf)
+ [A Concurrent Logical Framework II:
Examples and Applications](https://www.cs.cmu.edu/~fp/papers/CMU-CS-02-102.pdf)
+ [A Concurrent Logical Framework: The
Propositional Fragment](http://repository.cmu.edu/cgi/viewcontent.cgi?article=1022&context=compsci)


## Halpern's causality

All Halpern's publications are available [here](https://www.cs.cornell.edu/home/halpern/topics.html#counterfactual).

+ [Causes and Explanations: A Structural-Model Approach -- Part I: Causes](http://ftp.cs.ucla.edu/pub/stat_ser/R266-part1.pdf)
+ [Causes and Explanations: A Structural-Model Approach -- Part II: Explanations](http://ftp.cs.ucla.edu/pub/stat_ser/R266-part2.pdf)
+ [Actual causation and the art of modeling](https://www.cs.cornell.edu/home/halpern/papers/festschrift.pdf)
+ [What Causes a System to Satisfy a Specification?](https://www.cs.cornell.edu/home/halpern/papers/resp.pdf)
+ [From causal models to counterfactual structures](https://www.cs.cornell.edu/home/halpern/papers/lewiscausal.pdf)
+ [Cause, Responsibility, and Blame:A Structural-Model Approach](https://www.cs.cornell.edu/home/halpern/papers/causalitylaw.pdf)
+ [A modification of the Halpern-Pearl definition of causality](https://www.cs.cornell.edu/home/halpern/papers/modified-HPdef.pdf)


Hanna's suggestions:

+ [Program Actions as Actual Causes: A Building Block for Accountability](https://arxiv.org/pdf/1505.01131.pdf)
+ [A Complement to Blame -- Wadler](http://homepages.inf.ed.ac.uk/wadler/papers/complement/complement.pdf)
+ [Well Typed Programs Can't Be Blamed](https://www.eecs.northwestern.edu/~robby/pubs/papers/esop2009-wf.pdf)

Work on causality checking:

+ [From Probabilistic Counterexamples via Causality to Fault Trees
](http://www.uni-konstanz.de/soft/research/publications/pdf/soft-11-02.pdf)
+ [Causality checking for complex system models](http://www.uni-konstanz.de/soft/research/publications/pdf/soft-12-02.pdf)
+ [Symbolic Causality Checking Using Bounded Model Checking](http://spinroot.com/spin/symposia/ws15/SPIN_2015_submission_5.pdf)
+ [Explaining Counterexamples Using Causality](https://www.research.ibm.com/haifa/projects/verification/RB_Homepage/ps/red_dots.pdf)


Other references:

+ [Interventionist counterfactuals (Briggs, 2012)](https://www.jstor.org/stable/pdf/23262477.pdf)
+ [A Lewisian Logic of Causal Counterfactuals](https://link.springer.com/article/10.1007/s11023-011-9261-z)
+ [An axiomatic characterization of counterfactuals (Pearl, 1998)](http://ftp.cs.ucla.edu/pub/stat_ser/R250.pdf)


## Misc
+ [Structural Simplification of Chemical Reaction Networks Preserving Deterministic Semantics:](https://hal.archives-ouvertes.fr/hal-01168038/document)
I read this paper because I was curious whether or not it was possible
to automate model simplifications similar to the Michaelis-Menten 
approximation while still keeping provable guarantees on the resulting model.
The technique discussed in the paper is quite trivial and they provide
no way to estimate how legitimate an approximation is.
+ [Thin slicing:](http://manu.sridharan.net/files/pldi07.pdf) Stephen Chong
recommended this paper. The idea is to modify traditional slicing algorithms
by
    * Relaxing the requirement that a slice should be self-contained
    * Distinguishing between two kind of dependencies 
    (*producer* and *explainer*) 
    and only tracking the first kind
+ [Symbolic Causality Checking Using Bounded Model Checking:](http://spinroot.com/spin/symposia/ws15/SPIN_2015_submission_5.pdf) proposed by Matt.
+ [Avoiding Causal Dependencies via Proof Irrelevance in a Concurrent Logical Framework:](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.141.5860&rep=rep1&type=pdf) to read before meeting Frank 
([detailed version](https://www.cs.cmu.edu/~fp/papers/CMU-CS-07-107.pdf)).
It seems to solve an example I had where a kinase phosphorylates two sites
one after the other.

## To read

+ Partial order reduction:
    * [Clarke survey (1998)](https://www.cs.cmu.edu/~emc/15-820A/reading/partial-order.pdf)
    * [Ten years of POR:](http://link.springer.com/chapter/10.1007/BFb0028727)
    well written survey
    * [Slides from a CMU class:](http://www.cs.cmu.edu/~emc/15817-f09/partialorder.pdf) happens to be a quite good reference
    * [Godefroy thesis](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.56.8794&rep=rep1&type=pdf)

+ Kinetics matter:
    * [Influence Systems vs Reaction Systems](https://hal.archives-ouvertes.fr/hal-01378470/file/iFRS16cmsb.pdf)
    * [Knockout Prediction for Reaction Networks
with Partial Kinetic Information](http://www.lifl.fr/BioComputing/extendedPapers/vmcai13.pdf)
    * [Predicting Changes of Reaction Networks with Partial Kinetic Information:](https://hal.inria.fr/hal-01239198v2/document)
    * [Efficient reduction of Kappa models by static inspection of
the rule-set](https://arxiv.org/pdf/1501.00440.pdf)


## May be useful

+ [A stronger necessary condition for the multistationarity of chemical
reaction networks](https://hal.inria.fr/hal-00772438v5/document)
+ [A survey of methods for deciding whether a reaction network is
multistationary](https://arxiv.org/pdf/1412.5257.pdf)
+ [Differential Slicing: Identifying Causal Execution Differences for
Security Applications](http://bitblaze.cs.berkeley.edu/papers/diffslicing_oakland11.pdf)
+ [Dynamic program slicing](http://www.cse.buffalo.edu/LRG/CSE705/Papers/Agrawal-Dynamic-Slice.pdf). 
A very simple paper on how to slice a program dynamically. Probably
has some historical significance.
+ [Causal Analysis](http://www.dcs.gla.ac.uk/~johnson/book/parts/chap10.pdf)
. Lecture notes about causal analysis.
+ [Standford philosophy encyclopedia: counterfactual causation](https://plato.stanford.edu/entries/causation-counterfactual/)