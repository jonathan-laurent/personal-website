---
title: A Trace Query Language for Kappa
authors: Jonathan Laurent, Hector Medina Abarca, Pierre Boutillier, Jean Yang, Walter Fontana
venue: CMSB 2018
file: /pdf/papers/cmsb18.pdf
date: 2018-05-16
---

In this paper, we introduce a unified approach for querying simulation traces of rule-based models about the statistical behavior of individual agents. In our approach, a query consists in a trace pattern along with an expression that depends on the variables captured by this pattern. On a given trace, it evaluates to the multiset of all values of the expression for every possible matching of the pattern. We illustrate our proposed query language on a simple example, and then discuss its semantics and implementation for the Kappa language. Finally, we provide a detailed use case where we analyze the dynamics of beta-catenin degradation in Wnt signaling from an agent-centric perspective.