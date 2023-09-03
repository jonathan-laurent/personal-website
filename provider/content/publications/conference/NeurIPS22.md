---
title: "Learning to Find Proofs and Theorems by Learning to Refine Search Strategies: The Case of Loop Invariant Synthesis"
authors: Jonathan Laurent, André Platzer
venue: NeurIPS 2022
file: /pdf/papers/neurips22.pdf
date: 2022-10-15
---

We propose a new approach to automated theorem proving where an AlphaZero- style agent is self-training to refine a generic high-level expert strategy expressed as a nondeterministic program. An analogous teacher agent is self-training to generate tasks of suitable relevance and difficulty for the learner. This allows leveraging minimal amounts of domain knowledge to tackle problems for which training data is unavailable or hard to synthesize. As a specific illustration, we consider loop invariant synthesis for imperative programs and use neural networks to refine both the teacher and solver strategies.