---
title: From Kappa to CLF
tags: research, clf, kappa
math: kappa
date: 2017-02-13
---

Let's consider the following Kappa model of processive double phosphorylation:

```kappa
%agent:   K(d)
%agent:   S(x~u~p, y~u~p, d)

'px'      K(d!1), S(d!1, x~u) -> K(d!1), S(d!1, x~p)
'py'      K(d!1), S(d!1, y~u) -> K(d!1), S(d!1, y~p)
'bind'    K(d), S(d) -> K(d!1), S(d!1)
'unbind'  K(d!1), S(d!1) -> K(d), S(d)
'eoi'     S(d, x~p, y~p) -> S(d, x~p, y~p)

%init:    1 K(), S()
```

We are looking for a story explaining $"eoi"$. This can be encoded into 
the following CELF program.

```
% Kappa types

site:       type.
state:      type.
agent:      type.
agent-type: type.

has-type:   agent -> agent-type -> type.
free-site:  agent -> site -> type.
site-state: agent -> site -> state -> type.
bond:       agent -> site -> agent -> site -> type.

bond-comm:  bond A X B Y -o { @bond B Y A X }.


% Signature declaration

k: agent-type.
s: agent-type.

x: site.
y: site.
d: site.

u: state.
p: state.


% Rules

bind: 
    has-type S s * has-type K k *
    free-site S d * free-site K d
    -o { @bond S d K d *
         @has-type S s * @has-type K k }.

unbind: 
    has-type S s * has-type K k *
    bond S d K d
    -o { @free-site S d * @free-site K d *
         @has-type S s * @has-type K k }.

px: 
    has-type S s * has-type K k * bond S d K d *
    site-state S x u
    -o { @site-state S x p *
         @has-type S s * @has-type K k * @bond S d K d}.
    
py: 
    has-type S s * has-type K k * bond S d K d *
    site-state S y u
    -o { @site-state S y p *
         @has-type S s * @has-type K k * @bond S d K d}.


% Looking for a story!

#trace 5 
    (Exists S:agent. Exists K:agent.
    @has-type S s * @has-type K k * @free-site S d * @free-site K d *
    @site-state S x u * @site-state S y u).

#query 10 * * 10
    (Exists S:agent. Exists K:agent.
    @has-type S s * @has-type K k * @free-site S d * @free-site K d *
    @site-state S x u * @site-state S y u)
    -o { Exists S:agent.
         has-type S s * free-site S d *
         site-state S x p * site-state S y p }.
```

An example solution is:

```
Solution: \[!X40, [!X41, [@X42, [@X43, [@X44, [@X45, [@X46, @X47]]]]]]]. {
    let {[@X48, [@X49, @X50]]} = bind [X42, [X43, [X44, X45]]] in
    let {[@X51, [@X52, [@X53, @X54]]]} = py [X49, [X50, [X48, X47]]] in
    let {@X55} = bond-comm X54 in
    let {@X56} = bond-comm X55 in
    let {[@X57, [@X58, [@X59, @X60]]]} = px [X52, [X53, [X56, X46]]] in
    let {[@X61, [@X62, [@X63, @X64]]]} = unbind [X58, [X59, X60]] in
    let {[@X65, [@X66, @X67]]} = bind [X63, [X64, [X61, X62]]] in
    let {[@X68, [@X69, [@X70, @X71]]]} = unbind [X66, [X67, X65]] in
    let {[@X72, [@X73, @X74]]} = bind [X70, [X71, [X68, X69]]] in
    let {[@X75, [@X76, [@X77, @X78]]]} = unbind [X73, [X74, X72]] in [!X40, [X77, [X75, [X57, X51]]]]}
```

A very interesting effect of linearity is that it is impossible to apply a
rule with twice the same agent as an argument, as specified in
Kappa's semantics. Indeed, it would be required
to give two instances of the predicates $"has-type"$ for this agent, which is 
impossible given well-formed initial conditions.