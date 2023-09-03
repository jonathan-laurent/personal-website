---
title: A Trace Query Language for Kappa
tags: kappa
math: kappa
date: 2017-04-20
---

## Installation

In order to build the trace query engine, 
you first need to install the development version
of KaSim through opam. For this, just write
`opam pin --dev add KaSim` and then `opam upgrade && opam update`. Then,
download the source code from the github repo:

```
git clone https://github.com/jonathan-laurent/Kappa-Trace-Query-Engine.git
```
Finally, type `make` in this directory and add it into your `$PATH` variable.
The query engine can be invoked using the syntax:

```
query -t <your JSON trace file> -q <your query file>
```


## A small tutorial


Most examples in this tutorial rely on a model featuring
the following rules:

```kappa
# Agents
%agent: K(d, x{u,p})
%agent: S(d, x{u,p})

'b'  K(d[./1]), S(d[./1]) @ 'on_rate'
'u'  K(d[1/.], x{u}), S(d[1/.]) @ 'off_rate_fast'
'u*' K(d[1/.], x{p}), S(d[1/.]) @ 'off_rate_slow'
'p'  K(d[1]), S(d[1], x{u/p}) @ 'phos_rate'
```

### First example

Let's consider the following query, which prints the current time
every time a substrate binds to some kinase:

```katql
match e:{ S(d[/d.K]) } return time[e]
```

Some remarks:

+ The keyword `match` introduces a pattern and the keyword `return`
introduces an action which is executed for every instance of the
pattern in the trace.
+ Here, the pattern consists in a single event named `e` that obeys
the constraint `{ S(d[/d.K]) }`, meaning _event $e$ binds a substrate
to a kinase_. Any mixture modification can be specified between
curly brackets (`{ ... }`), the syntax being the same KaSim
uses for specifying a rule by a left-hand size along with a modification.
For example:
    * `S(x{/p})`: a substrate just got phosphorylated at $x$
    * `S(x{u/p})`: a substrate's $x$ site just went from state `u` to `p`
    * `S(x{u/p}, d[1]), K(d[1], x{p})`: idem, with the aditionnal constraint
    that the substrate had to be bound to a phosphorylated kinase when
    the binding occured
    * `S(d[/d.K])` is equivalent to `S(s[/1]), K(d[/1])` in our example
+ `time[e]` is an expression giving the time at which event `e` (defined in
the pattern) occured. When only an expression is given between
curly brackets after the `return` keyword, it refers to the action of priting
the value of this expression in a new line of the output file generated
by the query engine (`output.csv` by default).

In order to run this query against the trace `trace.json`, it should be 
saved in a file (`bind.query` for example). Then, run:

```
query -t trace.json -q bind.query
```

This generates a file `output.csv` with one floating
point number per line. 

If we also want to display the name of the associated rule every
time a binding event occur, we can augment our query as follows:
`match e:{ S(d[/d.K]) } return (time[e], rule[e])`.
This would produce a CSV file with two columns. It is possible
to specify a custom output file for a query, along with
column headers for the resulting CSV file, in which case our query becomes:

```katql
query 'bind.csv' {'binding-time', 'binding-rule'}
match e:{ S(/d[d.K]) } return (time[e], rule[e])
```

### An other simple example

Let's say we want to estimate how likely it is when a phosphorylation 
event happens
that the involved kinase is phosphorylated itself. For this, we can use
the following query:

```katql
match p:{ S(x{/p}, d[1]), k:K(d[1]) }
return int_state[.p]{k.x}
```

As opposed to `time` and `rule` that are _event measure_
(measures associated to a particular event), `int_state` is 
a _state measure_ (a measure associated to a particular mixture).
As a first argument and between square brackets, it takes a
_state expression_: for `e` an event identifier, `.e` refers to the
mixture **before** the triggering of `e` and `e.` refers to the
mixture **after** the triggering of `e`.

### Average lifespan of a bond

Let's suppose we want to estimate the average lifespan of a bond
between a kinase and a substrate. This can be returnne using the following query:

```katql
match b:{ s:S(d[/1]), K(d[/1]) }
and first u:{ s:S(d[/.]) } after b
return (time[u] - time[b])
```

Here, the clause `first u:{ s:S(d[/.]) } after b` defines `u` as the first event
after `b` matching the event pattern `{ s:S(d[/.]) }`. 

Now, let's say we want to compute the average lifespan of a bond between
a kinase and a substrate conditionned to the fact that the kinase is 
phosphorylated before the bond breaks. A tempting but **wrong** way to
answer this question would be using the following query:

```katql
match b:{ (d[/1]), k:K(d[/1]) }
and first u:{ k:K(d[/.], x{p}) } after b
return (time[u] - time[b])
```

Instead, one of the following queries should be used:

```katql
match b:{ S(d[/1]), k:K(d[/1]) }
and first u:{ k:K(d[/.]) } after b
and u:{ k:K(x{p}) }
return (time[u] - time[b])
```

```katql
match b:{ S(d[/1]), k:K(d[/1]) }
and first u:{ k:K(d[/.]) } after b
when int_state[.u]{k.x} = 'p'
return (time[u] - time[b])
```

```katql
match u:{ k:K(x{p}, d[/.]) }
and last b:[ k:K(d[/d.S]) ] before u
return (time[u] - time[b])
```


## Semantics of queries

The basic principle of the trace query language can be defined
as follows. A _query_ is given by:

+ A _trace pattern_, that is a predicate on traces
 $P[vec e, vec a]$ that is parametered by both event and
agent identifiers.
+ An _action_ $alpha[vec e, vec a]$ that admits the same parameters.

Then, the semantics of a query $(P, alpha)$ on a trace $t$ is basically
to execute $alpha[vec e, vec a]$ for every possible choice of $vec e$
and $vec a$ that makes $P[vec e, vec a]$ true in $t$.

## Reference

### Syntax of a query

A query is given by the following syntax, where square brackets denote 
optional arguments and angular brackets refer to syntactic elements whose
grammar will be given later.

```katql
[ query [ 'query-name' ] { 'col-header-1', 'col-header-2', ... } ]
match <trace-pattern>
[ every <num> seconds ]
[ when <expr> ]
return { <expr> }
```


Some remarks:

+ The first line is an optional **header** beginning with the keyword `query`.
It is optionally followed by the name of the query, which is also the name
of the output file produced. If no name is given, `output.csv` is selected
by default (this behavior can be overwritten using the `-o` option of the query engine). Finally, labels for each column in the resulting csv file can be
specified between curly brackets.

+ `<trace-pattern>` refers to a trace pattern, as defined in the next section of this document.

+ After the trace pattern, it is possible to specify an optional 
`[every <num> seconds]` clause, where `<num>` is any positive numeric literal.
Roughly speaking, it limits the query engine to match the specified pattern
at most once every `<num>` seconds of simulation.

+ An additional `when <expr>` clause can be provided to add an aditional
constraint to the specified trace-pattern as a boolean expression. 

+ There can be several queries in a single file, separated by new lines.

### Syntax of trace patterns

A _trace pattern_ is given by a list of _clauses_ separated by the `and`
keyword. Clauses can have three forms:

+ `<event_pattern>`
+ `last <event_pattern> before e` where `e` is an event id
+ `first <event_pattern> after e` where `e` is an event id

In turn, an _event pattern_ can be given by one of the following syntaxes:

+ `e:{ ... }` where `e` is an event id and `...` refers to a Kappa delta expression
+ `e:{ ... } with <expr>` where `<expr>` is an aditional constraint given
by a boolean expression

### Types and operators

The trace query language supports six types of data:

+ `bool`, `int`, `float`, `string` for booleans,
integers, floating-point real numbers, 
and strings respectively.
+ `agents_set` for **sets of agents** -- i.e. sets of pairs
consisting in an agent identifier and an agent type. 
It is the return type of the measure `component`
that returns the connected component of an agent in a given state.
+ `tuple` for tuples of objects of possibly different types. The comma 
operator (`,`) is used to concatenate tuples. Besides, any value is
implicitly converted to a tuple with a single element when used as
an argument for `,`.

String literals are delimited between simple quotes.
Numeric literals are provided for integers (`1`, `2`, `42`) 
and floating point numbers (`1.2`, `1.2E-4`, `3E10`). Besides, 
there is an implicit coercion from type `int` to `float`. Thus,
arithmetic expressions mixing both numeric types are allowed,
like in `1 + 3.6`.

Here are some operators to be used on expressions of these types. The
`:` character is sometimes used to specify the type of an argument or the
return type of an operator.

+ The equality operator `=` takes two arguments of the same type 
(with the exception of `tuple`)
and returns a value of type `bool`.
+ The logical operators `&&` and `||` are provided for combining values of 
type `bool`.
+ Arithmetic and comparison operators `+`, `-`, `*`, `<`, `<=`, `>=`, `>` work
with both types `int` and `float`.
+ The following operators work with values of type `agents_set`:
    * `size{s : agents_set} : int` Gives the size of a set of agents. For example,
        `size{component[.e]{ag}}` gives the size of the connected component
        of agent `ag` before event `e` triggers.
    * `count{l}{s : agents_set} : tuple` If `l` is a comma separated 
    list of strings representing agent kinds, returns a tuple giving
    the number of times each agent kind appears in `s`. For example,
    if `s` contains 3 agents of type A, two agents of type B and four
    agents of type C, then `count{'B','A'}{s}` yields the tuple `1, 3`.

Finally, the identifier of an agent alone is not a valid expression.
For example, the following query is ill-formed:
`match e:{ s:S(x~[p]) } return s`. In order to get the integer value
that corresponds to the identifier of agent `s`, use the `agent_id{s}`
construct.


### Measures

The following measures are _event measures_ and are to be given
an event as a first parameter (between brackets):

+ `time[e] : float` Gives the time of event `e`.
+ `rule[e] : string` Gives the name of the rule event `e` is an instance of.

The following measures are _state measures_ and are to be given a state 
expression as a first parameter, between brackets. There are two classes of state
expressions: `.e` denotes the state of the mixture before event `e` and
`e.` denotes the state of the mixture after event `e`.

+ `int_state[s]{k.x} : string` Gives the name of the internal state of
site `x` of agent `k` in global state `s`.
+ `component[s]{ag} : agents_set` Returns the connected component of agent
`ag` in state `s`.
+ `snapshot[s] : string` Takes a snapshot of state `s` and dumps it into a fresh
JSON file. Returns the name of the created JSON file. See option `--snapshots-names`
to change how generated files are named and `--native-snapshots` to switch the output
format to native KaSim expressions.

### Limitations of the current engine

The trace query engine as currently implemented is subject to the following
limitations:

+ Delta expressions in event patterns (delimited by `{ ... }`) have
to feature at least one modification. Besides, once an event pattern is
associated to an event, the agents it features must admit a unique consistent
mapping with agents in the mixture.

+ We define the _dependency graph_ of a trace pattern $P$ as follows: each
event pattern corresponds to a node and there is an arrow from event
$a$ to event $b$ if a clause of the form `first b:{ ... } after a` or
`last a:{ ... } before b` appears in $P$.
The current implementation requires that the
dependency graph be _rooted_ in the sense that it must admit a node from
which every other node is reachable.