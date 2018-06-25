# My Personal Website

This repo contains the source code of my [personal
website](https://www.cs.cmu.edu/~jlaurent/). It relies on
the [Hakyll](https://jaspervdj.be/hakyll/) static sites
generator.

## Setup

### Dependencies

The main dependencies that are to be installed manually are
[Sass](https://sass-lang.com/) and
[KaTex](https://github.com/Khan/KaTeX):

+ The `scss` executable must be in your `$PATH`
+ The `katex` folder of the latest KaTex
  [release](https://github.com/Khan/KaTeX/releases) must be
  added to the `provider` directory of this repo.

In order to use some advanced markdown features, you may
want to install the following additional dependencies:

+ The `pygment` Python library.
+ The `graphviz` package: the `dot` program should be in
  your `$PATH`.
+ The `diagrams` Haskell library: once installed, you must
  set the `STACK_YAML` variable in `scripts/diagrams.sh`.


### Installation

After cloning the repo and installing the dependencies (see
above), you must set the url of the website's root in the
`siteroot` file.


### Building the website

To build the Hakyll executable, use `stack build`. Then, run
`./watch.sh` to compile the website and start a preview
server on `0.0.0.0:4000`.
