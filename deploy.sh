#!/bin/sh

#stack exec site rebuild
rsync -avz generated/deploy/out/ jlauren1@linux.gp.cs.cmu.edu:~/www/