---
title: Jonathan Laurent
---

<section id=abstract>
<div class="inside cols-auto-small">
<div>
Je suis actuellement étudiant en M2 à 
l'[École Normale Supérieure](http://www.ens.fr). 
Mes centres d'intérêts se situent à la frontière de l'informatique, des mathématiques et de la biologie. 
Ils comprennent notamment les domaines suivants :

+ Langages de programmation, théorie des types et systèmes de preuves.
+ Etude de systèmes biologiques complexes à l'aide de méthodes formelles.
+ Probabilités et théorie de l'apprentissage statistique.

J'éprouve un plaisir tout particulier à partager ma passion pour les sciences, que ce soit dans un contexte de vulgarisation ou d'enseignement. Enfin, je suis un programmeur enthousiaste, utilisateur fanatique de
[langages fonctionnels fortement typés](http://www.haskell.org/haskellwiki/Haskell).
</div>
![](/img/bw/portrait.jpg){ #portrait .ornemental height=220px style="margin-left:3em"}
</div>
</section>

<div id=main>
<section><div class="inside">

# Stage de recherche à Harvard Medical School

<div class=cols-auto-small>
<div>
J'ai effectué un stage de recherche de cinq mois à Harvard, au sein du laboratoire de 
[Walter Fontana](https://fontana.hms.harvard.edu/), au cours duquel j'ai développé 
un formalisme pour mieux comprendre la structure causale de certains systèmes de 
[signalisation](https://fr.wikipedia.org/wiki/Signalisation) complexes, ainsi qu'un algorithme 
pour révéler cette structure dans des modèles écrits dans le 
[langage Kappa](http://www.kappalanguage.org/). Mon rapport de stage est disponible
[ici](pdf/reports/causal-analysis.pdf).
</div>
![](img/bw/harvard-logo.png){.lmargin .ornemental height=50px}
</div></section>

<section><div class="inside">

# Stage de recherche au centre NASA de Langley

<div class=cols-small-auto>
![](img/bw/nasa.png){ .rmargin .ornemental width=100px }
<div>
J'ai passé trois mois durant l'été 2014 au [NIA](http://www.nianet.org/), centre de recherche affilié au 
centre NASA de Langley, sous la supervision de [Dr. Alwyn Goodloe](http://shemesh.larc.nasa.gov/people/aeg/). J'ai notamment développé la bibliothèque [copilot-kind](https://github.com/jonathan-laurent/copilot-kind), composée d'une série d'outils permettant de prouver des propriétés simples sur des programmes synchrones écrits dans le langage [Copilot](http://leepike.github.io/Copilot/) de manière complètement automatique. J'ai présenté une synthèse des techniques utilisées dans le cadre d'une conférence d'une heure dont je copie ci-dessous l'intitulé (en anglais).
</div>
</div>

### An insight into SMT-based model checking techniques for formal software verification

Highly automated proof techniques are a necessary step for the widespread adoption of formal methods in the software industry. Moreover, it could provide a partial answer to one of its main issue which is scalabilty.

In the context of the formal verification of safety properties on synchronous dataflow programs, we examine a SMT-based approach which led to the development of the Kind2 model checker and yielded promising experimental results. We give an insight into the two algorithms powering this tool :

+ The k-induction algorithm, enriched with abstraction and path compression
+ The IC3 algorithm, with approximate quantifier elimination to generalize counterexamples

[Transparents](pdf/slides/talk-nia.pdf) | 
[Enregistrement vidéo](http://nia-mediasite.nianet.org/NIAMediasite100/Play/b9a15957f7574756bb52f0ea6a4ca5471d)

</div></section>
</div>