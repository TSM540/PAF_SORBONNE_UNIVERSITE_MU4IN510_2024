# Tutoriel sur containers

**Auteur** : Frédéric Peschanski (Sorbonne Université)


Ce projet (stack) contient un tutoriel sur la bibliothèque containers
pour les structures discrètes en Haskell.

Ce dépôt Copyright (C) 2024 Frédéric Peschanski, sous licence Creative Commons CC-BY-SA-4fr (cf. fichier `LICENCE`).


## Première partie : les séquences (Sequence)

Les séquences sont décrites dans `src/Seq.hs`

La documentation correspondante est disponible à l'adresse suivante :
<https://www.stackage.org/haddock/lts-22.6/containers-0.6.7/Data-Sequence.html>

## Seconde partie : les tables associations (Map)

Les séquences sont décrites dans `src/Map.hs`

La documentation correspondante est disponible à l'adresse suivante :
<https://www.stackage.org/haddock/lts-22.6/containers-0.6.7/Data-Map.html>

## Troisième partie : les ensembles (Set)

Les séquences sont décrites dans `src/Set.hs`

La documentation correspondante est disponible à l'adresse suivante :
<https://www.stackage.org/haddock/lts-22.6/containers-0.6.7/Data-Set.html>

----

En guise de complément, ce projet intègre les chaînes de caractères Unicode
"modernes" avec la bibliothèque text, et active l'extension de surcharge des chaînes de caractères (`OverloadedStrings`) directement dans le fichier de configuration  `package.yaml`

