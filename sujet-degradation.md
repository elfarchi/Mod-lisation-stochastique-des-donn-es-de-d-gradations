# Projet - Modélisation aléatoire et analyse statistique de données de dégradation

**Contact: Olivier Gaudoin (olivier.gaudoin@univ-grenoble-alpes.fr)**

## Contexte

Pour éviter les pannes intempestives comme les catastrophes de grande ampleur (par exemple l'effondrement du pont de Gênes en 2018) les grands systèmes industriels sont de plus en plus soumis à une surveillance étroite de leurs indicateurs de fonctionnement et de dégradation. Sur la base de ces mesures, les data scientists cherchent à prévoir à quel moment ces systèmes seront susceptibles d'avoir des défaillances, et à empêcher que cela se produise grâce à des opérations de maintenance préventive. Pour cela, il faut proposer des modèles aléatoires pour le processus de dégradation de ces systèmes, estimer leurs paramètres au vu de mesures de dégradation, afin d'en déduire une prévision des défaillances et d'optimiser la stratégie de maintenance.



## Objectif

Le but de ce projet est d'étudier les deux principaux modèles de dégradation, le [processus de Wiener](https://en.wikipedia.org/wiki/Wiener_process) et le [processus Gamma](https://en.wikipedia.org/wiki/Gamma_process), et de créer une application [Shiny](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/) en R qui simule ces modèles, estime leurs paramètres et prévoit leurs défaillances.
Selon l'état d'avancement du projet, on pourra aborder des notions plus avancées comme le processus Inverse Gaussien, les modèles de dégradation multidimensionnelle et les modèles de maintenance imparfaite.



## Modélisation et estimation

On note $X(t)$ le niveau de dégradation à l'instant $t$.



Le *processus de Wiener* homogène avec dérive s'écrit $X(t) = \mu t + \sigma B(t)$, où $B$ est un mouvement Brownien standard, $\mu$ est un paramètre de tendance et $\sigma$ est un paramètre de variabilité. La principale propriété du processus de Wiener est que ses accroissements sont indépendants et de loi normale : $\forall \, 0 < s < t, X(t)-X(s)$ est de loi ${\cal N}(\mu (t-s), \sigma^2 (t-s))$ et est indépendant de $X(s)$. Cette propriété fait qu'il est facile d'estimer $\mu$ et $\sigma^2$ au vu de mesures successives du niveau de dégradation.



Le *processus Gamma* homogène est similaire au processus de Wiener, mais ses accroissements sont de loi gamma au lieu d'être de loi normale : $\forall \, 0 < s < t, X(t)-X(s)$ est de loi ${\cal G}(a (t-s), b)$ et est indépendant de $X(s)$. Il est facile d'estimer $a$ et $b$ au vu de mesures successives du niveau de dégradation.



Il est possible de considérer des processus de Wiener et Gamma non homogènes, pour lesquels l'espérance du niveau de dégradation n'est pas une fonction linéaire du temps.



En général, on considère qu'une défaillance se produit quand le niveau de dégradation dépasse un certain seuil. Pour prévenir les défaillances, il est important de déterminer la loi de probabilité de la date de franchissement de seuil. Cette loi est connue pour les processus de Wiener et Gamma homogènes.



Dans ce projet, on simulera et on visualisera des trajectoires des processus de dégradation étudiés. Les paramètres des modèles seront estimés par la méthode du maximum de vraisemblance et des moments. On effectuera des prévisions des dates de défaillances futures au vu des niveaux de dégradation observés.





## Données

L'ensemble de la démarche sera appliquée pour analyser deux jeux de données :



* *Semi-conducteurs* : La dégradation par porteurs chauds des semi-conducteurs est une usure progressive due à l’impact répété de porteurs de charge très énergétiques à l’intérieur du transistor. Les données concernent 5 transistors sur lesquels 35 mesures de dégradation ont été faites. Un transistor est considéré comme défaillant lorsque son niveau de dégradation atteint le seuil de 0.15.
* *Lasers* : Un ensemble de 15 dispositifs laser à l'arséniure de gallium a été testé à 80 °C. Les caractéristiques de performance des dispositifs ont été observées toutes les 250 heures jusqu'à 4 000 heures. Un dispositif est considéré comme défaillant lorsque ses caractéristiques de performance atteignent le niveau critique de 10.



Il faudra choisir un modèle de dégradation pertinent pour ces données, estimer ses paramètres et prévoir les défaillances de ces systèmes.


## Equipes

Le sujet est fait pour des équipes de 4 étudiants.

## Références bibliographiques

* Cours de Probabilités et Statistique 1, 1A
* Kahle W., Mercier S., Paroissin C. "Degradation processes in reliability". Iste-Wiley, 2016.
* Ye Z.S., Xie M., "Stochastic modelling and analysis of degradation for highly reliable products". Applied Stochastic Models in Business and Industry, 31, 16–32, 2015.
