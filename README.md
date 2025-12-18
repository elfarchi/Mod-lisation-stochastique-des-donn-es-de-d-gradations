# Projet Math-appli 1A

# Modélisation stochastique de la dégradation – Processus de Wiener et Gamma

Ce dépôt contient le code et les ressources associés à un projet de mathématiques appliquées portant sur la modélisation stochastique de la dégradation de systèmes industriels à l’aide des processus de Wiener et Gamma.

## Objectifs du projet
Le projet vise à :
- Simuler des trajectoires de dégradation à l’aide des processus de Wiener et Gamma.
- Estimer les paramètres des modèles par la méthode des moments et par maximum de vraisemblance.
- Comparer les performances des estimateurs à partir de simulations Monte Carlo.
- Appliquer ces modèles à des jeux de données réels (lasers et semi-conducteurs).
- Développer une application interactive sous **R Shiny** pour la simulation, l’estimation et la prévision de la défaillance.
- Étudier des extensions vers des processus non homogènes et des politiques de maintenance imparfaite.

## Contenu du dépôt
- `Shinyapp/` est le répertoire contenant l'application Shiny codée en R :
   1. `app.R` est l'application Shiny qui étudie les différentes modèles de dégradations.

- `Processus_non_homogènes/` est le répertoire présentant les processus de Weiner et Gamma
   1. `Temps_de_défaillance_Gamma_non_homogène.Rmd`
   2. `estimation_params_weiner_methode_des_moments`

- `Maintenance/` est le répertoire présentant les simulations des maintenances imparfaites des modèles de dégradation.
   1. `Simulation_Maintenance_Imparfaite.py` : ce script Python simule des processus de wiener avec maintenance imparfaite par réduction d'état pour des paramétres différents.
   3. `Stratégie_Maintenance_Imparfaite.py` : ce script Python simule plusieurs plusieurs des cas de maintenance avec des seuils de sécurité différentes et renvoie les valeurs calculées de la fonction cout et la valeur du seuil qui la minimise.
   4. `Jh_a_ploter.py` : ce script fait un diagramme de plusieurs fonctions cout dont le paramétre Cf/Cm différe. Le diagramme affiche aussi les seuil de sécurité optimales.

- `Rapport/` contient tout ce qui concerne le rapport du projet.
   1. `Rapport.pdf` est le rapport déposé sur Tiede.
   2. `main.tex` est le fichier Latex.
   3. `ref.bib` contient les références utilisés.

## Modèles considérés
- **Processus de Wiener** : adapté aux phénomènes de dégradation fluctuants.
- **Processus Gamma** : adapté aux dégradations monotones et irréversibles.

Les modèles sont étudiés dans un cadre homogène, puis étendus à des versions non homogènes et à des politiques de maintenance imparfaite par réduction de l’état.

## Application R Shiny
L’application Shiny développée permet à l’utilisateur de :
- choisir le modèle de dégradation,
- simuler une ou plusieurs trajectoires,
- estimer les paramètres à partir de données observées,
- visualiser les trajectoires simulées et prédire la défaillance.

## Auteurs
Projet réalisé par :
- Saad Boubekri  
- Ayoub El Farchi  
- Aymane Lachkham  
- Mohammed Amine Zerouali  

ENSIMAG – 1A  
Projet de Mathématiques Appliquées