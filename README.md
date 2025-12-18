# Projet Math-appli 1A

## Fichiers importants :

- `Shinyapp` est le répertoire contenant l'application Shiny codée en R :
   1. `app.R` est l'application Shiny qui étudie les différentes modèles de dégradations.

- `Processus_non_homogènes` est le répertoire présentant les processus de Weiner et Gamma
   1. `Temps_de_défaillance_Gamma_non_homogène.Rmd`
   2. `estimation_params_weiner_methode_des_moments`

- `Maintenance` est le répertoire présentant les simulations des maintenances imparfaites des modèles de dégradation.
   1. `Simulation_Maintenance_Imparfaite.py` : ce script Python simule des processus de wiener avec maintenance imparfaite par réduction d'état pour des paramétres différents.
   3. `Stratégie_Maintenance_Imparfaite.py` : ce script Python simule plusieurs plusieurs des cas de maintenance avec des seuils de sécurité différentes et renvoie les valeurs calculées de la fonction cout et la valeur du seuil qui la minimise.
   4. `Jh_a_ploter.py` : ce script fait un diagramme de plusieurs fonctions cout dont le paramétre Cf/Cm différe. Le diagramme affiche aussi les seuil de sécurité optimales.

- `Rapport` contient tout ce qui concerne le rapport du projet.
   1. `Rapport.pdf` est le rapport déposé sur Tiede.
   2. `main.tex` est le fichier Latex.
   3. `ref.bib` contient les références utilisés.