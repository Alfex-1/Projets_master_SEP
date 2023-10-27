**Participants :**
> Dans notre équipe nous avons 3 étudiants qui viennent d'une formation économique :
> - BRUNET Alexandre ;
> - EDSON BRUMALE KPADONOU Carlos ;
> - SEJDI Benjamin.

> Nous avons également 3 autres étudiants qui viennent d'une formation de mathématiques :
> - ERTAS Elif ;
> - THIERUS Armand ;
> - LACROIX Amélie.


**Contexte :**
> Une entreprise d'environ 4 000 employés fait face a un problème : chaque année, 15 % des employés quittent leur poste et sont remplacés. Cette situation est évidemment problématique pour l'entreprise, qui voit ses projets retardés ainsi que ses coûts augmenter (en effet, chaque nouvel employé doit être formé, et le recrutement nécessite la création d'un pôle RH qui travaille en permanence).

> A partir des données fournies par l'entreprise concernant le taux d'attrition (départ) des employés. L'objectif est de comprendre les facteurs dans l'environnement de travail qui contribuent à ce taux élevé de rotation annuelle de 15 %, dans le but d'établir des recommandations pour l'entreprise. Grâce à ces recommandations, nous lui permettrons de se concentrer sur les éléments qui favorisent ou qui défavorisent l'attrition des employés selon certaines priorités que nous mettrons en avant.


**Nos données :**
> Dans cette analyse, nous utiliserons 2 bases de données reliées par l'identifiant de chaque employé (4410 individus): une contenant les réponses des employés à un sondage et l'autre contenant des informations plus générale sur chaque employé.

> La première contient, pour chaque employé, 3 informations exprimées par une note allant de 1 à 4 : sa satisfaction par rapport à l'environnement de travail, son implication au travail et l'équilibre dans sa vie personelle (des variables subjectives).
> La deuxième, plus complète contient pour chaque employé ses données administratives comme son âge, son département, son genre, la distance entre son domicile et son lieu de travail etc. (des variables objectives).


**Nettoyage des données :**
> Afin de pouvoir traiter notre base de donnée nous l'avons nettoyée, dans un premier temps les lignes contenant des NA ont été retirées (un peu moins d'une centaine), puis les colonnes non pertinentes ont été enlevées (comme celle indiquant si l'individu est majeur ou non)


**Modèle utilisé et analyse menée :**
> Pour mener à bien notre analyse, nous avons choisi d'utiliser un modèle de régression logistique pour évaluer la probabilité de départ d'un employé (son attrition) en fonction de ses caractéristiques. Dans le cadre de nos recommandations, nous mettrons l'accent sur les rapports de cotes qui expriment l'impact de chaque variable sur la variable cible. Pour les variables catégorielles, il n'est pas possible de procéder à une simple comparaison globale. À la place, nous comparerons les modalités de chaque variable les unes par rapport aux autres. Une modalité servira de référence, et les autres modalités de la variable seront comparées à cette référence. Pour chaque variable, nous sélectionnerons la modalité de référence que nous estimons être la plus appropriée.

> Avant d'analyser les rapports de cotes, nous avons effectué une sélection des variables en fonction de leur niveau de signification. Cette démarche s'explique par le fait qu'une variable non significative (c'est-à-dire sans impact sur l'attrition des employés) n'ajoute aucune valeur à notre analyse.


**Recommandations :**
> _-Voyages d'affaire :_ ayant noté une forte corrélation entre un grand nombre de voyages d'affaire et des risques de départs, nous recommandons au maximum de limiter les voyages d'affaire.

> _-Célibataires:_ permettre globalament aux employés d'avoir un meilleur équilibre pro/perso, ce qui passe, par exemple, par le fait de diminuer les voyages d'affaires. C'est d'autant plus important pour les célibataires.

> _-Profils spécifiques :_ Les employés ayant suivi une formation de RH et les directeurs de recherche étant des profils necessaires qui ont plus de chance de partir, nous recommandons des analyses plus poussé quand à ces employés (notamment via des sondages pour améliorer leur environnement de travail).

> _-Nombre d'experiences :_ éviter les profils "mercenaires" (change fréquement d'emplois), notamment pour les gens ayant suivi une formation de  RH et les employés occupant le poste de directeur de recherhe.

> _-Ecart entre les promotions:_ Plus un employé attend entre 2 promotions, plus il aura tendance à quitter la boite, ainsi, nous recommandons d'offrir des promotions plus fréquentes aux employés qui seraient les plus chers à remplacer, ainsi que de favoriser les évolutions au sein de la boîte aux parachutages. Cela leur donnerait l'occasion de changer de responsable, ce qui diminuerait leur envie de quitter l'entreprise.
