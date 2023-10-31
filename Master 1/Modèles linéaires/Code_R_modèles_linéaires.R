#--------------------------# Préparation des données #--------------------------#

#Packages utilisés
install.packages("lmtest")
install.packages("olsrr")

#Environnement de travail
setwd("C:/Personnel/Cours M1 S1/Projet final R + SAS")

###Importation de la table puis modification de celle-ci
dta<-read.csv("Data.csv", header=T, sep=";")

#Première colonne en individu et on retire ensuite cette première colonne de la base
rownames(dta)<-dta[,1]
don<-dta[,c(-1)]

###Nature des variables
str(don)

#-------------------------------# Introduction #-------------------------------#

###Test d'indépendance du Khi-2
don2 <- na.omit(don) #On retire les données manquantes pour effectuer correctement le test

chisq.test(table(don2$Inflation, don2$PIB + don2$PIBh
                 + don2$Investissement + don2$Volume
                 + don2$Consommation + don2$Consommationh
                 + don2$Emissions))

#-------------------------# Statistiques descriptives #-------------------------#

summary(don)
t(lapply(don2[, 0:8], var)) #Pour avoir la variance (sans compter les NA)


#-----------# PIB+Inflation - Consommation (global et par habitant) #-----------#
library("lmtest")
library("olsrr")
library("car")
###Niveau global

#Régression Consommation-PIB avec l'inflation comme variable de contrôle

X11()
plot(don$PIB+don$Inflation,don$Consommation, type="p", pch=17, xlab="PIB et inflation", ylab="Consommation d'énergies fossiles", col="black")
reg <- lm(don$Consommation~don$PIB+don$Inflation)
abline(reg, col="green")
summary(reg)

#Distance de Cook
X11()
ols_plot_cooksd_bar(reg)

      #Sans le Venezuela qui cache l'impact des 3 autres pays dans le modèle
      WV <- don[-64,]
      reg_WV <- lm(WV$Consommation~WV$PIB+WV$Inflation)
      
      ols_plot_cooksd_bar(reg_WV)

#Corrélation entre les indicateurs macro-économiques (les régresseurs)
x <- as.matrix(don2)
cor(x)

#VIF
vif(reg)

#Linéarité ?
raintest(reg)

###Par habitant

#Régression avec l'inflation comme variable de contrôle
X11()
plot(don$PIBh+don$Inflation,don$Consommationh, type="p", pch=17, xlab="PIB/hab et inflation", ylab="Consommation par habitant d'énergies fossiles", col="black")
reg2 <- lm(don$Consommationh~don$PIBh+don$Inflation)
abline(reg2, col="green")
summary(reg2)

#VIF
vif(reg2)

#Distance de Cook
X11()
ols_plot_cooksd_bar(reg2)

      #Sans Venezuela
      reg_WV2 <- lm(WV$Consommationh~WV$PIBh+WV$Inflation)
      
      ols_plot_cooksd_bar(reg_WV2)

#Linéarité ?
raintest(reg2)

#--------------# Indicateurs Macro-éco - Résultats écolo/énergie #--------------#

#Création d'une table qui enlève les variables qui ne nous intéresse plus
don0 <- don[,-2]
don3 <- don0[,-6]

###Régression Macro-éco - Consommation

#Régression
X11()
plot(don3$PIB+don3$Investissement+don3$Inflation+don3$Volume,don3$Consommation, type="p",
     pch=17, xlab="Indicateurs macro-économiques",
     ylab="Consommation d'énergies fossiles", col="black")
reg3 <- lm(don3$Consommation~don3$PIB+don3$Investissement+don3$Inflation+don3$Volume)
abline(reg3, col="green")
summary(reg3)

#VIF
vif(reg3)

#Linéarité
raintest(reg3)

#Distance de Cook
X11()
ols_plot_cooksd_bar(reg3)

  #Sans le Venezuela
  reg3_WV <- lm(WV$Consommation~WV$PIB+WV$Investissement+WV$Inflation+WV$Volume)

  X11()
  ols_plot_cooksd_bar(reg3_WV)

###Régression Macro-éco - Emissions

#Régression
X11()
plot(don3$PIB+don3$Investissement+don3$Inflation+don3$Volume,don3$Emissions, type="p",
     pch=17, xlab="Indicateurs macro-économiques",
     ylab="Emissions d'énergies fossiles", col="black")
reg4 <- lm(don3$Emissions~don3$PIB+don3$Investissement+don3$Inflation+don3$Volume)
abline(reg4, col="green")
summary(reg4)

#VIF
vif(reg4)

#Linéarité
raintest(reg4)

# Distance de Cook
X11()
ols_plot_cooksd_bar(reg4)

  #Sans le Venezuela
  reg4_WV <- lm(WV$Emissions~WV$PIB+WV$Investissement+WV$Inflation+WV$Volume)

  X11()
  ols_plot_cooksd_bar(reg4_WV)

#-------------------------------# Prédictions ? #------------------------------#

### Modèle Consommation

#Normalité des résidus
X11()
plot(reg3, which=2)

#Test de Fischer et de Student
summary(reg3)

#Homoscédascité
X11()
plot(reg3,which=3)

#Test de Durbin-Watson
durbinWatsonTest(reg3)

#Orthogonalité
X11()
plot(reg3,1)

### Modèle Emissions

#Test de Fischer et de Student
summary(reg4)

#Homoscédascité
X11()
plot(reg4,which=3)

#Test de Durbin-Watson
durbinWatsonTest(reg4)

#Normalité des résidus
X11()
plot(reg4, which=2)

#Orthogonalité
X11()
plot(reg4,1)
