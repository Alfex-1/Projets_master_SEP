
# EN TETE  ----------------------------------------------------------------
# Auteurs : 
# - Garance GABAUT     (M1 MA SEP )
# - Viktoriia KABAKOVA (M1 APE SEP)
# - Dylan MEYER        (M1 MA CS  )
# - Alexandre BRUNET   (M1 APE SEP)

# Enseignant : M. KEZIOU 
# dans le cadre du projet de fin de semestre 
# Module : Methode d'echantillonnage 

#Vider la mémoire
rm(list = ls())

#Packages
library(readr)
library(tidyverse)
library(broom)
library(glmulti)
library(glmnet)
library(MASS)
library(car)
library(lmtest)
library(olsrr)

#Environnement de travail
setwd("C:/Personnel/Cours M1 S2/Méthode d'échantillonnage/Projet/Alexandre_Brunet.zip")

# Nettoyage et Importation ------------------------------------------------

###Importation de la table puis modification de celle-ci
dta <- read_csv("cars_us_2022.csv")

# Correction des valeurs
dta$Electric.Adjustable.Seats <- gsub("Yes", "es", dta$Electric.Adjustable.Seats)
dta$Electric.Adjustable.Seats <- gsub("es", "Yes", dta$Electric.Adjustable.Seats)

dta$Outside.Temperature.Display <- gsub("Yes", "Yyes", dta$Outside.Temperature.Display)
dta$Outside.Temperature.Display <- gsub("Yyes", "Yes", dta$Outside.Temperature.Display)

dta$Power.Door.Locks <- gsub("Yes", "yes", dta$Power.Door.Locks)
dta$Power.Door.Locks <- gsub("yes", "Yes", dta$Power.Door.Locks)

dta$Audio.System.Remote.Control <- gsub("Yes", "YES", dta$Audio.System.Remote.Control)
dta$Audio.System.Remote.Control <- gsub("YES", "Yes", dta$Audio.System.Remote.Control)

dta$Voice.Control <- gsub("Yes", "Ye", dta$Voice.Control)
dta$Voice.Control <- gsub("Ye", "Yes", dta$Voice.Control)

dta$Low.Fuel.Warning.Light <- gsub("Yes", "Yers", dta$Low.Fuel.Warning.Light)
dta$Low.Fuel.Warning.Light <- gsub("Yers", "Yes", dta$Low.Fuel.Warning.Light)

#Conversion des colonnes
dta[sapply(dta, is.character)] <- lapply(dta[sapply(dta, is.character)], 
                                         as.factor)

#Suppression des colonnes où le niveau de facteur est inférieur à 2
dta2 <- dta[, which(sapply(dta, function(x) length(levels(x)) >= 2))]

#Stockage des variables numériques
numeric_cols <- dta[sapply(dta, is.numeric)]
dta_num <- data.frame(numeric_cols)

#Réintégration des variables numériques dans don0
don0 <- cbind(dta_num,dta2)

#On garde que les variables exploitables
don <- data.frame(Price = don0$Price,Power.hp = don0$Power.hp,
                  Displacement.l = don0$Displacement.l,Torque.lbft = don0$Torque.lbft,
                  Cylinders = don0$Cylinders,MPG.City = don0$MPG.City,
                  MPG.Highway = don0$MPG.Highway,Seats = don0$Seats,Doors = don0$Doors,
                  Height.in = don0$Height.in,Length.in = don0$Length.in,Width.in = don0$Width.in,
                  Wheelbase.in = don0$Wheelbase.in,Clearance.in = don0$Clearance.in,
                  Brand = don0$Brand,Fuel.Type = don0$Fuel.Type,
                  Gearbox.Type = don0$Gearbox.Type,Drivetrain = don0$Drivetrain)


# Classement des variables ------------------------------------------------
## des plus explicatives au moins explicatives 

#On fait une régression linéaire multiple pour voir les résultats des tests de student
reg<-lm(Price~.,data=don)

#On stocke l'anova de la régression
reg_anova <- anova(reg)

#On arrondi les p-value pour que ça soit plus lisible
reg_anova[,5] <- round(reg_anova[,5], digits = 4)

#On trie les p-value du plus petit au plus grand
values <- reg_anova[order(reg_anova[,5]), ]



# Selection des variables -------------------------------------------------
# On retire les variables non significatives
#On retire les variables qu'on utilisera pas
df <- subset(don, select = -c(Seats, Drivetrain, Fuel.Type, Gearbox.Type, Brand ))


#### Méthode 1 - algorithme Pas a Pas ####
modele.complet <- lm(Price ~ ., data = df)
df<-na.omit(df)

#### selon AIC 
Time1PasaPasAIC <- Sys.time()
res.select.AIC.bac <- step(modele.complet, data = don, direction = "backward", k = 2)
Time2PasaPasAIC <- Sys.time()
TdiffPasaPasAIC= difftime(Time2PasaPasAIC, Time1PasaPasAIC)
res.select.AIC.bac
TdiffPasaPasAIC
# Time difference of 0.03752112 secs


#### selon BIC (k = log(n))
n <- nrow(dta)
Time1PasaPasBIC <- Sys.time()
res.select.BIC.bac <- step(modele.complet, data = dta, direction = "backward", k = log(n))
Time2PasaPasBIC <- Sys.time()
TdiffPasaPasBIC= difftime(Time2PasaPasBIC, Time1PasaPasBIC)
TdiffPasaPasBIC
# Time difference of 0.05293894  secs

#### Methode 2 - algorithme génétique ####
#### selon AIC 
Time1AIC<-Sys.time()
select.mod.gen <- glmulti(Price ~ ., data = df, level = 1, method = "g", 
                          fitfunction = lm, crit = 'aic', plotty = F)
Time2AIC<-Sys.time()
TdiffAIC= difftime(Time2AIC, Time1AIC)
TdiffAIC

# Time difference 25.14777 secs

aic.best.model <- summary(select.mod.gen)$bestmodel
aic.best.model
#[1] "Price ~ 1 + Power.hp + Displacement.l + Torque.lbft + MPG.City + "
# [2] "    MPG.Highway + Height.in + Length.in + Width.in + Wheelbase.in"

# Nbr de génération : 790 


#### selon BIC
Time1BIC<-Sys.time()
select.mod.gen <- glmulti(Price ~ ., data = df, level = 1, method = "g", 
                          fitfunction = lm, crit = 'bic', plotty = F)
Time2BIC<-Sys.time()
TdiffBIC= difftime(Time2BIC, Time1BIC)
TdiffBIC
# Time difference of 20.88351 secs

bic.best.model <- summary(select.mod.gen)$bestmodel
bic.best.model
# res : 
#[1] "Price ~ 1 + Power.hp + Displacement.l + Torque.lbft + MPG.City + "
#[2] "    MPG.Highway + Width.in + Clearance.in"  
# Nbr de génération : 660 generations 


# Selection du modèle  ----------------------------------------------------
DataFrameModele <- subset(df, select=c(Price, Power.hp, Displacement.l,Torque.lbft, MPG.City, MPG.Highway, Width.in, Wheelbase.in, Height.in,Length.in))
DataFrameModele <- na.omit(DataFrameModele)

#### Modèle Ridge ####
XX <- model.matrix(Price ~., data = DataFrameModele)[,-1]

reg.ridge <- glmnet(x = scale(XX), y = DataFrameModele[,"Price"], alpha = 0)
par(mfrow = c(1,2))

#axe y représente la valeur du coefficient, tandis que l'axe x représente le log de lambda
plot(reg.ridge, label = TRUE, lwd = 2)


#critère de perte en fct du log du lambda
# On cherche a lambda qui minimise ce critère
plot(reg.ridge, xvar = "lambda", label = TRUE, lwd = 2)

reg.cvridge <- cv.glmnet(x = scale(XX), y = DataFrameModele[,"Price"], alpha = 0)
bestlam <- reg.cvridge$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvridge)
min(reg.cvridge$cvm) #Erreur de prevision du modele ridge optimal 
coef(reg.cvridge)



#### Modèle Lasso ####
reg.lasso <- glmnet(x = scale(XX), y = DataFrameModele[,"Price"], alpha = 1)
par(mfrow = c(1,2))
plot(reg.lasso, label = TRUE, lwd=2)

plot(reg.lasso, xvar = "lambda", label = TRUE, lwd = 2)

reg.cvlasso <- cv.glmnet(x = scale(XX), y = DataFrameModele[,"Price"], alpha = 1)
bestlam <- reg.cvlasso$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvlasso)
coef(reg.cvlasso)
erreur.modele.lasso.opt <- min(reg.cvlasso$cvm) #Erreur de prevision du modele lasso optimal 
erreur.modele.lasso.opt


#### modèle RLM ####
modeleRLM <- lm(formula = Price ~ ., data = DataFrameModele) # On construit le modèle avec TOUTES les autres variables
attributes(modeleRLM)
summary(modeleRLM)
summary(modeleRLM)$coefficients

modele_bic <- step(modeleRLM, direction="both", k=log(nrow(DataFrameModele)))
summary(modele_bic)

modele_aic <- step(modeleRLM, direction="both")
summary(modele_aic)


# Estimation de l'erreur de prévision du modèle 
n=nrow(DataFrameModele)
M <- 1000

# Selon le critère AIC
erreur_modeleRLM1 = NULL
for (i in 1:M){
  indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
  apprentissage <- DataFrameModele[indices, ]
  validation <- DataFrameModele[ - indices, ]
  modele1 <- step(modeleRLM, direction = "both", data = apprentissage)
  valeurs_predites1 <- predict(object = modele1, newdata = validation)
  erreur_modeleRLM1[i] <- mean((validation$Price - valeurs_predites1)^2)
}
Err1 = NULL

for (m in 1:M){Err1[m] = mean(erreur_modeleRLM1[1:m])
}

plot(Err1, type = 'l',lwd=2,ylab='Erreur estimée', xlab='nombre d itérations', main='Erreur estimée du modèle RLM, selon AIC')

# Erreur finale retenue : 
Err1[m]


# Selon le critère BIC
erreur_modeleRLM2 = NULL
for (i in 1:M){
  indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
  apprentissage <- DataFrameModele[indices, ]
  validation <- DataFrameModele[ - indices, ]
  modele2 <- step(modeleRLM,  direction="both", k=log(nrow(DataFrameModele)))
  valeurs_predites1 <- predict(object = modele2, newdata = validation)
  erreur_modeleRLM2[i] <- mean((validation$Price - valeurs_predites1)^2)
}
Err2 = NULL


for (m in 1:M){Err2[m] = mean(erreur_modeleRLM2[1:m])
}
plot(Err2, ylab='Erreur estimée', xlab='nombre d itérations', type = 'l',lwd=2, main='Erreur estimée du modèle RLM, selon BIC')

# Erreur finale retenue : 
Err2[m]


# Vérification du modèle choisi ------------------------------------------

#Rappel du modèle choisi
regb <- modele_bic

#Test de colinéarité
vif(regb) #Plus de la moitié on un VIF > 4, ce qui montre
#un gros problème de colinéarité

#Test de linéarité
raintest(regb) #p-value > 1, donc on conserve l'hypothèse de linéarité

#Test de la normalité des résidus et représentation graphique
ols_test_normality(residuals(regb)) #Aucun des test ne conserve l'hypothèse
#selon laquelle les résidus de la régression suivent une loi normale


qqPlot(regb) #La représentation confirme les résultats des tests car les points
#ne suivent pas la courbe droite en bleu

#Tester si les résidus sont indépendants entre eux
durbinWatsonTest(regb) #Le test rejette l'hypothèse de l'indépendance des résidus

#Visualiser l'orthogonalité de la régression 
plot(regb,1) #On observe que le nuage de point peut être ajusté par uen droite,
#ce qui signifie que l'hypothèse d'orthogonalité n'est pas respectée

#Tester l'homoscédascité (égalité des variances)
ols_test_score(regb) #la p-value est inférieure à 5%, de ce fait, on rejette
#l'hypothèse d'homogénéité des variables

X11()
plot(regb,which=3) #On visualise une structure du nuage de points
#dans la représentation graphique,
#cela confirme les résultats du test précédent