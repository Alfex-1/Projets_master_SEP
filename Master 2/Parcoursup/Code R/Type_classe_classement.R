#Environnement de travail et chargement des données
setwd("A:/New_data")
df_troyes <- read.csv("CandidatsTroyes2.csv", fileEncoding = "Windows-1252",sep=";")
df_reims <- read.csv("CandidatsReims2.csv", fileEncoding = "Windows-1252",sep=";")

#Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(gridExtra)
setwd("A:/Classement_formation")
#-----------------------------------#TROYES#-----------------------------------#

#On ne prend que ce qui nous intéresse
df_troyes <- df_troyes %>%
  select(INE.Candidat,Nom.Etablissement.origine.2022.2023,Classement,
         Type.Classe...Code.2022.2023,Type.Classe...Libellé.2022.2023) %>%
  filter(grepl("[[:alnum:]]", Nom.Etablissement.origine.2022.2023))

df_classe_troyes <- df_troyes %>%
  filter(grepl("[[:alnum:]]",Type.Classe...Libellé.2022.2023))

#Etude sur la filière et le classement
#Traitement des données
df_classe_troyes <- df_classe_troyes[!(df_classe_troyes$Classement == "ENCF" | df_classe_troyes$Classement == "NC"), ]
df_classe_troyes$Classement <- as.numeric(df_classe_troyes$Classement)
df_classe_troyes <- df_classe_troyes[!(df_classe_troyes$Type.Classe...Libellé.2022.2023 == "Mise à niveau"), ]
df_classe_troyes <- na.omit(df_classe_troyes)

#Retirer "Autre" car inaterprétable
df_classe_troyes <- df_classe_troyes[!(df_classe_troyes$Type.Classe...Libellé.2022.2023 == "Autre" | df_classe_troyes$Type.Classe...Libellé.2022.2023 == "Brevet"), ]

#Changement des noms des variables
colnames(df_classe_troyes) <- c("INE", "Nom_etablissement", "Classement","Classe_code","Classe_libelle")

#Enregistrement de la dataframe
#write.csv(df_classe_troyes, file = "df_classe_troyes_troyes.csv")

###Analyse croisée entre les types de scolarité et les classement

#Tableau des moyennes
rank_by_class_troyes <- df_classe_troyes %>%
  group_by(Classe_libelle) %>%
  summarise(Moyenne_Classement = round(mean(Classement, na.rm = TRUE)),0)
rank_by_class_troyes <- rank_by_class_troyes[,-3]
rank_by_class_troyes

#Visualisation graphique : boîtes à moustaches
# Créer un facteur avec les niveaux dans l'ordre souhaité
X11()
ggplot(df_classe_troyes) +
  aes(x = Classement, y = Classe_libelle) +
  geom_violin(fill = "#4682B4") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Classement", y = "Type de formation suivie") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

#Regarder la variance de tous les groupes
n_troyes = max(df_classe_troyes$Classement)

df_var_troyes <- df_classe_troyes %>%
  group_by(Classe_libelle) %>%
  summarise(Troyes = round((var(Classement))/n_troyes,2))
view(df_var_troyes)

#On regarde si les variances des groupes Géné-pro-techno sont identiques
gene_pro_techno_troyes <- df_classe_troyes[(df_classe_troyes$Classe_libelle =="Générale"
                                          | df_classe_troyes$Classe_libelle =="Technologique"
                                          | df_classe_troyes$Classe_libelle =="Professionnelle"), ]

levene_result_troyes <- leveneTest(Classement ~ Classe_libelle, data = gene_pro_techno_troyes)
levene_result_troyes #P-value trop grande = variances égales

bartlett_troyes <- bartlett.test(Classement ~ Classe_libelle, data = gene_pro_techno_troyes)
bartlett_troyes #Confirmation

#On regarde si les variances des groupes université-IUT sont identiques
uni_uit_troyes <- df_classe_troyes[(df_classe_troyes$Classe_libelle =="Université"
                                  | df_classe_troyes$Classe_libelle =="I.U.T"), ]

levene_result_troyes2 <- leveneTest(Classement ~ Classe_libelle, data = uni_uit_troyes)
levene_result_troyes2 #P-value trop grande = Variances égales

bartlett_troyes2 <- bartlett.test(Classement ~ Classe_libelle, data = uni_uit_troyes)
bartlett_troyes2 #Confirmation

#Analyse des ENCF et NC
#Traitement des données : un peu près la même chose que précédemment
df_rest_troyes <- df_troyes %>%
  filter(grepl("[[:alnum:]]",Type.Classe...Libellé.2022.2023))

#Changement des noms des variables
colnames(df_rest_troyes) <- c("INE", "Nom_etablissement", "Classement","Classe_code","Classe_libelle")

#Traitement des données
df_rest_troyes <- df_rest_troyes[(df_rest_troyes$Classement == "ENCF" | df_rest_troyes$Classement == "NC"), ]
df_rest_troyes <- df_rest_troyes[!(df_rest_troyes$Classe_libelle == "Mise à niveau"), ]
df_rest_troyes <- na.omit(df_rest_troyes)
df_rest_troyes <- df_rest_troyes[!(df_rest_troyes$Classe_libelle == "Autre" | df_rest_troyes$Classe_libelle == "Brevet"), ]

#Enregistrement de la dataframe
#write.csv(df_rest_troyes, file = "df_rest_troyes.csv")

#Moyenne des classement par type de formation
rank_by_class_encf_troyes <- df_rest_troyes %>%
  dplyr::filter(Classement == "ENCF") %>%
  group_by(Classe_libelle) %>%
  summarise(Compte_Classement = n())

rank_by_class_nc_troyes <- df_rest_troyes %>%
  dplyr::filter(Classement == "NC") %>%
  group_by(Classe_libelle) %>%
  summarise(Compte_Classement = n())

#------------------------------------#REIMS#------------------------------------#

#On ne prend que ce qui nous intéresse
df_reims <- df_reims %>%
  select(INE.Candidat,Nom.Etablissement.origine.2022.2023,Classement,
         Type.Classe...Code.2022.2023,Type.Classe...Libellé.2022.2023) %>%
  filter(grepl("[[:alnum:]]", Nom.Etablissement.origine.2022.2023))

df_classe_reims <- df_reims %>%
  filter(grepl("[[:alnum:]]",Type.Classe...Libellé.2022.2023))

#Etude sur la filière et le classement
#Traitement des données
df_classe_reims <- df_classe_reims[!(df_classe_reims$Classement == "ENCF" | df_classe_reims$Classement == "NC"), ]
df_classe_reims$Classement <- as.numeric(df_classe_reims$Classement)
df_classe_reims <- df_classe_reims[!(df_classe_reims$Type.Classe...Libellé.2022.2023 == "Mise à niveau"), ]
df_classe_reims <- na.omit(df_classe_reims)

#Retirer "Autre" car inaterprétable
df_classe_reims <- df_classe_reims[!(df_classe_reims$Type.Classe...Libellé.2022.2023 == "Autre" | df_classe_reims$Type.Classe...Libellé.2022.2023 == "Brevet"), ]

#Changement des noms des variables
colnames(df_classe_reims) <- c("INE", "Nom_etablissement", "Classement","Classe_code","Classe_libelle")

#Enregistrement de la dataframe
#write.csv(df_classe_reims, file = "df_classe_reims_reims.csv")

###Analyse croisée entre les types de scolarité et les classement

#Tableau des moyennes
rank_by_class_reims <- df_classe_reims %>%
  group_by(Classe_libelle) %>%
  summarise(Moyenne_Classement = round(mean(Classement, na.rm = TRUE)),0)
rank_by_class_reims <- rank_by_class_reims[,-3]
rank_by_class_reims

#Visualisation graphique : boîtes à moustaches
X11()
ggplot(df_classe_reims) +
  aes(x = Classement, y = Classe_libelle) +
  geom_violin(fill = "#4682B4") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Classement", y = "Type de formation suivie") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13,face="italic"))

#Regarder la variance de tous les groupes
n_reims = max(df_classe_reims$Classement)

df_var_reims <- df_classe_reims %>%
  group_by(Classe_libelle) %>%
  summarise(Reims = round((var(Classement))/n_reims,2))
view(df_var_reims)

#Joindre les deux tables des variances
df_var <- merge(df_var_reims,df_var_troyes, by = "Classe_libelle")

#On regarde si les variances des groupes Géné-pro-techno sont identiques
gene_pro_techno_reims <- df_classe_reims[(df_classe_reims$Classe_libelle =="Générale"
                                          | df_classe_reims$Classe_libelle =="Technologique"
                                          | df_classe_reims$Classe_libelle =="Professionnelle"), ]

levene_result_reims <- leveneTest(Classement ~ Classe_libelle, data = gene_pro_techno_reims)
levene_result_reims #P-value trop petites = variances inégales

bartlett_reims <- bartlett.test(Classement ~ Classe_libelle, data = gene_pro_techno_reims)
bartlett_reims #Confirmation

#On regarde si les variances des groupes université-IUT sont identiques
uni_uit_reims <- df_classe_reims[(df_classe_reims$Classe_libelle =="Université"
                                  | df_classe_reims$Classe_libelle =="I.U.T"), ]

levene_result_reims2 <- leveneTest(Classement ~ Classe_libelle, data = uni_uit_reims)
levene_result_reims2 #P-value trop grandes = Variances égales

bartlett_reims2 <- bartlett.test(Classement ~ Classe_libelle, data = uni_uit_reims)
bartlett_reims2 #Confirmation

#Analyse des ENCF et NC
#Traitement des données : un peu près la même chose que précédemment
df_rest_reims <- df_reims %>%
  filter(grepl("[[:alnum:]]",Type.Classe...Libellé.2022.2023))

#Changement des noms des variables
colnames(df_rest_reims) <- c("INE", "Nom_etablissement", "Classement","Classe_code","Classe_libelle")

#Traitement des données
df_rest_reims <- df_rest_reims[(df_rest_reims$Classement == "ENCF" | df_rest_reims$Classement == "NC"), ]
df_rest_reims <- df_rest_reims[!(df_rest_reims$Classe_libelle == "Mise à niveau"), ]
df_rest_reims <- na.omit(df_rest_reims)
df_rest_reims <- df_rest_reims[!(df_rest_reims$Classe_libelle == "Autre" | df_rest_reims$Classe_libelle == "Brevet"), ]

#Enregistrement de la dataframe
#write.csv(df_rest_reims, file = "df_rest_reims.csv")

#Moyenne des classement par type de formation
rank_by_class_encf_reims <- df_rest_reims %>%
  dplyr::filter(Classement == "ENCF") %>%
  group_by(Classe_libelle) %>%
  summarise(Compte_Classement = n())

rank_by_class_nc_reims <- df_rest_reims %>%
  dplyr::filter(Classement == "NC") %>%
  group_by(Classe_libelle) %>%
  summarise(Compte_Classement = n())

#Mettre les deux graphiques ensemble
plot_reims <- ggplot(df_classe_reims) +
  aes(x = Classement, y = Classe_libelle) +
  geom_violin(fill = "#4682B4") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Classement", y = NULL) +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, max(df_classe_reims$Classement), by = 200))

plot_troyes <- ggplot(df_classe_troyes) +
  aes(x = Classement, y = Classe_libelle) +
  geom_violin(fill = "#4682B4") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Classement", y = "Type de formation suivie") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))+
  scale_x_continuous(breaks = seq(0, max(df_classe_troyes$Classement), by = 100)) 

# Organiser les graphiques dans une seule image
combined_plots <- grid.arrange(plot_reims, plot_troyes, ncol = 1)

# Enregistrer l'image dans un fichier PNG
ggsave("formations.png", combined_plots, width = 20, height = 10)
