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
library(tools)
library(stringr)
library(reshape2)
library(gridExtra)

#Variable des spécialités (général) : é partir de OY sur Excel

#----------------------------#Traitement des bases#----------------------------#

#Troyes
df_troyes <- df_troyes %>%
  dplyr::select(INE.Candidat,Classement,Type.Classe...Libellé.2022.2023,
                Série.de.classe...Code.2022.2023,
                Spécialité...Mention...Libellé..2022.2023,
                EDS.Scolarité.Terminale.1...Libellé.BCN,
                EDS.Scolarité.Terminale.2...Libellé.BCN,
                EDS.Scolarité.Terminale.Abandon...Libellé.BCN,
                Option.facultative.1.Scolarité...Libellé.2022.2023,
                Option.facultative.2.Scolarité...Libellé.2022.2023,
                Option.facultative.3.Scolarité...Libellé.2022.2023)


df_classe_troyes <- df_troyes %>%
  filter(grepl("[[:alnum:]]",Type.Classe...Libellé.2022.2023))

df_classe_troyes <- df_classe_troyes[!(df_classe_troyes$Classement == "ENCF" | df_classe_troyes$Classement == "NC"), ]
df_classe_troyes$Classement <- as.numeric(df_classe_troyes$Classement)
df_classe_troyes <- na.omit(df_classe_troyes)

#Changement des noms des variables
colnames(df_classe_troyes) <- c("INE","Classement","Type_classe","Serie",
                                "Spécialité_mention","Spe1_terminale",
                                "Spe2_terminale","Spe_abandon_terminale",
                                "Option1","Option2","Option3")

#Reims
df_reims <- df_reims %>%
  dplyr::select(INE.Candidat,Classement,Type.Classe...Libellé.2022.2023,
                Série.de.classe...Code.2022.2023,
                Spécialité...Mention...Libellé..2022.2023,
                EDS.Scolarité.Terminale.1...Libellé.BCN,
                EDS.Scolarité.Terminale.2...Libellé.BCN,
                EDS.Scolarité.Terminale.Abandon...Libellé.BCN,
                Option.facultative.1.Scolarité...Libellé.2022.2023,
                Option.facultative.2.Scolarité...Libellé.2022.2023,
                Option.facultative.3.Scolarité...Libellé.2022.2023)

df_classe_reims <- df_reims %>%
  filter(grepl("[[:alnum:]]",Type.Classe...Libellé.2022.2023))

df_classe_reims <- df_classe_reims[!(df_classe_reims$Classement == "ENCF" | df_classe_reims$Classement == "NC"), ]
df_classe_reims$Classement <- as.numeric(df_classe_reims$Classement)

#Changement des noms des variables
colnames(df_classe_reims) <- c("INE","Classement","Type_classe","Serie",
                                "Spécialité_mention","Spe1_terminale",
                                "Spe2_terminale","Spe_abandon_terminale",
                               "Option1","Option2","Option3")

#---------------------------------#Graphiques#---------------------------------#

###Troyes

#Pour les formations générales : focus sur les candidats qui ont fait spé maths

#Création d'une colonne regroupant les options : si le candidat à pris maths en complémentaire
df_classe_troyes$Complement_maths <- apply(df_classe_troyes[, c("Option1", "Option2", "Option3")], 1, function(row) {
  if ("Mathématiques Complémentaires" %in% row) {
    return("Mathématiques Complémentaires")
  } else {
    return("")  }
})

#Retirer les variables inutiles
df_classe_troyes <- df_classe_troyes %>%
  select(-Option1, -Option2, -Option3)

#On ne garde que les candidats issus d'une formation générale
df_gene_troyes <- df_classe_troyes %>%
  dplyr::filter(Type_classe == "Générale")

#On créer uen dataframe permettant de faire un graphqiue correctement
df_gene_troyes_maths <- df_gene_troyes %>%
  mutate(Specialisation = case_when(
    Spe1_terminale == "MATHEMATIQUES" ~ "Spécialité 1",
    Spe2_terminale == "MATHEMATIQUES" ~ "Spécialité 2",
    Complement_maths == "Mathématiques Complémentaires" ~ "Complémentaire",
    TRUE ~ "Aucun"  # Si aucune des conditions n'est remplie
  )) %>%
  mutate(Abandon = case_when(
    Spe_abandon_terminale == "MATHEMATIQUES" ~ "Abandon",
    TRUE ~ "Pas d'abandon"))

#Avoir les moyennes de classement
df_gene_troyes_maths$Spe_abandon <- paste(df_gene_troyes_maths$Specialisation, df_gene_troyes_maths$Abandon, sep="_")

moy_troyes <- df_gene_troyes_maths %>%
  group_by(Spe_abandon) %>%
  summarise(Moyenne = round(mean(Classement),0))

#Graphique
X11()
ggplot(df_gene_troyes_maths, aes(x = Specialisation, y = Classement, fill = Abandon)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Enseignement en mathématiques", y = "Classement", fill = "Légende :") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 17)) +
  scale_fill_manual(values = c("#B22222", "#4682B4")) +
  scale_y_continuous(breaks = seq(0, max(df_gene_troyes_maths$Classement), by = 100))

#Pour les formations technologiques

#Création de la dataframe
df_techno_troyes <- df_classe_troyes %>%
  dplyr::filter(Type_classe=="Technologique") %>%
  select(INE, Classement,Serie)

#Graphique
X11()
ggplot(df_techno_troyes) +
  aes(x = Classement, y = Serie) +
  geom_violin(fill = "#4682B4") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Classement", y = "Formations technologiques") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 17)) +
  scale_x_continuous(breaks = seq(0, max(df_techno_troyes$Classement), by = 100))

###Reims

#Pour les formations générales : focus sur les candidats qui ont fait spé maths

#Création d'une colonne regroupant les options : si le candidat à pris maths en complémentaire
df_classe_reims$Complement_maths <- apply(df_classe_reims[, c("Option1", "Option2", "Option3")], 1, function(row) {
  if ("Mathématiques Complémentaires" %in% row) {
    return("Mathématiques Complémentaires")
  } else {
    return("")  }
})

#Retirer les variables inutiles
df_classe_reims <- df_classe_reims %>%
select(-Option1, -Option2, -Option3)

#On ne garde que les candidats issus d'une formation générale
df_gene_reims <- df_classe_reims %>%
  dplyr::filter(Type_classe == "Générale")

#On créer uen dataframe permettant de faire un graphqiue correctement
df_gene_reims_maths <- df_gene_reims %>%
  mutate(Specialisation = case_when(
    Spe1_terminale == "MATHEMATIQUES" ~ "Spécialité 1",
    Spe2_terminale == "MATHEMATIQUES" ~ "Spécialité 2",
    Complement_maths == "Mathématiques Complémentaires" ~ "Complémentaire",
    TRUE ~ "Aucun"  # Si aucune des conditions n'est remplie
  )) %>%
  mutate(Abandon = case_when(
    Spe_abandon_terminale == "MATHEMATIQUES" ~ "Abandon",
    TRUE ~ "Pas d'abandon"))

#Avoir la moyenne de classement des groupes
df_gene_reims_maths$Spe_abandon <- paste(df_gene_reims_maths$Specialisation, df_gene_reims_maths$Abandon, sep="_")

moy_reims <- df_gene_reims_maths %>%
  group_by(Spe_abandon) %>%
  summarise(Moyenne = round(mean(Classement),0))

#Graphique
X11()
ggplot(df_gene_reims_maths, aes(x = Specialisation, y = Classement, fill = Abandon)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Enseignement en mathématiques", y = "Classement", fill = "Légende :") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 17)) +
  scale_fill_manual(values = c("#B22222", "#4682B4")) +
  scale_y_continuous(breaks = seq(0, max(df_gene_reims_maths$Classement), by = 250))

#Pour les formations technologiques

#Création de la dataframe
df_techno_reims <- df_classe_reims %>%
  dplyr::filter(Type_classe=="Technologique") %>%
  select(INE, Classement,Serie)

#Graphique
X11()
ggplot(df_techno_reims) +
  aes(x = Classement, y = Serie) +
  geom_violin(fill = "#4682B4") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Classement", y = "Formations technologiques") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 17)) +
  scale_x_continuous(breaks = seq(0, max(df_techno_reims$Classement), by = 250))

#Création de PNG

#Pour les graphiques des généraux
plot_reims <- ggplot(df_gene_reims_maths, aes(x = Specialisation, y = Classement, fill = Abandon)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Enseignement en mathématiques", y = "Classement") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  scale_y_continuous(breaks = seq(0, max(df_gene_reims_maths$Classement), by = 200)) +
  scale_fill_manual(values = c("#B22222", "#4682B4")) +
  guides(fill = FALSE)  # Désactiver la légende pour ce graphique

plot_troyes <- ggplot(df_gene_troyes_maths, aes(x = Specialisation, y = Classement, fill = Abandon)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Enseignement en mathématiques", y = NULL) +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  scale_y_continuous(breaks = seq(0, max(df_gene_troyes_maths$Classement), by = 100))+
  scale_fill_manual(values = c("#B22222", "#4682B4")) +
  guides(fill = guide_legend(title = "Légende :"))

# Organiser les graphiques dans une seule image
combined_plots <- grid.arrange(plot_reims, plot_troyes, ncol = 2)

# Enregistrer l'image dans un fichier PNG
ggsave("maths.png", combined_plots, width = 14, height = 7)

#Pour les formations technologiques
plot_techno_reims <- ggplot(df_techno_reims) +
  aes(x = Classement, y = Serie) +
  geom_violin(fill = "#4682B4") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = "Classement", y = "Filières technologiques") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 17, face = "bold"),
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, max(df_techno_reims$Classement), by = 200))

plot_techno_troyes <- ggplot(df_techno_troyes) +
  aes(x = Classement, y = Serie) +
  geom_violin(fill = "#4682B4") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black", position = position_dodge(width = 0.75)) +
  labs(x = NULL, y = "Filières technologiques") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 17, face = "bold"),
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(0, max(df_techno_troyes$Classement), by = 100))

# Organiser les graphiques dans une seule image
combined_plots_techno <- grid.arrange(plot_techno_reims, plot_techno_troyes, ncol = 2)

# Enregistrer l'image dans un fichier PNG
ggsave("techno.png", combined_plots_techno, width = 14, height = 7)

#Les deux tables des moyennes
moyennes <- merge(moy_reims, moy_troyes, by = "Spe_abandon", all = FALSE)

df_percentages <- moyennes %>%
  mutate(pourcentage_reims = Moyenne.x / max(df_classe_reims$Classement) * 100) %>%
  mutate(pourcentage_troyes = Moyenne.y / max(df_classe_troyes$Classement) * 100)

df_percentages$pourcentage_reims <- round(df_percentages$pourcentage_reims,0)
df_percentages$pourcentage_troyes <- round(df_percentages$pourcentage_troyes,0)