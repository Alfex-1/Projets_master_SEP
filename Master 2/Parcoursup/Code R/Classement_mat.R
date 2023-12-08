#Environnement de travail et chargement des données
setwd("A:/New_data")
df_troyes <- read.csv("CandidatsTroyes2.csv", fileEncoding = "Windows-1252",sep=";")
df_reims <- read.csv("CandidatsReims2.csv", fileEncoding = "Windows-1252",sep=";")

#Packages
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(lmtest)
library(car)
library(gridExtra)
setwd("A:/Classement_matières")
#-----------------------------------#TROYES#-----------------------------------#

#On ne prend que ce qui nous intéresse et on modifie
df_troyes0 <- df_troyes %>%
  dplyr::filter(Type.Classe...Libellé.2022.2023 == "Générale")

df_troyes0 <- df_troyes0[, c(2, 7, 415:437)]

df_troyes0 <- df_troyes0 %>%
  select(-contains("Code"))

colonnes_a_traiter <- names(df_troyes0)[3:ncol(df_troyes0)]

df_troyes0 <- df_troyes0 %>%
  mutate_at(vars(colonnes_a_traiter), ~str_to_title(.)) %>%
  filter(if_any(all_of(colonnes_a_traiter), ~grepl("[[:alnum:]]", .))) %>%
  filter(INE.Candidat != "101066903BG")

df_mat_troyes <- df_troyes0[!(df_troyes0$Classement == "ENCF" | df_troyes0$Classement == "NC"), ]
df_mat_troyes$Classement <- as.numeric(df_mat_troyes$Classement)

df_mat_troyes <- df_mat_troyes[,1:5]

df_mat_troyes <- df_mat_troyes %>%
  mutate(
    EDS.Scolarité.Terminale.1...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Hist.geo. Geopolitique Sc.po. Hors Etab." ~ "Hist.-Geo. Geopolitique & Sc.politiques",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Humanites Litterature Philo Hors Etab." ~ "Humanites, Litterature Et Philosophie",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences Economiques Sociales A Distance" ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Llc Anglais Monde Contemporain" ~ "Llc En Anglais",
      TRUE ~ EDS.Scolarité.Terminale.1...Libellé.BCN
    ),
    EDS.Scolarité.Terminale.2...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Hist.geo. Geopolitique Sc.po. Hors Etab." ~ "Hist.-Geo. Geopolitique & Sc.politiques",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Humanites Litterature Philo Hors Etab." ~ "Humanites, Litterature Et Philosophie",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences Economiques Sociales A Distance" ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Mathematiques A Distance" ~ "Mathematiques",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences De La Vie & La Terre Hors Etab." ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences Economiques Sociales Hors Etab." ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Llc Anglais Monde Contemporain" ~ "Llc En Anglais",
      TRUE ~ EDS.Scolarité.Terminale.2...Libellé.BCN
    ),
    EDS.Scolarité.Terminale.Abandon...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Hist.geo. Geopolitique Sc.po. Hors Etab." ~ "Hist.-Geo. Geopolitique & Sc.politiques",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Humanites Litterature Philo Hors Etab." ~ "Humanites, Litterature Et Philosophie",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences Economiques Sociales A Distance" ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Llc Anglais Monde Contemporain" ~ "Llc En Anglais",
      TRUE ~ EDS.Scolarité.Terminale.Abandon...Libellé.BCN
    )
  )

df_mat_troyes <- df_mat_troyes %>%
  mutate(
    EDS.Scolarité.Terminale.1...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Llc En Anglais" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Llc En Espagnol" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Physique-Chimie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences De L'ingenieur Et Sc. Physiques" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences De La Vie Et De La Terre" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Biologie-Ecologie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Msgn/Marketing" ~ "MSGN",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Msgn/Rh Et Communication" ~ "MSGN",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Msgn/Gestion Et Finance" ~ "MSGN",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Numerique Et Sciences Informatiques" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences De Gestion Et Numerique" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Ingenierie  Innov. Et Develop.  Durable" ~ "SVT/Phys-Ch/Ing",
      TRUE ~ EDS.Scolarité.Terminale.1...Libellé.BCN
    ),
    EDS.Scolarité.Terminale.2...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Llc En Anglais" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Llc En Espagnol" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Physique-Chimie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences De L'ingenieur Et Sc. Physiques" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences De La Vie Et De La Terre" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Biologie-Ecologie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Msgn/Marketing" ~ "MSGN",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Msgn/Rh Et Communication" ~ "MSGN",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Msgn/Gestion Et Finance" ~ "MSGN",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Numerique Et Sciences Informatiques" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences De Gestion Et Numerique" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Histoire Des Arts" ~ "Art/Culture",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Cinema-Audiovisuel" ~ "Art/Culture",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Arts Plastiques" ~ "Art/Culture",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Ingenierie  Innov. Et Develop.  Durable" ~ "SVT/Phys-Ch/Ing",
      TRUE ~ EDS.Scolarité.Terminale.2...Libellé.BCN
    ),
    EDS.Scolarité.Terminale.Abandon...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Llc En Anglais" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Llc En Espagnol" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Physique-Chimie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences De L'ingenieur Et Sc. Physiques" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences De La Vie Et De La Terre" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Biologie-Ecologie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Msgn/Marketing" ~ "MSGN",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Msgn/Rh Et Communication" ~ "MSGN",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Msgn/Gestion Et Finance" ~ "MSGN",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Numerique Et Sciences Informatiques" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences De Gestion Et Numerique" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Ingenierie  Innov. Et Develop.  Durable" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Arts Plastiques" ~ "Art/Culture",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Litterature Et Lca Latin" ~ "Art/Culture",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences De L'ingenieur" ~ "SVT/Phys-Ch/Ing",
      TRUE ~ EDS.Scolarité.Terminale.Abandon...Libellé.BCN
    )
  )

#Tableau des moyennes des classements
rank_by_mat_troyes1 <- df_mat_troyes %>%
  group_by(EDS.Scolarité.Terminale.1...Libellé.BCN) %>%
  summarise(Moyenne = round(mean(Classement, na.rm = TRUE)),0)
rank_by_mat_troyes1 <- rank_by_mat_troyes1[,-3]

rank_by_mat_troyes2 <- df_mat_troyes %>%
  group_by(EDS.Scolarité.Terminale.2...Libellé.BCN) %>%
  summarise(Moyenne = round(mean(Classement, na.rm = TRUE)),0)
rank_by_mat_troyes2 <- rank_by_mat_troyes2[,-3]

rank_by_mat_troyesABON <- df_mat_troyes %>%
  group_by(EDS.Scolarité.Terminale.Abandon...Libellé.BCN) %>%
  summarise(Moyenne = round(mean(Classement, na.rm = TRUE)),0)
rank_by_mat_troyesABON <- rank_by_mat_troyesABON[,-3]

#------------------------------------#REIMS#------------------------------------#

#On ne prend que ce qui nous intéresse et on modifie
df_reims0 <- df_reims %>%
  dplyr::filter(Type.Classe...Libellé.2022.2023 == "Générale")

df_reims0 <- df_reims0[, c(2, 7, 415:430)]

df_reims0 <- df_reims0 %>%
  select(-contains("Code"))

df_reims0 <- df_reims0[,1:5]

colonnes_a_traiter <- names(df_reims0)[3:ncol(df_reims0)]

df_reims0 <- df_reims0 %>%
  mutate_at(vars(colonnes_a_traiter), ~str_to_title(.)) %>%
  filter(if_any(all_of(colonnes_a_traiter), ~grepl("[[:alnum:]]", .)))

df_reims0 <- df_reims0 %>%
  mutate_all(~na_if(.,""))

df_reims0 <- na.omit(df_reims0)

df_mat_reims <- df_reims0[!(df_reims0$Classement == "ENCF" | df_reims0$Classement == "NC"), ]
df_mat_reims$Classement <- as.numeric(df_mat_reims$Classement)

df_mat_reims <- df_mat_reims %>%
  mutate(
    EDS.Scolarité.Terminale.1...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Hist.geo. Geopolitique Sc.po. Hors Etab." ~ "Hist.-Geo. Geopolitique & Sc.politiques",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Humanites Litterature Philo Hors Etab." ~ "Humanites, Litterature Et Philosophie",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences Economiques Sociales A Distance" ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences Economiques Sociales Hors Etab." ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Llc Anglais Monde Contemporain" ~ "Llc En Anglais",
      TRUE ~ EDS.Scolarité.Terminale.1...Libellé.BCN
    ),
    EDS.Scolarité.Terminale.2...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Hist.geo. Geopolitique Sc.po. Hors Etab." ~ "Hist.-Geo. Geopolitique & Sc.politiques",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Humanites Litterature Philo Hors Etab." ~ "Humanites, Litterature Et Philosophie",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences Economiques Sociales A Distance" ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Mathematiques A Distance" ~ "Mathematiques",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences De La Vie & La Terre Hors Etab." ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences Economiques Sociales Hors Etab." ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Arts Plastiques Hors Etab." ~ "Arts Plastiques",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Numerique Sciences Info. Hors Etab." ~ "Numerique Et Sciences Informatiques",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Llc Anglais Monde Contemporain" ~ "Llc En Anglais",
      TRUE ~ EDS.Scolarité.Terminale.2...Libellé.BCN
    ),
    EDS.Scolarité.Terminale.Abandon...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Hist.geo. Geopolitique Sc.po. Hors Etab." ~ "Hist.-Geo. Geopolitique & Sc.politiques",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Humanites Litterature Philo Hors Etab." ~ "Humanites, Litterature Et Philosophie",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences Economiques Sociales A Distance" ~ "Sciences Economiques Et Sociales",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Llc Anglais Monde Contemporain" ~ "Llc En Anglais",
      TRUE ~ EDS.Scolarité.Terminale.Abandon...Libellé.BCN
    )
  )

df_mat_reims <- df_mat_reims %>%
  mutate(
    EDS.Scolarité.Terminale.1...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Llc En Anglais" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Llc En Espagnol" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Physique-Chimie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences De L'ingenieur Et Sc. Physiques" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences De La Vie Et De La Terre" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Biologie-Ecologie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Msgn/Marketing" ~ "MSGN",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Msgn/Rh Et Communication" ~ "MSGN",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Msgn/Gestion Et Finance" ~ "MSGN",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Numerique Et Sciences Informatiques" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Sciences De Gestion Et Numerique" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.1...Libellé.BCN == "Ingenierie  Innov. Et Develop.  Durable" ~ "SVT/Phys-Ch/Ing",
      TRUE ~ EDS.Scolarité.Terminale.1...Libellé.BCN
    ),
    EDS.Scolarité.Terminale.2...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Llc En Anglais" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Llc En Espagnol" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Physique-Chimie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences De L'ingenieur Et Sc. Physiques" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences De La Vie Et De La Terre" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Biologie-Ecologie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Msgn/Marketing" ~ "MSGN",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Msgn/Rh Et Communication" ~ "MSGN",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Msgn/Gestion Et Finance" ~ "MSGN",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Numerique Et Sciences Informatiques" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Sciences De Gestion Et Numerique" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Ingenierie  Innov. Et Develop.  Durable" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Histoire Des Arts" ~ "Art/Culture",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Cinema-Audiovisuel" ~ "Art/Culture",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Arts Plastiques" ~ "Art/Culture",
      EDS.Scolarité.Terminale.2...Libellé.BCN == "Theatre" ~ "Art/Culture",
      TRUE ~ EDS.Scolarité.Terminale.2...Libellé.BCN
    ),
    EDS.Scolarité.Terminale.Abandon...Libellé.BCN = case_when(
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Llc En Anglais" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Llc En Espagnol" ~ "LLC Anglais/Espagnol",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Physique-Chimie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences De L'ingenieur Et Sc. Physiques" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences De La Vie Et De La Terre" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Biologie-Ecologie" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Msgn/Marketing" ~ "MSGN",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Msgn/Rh Et Communication" ~ "MSGN",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Msgn/Gestion Et Finance" ~ "MSGN",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Numerique Et Sciences Informatiques" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences De Gestion Et Numerique" ~ "Sciences/Gestion numérique",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Ingenierie  Innov. Et Develop.  Durable" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Sciences De L'ingenieur" ~ "SVT/Phys-Ch/Ing",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Histoire Des Arts" ~ "Art/Culture",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Cinema-Audiovisuel" ~ "Art/Culture",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Arts Plastiques" ~ "Art/Culture",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Theatre" ~ "Art/Culture",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Danse" ~ "Art/Culture",
      EDS.Scolarité.Terminale.Abandon...Libellé.BCN == "Musique" ~ "Art/Culture",
      TRUE ~ EDS.Scolarité.Terminale.Abandon...Libellé.BCN
    )
  )

#Tableau des moyennes des classements
rank_by_mat_reims1 <- df_mat_reims %>%
  group_by(EDS.Scolarité.Terminale.1...Libellé.BCN) %>%
  summarise(Moyenne = round(mean(Classement, na.rm = TRUE)),0)
rank_by_mat_reims1 <- rank_by_mat_reims1[,-3]

rank_by_mat_reims2 <- df_mat_reims %>%
  group_by(EDS.Scolarité.Terminale.2...Libellé.BCN) %>%
  summarise(Moyenne = round(mean(Classement, na.rm = TRUE)),0)
rank_by_mat_reims2 <- rank_by_mat_reims2[,-3]

rank_by_mat_reimsABON <- df_mat_reims %>%
  group_by(EDS.Scolarité.Terminale.Abandon...Libellé.BCN) %>%
  summarise(Moyenne = round(mean(Classement, na.rm = TRUE)),0)
rank_by_mat_reimsABON <- rank_by_mat_reimsABON[,-3]

#Enregistrement de la dataframe
#write.csv(rank_by_mat_reims1, file = "Mat1.csv", fileEncoding = "Windows-1252")
#write.csv(rank_by_mat_reims2, file = "Mat2.csv", fileEncoding = "Windows-1252")
#write.csv(rank_by_mat_reims2, file = "Aban.csv", fileEncoding = "Windows-1252")

#Créer les graphiques
setwd("A:/Classement_matières")
library(readxl)
library(ggplot2)
moyenne_troyes <- as.data.frame(read_excel("Moyenne_classement_troyes.xlsx"))
moyenne_reims <- as.data.frame(read_excel("Moyenne_classement_reims.xlsx"))

rownames(moyenne_troyes) <- moyenne_troyes[, 1]
moyenne_troyes <- moyenne_troyes[,-1]

rownames(moyenne_reims) <- moyenne_reims[,1]
moyenne_reims <- moyenne_reims[,-1]

#Troyes
df <- data.frame(
  Spécialités = c("LLC Anglais/Espagnol", "Humanites, Litterature Et Philosophie", "Hist.-Geo. Geopolitique & Sc.politiques", 
                  "Mathématiques", "Sciences/Gestion du numérique", "Sciences Economiques Et Sociales", 
                  "SVT/Phys-Ch/Ing", "Art/Culture"),
  `Spécialité 1` = c(402, 429, 326, 404, 459, 302, 405, NA),
  `Spécialité 2` = c(294, 628, 562, 298, 311, 351, 408, 417),
  `Spécialité abandonnée` = c(NA, 397, 312, 330, 433, 452, 382, 420)
)

# Transformer le format de la dataframe pour ggplot2
df_long <- tidyr::gather(df, key = "Spécialité", value = "Moyenne", -Spécialités)

# Créer l'histogramme groupé avec ggplot2
moy_troyes <- ggplot(df_long, aes(x = Spécialités, y = Moyenne, fill = Spécialité)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Spécialités des candidats en terminale",
       y = "Classement moyen",
       fill = NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=7),
        legend.position = "bottom")+
  scale_y_continuous(breaks = seq(0, max(df_long$Moyenne, na.rm = TRUE), by = 100))

moy_troyes <- moy_troyes + 
  geom_text(aes(label = ifelse(!is.na(Moyenne), sprintf("%.0f", Moyenne), "")), 
            position = position_dodge2(width = 0.9, preserve = "single"), vjust = -0.5,size=2.5)

#Reims
df_nouveau <- data.frame(
  Spécialités = c("LLC Anglais/Espagnol", "Humanites, Litterature Et Philosophie", "Hist.-Geo. Geopolitique & Sc.politiques", 
                  "Mathematiques", "Sciences/Gestion numérique", "SVT/Phys-Ch/Ing", 
                  "Sciences Economiques Et Sociales", "Droit Et Economie", "Art/Culture"),
  `Spécialité 1` = c(883, 1100, 925, 893, 1080, 1071, 699, 920, NA),
  `Spécialité 2` = c(1104, NA, 1060, 642, 1032, 967, 903, 1590, 1060),
  `Spécialité abandonnée` = c(733, 969, 717, 861, 1049, 872, 921, NA, NA)
)

df_nouveau_long <- tidyr::gather(df_nouveau, key = "Spécialité", value = "Moyenne", -Spécialités)

moy_reims <- ggplot(df_nouveau_long, aes(x = Spécialités, y = Moyenne, fill = Spécialité)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL,
       y = "Classement moyen",
       fill = NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=7),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(0, max(df_nouveau_long$Moyenne, na.rm = TRUE), by = 200))

moy_reims <- moy_reims + 
  geom_text(aes(label = ifelse(!is.na(Moyenne), sprintf("%.0f", Moyenne), "")), 
            position = position_dodge2(width = 0.9, preserve = "single"), vjust = -0.5,size=2.5)

# Organiser les graphiques dans une seule image
library(gridExtra)
combined_plots_spe <- grid.arrange(moy_reims, moy_troyes, ncol = 1)

# Enregistrer l'image dans un fichier PNG
ggsave("specialites.png", combined_plots_spe, width = 14, height = 7)

#Renomer les variables
colnames(df_mat_troyes) <- c("Classement","INE Candidat","Spé1","Spé2","SpéAbandonée")
colnames(df_mat_reims) <- c("Classement","INE Candidat","Spé1","Spé2","SpéAbandonée")