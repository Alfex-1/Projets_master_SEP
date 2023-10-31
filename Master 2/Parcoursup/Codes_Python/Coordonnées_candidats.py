import folium
from folium.plugins import HeatMap
from folium.plugins import MarkerCluster
import pandas as pd
import numpy as np

RCandi = pd.read_table('CandidatsReims2.csv', sep=';')
TCandi = pd.read_table('CandidatsTroyes2.csv', sep=';')
Villes = pd.read_table('Villes_coordonnes.csv',sep=';',encoding='ISO-8859-1')

Villes['Latitude'] = Villes['Latitude'].str.replace(',', '.')
Villes['Longitude'] = Villes['Longitude'].str.replace(',', '.')
columns_to_keep = ["INE Candidat", "Commune Etablissement origine - Libellé 2022/2023"]
RCandi_geo = RCandi[columns_to_keep]
TCandi_geo = TCandi[columns_to_keep]

#Fusion de Reims et Troyes : cartes + données
combined_df = pd.concat([TCandi_geo, RCandi_geo], ignore_index=True)
Coordonnes = pd.merge(Villes, combined_df, on='Commune Etablissement origine - Libellé 2022/2023', how='inner')

#Reims : cartes + données
Coordonnes_R = pd.merge(Villes, RCandi_geo, on='Commune Etablissement origine - Libellé 2022/2023', how='inner')

#Troyes : cartes + données
Coordonnes_T = pd.merge(Villes, TCandi_geo, on='Commune Etablissement origine - Libellé 2022/2023', how='inner')

Coordonnes.to_csv("Coordonnees.csv", index=False, sep=";", encoding="utf-8")
Coordonnes_R.to_csv("Coordonnees_R.csv", index=False, sep=";", encoding="utf-8")
Coordonnes_T.to_csv("Coordonnees_T.csv", index=False, sep=";", encoding="utf-8")













