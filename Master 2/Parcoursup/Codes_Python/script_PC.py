import pandas as pd

def read_and_clean_data(filename):
    data = pd.read_table(filename, sep=';')
    
    # Supprimer toutes les variables vides communes
    nan_columns = data.columns[data.isna().all()]
    data = data.drop(columns=nan_columns)
    
    # Supprimer toutes les colonnes avec des préfixes spécifiques
    prefixes_to_drop = [
        "Appréciation",
        "Origine de la saisie",
        "Moyenne Basse Classe",
        "Moyenne Haute Classe",
        "Matière - Code BCN",
        "Moyenne du Candidat en Composition",
        "Département Etablissement origine - Libellé",
        "Département Etablissement origine - Code",
        "Spécialité / Mention - Code",
        "Série de classe - Libellé ",
        "Dominante (bac non réformé)",
        "Classe",
        "Note de l'épreuve.",
        "Dominante (bac réformé)",
        "Epreuve - Libelle.",
        "Epreuve - Code.",
        "Note remontée par Cyclades",
        "Notation",
        "Moyenne du Candidat en DNL", 
        "Matière - Libellé en DNL",
        "Statut de la matière - Libellé en DNL",
        "Matière - Code",
        "Candidat scolarisé au début de l'année - Libellé",
        "Candidat scolarisé au début de l'année - Code",
        "Scolarité suivi dans l'hémisphère sud - Libellé",
        "Formation suivie en apprentissage - Libellé",
        "Scolarité suivi dans l'hémisphère sud - Code",
        "Formation suivie en apprentissage - Code",
        "Eléments liés à la scolarité/activité",
        "Commune Etablissement origine - Code",
        "UAI Etablissement origine",
        "Type de contrat établissement d'origine - Code",
        "Type Etablissement d'origine - Code",
        "Classe candidat - Code",
        "Langue vivante C scolarité - Libellé",
        "Langue vivante C scolarité - Code",
        "Langue vivante B scolarité - Code",
        "Langue vivante A scolarité - Code",
        "Section linguistique  Scolarité",
        "Code MEF Scolarité",
        "Dominante (bac non réformé)- Code",
        "Filiere (pour scolarité du supérieur)- Code",
        "Type Formation - Code",
        "Niveau Etude - Code",
        "Type de Scolarité - Code",
        "Année Scolaire - Code",
        "Option facultative 4",
        "Option facultative 3",
        "Option facultative 2",
        "Moyenne du Candidat en Modes conventionnels",
        "Statut de la matière - Code",
        "Moyenne classe Candidat",
        "Matière - Libellé BCN",
        "Moyenne du Candidat en Composition"
    ]
    
    for prefix in prefixes_to_drop:
        columns_to_drop = [col for col in data.columns if col.startswith(prefix)]
        data = data.drop(columns=columns_to_drop)
        
    return data

def save_to_csv(data, filename):
    data.to_csv(filename, index=False, sep=";", encoding="utf-8")

data5 = read_and_clean_data('PCReims.csv')
data6 = read_and_clean_data('PCTroyes.csv')

# Copier-coller de la table
data6 = data6[data5.columns]

# Conversion des dataframes en CSV
save_to_csv(data5, "PCReims2.csv")
save_to_csv(data6, "PCTroyes2.csv")
