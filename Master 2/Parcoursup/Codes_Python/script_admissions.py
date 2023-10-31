import pandas as pd

def read_table(filename):
    return pd.read_table(filename, sep=';')

def drop_columns_by_prefix(df, prefix_list):
    for prefix in prefix_list:
        columns_to_drop = [col for col in df.columns if col.startswith(prefix)]
        df = df.drop(columns=columns_to_drop)
    return df

def create_data2_with_same_columns(data1, data2):
    data2 = data2[data1.columns]
    return data2

def drop_common_nan_columns(data1, data2):
    nan_columns_data1 = data1.columns[data1.isna().all()]
    nan_columns_data2 = data2.columns[data2.isna().all()]
    common_nan_columns = set(nan_columns_data1) & set(nan_columns_data2)
    data1 = data1.drop(columns=common_nan_columns)
    data2 = data2.drop(columns=common_nan_columns)
    return data1, data2

def save_to_csv(data, filename):
    data.to_csv(filename, index=False, sep=";")

data1 = read_table('TroyesAdmissions.csv')
data2 = read_table('ReimsAdmission.csv')

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

data1 = drop_columns_by_prefix(data1, prefixes_to_drop)
data2 = create_data2_with_same_columns(data1, data2)
data1, data2 = drop_common_nan_columns(data1, data2)

col_supp = [1] + list(range(5, 13))
data1 = data1.drop(columns=data1.columns[col_supp])

save_to_csv(data1, "TroyesAdmissions2.csv")
save_to_csv(data2, "ReimsAdmission2.csv")