import pandas as pd

def read_and_clean_data(filename):
    data = pd.read_table(filename, sep=';')
    
    # Supprimer toutes les variables vides communes
    nan_columns = data.columns[data.isna().all()]
    data = data.drop(columns=nan_columns)
       
    return data

def save_to_csv(data, filename):
    data.to_csv(filename, index=False, sep=";", encoding="utf-8")

data3 = read_and_clean_data('CandidatsReims.csv')
data4 = read_and_clean_data('CandidatsTroyes.csv')

# Copier-coller de la table
data4 = data4[data3.columns]

# Conversion des dataframes en CSV
save_to_csv(data3, "CandidatsReims2.csv")
save_to_csv(data4, "CandidatsTroyes2.csv")
