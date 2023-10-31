import pandas as pd

import pandas as pd

def read_and_clean_data(filename, columns_to_drop, cols_to_drop_manually):
    data = pd.read_table(filename, sep=';', encoding='ISO-8859-1')
    
    # Supprimer toutes les variables vides communes
    nan_columns = data.columns[data.isna().all()]
    data = data.drop(columns=nan_columns)
    
    # Supprimer les colonnes spécifiques
    for col in columns_to_drop:
        data = data.drop(columns=col)
    
    # Variables à supprimer manuellement
    data = data.drop(columns=data.columns[cols_to_drop_manually])

    return data

def merge_and_clean_geo_data(data, Villes, columns_to_keep):
    dataX = data[columns_to_keep]
    
    dataX['Commune Etablissement origine - Libellé 2022/2023'] = dataX['Commune Etablissement origine - Libellé 2022/2023'].str.strip()
    
    geo_data1 = pd.merge(dataX, Villes, on='Commune Etablissement origine - Libellé 2022/2023', how='inner')
    geo_data = geo_data1.dropna(subset=['Commune Etablissement origine - Libellé 2022/2023'])
    
    return geo_data

def main():
    RPC = read_and_clean_data('PCReims2.csv', columns_to_drop=[], cols_to_drop_manually=list(range(4, 12)))
    TPC = read_and_clean_data('PCTroyes2.csv', columns_to_drop=[], cols_to_drop_manually=list(range(4, 12))
    
    Villes = pd.read_table('Villes_coordonnes.csv', sep=';', encoding='Windows-1252')

    columns_to_keep = ["INE Candidat", "Commune Etablissement origine - Libellé 2022/2023", "Situation du voeu - Libellé"]
    R_geo = merge_and_clean_geo_data(RPC, Villes, columns_to_keep)
    T_geo = merge_and_clean_geo_data(TPC, Villes, columns_to_keep)

    Test1 = R_geo[(R_geo["Pays"]=="France") & (R_geo["Situation du voeu - Libellé"]=="Formation acceptée")]

    RAdmin = read_and_clean_data('ReimsAdmissions2.csv', columns_to_drop=[], cols_to_drop_manually=[])
    TAdmin = read_and_clean_data('TroyesAdmissions2.csv', columns_to_drop=[], cols_to_drop_manually=[])

    columns_to_keep = ["INE Candidat", 'Commune Etablissement origine - Libellé 2022/2023', "Type admission - Libellé"]
    R_phases = merge_and_clean_geo_data(RAdmin, Villes, columns_to_keep)
    T_phases = merge_and_clean_geo_data(TAdmin, Villes, columns_to_keep)

    R_phases = R_phases.dropna(subset=['Latitude'])
    T_phases = T_phases.dropna(subset=['Latitude'])

    Test2 = R_phases[(R_phases["Pays"]=="France") & (R_phases["Type admission - Libellé"]=="Phase complémentaire")]

if __name__ == "__main__":
    main()