import pandas as pd

def read_table(filename):
    return pd.read_table(filename, sep=';')

def merge_tables(left, right, on_column):
    return pd.merge(left, right, on=on_column, how='inner')

def drop_common_nan_columns(df1, df2):
    nan_col_df1 = df1.columns[df1.isna().all()]
    nan_col_df2 = df2.columns[df2.isna().all()]
    common_nan_columns = set(nan_col_df1) & set(nan_col_df2)
    return df1.drop(columns=common_nan_columns), df2.drop(columns=common_nan_columns)

def drop_duplicate_columns(df):
    duplicate_columns = df.columns[df.T.duplicated(keep='first')]
    return df.drop(columns=duplicate_columns)

TAdmi = read_table('TroyesAdmissions2.csv')
RAdmi = read_table('ReimsAdmissions2.csv')
RCandi = read_table('CandidatsReims2.csv')
TCandi = read_table('CandidatsTroyes2.csv')
RPC = read_table('PCReims2.csv')
TPC = read_table('PCTroyes2.csv')

TAC = merge_tables(TAdmi, TCandi, 'INE Candidat')
RAC = merge_tables(RAdmi, RCandi, 'INE Candidat')

Reims = merge_tables(RAdmi, RPC, 'INE Candidat')
Troyes = merge_tables(TAdmi, TPC, 'INE Candidat')

Troyes, Reims = drop_common_nan_columns(Troyes, Reims)
TAC, RAC = drop_common_nan_columns(TAC, RAC)

Troyes = drop_duplicate_columns(Troyes)
Reims = drop_duplicate_columns(Reims)

Troyes.to_csv("Troyes_Admin_PC.csv", index=False, sep=";", encoding="utf-8")
Reims.to_csv("Reims_Admin_PC.csv", index=False, sep=";", encoding="utf-8")
TAC.to_csv("Troyes_Admin_Candi.csv", index=False, sep=";", encoding="utf-8")
RAC.to_csv("Reims_Admin_Candi.csv", index=False, sep=";", encoding="utf-8")