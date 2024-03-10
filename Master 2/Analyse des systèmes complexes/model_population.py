#!/usr/bin/env python
# coding: utf-8

# # Modélisation de la population

# In[1]:


import random
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
random.seed(1998)


# ## Modèle

# ### Fonctions régissant la dynamique de population
# 
# - population initiale de n (3928 par défaut)
# - proba de mourir fonction de l'âge données de l'INSEE pour la France en 2022
# - une femme donne naissance si elle a entre 15 et 50 ans avec un probabilité égale au nombre d'enfant par femme désiré divisé par le nombre d'année de fertilité
# - possible arrivé d'une pop extérieure

# In[2]:


def pop_initiale(n=3928):
    """Création d'une population initiale suivant une distribution normale

    Args:
        n (int, optional): Nombre d'individus. Par défaut, n est 3928.

    Returns:
        df: Jeu de données de la population initiale sous forme de DataFrame pandas
    """
    # Génération des identifiants uniques pour chaque individu
    id = [str(i) for i in range(1, n+1)]

    # Génération aléatoire des âges selon une distribution normale
    mean_age = 35
    std_dev_age = 4
    ages = np.round(np.random.normal(mean_age, std_dev_age, n))

    # Tronquer les âges pour qu'ils restent dans la plage [15, 50]
    ages = np.clip(ages, 15, 50)

    # Attribution aléatoire du sexe ('M' pour masculin, 'F' pour féminin) pour chaque individu
    sexe = [random.choice(['M', 'F']) for _ in range(len(id))]

    # Tous les individus sont initialisés comme vivants au début
    en_vie = [True for _ in range(len(id))]
    
    population_df = pd.DataFrame({'id': id, 'Age': ages.astype(int), 'Sexe': sexe, 'Statut': en_vie})

    # Création d'un DataFrame pandas avec les données générées
    return population_df


# In[3]:


# Visualisation graphique de la distribution des âges
population_df=pop_initiale()
plt.figure(figsize=(10, 6))
plt.hist(population_df['Age'], bins=30, edgecolor='black', color='skyblue')
plt.title('Distribution des âges dans la population initiale')
plt.xlabel('Âge')
plt.ylabel("Nombre d'individus")
plt.show()


# In[4]:


def death(age):
    """Détermine si la personne meurt chaque année en fonction de son âge.

    Args:
        age (int): Âge de la personne.

    Returns:
        bool: True si la personne survit, False si elle décède.
    """
    # Détermination de la probabilité de décès en fonction de l'âge
    if age < 1:
        proba = 0.003
    elif age >=1 and age <=4:
        proba=0.0002
    elif age >=5 and age <=14:
        proba=0.0001
    elif age >=15 and age <=19:
        proba=0.0002
    elif age >=20 and age <=29:
        proba=0.0004
    elif age >=30 and age <=34:
        proba=0.0006
    elif age >=35 and age <=39:
        proba=0.0008
    elif age >=40 and age <=44:
        proba=0.0012
    elif age >=45 and age <=49:
        proba=0.002
    elif age >=50 and age <=54:
        proba=0.0032
    elif age >=55 and age <=59:
        proba=0.005
    elif age >=60 and age <=64:
        proba=0.0077
    elif age >=65 and age <=69:
        proba=0.0112
    elif age >=70 and age <=79:
        proba=0.0196
    elif age >=80 and age <=89:
        proba=0.0627
    else :proba=0.1995

# source : https://www.ined.fr/fr/tout-savoir-population/chiffres/france/mortalite-cause-deces/taux-mortalite-sexe-age/

    # Simulation du décès en fonction de la probabilité calculée
    if random.random() < proba:
        return False  # La personne décède
    else:
        return True   # La personne survit
    
def birth(sexe, age, nb_enf=2):
    """Détermine si une personne donne naissance à un enfant.

    Args:
        sexe (str): Sexe de la personne (M ou F).
        age (int): Âge de la personne.
        nb_enf (int, optional): Nombre moyen d'enfants par femme. Par défaut, nb_enf est 2.

    Returns:
        bool: True si la personne donne naissance à un enfant, False sinon.
    """
    # Vérification des conditions pour la naissance
    if sexe == "M" or (age < 18 or age > 45) :
        return False
    else:
        # Simulation de la naissance en fonction du nombre moyen d'enfants par femme
        if random.random() * 28 < nb_enf:
            return True  # La personne donne naissance à un enfant
        else:
            return False  # La personne ne donne pas naissance à un enfant


# ### Fonctions pour estimer l'évolution de la pop

# In[5]:


def evolution_tp1(pop_init, naissances=True, nb_enf=4, pop_exter=False, n_ext=0):
    """Calcule l'évolution de la population annuelle

    Args:
        pop_init (pd.DataFrame): Population de base (id, Age, Sexe, Statut)
        naissances (bool, optional): Il y a-t-il des naissances ? Defaults to True.
        nb_enf (int, optional): Nombre d'enfants par femme. Defaults to 4.
        pop_exter (bool, optional): Arrivée de population externe ? Defaults to False.
        n_ext (int, optional): Nombre d'individus dans la population externe. Defaults to 0.

    Returns:
        pd.DataFrame: Nouvelle population en t+1
    """
    pop = pop_init.copy()
    
    # Augmentation de l'âge de chaque individu de 1 an
    pop['Age'] = pop['Age'] + 1
    
    # Simulation du statut de vie (mort ou pas) pour chaque individu
    pop['Statut'] = pop['Age'].apply(death)
    
    if naissances == False:  # Pas de naissance, seulement évolution de la population par mortalité
        return pop[pop['Statut'] == True]
    
    # Simulation des naissances pour chaque femme en fonction de l'âge et du nombre moyen d'enfants
    pop['Naissance'] = pop.apply(lambda x: birth(x['Sexe'], x['Age'], nb_enf), axis=1)
    
    # Création d'un jeu de données pour les nouveaux-nés
    pop_new = pop[pop['Naissance'] == True][['id', 'Age', 'Sexe']]
    pop_new['id'] = pop_new['id'] + "_1" 
    pop_new['Age'] = 0
    pop_new['Sexe'] = pop_new['Sexe'].apply(lambda x: random.choice(['M', 'F']))
    pop_new['Statut'] = True
    
    # Concaténation des données de la population existante, des nouveaux-nés et éventuellement de la population externe
    if pop_exter == False:
        return pd.concat([pop[pop['Statut']].drop('Naissance', axis=1), pop_new], ignore_index=True)
    else:
        pop_exter = pop_exter(n=n_ext)
        return pd.concat([pop[pop['Statut']].drop('Naissance', axis=1), pop_new, pop_exter(n_ext)], ignore_index=True)


# In[6]:


def evo_n_year(pop_init, years_to_simulate, naissances=True, nb_enf=4, pop_exter=False, n_ext=0):
    """Retourne la population après n années d'évolution avec la fonction précédente

    Args:
        pop_init (_type_): Population initiale (DataFrame).
        years_to_simulate (int): Nombre d'années à simuler.
        naissances (bool, optional): Indique s'il y a des naissances. Defaults to True.
        nb_enf_pf (int, optional): Nombre d'enfants par femme. Defaults to 4.

    Returns:
        dict: Un dictionnaire contenant le DataFrame de la population et un DataFrame de la taille de la population au fil du temps.
    """
    pop = pop_init.copy()
    pop_sizes = {'time': [], 'total': [], '0_20': [], '20_40': [], '40_60': [], '60_80': [], '80p': []}

    for i in range(years_to_simulate):
        pop_sizes['time'].append(i) 
        pop_sizes['total'].append(len(pop))
        pop_sizes['0_20'].append(len(pop[(pop['Age'] >= 0) & (pop['Age'] < 20)]))
        pop_sizes['20_40'].append(len(pop[(pop['Age'] >= 20) & (pop['Age'] < 40)])) 
        pop_sizes['40_60'].append(len(pop[(pop['Age'] >= 40) & (pop['Age'] < 60)])) 
        pop_sizes['60_80'].append(len(pop[(pop['Age'] >= 60) & (pop['Age'] < 80)]))
        pop_sizes['80p'].append(len(pop[(pop['Age'] >= 80)]))
        pop = evolution_tp1(pop, naissances, nb_enf, pop_exter, n_ext)

    return {"pop_df": pop, "size_df": pd.DataFrame(pop_sizes)}


# ## Exemples

# ### Exemple 1 - pas de naissances

# In[7]:


def stack_plot_evo_by_age(data):
    plt.figure(figsize=(10, 6))

    # Define the data for the stacked area chart
    x = data["size_df"]['time']
    y1 = data["size_df"]['0_20']
    y2 = data["size_df"]['20_40']
    y3 = data["size_df"]['40_60']
    y4 = data["size_df"]['60_80']
    y5 = data["size_df"]['80p']

    # Create the stacked area chart
    plt.stackplot(x, y1, y2, y3, y4, y5, labels=['0-20', '20-40', '40-60', '60-80', '80+'], colors=sns.color_palette('magma', 5), alpha=1)

    plt.xlabel('Années')
    plt.ylabel('Taille de la population')
    plt.title("Tailles des tranches d'âge par année")

    plt.legend()
    return plt


# In[9]:


def stack_plot_evo_by_age(data):
    plt.figure(figsize=(10, 6))

    # Define the data for the stacked area chart
    x = data["size_df"]['time']
    y1 = data["size_df"]['0_20']
    y2 = data["size_df"]['20_40']
    y3 = data["size_df"]['40_60']
    y4 = data["size_df"]['60_80']
    y5 = data["size_df"]['80p']

    # Create the stacked area chart
    plt.stackplot(x, y1, y2, y3, y4, y5, labels=['0-20', '20-40', '40-60', '60-80', '80+'], colors=sns.color_palette('magma', 5), alpha=1)

    plt.xlabel('Années')
    plt.ylabel('Taille de la population')
    plt.title("Population par âge sur 100 ans dans le cas où aucun enfant par femme n'est autorisé")

    plt.legend()
    return plt
pop_init = pop_initiale()
evo_no_birth = evo_n_year(pop_init, 100, naissances=False, nb_enf=0)
stack_plot_evo_by_age(evo_no_birth).show()


# ### Exemple 2 - 1 enfant par femme

# In[10]:


def stack_plot_evo_by_age(data):
    plt.figure(figsize=(10, 6))

    # Define the data for the stacked area chart
    x = data["size_df"]['time']
    y1 = data["size_df"]['0_20']
    y2 = data["size_df"]['20_40']
    y3 = data["size_df"]['40_60']
    y4 = data["size_df"]['60_80']
    y5 = data["size_df"]['80p']

    # Create the stacked area chart
    plt.stackplot(x, y1, y2, y3, y4, y5, labels=['0-20', '20-40', '40-60', '60-80', '80+'], colors=sns.color_palette('magma', 5), alpha=1)

    plt.xlabel('Années')
    plt.ylabel('Taille de la population')
    plt.title("Population par âge sur 100 ans dans le cas où un seul enfant par femme est autorisé")

    plt.legend()
    return plt
evo_1child = evo_n_year(pop_init, 100, naissances=True, nb_enf=1)
stack_plot_evo_by_age(evo_1child).show()


# ### Exemple 3 - 2 enfants par femme

# In[11]:


def stack_plot_evo_by_age(data):
    plt.figure(figsize=(10, 6))

    # Define the data for the stacked area chart
    x = data["size_df"]['time']
    y1 = data["size_df"]['0_20']
    y2 = data["size_df"]['20_40']
    y3 = data["size_df"]['40_60']
    y4 = data["size_df"]['60_80']
    y5 = data["size_df"]['80p']

    # Create the stacked area chart
    plt.stackplot(x, y1, y2, y3, y4, y5, labels=['0-20', '20-40', '40-60', '60-80', '80+'], colors=sns.color_palette('magma', 5), alpha=1)

    plt.xlabel('Années')
    plt.ylabel('Taille de la population')
    plt.title("Population par âge sur 100 ans dans le cas où deux enfants par femme sont autorisés")

    plt.legend()
    return plt
evo_2child = evo_n_year(pop_init, 100, naissances=True, nb_enf=2)
stack_plot_evo_by_age(evo_2child).show()


# ### Exemple 4 - 3 enfants par femme

# In[12]:


def stack_plot_evo_by_age(data):
    plt.figure(figsize=(10, 6))

    # Define the data for the stacked area chart
    x = data["size_df"]['time']
    y1 = data["size_df"]['0_20']
    y2 = data["size_df"]['20_40']
    y3 = data["size_df"]['40_60']
    y4 = data["size_df"]['60_80']
    y5 = data["size_df"]['80p']

    # Create the stacked area chart
    plt.stackplot(x, y1, y2, y3, y4, y5, labels=['0-20', '20-40', '40-60', '60-80', '80+'], colors=sns.color_palette('magma', 5), alpha=1)

    plt.xlabel('Années')
    plt.ylabel('Taille de la population')
    plt.title("Population par âge sur 100 ans dans le cas où trois enfants par femme sont autorisés")

    plt.legend()
    return plt
evo_3child = evo_n_year(pop_init, 100, naissances=True, nb_enf=3)
stack_plot_evo_by_age(evo_3child).show()


# ### Exemple 5 - 4 enfants par femme

# In[13]:


def stack_plot_evo_by_age(data):
    plt.figure(figsize=(10, 6))

    # Define the data for the stacked area chart
    x = data["size_df"]['time']
    y1 = data["size_df"]['0_20']
    y2 = data["size_df"]['20_40']
    y3 = data["size_df"]['40_60']
    y4 = data["size_df"]['60_80']
    y5 = data["size_df"]['80p']

    # Create the stacked area chart
    plt.stackplot(x, y1, y2, y3, y4, y5, labels=['0-20', '20-40', '40-60', '60-80', '80+'], colors=sns.color_palette('magma', 5), alpha=1)

    plt.xlabel('Années')
    plt.ylabel('Taille de la population')
    plt.title("Population par âge sur 100 ans dans le cas où quatre enfants par femme sont autorisés")

    plt.legend()
    return plt
evo_4child = evo_n_year(pop_init, 100, naissances=True, nb_enf=4)
stack_plot_evo_by_age(evo_4child).show()


# ### Exemple 6 - 2 enfants par femme long terme

# In[14]:


def stack_plot_evo_by_age(data):
    plt.figure(figsize=(10, 6))

    # Define the data for the stacked area chart
    x = data["size_df"]['time']
    y1 = data["size_df"]['0_20']
    y2 = data["size_df"]['20_40']
    y3 = data["size_df"]['40_60']
    y4 = data["size_df"]['60_80']
    y5 = data["size_df"]['80p']

    # Create the stacked area chart
    plt.stackplot(x, y1, y2, y3, y4, y5, labels=['0-20', '20-40', '40-60', '60-80', '80+'], colors=sns.color_palette('magma', 5), alpha=1)

    plt.xlabel('Années')
    plt.ylabel('Taille de la population')
    plt.title("Population par âge sur 2500 ans dans le cas où deux enfants par femme sont autorisés")

    plt.legend()
    return plt
evo_2child_lr = evo_n_year(pop_init, 2500, naissances=True, nb_enf=2)
stack_plot_evo_by_age(evo_2child_lr).show()


# ### Exemple 7 - 3 enfants par femme long terme

# In[11]:


def stack_plot_evo_by_age(data):
    plt.figure(figsize=(10, 6))

    # Define the data for the stacked area chart
    x = data["size_df"]['time']
    y1 = data["size_df"]['0_20']
    y2 = data["size_df"]['20_40']
    y3 = data["size_df"]['40_60']
    y4 = data["size_df"]['60_80']
    y5 = data["size_df"]['80p']

    # Create the stacked area chart
    plt.stackplot(x, y1, y2, y3, y4, y5, labels=['0-20', '20-40', '40-60', '60-80', '80+'], colors=sns.color_palette('magma', 5), alpha=1)

    plt.xlabel('Années')
    plt.ylabel('Taille de la population')
    plt.title("Population par âge sur 500 ans dans le cas où trois enfants par femme sont autorisés")

    plt.legend()
    return plt
pop_init = pop_initiale()
evo_3child_lr = evo_n_year(pop_init,500, naissances=True, nb_enf=3)
stack_plot_evo_by_age(evo_3child_lr).show()

