#Importation des packages n�cessaires pour tout le projet
from bs4 import BeautifulSoup
import requests
import pandas as pd
import time
import numpy as np
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
from sklearn.preprocessing import PolynomialFeatures

def crawler(iteration,final_time):#D�finition du crawler qui va extraire les donn�es
    """
    Fonction qui r�cup�re les donn�es � partir d'un site internet en continue
    jusqu'� un certain moment.

    Parameters
    ----------
    interation : int
        Indique les intervalles de temps auxquels le crawler doit r�cup�rer
        les donn�es. Par d�faut, l'iteration est de 30 secondes. Donc interation = 30.
    final_time : int
        Indique le moment auquel le crawler cesse de r�cup�rer les donn�es.
        Il est exprim� en secondes et doit >= 30 pour avoir au moins une observation.

    Returns
    -------
    Cette fonction retourne les observations g�n�r�es, sous forme de liste.

    """
    #Initialisation de la liste et de la fonction time.
    Ens_obs=[]
    start=time.time()
    #Lancement de la boucle "tant que":
    while time.time()-start<final_time:
        freq=time.time()
        #R�cup�ration de la page internet et de la zone (et les valeurs) qui nous int�resse.
        pg=requests.get('https://www.boursorama.com/bourse/actions/cotations/?quotation_az_filter%5Bmarket%5D=1rPCAC&quotation_az_filter%5Bletter%5D=&quotation_az_filter%5Bfilter%5D=&pagination_603981177=')
        soup=BeautifulSoup(pg.content,'html.parser')
        Page=soup.find_all(class_='c-table__row')
        #Suppression de la premi�re colonne car elle n'apporte aucune information...
        del Page[0]
        #Ajout des observations de la page internet dans la liste initialis�e au pr�alable.
        Ens_obs.append([Page,time.strftime('%H:%M:%S',time.localtime())])
        time.sleep(freq+iteration-time.time()) #Pour �viter que le crawler ne r�cup�re que des doublons.
    return(Ens_obs)

def Observations(contents,hcont,dcont):
    """
    Cette fonction place les donn�es extraites par le crawler au m�me niveau
    que leur intilu� g�n�ral.

    Parameters
    ----------
    contents : list
        Donn�es obtenues par le crawler
    hcont : time
        Heure � laquelle les donn�es ont �t� extraites.
    dcont : date
        Date � laquelle les donn�es ont �t� exrtraites.

    Returns
    -------
    Cette fonction retourne les m�mes variables que le site internet
    avec la date et l'heure auxquelles les donn�es ont �t� extraites.

    """
	#Initialisation des composants de la liste
    Libele=['']*len(contents)
    Dernier=['']*len(contents)
    Ex_rt=['']*len(contents)
    Hour=[hcont]*len(contents)
    Date=[dcont]*len(contents)
    #Lancement d'une boucle "pour" pour r�cup�rer les donn�es pour
    #chaque observations puis traitement des donn�es pour qu'elles soient
    #utilisables
    for i in range(len(contents)):
        contents[i]=contents[i].get_text()
        contents[i]=contents[i].replace('\n                                    SRD\n                               ','')
        contents[i]=contents[i].replace('\n                                        \n                   ','')
    #Traitement de texte pour que chaque donn�es soient plac�es � la bonne
    #place dans un but esth�tique mais aussi pratique.
    for i in range (len(contents)):
        colonne=0
        for j in contents[i]:
            if (j in '0123456789') and (colonne==0):
                colonne=10
            if j == '.':
                colonne+=5
            if int(colonne/10)<colonne/10:
                colonne+=1
            if j == '*':
                colonne+=10
            if colonne==0:
                Libele[i]+=j
            if (colonne<20) & (colonne>=10):
                Dernier[i]+=j
            if (colonne<30) & (colonne>=20):
                Ex_rt[i]+=j
            #Suppression des caract�res ind�sirables
            Libele[i]=Libele[i].replace(' ','')
            Ex_rt[i]=Ex_rt[i].replace(' (c) ','')
            Ex_rt[i]=Ex_rt[i].replace('%','')
    return(Libele,Ex_rt,Hour,Date) #On ne retourne que les �l�ments qui nous int�resse

def df_com(Ens_obs):
    """
    Cette fonction transforme les donn�es que l'on a extraite gr�ce au
    crawler qui sont en format list dans le format dataframe.

    Parameters
    ----------
    Ens_obs : list
        Observation que l'on a obtenu gr�ce au crawler et qui sont
        en format list.

    Returns
    -------
    Cette fonction retourne la dataframe avec son nom 'Companies_dta'.

    """
    #Initialisation des varaibles
    list_df=[]
    date=time.strftime('%d/%m/%Y',time.localtime())
    final_df=pd.DataFrame({'Libele':[],'Ex_rt':[],'Hour':[],'Date':[]})
    #Lancement d'une boucle "pour" qui compile les informations
    for i in Ens_obs:
        dta_list=Observations(i[0],i[1],date)
        df_Obs=pd.DataFrame({'Libele':dta_list[0],'Ex_rt':dta_list[1],'Hour':dta_list[2],'Date':dta_list[3]})
        list_df.append(df_Obs)
    #Lancement d'une boucle "pour" qui met les informations compil�es dans la dataframe
    for i in list_df:
        final_df=pd.concat([final_df,i])
    return(final_df,'Companies_dta')

def Airbus(iteration,final_time):
    """
    Fonction finale qui donne un fichier csv des donn�es extraites
    concernant l'entreprise Airbus par le crwaler pour en faire l'�tude.

    Parameters
    ----------
    interation : int
        Indique les intervalles de temps que le crawler doit r�cup�rer
        les donn�es. Par d�faut, le pas est de 30 secondes. Donc interation = 30.
    final_time : int
        Indique le moment auquel le crawler cesse de r�cup�rer les donn�es.
        Il est exprim� en secondes et doit >= 30 pour avoir au moins une observation.

    Returns
    -------
    Cette fonction ne retourne rien.

    """
    Ens_obs=crawler(iteration,final_time)
    [DF,nom_csv]=df_com(Ens_obs)
    DF.to_csv('Companies_dta.csv')
    df_companies = pd.read_table('Companies_dta.csv', sep=',')
    #Parmi toutes les entreprises pr�sentes, on ne prend que Airbus
    Airbus = df_companies[df_companies['Libele'] == 'AIRBUS']
    #Manipulation pour n'avoir que les cours et les dates
    del Airbus['Unnamed: 0'],Airbus['Libele']
    #Cr�ation d'une colonne fusionnant les dates et les horaires
    #puis suppression de ces deux derni�res colonnes car inutiles
    Airbus['Timestamp'] = Airbus['Date'] + ' ' + Airbus['Hour']
    del Airbus['Hour'],Airbus['Date']
    Airbus = Airbus.reindex(columns=['Timestamp','Ex_rt'])
    #Enregistrement sur un fichier csv final
    Airbus.to_csv('Donn�es_Airbus.csv')
    
Airbus(30,30600)

#Importation du dataset pour ensuite supprimer la colonne inutile 
df_airbus = pd.read_csv('Donn�es_Airbus.csv',sep=',')
del df_airbus['Unnamed: 0']

liste = [i for i in range (1016)]
df_liste = pd.DataFrame(liste,columns=['Times'])
df_airbus2=pd.concat([df_liste,df_airbus], axis=1, join='inner')
del df_airbus2['Timestamp']

#On manipule nos variables explicatives et � expliquer pour qu'elles
#soient exploitables
x = np.array(df_airbus2['Times']).reshape(-1,1)
y = np.array(df_airbus2['Ex_rt']).reshape(-1,1)

#Construction de la r�gression lin�aire
reg0 = LinearRegression()
reg0_fit=reg0.fit(x,y)
y_pred0=reg0.predict(x)

#Construction du mod�le de r�gression polynomiale
poly_reg = PolynomialFeatures(degree=6)
x_poly=poly_reg.fit_transform(x)
reg1=LinearRegression()
reg1.fit(x_poly,y)
y_poly_pred=reg1.predict(x_poly)

#Visulalisation des r�gressions
plt.scatter(x,y,color='green')
plt.plot(x,reg0.predict(x),color='blue')
plt.plot(x,reg1.predict(x_poly),color='red')
plt.title('Variation des cours toutes les 30 secondes')
plt.xlabel('Temps')
plt.ylabel('Cours')
plt.show

#Conclusion sur le mod�le � choisir
r2=r2_score(y,y_pred0)
r2_poly=r2_score(y,y_poly_pred)

r2,r2_poly=100*round(r2,4),100*round(r2_poly,4)

print('Le R� de la r�gression lin�aire simple est de',r2,'%')
print('Le R� de la r�gression polynomiale est de',r2_poly,'%')

if r2 > r2_poly:
    print('Il faut privil�gier la r�gression lin�aire simple')
else:
    print('Il faut privil�gier la r�gression polynomiale')

#Utilisation du mod�le choisi
x_train,x_test,y_train,y_test=train_test_split(x,y,test_size=0.2)
pf = PolynomialFeatures(6)
x_train_poly = pf.fit_transform(x_train)
x_test_poly=pf.fit_transform(x_test)
reg=LinearRegression()
reg.fit(x_train_poly,y_train)

#Pr�dictions sur l'�chantillon test
y_test_predict=reg.predict(x_test_poly)

#Pr�dictions sur le futur
predi=poly_reg.fit_transform([[1017],[1077],[1137],[1197],[1257],[1317],
                              [1377],[1437],[1497],[1557],[1617],[1677],
                              [1737],[1797],[1857],[1917],[1977],[2032]])
predi_u=reg.predict(predi)

#Mise en forme des pr�dictions dans une DataFrame avec les temps cl�s de cours
times_f = pd.DataFrame({'Key-times': ["9:00", "9:30", "10:00","10:30","11:00","11:30",
                        "12:00", "12:30", "13:00","13:30","14:00","14:30",
                        "15:00", "15:30", "16:00","16:30","17:00","17:30"]})

df_pred = pd.DataFrame(predi_u,columns=['Predictions pour le jour suivant la r�colte des donn�es'])
df_prediction=pd.concat([times_f,df_pred], axis=1, join='inner')
df_prediction.set_index('Key-times', inplace = True)

#Enregistrement des pr�dictions dans un fichier csv
df_prediction.to_csv('Pr�dictions des cours')