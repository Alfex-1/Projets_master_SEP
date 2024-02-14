############################################
##### Projet Apprentissage Automatique #####
############################################

#### K-menoids  ####

# Installez les packages si vous ne les avez pas déjà installés
# install.packages("cluster")
# install.packages("factoextra")

# Chargez les bibliothèques
library(cluster)
library(factoextra)

data("USArrests")      # Chargement d'une database
data("iris")      # Chargement d'une database
df <- scale(USArrests[,1:4]) # Standardisez les données
head(df, n = 3)        # Head de la db

### Determination du nombre de clusters
## Silhouette
# Utilisez fviz_nbclust pour visualiser le nombre optimal de clusters
result <- fviz_nbclust(df, pam, method = "silhouette") # PAM = k-menoids

# Affichez le graphique
print(result)

## Méthode du coude
result2 <- fviz_nbclust(df, pam, method = "wss")
# Affichez le graphique
print(result2)

# On remarque ici que le nombre idéal est 2 clusters

pam.res <-pam(df, 4, metric = "manhattan", stand = FALSE)
 # Execution des k-medoids
print(pam.res)

dd <- cbind(USArrests, cluster = pam.res$cluster) # Affectation des clusters
head(dd, n = 3) # Visualisation de la base

pam.res$medoids # Cluster medoids: New Mexico, Nebraska
head(pam.res$clustering) # Visualition des clusters

# Visualisez les résultats du clustering avec fviz_cluster
fviz_cluster(pam.res, data = df)

#### K-means  ####
### Determination du nombre de clusters
## Silhouette
# Utilisez fviz_nbclust pour visualiser le nombre optimal de clusters
result <- fviz_nbclust(df, kmeans, method = "silhouette") # PAM = k-menoids

# Affichez le graphique
print(result)

## Méthode du coude
result2 <- fviz_nbclust(df, kmeans, method = "wss")
# Affichez le graphique
print(result2)

# On remarque ici que le nombre idéal est 2 clusters

kmeans.res <-kmeans(df, 4)
# Execution des k-medoids
print(kmeans.res)

ddk <- cbind(iris, cluster = kmeans.res$cluster) # Affectation des clusters
head(ddk, n = 3) # Visualisation de la base

kmeans.res$centers # Cluster medoids: New Mexico, Nebraska
head(kmeans.res$cluster) # Visualition des clusters

# Visualisez les résultats du clustering avec fviz_cluster
fviz_cluster(kmeans.res, data = df)
    

