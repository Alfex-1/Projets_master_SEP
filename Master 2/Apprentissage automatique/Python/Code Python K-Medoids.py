# Importation des packages
import pandas as pd
from sklearn.preprocessing import StandardScaler
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.colors import ListedColormap
from sklearn import datasets
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
from scipy.spatial.distance import cdist
from sklearn.metrics import silhouette_score, silhouette_samples
from pyclustering.cluster.kmeans import kmeans
from pyclustering.cluster.kmedoids import kmedoids
from pyclustering.utils import calculate_distance_matrix
from sklearn.metrics import davies_bouldin_score
import warnings
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)

# Base de données sur les caractéristiques des iris
iris = datasets.load_iris()
data = pd.DataFrame(iris.data,columns = iris.feature_names)

target = iris.target_names
labels = iris.target

# Mise à l'échelle : normalisation
scaler = StandardScaler()
data = pd.DataFrame(scaler.fit_transform(data), columns=data.columns)


# # Trouver le k optimal

# ## Méthode du coude

# Liste pour stocker les valeurs de la variance intra-cluster
inertia_values = []

# Nombre de clusters à tester
k_values = range(1, 11)

# Application de K-means pour différents K
for k in k_values:
    kmeans = KMeans(n_clusters=k, random_state=42)
    kmeans.fit(data)
    inertia_values.append(kmeans.inertia_)

# Tracer la courbe du coude
plt.figure(figsize=(8, 5))
plt.plot(k_values, inertia_values, marker='o')
#plt.title('Elbow method to find the optimal K')
plt.xlabel('Number of clusters (K)')
plt.ylabel('Intra-cluster variance')

# Ajouter une ligne pour indiquer le coude (point optimal)
plt.axvline(x=3, color='red', linestyle='--', label='Optimal number of clusters')

plt.legend()
plt.show()


# ## Méthode de la silhouette

# Nombre de clusters à tester
k_values = range(2, 11)

# Liste pour stocker les valeurs de la silhouette moyenne
silhouette_avg_values = []

for k in k_values:
    # Appliquer K-means pour différents K
    kmeans = KMeans(n_clusters=k, random_state=42)
    cluster_labels = kmeans.fit_predict(data)
    
    # Calculer la silhouette moyenne
    silhouette_avg = silhouette_score(data, cluster_labels)
    silhouette_avg_values.append(silhouette_avg)

# Tracer la courbe de la silhouette
plt.figure(figsize=(8, 5))
plt.plot(k_values, silhouette_avg_values, marker='o')
#plt.title('Figure method to find the optimal K')
plt.xlabel('Number of clusters (K)')
plt.ylabel('Average figure')

# Ajouter une ligne pour indiquer le nombre de cluster optimal
optimal_k = k_values[np.argmax(silhouette_avg_values)]
plt.axvline(x=optimal_k, color='red', linestyle='--', label='Optimal number of clusters')

plt.legend()
plt.show()


# ## Visualisation graphique

# # Détermination des clusters

def visualize_clusters(data, num_clusters):
    # Appliquer KMeans sur les données brutes
    kmeans = KMeans(n_clusters=num_clusters)
    labels = kmeans.fit_predict(data.values)

    # Visualiser les données brutes avec les clusters
    fig, ax = plt.subplots(figsize=(7, 6))
    scatter = ax.scatter(data.values[:, 0], data.values[:, 1], c=labels, cmap=plt.cm.Set1, edgecolor='k', s=40)

    # Ajouter une légende pour les clusters
    legend_labels = list(set(labels))
    legend_labels.sort()
    legend = ax.legend(*scatter.legend_elements(), title="Clusters")
    for i, label in enumerate(legend_labels):
        legend.get_texts()[i].set_text(f'Cluster {label + 1}')

    ax.set_xlabel("SepalLengthCm")
    ax.set_ylabel("SepalWidthCm")
    plt.show()


# k=2
visualize_clusters(data, num_clusters=2)
# k=3
visualize_clusters(data, num_clusters=3)


# # K-Means

def kmeans_clustering(data, num_clusters):
    # Appliquer KMeans sur les données
    kmeans = KMeans(n_clusters=num_clusters, random_state=42)
    labels = kmeans.fit_predict(data.values)
    centers = kmeans.cluster_centers_

    # Visualiser les données avec les clusters
    scatter = plt.scatter(data.values[:, 0], data.values[:, 1], c=labels, cmap='viridis',marker='o', edgecolors='black', s=50)
    plt.scatter(centers[:, 0], centers[:, 1], c='red', marker='*', s=200, label='Centroids')

    # Ajouter des points dans la légende pour chaque cluster
    legend_labels = list(set(labels))
    for label in legend_labels:
        plt.scatter([], [], c=scatter.cmap(scatter.norm(label)), marker='o', s=50, label=f'Cluster {label + 1}')

    plt.xlabel('SepalLengthCm')
    plt.ylabel('SepalWidthCm')
    plt.legend()
    plt.show()


# k = 3
kmeans_clustering(data, num_clusters=3)
# k = 2
kmeans_clustering(data, num_clusters=2)


# # K-Means Medoids

# Définition de la fonction K-Medoids avec PAM
def k_medoids_pam(X, k, max_iterations=1000):
    # Initialisation des médoïdes en choisissant k indices du jeu de données aléatoirement
    medoids_indices = np.random.choice(len(X), k, replace=False)
    medoids = X[medoids_indices, :].copy()

    # Boucle principale de l'algorithme PAM
    for _ in range(max_iterations):
        # Calcul des distances entre chaque point et les médoïdes
        distances = cdist(X, medoids, metric='euclidean')
        # Affectation de chaque point au cluster du médoïde le plus proche
        labels = np.argmin(distances, axis=1)
        
        # Calcul du coût (somme des distances) et enregistrement de la valeur initiale
        cost = np.sum(distances[np.arange(len(X)), labels])
        c_0 = cost
        
        # Itération sur chaque point pour tester le changement de médoïde
        for i in range(len(X)):
            if i not in medoids_indices:
                # Copie temporaire des indices des médoïdes
                temp_medoids_indices = medoids_indices.copy()
                # Remplacement du médoïde du cluster du point i par le point i
                temp_medoids_indices[np.where(medoids_indices == labels[i])] = i
                
                # Mise à jour des médoïdes temporaires
                temp_medoids = X[temp_medoids_indices, :]
                temp_distances = cdist(X, temp_medoids, metric='euclidean')
                temp_cost = np.sum(temp_distances[np.arange(len(X)), labels])
                
                # Si le changement diminue le coût, accepter le nouveau médoïde
                if temp_cost < cost:
                    cost = temp_cost
                    medoids_indices = temp_medoids_indices
                    medoids = temp_medoids
        
        # Condition d'arrêt : aucune amélioration du coût
        if cost == c_0:
            break
    
    # Retourner les labels et les médoïdes finaux
    return labels, medoids


def kmedoids_visualization(data, k_values):
    # Utiliser l'algorithme K-Medoids avec PAM
    labels, medoids = k_medoids_pam(data.values, k_values)
    
    # Visualiser les clusters et les médoïdes dans l'espace d'origine
    scatter = plt.scatter(data.values[:, 0], data.values[:, 1], c=labels, cmap='viridis', marker='o', edgecolors='black', s=50)
    plt.scatter(medoids[:, 0], medoids[:, 1], c='red', marker='X', s=200, label='Medoids')
    
    # Ajouter des légendes distinctes pour chaque cluster
    legend_labels = list(set(labels))
    for label in legend_labels:
        plt.scatter([], [], c=scatter.cmap(scatter.norm(label)), label=f'Cluster {label + 1}')
    
    #plt.title('Clustering K-Means Medoids')
    plt.xlabel('SepalLengthCm')
    plt.ylabel('SepalWidthCm')
    plt.legend()
    plt.show()


# k = 2
kmedoids_visualization(data, 2)
# k = 3
kmedoids_visualization(data, 3)


# # Choisir définitivement le nombre de cluster

# Fonction pour calculer l'indice de Davies-Bouldin avec K-Means Medoids
def calculate_davies_bouldin_kmedoids(data, k, initial_medoids=None):
    # Calculer la matrice de distances
    distance_matrix = calculate_distance_matrix(data)
    
    # Initialiser les médoïdes aléatoirement si aucun n'est fourni
    if initial_medoids is None:
        initial_medoids = np.random.choice(len(data), k, replace=False)
    
    # Appliquer l'algorithme K-Means Medoids
    kmedoids_instance = kmedoids(distance_matrix, initial_medoids, tolerance=0.001, ccore=True)
    kmedoids_instance.process()
    
    # Récupérer les résultats
    clusters = kmedoids_instance.get_clusters()
    medoids = kmedoids_instance.get_medoids()
    
    # Calcul de l'indice de Davies-Bouldin
    labels = np.concatenate([np.full(len(cluster), i) for i, cluster in enumerate(clusters)])
    db = davies_bouldin_score(data, labels)
    
    return db,labels,medoids


# k = 3
k = 3
db, labels, medoids = calculate_davies_bouldin_kmedoids(data.values, k)

print(f'Indice de Davies-Bouldin pour k={k}: {db}')

# k = 2
k = 2
db, labels, medoids = calculate_davies_bouldin_kmedoids(data.values, k)

# Affichage des résultats
print(f'Indice de Davies-Bouldin pour k={k}: {db}')

