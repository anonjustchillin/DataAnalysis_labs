import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sklearn.metrics.pairwise import euclidean_distances

maxIter = 2

path = 'D:\\uni\\2курс\\Аналіз_даних\\Data.csv'
data = pd.read_csv(path)
data = data.drop([0])
print(data.head())
print(data.shape)
k = data.shape[0]

# графік 1
ax = sns.scatterplot(data=data, x='GrowthRate_2015_2014', y='GrowthRate_summer_2014_summer_2013', hue='Region')
sns.move_legend(ax, "upper left", bbox_to_anchor=(1, 1))
plt.show()

# графік 2
ax = sns.histplot(data=data, x='Turnover_2015', bins=10)
plt.show()

data = data.set_index('Region')
print(data.head())

# Нормалізація
norm_data = pd.DataFrame(StandardScaler().fit_transform(data))
print('Нормалізовані дані')
print(norm_data)


# Методи ліктя та силуету
wcss = []
sil_scores = []
for i in range(2, k):
    kmeans = KMeans(n_clusters=i, max_iter=maxIter)
    kmeans.fit(norm_data)
    wcss.append(kmeans.inertia_)
    sil_scores.append(silhouette_score(norm_data, kmeans.labels_, metric='euclidean'))

plt.plot(range(2, k), wcss)
plt.title('Метод ліктя')
plt.xlabel('Кількість кластерів')
plt.ylabel('WCSS')
plt.xticks(np.arange(2, k))
plt.show()

plt.plot(range(2, k), sil_scores)
plt.title('Метод силуету')
plt.xlabel('Кількість кластерів')
plt.ylabel('Silhouette Score')
plt.xticks(np.arange(2, k))
plt.show()

# з графіків бачимо, що оптимальна кількість кластерів - 4
k = 4

# Евклідова метрика відстані
print('Відстані (Евклідова метрика)')
dist = euclidean_distances(norm_data)
print(dist)
sns.heatmap(dist)
plt.show()

# scikit-learn current implementation of k-means only uses Euclidean distances
kmeans_data = KMeans(n_clusters=k, max_iter=maxIter)
kmeans_data.fit(norm_data)
print('Кластери')
print(kmeans_data.labels_)
print('Центри кластерів')
centroids = kmeans_data.cluster_centers_
print(kmeans_data.cluster_centers_)
print('Сума квадратів всередині кластера')
print(kmeans_data.inertia_)

# пари зі звичайними даними
data['cluster'] = kmeans_data.predict(norm_data)
print(data.head())
sns.pairplot(data, hue='cluster')
plt.show()

# пари з нормалізованими даними
norm_data['cluster'] = kmeans_data.predict(norm_data)
print(norm_data.head())
sns.pairplot(norm_data, hue='cluster')
plt.show()

# графік 3
plt.scatter(x=norm_data.iloc[:,1], y=norm_data.iloc[:,2], c=norm_data.cluster)
plt.xlabel('GrowthRate_2015_2014 (Нормалізоване)')
plt.ylabel('GrowthRate_summer_2014_summer_2013 (Нормалізоване)')
plt.show()

# графік 3 з центрами
plt.scatter(x=norm_data.iloc[:,1], y=norm_data.iloc[:,2], c=kmeans_data.labels_)
plt.scatter(x=centroids[:,1], y=centroids[:,2], marker='x', color='red', alpha=0.5, s=50)
plt.xlabel('GrowthRate_2015_2014 (Нормалізоване)')
plt.ylabel('GrowthRate_summer_2014_summer_2013 (Нормалізоване)')
plt.title('Огляд кластерів')
plt.show()

# Паралельні координати
pd.plotting.parallel_coordinates(norm_data, 'cluster')
plt.show()
