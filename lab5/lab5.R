library(readxl)
library(ggplot2)
library(ggrepel)
library(stats)
library(ggdendro)
library(cluster)
library(stringr)
library(factoextra)
library(parameters)
library(philentropy)

path <- 'D:\\uni\\2курс\\Аналіз_даних\\data10.xlsx'
data <- read_excel(path)
View(data)

str(data)

pairs(data[2:16])

p <- ggplot(data,
            aes(x = unlist(data['Оборотні_активи_всього1']),
                y = unlist(data['Оборотні_активи_всього2']),
                label = unlist(data['Категорія'])))
p <- p +
  geom_point() +
  geom_label_repel() +
  labs(x = "Оборотні_активи_всього Січень 2013", y = "Оборотні_активи_всього Грудень 2013") +
  theme_minimal()
p
p + xlim(0, 20000) + ylim(0, 10000)

labels.str <- lapply(data['Категорія'], str_trunc, 10, ellipsis = "")

# Нормалізація
df <- data[, -c(1,1)]
data.scaled <- scale(df)

# Оптимальна кількість кластерів
fviz_nbclust(data.scaled, hcut, method = "silhouette") +
  labs(subtitle = "Silhouette method")
k <- 3

####
set.seed(123)

create.dendro <- function (df, m, metric) {
  if (metric == "std_euclidean"){
    d <- dist(df, method = "euclidean")
  }
  else if (metric == "chebyshev"){
    d <- distance(df, method = metric)
    d <- as.dist(d)
  }
  else{
    d <- dist(df, method = metric)
  }

  data.hclust <- hclust(d, method = m)

  ###

  coph <- cophenetic(data.hclust)
  k.coph <- cor(d, coph)
  print(paste("Cophenetic correlation: ", round(k.coph, 6)))

  ###

  plot(data.hclust,
       hang=-1,
       labels=unlist(labels.str),
       main=paste('Dendrogram (', m, ',', metric, ')'))

  plot(data.hclust,
       hang=-1,
       labels=unlist(labels.str),
       main=paste('Dendrogram (', m, ',', metric, ')'))

  rect.hclust(data.hclust, k=k, border="red")

  ###
  plot(df[, 1:2], pch=20, col=cutree(data.hclust, k))
  ###

  clusters <- cutree(data.hclust, k=k)
  centers <- aggregate(df, list(cluster = clusters), mean)
  print("Cluster centers")
  print(centers)

  print("Distance between each center")
  print(dist(centers[,-1]))

  ###
  # data.eclust <- eclust(data.scaled, "hclust", hc_method=m, k.max = k)
  # fviz_cluster(data.eclust, data=df)
  fviz_cluster (list(data = df, cluster = clusters))
}


### Середнього зв'язку

# Евклідова відстань
create.dendro(df, "average", "euclidean")
# Стандартизована Евклідова відстань
create.dendro(data.scaled, "average", "std_euclidean")
# відстань Чебишева
create.dendro(data.scaled, "average", "chebyshev")


### Центроїдний

# Евклідова відстань
create.dendro(data.scaled, "centroid", "euclidean")
# Стандартизована Евклідова відстань
create.dendro(data.scaled, "centroid", "std_euclidean")
# відстань Чебишева
create.dendro(data.scaled, "centroid", "chebyshev")

### Медіанного зв'язку

# Евклідова відстань
create.dendro(data.scaled, "median", "euclidean")
# Стандартизована Евклідова відстань
create.dendro(data.scaled, "median", "std_euclidean")
# відстань Чебишева
create.dendro(data.scaled, "median", "chebyshev")
