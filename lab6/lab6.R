library(readxl)
library(ggplot2)
library(ggrepel)
library(stats)
library(stringr)
library(factoextra)
library(flexclust)

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
head(data.scaled)

# Оптимальна кількість кластерів
fviz_nbclust(data.scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

fviz_nbclust(data.scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
k <- 2

####
set.seed(123)

data.kmeans <- kmeans(data.scaled, k, iter.max = 1)
print(data.kmeans)
# Cluster centers
print(data.kmeans$centers)
# The between-cluster sum of squares
print(data.kmeans$betweenss)
# The total sum of squares
print(data.kmeans$totss)
# Total within-cluster sum of squares (Q)
print(data.kmeans$tot.withinss)
# The number of points in each cluster
print(data.kmeans$size)
# R^2
print(1 - data.kmeans$tot.withinss/data.kmeans$totss)

fviz_cluster(data.kmeans, data=data.scaled, geom='point')

data$cluster <- data.kmeans$cluster
View(data)

###
p <- ggplot(data, aes(x = unlist(data['Оборотні_активи_всього1']),
                      y = unlist(data['Оборотні_активи_всього2']),
                      color=factor(cluster),
                      label=unlist(data['Категорія']))) + geom_point()
p <- p +
  geom_point() +
  geom_label_repel(max.overlaps = 4) +
  labs(x = "Оборотні_активи_всього Січень 2013", y = "Оборотні_активи_всього Грудень 2013") +
  theme_minimal()
p


###

dist.mink <- stats::dist(data.scaled, method="minkowski", p=4)
round(as.matrix(dist.mink)[1:5,], 4)
fviz_dist(dist.mink)

mink.dist <- function(x, centers) {
  distMinkowski(x,
                centers,
                p=4)
}

my.controls <- list(iter.max=1)
as(my.controls, "flexclustControl")
data.kmeans2 <- kcca(data.scaled, k, family=kccaFamily(dist=mink.dist), save.data = TRUE)
data.kmeans2
image(data.kmeans2)

clusters(data.kmeans2)
## cluster sizes
info(data.kmeans2, "size")
## average within cluster distances
info(data.kmeans2, "av_dist")
## the maximum distance between any data point
info(data.kmeans2, "max_dist")
## the minimum distance to any other cluster center
info(data.kmeans2, "separation")
## the sum of all within-cluster distances
info(data.kmeans2, "distsum")
## centers
parameters(data.kmeans2)

slsw.cl3 <- slswFlexclust(data.scaled, data.kmeans2, nsamp = 20)
densityplot(slsw.cl3)
barplot(data.kmeans2, oneplot=FALSE)
###
data$cluster2 <- factor(clusters(data.kmeans2))
View(data)

###
p <- ggplot(data, aes(x = unlist(data['Оборотні_активи_всього1']),
                      y = unlist(data['Оборотні_активи_всього2']),
                      color=factor(cluster2),
                      label=unlist(data['Категорія']))) + geom_point()
p <- p +
  geom_point() +
  geom_label_repel(max.overlaps = 4) +
  labs(x = "Оборотні_активи_всього Січень 2013", y = "Оборотні_активи_всього Грудень 2013") +
  theme_minimal()
p
