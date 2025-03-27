library(arules)
library('arulesViz')
library(tidyverse)
library(lsr)


path <- 'D:\\uni\\2курс\\Аналіз_даних\\DataAnalysis_code\\lab4\\groceries.csv'

data <- read.transactions(path, sep = ",", format='basket')
nrow(data)
summary(data)
data %>% head(n=10) %>% inspect()

itemFrequencyPlot(data, topN = 20, col = rainbow(20), type = "relative")

###
### APRIORI
rules.arp <- data %>% apriori(supp = 0.01, conf = 0.04, target = "rules", minlen=2)

plot(rules.arp)

p <- plot(rules.arp, method = "graph", engine = "htmlwidget")
htmlwidgets::saveWidget(p, "rules_arp.html")
browseURL("rules_arp.html")
###
rules.arp <- cbind(as(rules.arp, "data.frame"), conviction=interestMeasure(rules.arp, "conviction", data))
filter(rules.arp, conviction != 1)
summary(rules.arp)
###
sortFrame(rules.arp[,c("rules","support")], -support)
###
sortFrame(rules.arp[,c("rules","lift")], -lift)
###
sortFrame(rules.arp[,c("rules","confidence")], -confidence)
###
sortFrame(rules.arp[,c("rules","conviction")], -conviction)


###
### ECLAT
rules.ecl <- data %>% eclat(supp = 0.01, minlen=2)
rules.ecl %>% head(n=20) %>% inspect
summary(rules.ecl)
rules.ecl

plot(rules.ecl, method="paracoord")
plot(rules.ecl, engine="htmlwidget")

p <- plot(rules.ecl, method = "graph", engine = "htmlwidget")
htmlwidgets::saveWidget(p, "rules_ecl2.html", selfcontained = FALSE)
browseURL("rules_ecl2.html")

###
### FP GROWTH
rules.fp <- fim4r(data, method = "fpgrowth", supp = 0.01, conf = 0.04, target = "rules")

plot(rules.fp)

p <- plot(rules.fp, method = "graph", engine = "htmlwidget")
htmlwidgets::saveWidget(p, "rules_fp.html", selfcontained = FALSE)
browseURL("rules_fp.html")

rules.fp <- cbind(as(rules.fp, "data.frame"), conviction=interestMeasure(rules.fp, "conviction", data))
filter(rules.fp, conviction != 1)
summary(rules.fp)
###
sortFrame(rules.fp[,c("rules","support")], -support)
###
sortFrame(rules.fp[,c("rules","lift")], -lift)
###
sortFrame(rules.fp[,c("rules","confidence")], -confidence)
###
sortFrame(rules.fp[,c("rules","conviction")], -conviction)