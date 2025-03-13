library(randomForest)
library(caTools)
library(caret)
library(Metrics)

get.tree.info <- function(rf.model, test){
  y.pred <- predict(rf.model, test)

  print(rf.model)

  conf.mtx <- caret::confusionMatrix(y.pred, test$Salary)
  print(conf.mtx)

  recall.res <- conf.mtx$byClass[, "Sensitivity"]
  mc.av.recall <- sum(recall.res) / length(recall.res)

  precision.res <- conf.mtx$byClass[, "Pos Pred Value"]
  mc.av.precision <- sum(precision.res) / length(precision.res)

  print("")

  cat("Recall:", recall.res, "\n")
  cat("Macro-average Recall:", mc.av.recall, "\n")
  cat("Precision:", precision.res, "\n")
  cat("Macro-average Precision:", mc.av.precision, "\n")

  plot(rf.model)
  varImpPlot(rf.model)

}


# The Employee Satisfaction Survey dataset is a comprehensive collection of information
# regarding employees within a company.
# It includes essential details such as:
#   employee identification numbers,
#   self-reported satisfaction levels,
#   performance evaluations,
#   project involvement,
#   work hours,
#   tenure with the company,
#   work accidents,
#   promotions received in the last 5 years,
#   departmental affiliations,
#   salary levels.

# Importing a dataset
dataset <- read.csv("D:/uni/2курс/Аналіз_даних/DataAnalysis_code/lab3/Employee Attrition.csv", stringsAsFactors = F)
summary(dataset)

# Removing rows with NA
df <- dataset[complete.cases(dataset),]
# Deleting Emp.ID
df$Emp.ID <- NULL
# Changing column name
colnames(df)[9] <- "Salary"
# Changing Salary and dept data type to factor
df$Salary <- factor(df$Salary)
df$dept <- factor(df$dept)

# Number of columns without Salary
n <- ncol(df)-1

str(df)
summary(df)

# Splitting data into train and test data
set.seed(1)
split <- sample.split(df$Salary, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

print("-------- DEFAULT FOREST (500 trees, all columns from df) --------")

rf.model <- randomForest(reformulate(response = 'Salary',
                                     names(train)[1:n]),
                         data=train)

get.tree.info(rf.model, test)
importance(rf.model)

# Removing unimportant columns for a new forest
new.df <- df
impr <- importance(rf.model)

unimpr.col <- function(x){
  m <- mean(impr)
  return (rownames(x)[which(x<m)])
}

cols <- unimpr.col(impr)

for (i in cols){
  new.df[i] <- NULL
  impr <- impr[-which(names(impr) == i), , drop = FALSE]
}
n <- ncol(new.df)-1
str(new.df)

split <- sample.split(new.df$Salary, SplitRatio = 0.7)
train <- subset(new.df, split == TRUE)
test <- subset(new.df, split == FALSE)

print("-------- FOREST (500 trees, important columns from df) --------")

new.rf.model <- randomForest(reformulate(response = 'Salary',
                                         names(train)[1:n]),
                             data=train)
get.tree.info(new.rf.model, test)
importance(new.rf.model)

print("-------- FOREST (500 trees, max nodes 50) --------")

rf.model.maxnodes <- randomForest(reformulate(response = 'Salary',
                                              names(train)[1:n]),
                                  data=train, maxnodes=50)
get.tree.info(rf.model.maxnodes, test)
importance(rf.model.maxnodes)

print("-------- FOREST (500 trees, node size 100) --------")

new.rf.model.nodesize <- randomForest(reformulate(response = 'Salary',
                                                  names(train)[1:n]),
                                      data=train, nodesize=100)
get.tree.info(new.rf.model.nodesize, test)
importance(new.rf.model.nodesize)

print("-------- FOREST TUNING --------")

tuned.rf <- tuneRF(train[, -(n+1)], train$Salary,
                   improve=0.05,
                   stepFactor=1.5,
                   doBest = TRUE)
get.tree.info(tuned.rf, test)
importance(tuned.rf)


print("-------- FOREST (5000 trees) --------")

new.rf.model1 <- randomForest(reformulate(response = 'Salary',
                                          names(train)[1:n]),
                              data=train, ntree=5000)
get.tree.info(new.rf.model1, test)
importance(new.rf.model1)

print("-------- FOREST (10 trees) --------")

new.rf.model2 <- randomForest(reformulate(response = 'Salary',
                                          names(train)[1:n]),
                              data=train, ntree=10)
get.tree.info(new.rf.model2, test)
importance(new.rf.model2)