library(randomForest)
library(caTools)

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
dataset <- read.csv("lab3/Employee Attrition.csv", stringsAsFactors = F)
summary(dataset)

# Removing rows with NA
df <- dataset[complete.cases(dataset),]
df$Emp.ID <- NULL
colnames(df)[9] <- "Salary"
df$Salary <- factor(df$Salary)
df$dept <- factor(df$dept)
str(df)
summary(df)

# Splitting data into train and test data
set.seed(1)
split <- sample.split(df$Salary, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

rf.model <- randomForest(reformulate(response = 'Salary',
                                     names(train)[1:8]), data=train, ntree = 500, keep.forest=TRUE)

y.pred <- predict(rf.model, test)

conf.mtx <- table(test$Salary, y.pred)
conf.mtx

plot(rf.model)
importance(rf.model)
varImpPlot(rf.model)
