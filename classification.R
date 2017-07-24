# author: Ivan Sekulic
# deadline: 23.5. 12:00
# other deadline: 31.5. 9:00
# In this task we will deal with classification problem on Cars dataset.
# The dataset describes different feaures of cars, with class labels being
# how good/bad the car is: unacc, acc, good, vgood
# We want a model that will tell us if the car with certain specifications is good enough for us.
# By applying grid search on decision tree model, we will obtain the best classifier. It
# will be picked by the highest F1-micro score.

set.seed(43)
setwd('~/lab5')
library(caret)
library(party)
library(rpart)
library(e1071)

######################################
# Loading and inspecting the data
######################################
# download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data', 'car.data')
cars_ds = read.csv("car.data", header = FALSE, col.names = c('buying', 'maint', 'doors', 'persons', 'lug_boot','safety', "category"))

head(cars_ds, 10)
# true labels --> category column 
summary(cars_ds)
#cars_ds$category

######################################
# Preparing the dataset
######################################
cars_ds$buying <- as.factor(cars_ds$buying)
head(cars_ds$maint)

# Splitting the dataset into 3 parts:
# 1) train set -> used for training of the model
# 2) validation set -> used for validating the model in 'grid search'
# 3) test set -> used only at the end for final evaluation
ind <- sample(3, nrow(cars_ds), replace=T, prob=c(0.5, 0.25, 0.25))
X_train <- cars_ds[ind==1, ]
X_valid <- cars_ds[ind==2, ] 
X_test <- cars_ds[ind==3, ]

dim(X_train)
dim(X_valid)
dim(X_test)

######################################
# Training my classifier
######################################
# will try few hyperparameters in order to obtain the best model
# the best model will be picked by the highest F1-micro score
?ctree
clf <- ctree(category ~ ., X_train)
table(predict(clf, newdata=X_valid), X_valid$category)
?ctree_control
myParam =ctree_control(minsplit=25, maxdepth=1)

myParam
# a new decision tree
iris_Ctree2<-ctree(Species~., data=trainData,controls = myParam )

testtypes = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")
minsplits = c(1, 20, 25)
maxdepths = c(1, 5, 10, 20)

for (tt in testtypes){
  for (ms in minsplits){
    for (md in maxdepths){
      #c(tt, ms, md)
      myParam = ctree_control(testtype=tt, minsplit=ms, maxdepth=md)
      clf <- ctree(category ~ ., data=X_train, controls=myParam)
      preds <- predict(clf, newdata=X_valid)
      cm <- table(preds, X_valid$category)
      # calculating A, P, R and F1
      n = sum(cm) # number of instances
      nc = nrow(cm) # number of classes
      diag = diag(cm) # number of correctly classified instances per class 
      rowsums = apply(cm, 1, sum) # number of instances per class
      colsums = apply(cm, 2, sum) # number of predictions per class
  
      acc = sum(diag) / n
      precision = diag / colsums 
      recall = diag / rowsums 
      f1 = 2 * precision * recall / (precision + recall)
  
      # mean of F1s over classes -> micro
      f1
      print(c(tt, ms, md, mean(f1)))
    }
  }
}
# Output:
# [1] "Bonferroni" "1"          "1"          "NaN"       
# [1] "Bonferroni"       "1"                "5"                "0.59866483273954"
# [1] "Bonferroni"        "1"                 "10"                "0.795669032827608"
# [1] "Bonferroni"        "1"                 "20"                "0.795669032827608"
# [1] "Bonferroni" "20"         "1"          "NaN"       
# [1] "Bonferroni"       "20"               "5"                "0.59866483273954"
# [1] "Bonferroni"        "20"                "10"                "0.795669032827608"
# [1] "Bonferroni"        "20"                "20"                "0.795669032827608"
# [1] "Bonferroni" "25"         "1"          "NaN"       
# [1] "Bonferroni"       "25"               "5"                "0.59866483273954"
# [1] "Bonferroni"        "25"                "10"                "0.781649002255438"
# [1] "Bonferroni"        "25"                "20"                "0.781649002255438"
# [1] "MonteCarlo" "1"          "1"          "NaN"       
# [1] "MonteCarlo"        "1"                 "5"                 "0.554769741330271"
# [1] "MonteCarlo"        "1"                 "10"                "0.805752476113188"
# [1] "MonteCarlo"        "1"                 "20"                "0.795426889896441"
# [1] "MonteCarlo" "20"         "1"          "NaN"       
# [1] "MonteCarlo"        "20"                "5"                 "0.554769741330271"
# [1] "MonteCarlo"        "20"                "10"                "0.724054708732128"
# [1] "MonteCarlo"        "20"                "20"                "0.724054708732128"
# [1] "MonteCarlo" "25"         "1"          "NaN"       
# [1] "MonteCarlo"        "25"                "5"                 "0.554769741330271"
# [1] "MonteCarlo"        "25"                "10"                "0.712802593449053"
# [1] "MonteCarlo"        "25"                "20"                "0.712802593449053"
# [1] "Univariate" "1"          "1"          "NaN"       
# [1] "Univariate"       "1"                "5"                "0.59866483273954"
# [1] "Univariate"        "1"                 "10"                "0.799490450268229"
# [1] "Univariate"        "1"                 "20"                "0.799490450268229"
# [1] "Univariate" "20"         "1"          "NaN"       
# [1] "Univariate"       "20"               "5"                "0.59866483273954"
# [1] "Univariate"        "20"                "10"                "0.795669032827608"
# [1] "Univariate"        "20"                "20"                "0.795669032827608"
# [1] "Univariate" "25"         "1"          "NaN"       
# [1] "Univariate"       "25"               "5"                "0.59866483273954"
# [1] "Univariate"        "25"                "10"                "0.781649002255438"
# [1] "Univariate"        "25"                "20"                "0.781649002255438"
# [1] "Teststatistic" "1"             "1"             "NaN"          
# [1] "Teststatistic"    "1"                "5"                "0.59866483273954"
# [1] "Teststatistic"     "1"                 "10"                "0.802812215409817"
# [1] "Teststatistic"     "1"                 "20"                "0.802812215409817"
# [1] "Teststatistic" "20"            "1"             "NaN"          
# [1] "Teststatistic"    "20"               "5"                "0.59866483273954"
# [1] "Teststatistic"     "20"                "10"                "0.800523463418705"
# [1] "Teststatistic"     "20"                "20"                "0.800523463418705"
# [1] "Teststatistic" "25"            "1"             "NaN"          
# [1] "Teststatistic"    "25"               "5"                "0.59866483273954"
# [1] "Teststatistic"     "25"                "10"                "0.791832504145937"
# [1] "Teststatistic"     "25"                "20"                "0.791832504145937"

# The classifier obtained the highest score with [MonteCarlo, 1, 10] hyperparameters.
# We can see we have some "Nan"s. I left them because they are also indication of 
# how bad some hyperparameters are.

# Inspecting the best model by looking at conf matricies of train and test set
myParam = ctree_control(testtype="MonteCarlo", minsplit=1, maxdepth=10)
clf <- ctree(category ~ ., data=X_train, controls=myParam)
preds <- predict(clf, newdata=X_train)
cm <- table(preds, X_train$category)
cm
#preds   acc good unacc vgood
#acc   190    6    20     7
#good    7   28     2     3
#unacc   5    0   580     0
#vgood   0    2     0    21

# On train set, our model works pretty good. We might thing that by
# having minsplit parameter so low (1) and rather big depth for this task,
# the model might be overtrained, but it works great on the test set too! 
preds <- predict(clf, newdata=X_test)
cm <- table(preds, X_test$category)
cm
#preds   acc good unacc vgood
#acc    73    7    11     1
#good    4    6     1     3
#unacc   5    0   298     0
#vgood   0    1     0     9

# calculating A, P, R and F1
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class

acc = sum(diag) / n
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

acc
# 0.9212411
f1
#      acc      good     unacc     vgood 
#0.8390805 0.4285714 0.9722675 0.7826087 

c(acc, mean(precision), mean(recall), mean(f1))
#[1] 0.9212411 0.7431033 0.7763870 0.7556320

# The conclusion would be that this calsifier would probably tell us 
# corectly which car to buy. In the end, that was the goal of this excercise.
# We might be concerned about false positives for classes acc, and espetialy for
# unacc. We do not want to get a bad car! To solve that problem, we might consider 
# implementing that fact into our model by doing some kind of weightening. 
# Cost Sensitive Learning is one approach, but I felt it would go out of the scope 
# of this lab.
