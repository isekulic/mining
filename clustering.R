# Author: Ivan Sekulic
# Deadline: 30.05. 12:00

# Our goal here is to learn about clustering. The date we will use is Cardiotocography data set.
# It contains more than 2000 fetal cardiotocograms (CTGs) with gold labels in 10 classes.
# We will use the labels to help evaluate our clustering quality. We will try different values
# For k in k-means clustering algorithm and see what gives the highest value of our eval funcion.
# This k selection method would not be possible because we would not have the true classes
# (Labels). Therefore, this experiment is more just for experimenting and learning about clustering.

set.seed (42)
setwd ( '~ / LaB6')


# Inspecting the dataset.
# Download.file ('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_all_corr.csv', 'cardio.csv')
Ctg_all <- read.csv ("cardio.csv", row.names = 1)
SUMMARY (ctg_all)
head (ctg_all)

Ctg_all2 = ctg_all
Ctg_all2 $ CLASS = NULL
Ctg_all2 $ NSP = NULL
head (ctg_all2)

# Evaluation function
# The evaluation function will reward the higher number of classes.
AccuracyCalc <- function (confTbl, startCol)
{
  Corr = 0;
  For (and in startCol: ncol (confTbl))
  {
    Corr = corr + max (confTbl [, i])  
  }
  Accuracy = corr / sum (confTbl)
  accuracy  
}

# Employing grid search to find the best k. The best k will be selected
# By the highest value of accuracy of our defined evaluation function.
For (k in 2:15) {
  Model = kmeans (ctg_all2, k, iter.max = 20)
  # Evaluate model
  Cm = table (ctg_all $ CLASS, model $ cluster)
  Acc = accuracyCalc (cm, 1)
  Print (c (k, acc))
}
# Output:
# [1] 2.0000000 0.3024894
# [1] 3.0000000 0.3231564
# [1] 4.0000000 0.3170503
# [1] 5.0000000 0.3781118
# [1] 6.0000000 0.3983091
# [1] 7.0000000 0.3950211
# [1] 8.0000000 0.4264913
# [1] 9.0000000 0.4053546
# [1] 10.0000000 0.4105214
# [1] 11.0000000 0.4213246
# [1] 12.0000000 0.4199155
# [1] 13.0000000 0.4368248
# [1] 14.0000000 0.4297792
# [1] 15.0000000 0.4518553

# Testing the k-medoids to see which k it will chose
library (FPC)
? Pamka
Model2 <- pamk (ctg_all2, krange = 2:15)
# Number of clusters
MODEL2 $ nc
# Pamk claims the best number of clusters is 2. By our evaluation methods,
# We can conclude that is not true! Also, knowing there are 10 classes helps :)

# As we can see, the highest score for our accuracy evaluation function
# We got with 15 clusters. Therefore, let's inspect the results a bit further.
Table (ctg_all $ CLASS, model $ cluster)
# 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
# 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# 1 4 0 17 25 141 21 0 0 0 0 7 99 3 67 0
# 2 10 0 48 112 99 65 51 0 1 4 36 134 15 2 2
# 3 3 0 5 6 27 3 1 0 0 0 7 1 0 0 0
# 4 0 0 0 23 1 7 1 0 4 10 0 8 23 0 4
# 5 0 0 2 17 1 10 0 0 0 0 0 34 1 7 0
# 6 6 0 37 21 10 14 38 9 18 72 60 0 16 0 31
# 7 23 0 34 7 21 19 13 3 2 16 60 0 19 0 35
# 8 23 13 2 0 0 0 0 36 28 0 1 0 0 0 4
# 9 6 0 1 0 2 2 0 0 0 0 0 10 0 48 0
# 10 0 0 17 2 5 16 0 0 0 0 0 64 2 91 0
# We can see the mappings of our clusters to the reference ones. We can not conclude
# Flies about the quality of our mappings, but they look ok. 
