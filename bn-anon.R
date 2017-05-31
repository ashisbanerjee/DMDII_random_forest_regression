library(bnlearn)
library(caret)

#Learns a Bayesian Network using 4 different network learning algorithms.
#Feel free to test each learning algorithm on its own.

#Reading in data
df.data <- read.csv('sim-anon.csv', header = TRUE, colClasses=c(rep("factor",28)))

#Creating train and test splits
train <- createDataPartition(df.data$y1, times=10, p=0.8)
df.train <- df.data[train[[1]],]
df.test <- df.data[-train[[1]],]

#score based
bn.hc <- hc(x=df.train) #Network learning
bn.hc <- bn.fit(bn.hc, data=df.train) #Parameter learning
table(predict(bn.hc, node="y1", method="parents", data=df.test), df.test$y1) #Prediction

bn.tabu <- tabu(x=df.train) #Network learning
bn.tabu <- bn.fit(bn.tabu, data=df.train) #Parameter Learning
table(predict(bn.tabu, node="y1", method="parents", data=df.test), df.test$y1) #Predicting

#constraint absed
bn.gs <- gs(x=df.train) #Network learning
bn.gs <- pdag2dag(bn.gs, ordering = colnames(df.train)) #Setting edge directions
bn.gs <- bn.fit(bn.gs, data=df.train) #Parameter Learning
table(predict(bn.gs, node="y1", method="parents", data=df.test), df.test$y1) #Predicting

bn.mmpc <- mmpc(x=df.train) #Network learning
bn.mmpc <- pdag2dag(bn.mmpc, ordering = colnames(df.train)) #Setting edge directions
bn.mmpc <- bn.fit(bn.mmpc, data=df.train) #Parameter Learning
table(predict(bn.mmpc, node="y1", method="parents", data=df.test), df.test$y1) #Predicting
