library(dplyr)
library(caret)
library(randomForest)

df.ge <- read.csv(file = 'GE-PO Data.csv', header=TRUE, colClasses = c(rep("factor",4),
                                                                       "character",
                                                                       rep("factor",2),
                                                                       rep("character",2),
                                                                       rep("factor",2),
                                                                       rep("character",2),
                                                                       rep("character",2),
                                                                       rep("character",4),
                                                                       "character",
                                                                       "factor"))

####Data preprocessing####
#Fixing data types, getting rid of commas in quantities
df.ge$Quantity <- gsub(",","",df.ge$Quantity)
df.ge$Delivered <- gsub(",","",df.ge$Delivered)
df.ge$Qty.Deliv. <- gsub(",","",df.ge$Qty.Deliv.)
df.ge$Sched.Qty <- gsub(",","",df.ge$Sched.Qty)

df.ge$Quantity <- as.double(df.ge$Quantity)
df.ge$Delivered <- as.double(df.ge$Delivered)
df.ge$Qty.Deliv. <- as.double(df.ge$Qty.Deliv.)
df.ge$Sched.Qty <- as.double(df.ge$Sched.Qty)
df.ge$Quantity.PO.Quantity <- as.double(df.ge$Quantity.PO.Quantity)

#Closed POs only
df.ge <- df.ge %>% filter(Open.PO == "Closed PO")

#Get rid of negative values
df.ge <- df.ge %>% filter(Quantity >= 0)

#Fixing data type issues with pdt and cycle time
df.ge$Pdt <- as.integer(df.ge$Pdt)
df.ge$PO.Cycle.Time <- as.integer(df.ge$PO.Cycle.Time)

#Get rid of NA po cycle time
df.ge <- df.ge[!is.na(df.ge$PO.Cycle.Time),]

#Correcting delivered and ordered fields. Since two different datasets were merged with same values, one is more reliable than other.
df.ge$Quantity[df.ge$Sched.Qty==df.ge$Delivered | df.ge$Sched.Qty==df.ge$Qty.Deliv.] <- df.ge$Sched.Qty[df.ge$Sched.Qty==df.ge$Delivered | df.ge$Sched.Qty==df.ge$Qty.Deliv.]

#Get rid of all non ABC items and items with no material group
df.ge <- df.ge %>% filter(!(ABC.Indicator==""))
df.ge$Material.Group <- factor(df.ge$Material.Group)
df.ge$ABC.Indicator <- factor(df.ge$ABC.Indicator)

#Get rid of po cycle time and pdt of 0
df.ge <- df.ge %>% filter(PO.Cycle.Time > 0 & Pdt > 0)

#Trim columns
df.ge <- df.ge %>% dplyr::select(Plnt, ABC.Indicator, Quantity, Pdt, PO.Cycle.Time)

####Building random forest regression####
train <- createDataPartition(df.ge$Plnt, times=1, p=0.8, list=FALSE)
df.train <- df.ge[train,]
df.test <- df.ge[-train,]

###Parameters that need changing and tuning: ntree (number of trees), nodesize (min number of observations for leaf nodes of decision trees), mtry (number of variables to split on at each node in the tree)###
###Will have to run some loop to optimize the parameters that returns the random forest with the best model metric###
rf.reg <- randomForest(PO.Cycle.Time~., data=df.train, mtry= 2, ntree = 1000, nodesize=5)
rf.pred <- predict(rf.reg, df.test)

#Calculating prediction error
rf.pred <- rf.pred - df.test$PO.Cycle.Time