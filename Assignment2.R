# ------------------------------------------------
# Assignment 2
# Name: Ke Er Ang
# Student ID: 32581343
#-------------------------------------------------
library(dplyr)
library(skimr)
library(tree)
library(e1071)
library(adabag)
library(randomForest)
library(ROCR)
library(ggplot2)
library(xgboost)
library(ipred)
library(pROC)
library(neuralnet)

rm(list = ls())
Phish <- read.csv("PhishingData.csv")
set.seed(32581343) # Your Student ID is the random seed
L <- as.data.frame(c(1:50))
L <- L[sample(nrow(L), 10, replace = FALSE),]
Phish <- Phish[(Phish$A01 %in% L),]
PD <- Phish[sample(nrow(Phish), 2000, replace = FALSE),] # sample of 2000 rows

View(PD)

'--------------------- Question 1 ----------------------'
# Check occurrences of NA values in all columns
colSums(is.na(PD))
# Drop NA
PD = na.omit(PD) # left 1535 rows

View(PD)
str(PD)
attach(PD)

# What is the proportion of phishing sites to legitimate sites? legitimate =0 , phishing = 1
props1 = sum(PD$Class == 1) # phishing
print(props1) # 505
propotion.props1 = props1/nrow(PD)*100
print(propotion.props1)# 32.89902

props0 = sum(PD$Class == 0) # legitimate
print(props0) # 1030
propotion.props0 = props0/nrow(PD)*100
print(propotion.props0) # 67.10098

proportion = props1/props0 # 0.490291262135922
print(paste("Proportion:",proportion)) # 0.49

# correlation 
cor = cor(PD)
my_palette = colorRampPalette(c("#FFD700","#FFE455", "#FFF1AA", "#FFFFFF","#AAE9FF","#54D4FF","#00BFFF","#007FFF","#003FFF","#0000FF"))(n=299)
cor[upper.tri(cor)] =  NA
heatmap(cor,Rowv = NA, Colv = NA, symm = TRUE, col = my_palette)

# descriptive statistic
skim(PD) # before scaled

'---------------------Question 2----------------------'
# scale the data
PD[,1:25] = scale(PD[,1:25])
# factorize class attribute
PD$Class = factor(PD$Class)
# drop attributes
PD$A03 = NULL
PD$A07 = NULL
PD$A25 = NULL

'---------------------Question 3----------------------'
# Divide your data into a 70% training and 30% test set 
set.seed(32581343) #Student ID as random seed 
train.row = sample(1:nrow(PD), 0.7*nrow(PD)) 
PD.train = PD[train.row,]
PD.test = PD[-train.row,]
'---------------------Question 4----------------------'
# Decision tree
DTree.fit = tree(Class~., data = PD.train)
# summary(DTree.fit) # Misclassification error rate: 0.2197 = 236 / 1074 
DTree.predict = predict(DTree.fit,PD.test, type = "class")
plot(DTree.fit)
text(DTree.fit)
DTree.conf = table(actual = PD.test$Class, predicted = DTree.predict)
DTree.conf

# Naive Bayes
set.seed(32581343)
NB.fit = naiveBayes(Class ~. ,data = PD.train)
NB.predict = predict(NB.fit, PD.test, type = "class")
NB.conf = table(actual = PD.test$Class, predicted =NB.predict)
NB.conf

# Bagging
set.seed(32581343)
Bagging.fit = bagging(Class ~. ,data = PD.train, mfinal = 15)
Bagging.predict = predict.bagging(Bagging.fit, PD.test, type = "class")
Bagging.conf = Bagging.predict$confusion
Bagging.conf

# Boosting
set.seed(32581343)
Boosting.fit = boosting(Class ~. ,data = PD.train, mfinal = 10)
Boosting.predict = predict.boosting(Boosting.fit, newdata = PD.test, type = "class")
Boosting.conf = Boosting.predict$confusion
Boosting.conf

# Random Forest
set.seed(32581343)
RForest.fit = randomForest(Class ~. ,data = PD.train,na.action = na.exclude)
RForest.predict = predict(RForest.fit, PD.test, type = "class")
RForest.conf = table(actual = PD.test$Class, predicted = RForest.predict)
RForest.conf

'---------------------Question 5----------------------'
DTree.accuracy = (sum(diag(DTree.conf))/sum(DTree.conf))
DTree.accuracy

NB.accuracy = (sum(diag(NB.conf))/sum(NB.conf))
NB.accuracy

Bagging.accuracy = (sum(diag(Bagging.conf))/sum(Bagging.conf))
Bagging.accuracy

Boosting.accuracy = (sum(diag(Boosting.conf))/sum(Boosting.conf))
Boosting.accuracy

RForest.accuracy = (sum(diag(RForest.conf))/sum(RForest.conf))
RForest.accuracy

'---------------------Question 6----------------------'
# Create ROC for all models
# Tree
DTree.conf = predict(DTree.fit,PD.test, type = "vector")
DTree.conf.pred = ROCR::prediction(DTree.conf[,2],PD.test$Class)
DTree.conf.pred.perf = ROCR::performance(DTree.conf.pred, "tpr", "fpr")
plot(DTree.conf.pred.perf, col = "orange", loc = "bottomleft")
abline(0,1, col = "grey")

# Naive Bayes
NB.conf = predict(NB.fit, PD.test, type = "raw")
NB.conf.pred = ROCR::prediction(NB.conf[,2],PD.test$Class)
NB.conf.pred.perf = ROCR::performance(NB.conf.pred, "tpr", "fpr")
plot(NB.conf.pred.perf, col = "blue", add= TRUE)

# Bagging
Bagging.conf.pred = ROCR::prediction(Bagging.predict$prob[,2],PD.test$Class)
Bagging.conf.pred.perf = ROCR::performance(Bagging.conf.pred, "tpr", "fpr")
plot(Bagging.conf.pred.perf, col = "red", add= TRUE)

# Boosting
Boosting.conf.pred = ROCR::prediction(Boosting.predict$prob[,2],PD.test$Class)
Boosting.conf.pred.perf = ROCR::performance(Boosting.conf.pred, "tpr", "fpr")
plot(Boosting.conf.pred.perf, col = "darkturquoise", add= TRUE)

# Random Forest
RForest.conf = predict(RForest.fit, PD.test, type="prob")
RForest.conf.pred = ROCR::prediction(RForest.conf[,2],PD.test$Class)
RForest.conf.pred.perf = ROCR::performance(RForest.conf.pred, "tpr", "fpr")
plot(RForest.conf.pred.perf, col = "green", add= TRUE)

# Add a legend
legend("bottomright",            
       legend = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"),   
       col = c("orange", "blue", "red", "darkturquoise", "green"),  
       lty = 1,  # Line type
       cex = 0.8) 


# Create AUC for all models
# Decision Tree
DTree.auc.perf = performance(DTree.conf.pred, "auc")
DTree.auc = round(as.numeric(DTree.auc.perf@y.values), 4)
DTree.auc

# Naive Bayes
NB.auc.perf = performance(NB.conf.pred, "auc")
NB.auc = round(as.numeric(NB.auc.perf@y.values), 4)
NB.auc

# Bagging
Bagging.auc.perf = performance(Bagging.conf.pred, "auc")
Bagging.auc = round(as.numeric(Bagging.auc.perf@y.values), 4)
Bagging.auc

# Boosting
Boosting.auc.perf = performance(Boosting.conf.pred, "auc")
Boosting.auc = round(as.numeric(Boosting.auc.perf@y.values), 4)
Boosting.auc

# Random Forest
RForest.auc.perf = performance(RForest.conf.pred, "auc")
RForest.auc = round(as.numeric(RForest.auc.perf@y.values), 4)
RForest.auc

'---------------------Question 8----------------------'
# Find the most important attributes
# Tree
summary(DTree.fit)

# Bagging
bag.sorted.importance = sort(Bagging.fit$importance,decreasing = T)
print(bag.sorted.importance)

# Boosting
boost.sorted.importance = sort(Boosting.fit$importance,decreasing = T)
print(boost.sorted.importance)

# Random Forest
forest.sorted.importance = RForest.fit$importance[order(-RForest.fit$importance),]
print(forest.sorted.importance)

'-------------------- Question 9  ------------------------'
# Create a smaller tree with lesser attributes
set.seed(32581343) 
View(PD)
# Extract only the used variables in original Decision Tree
DTree.small = PD[,c('A01','A23','A18','A22','A12','Class')]

# Fit the model and plot the tree
DTree.small.fit = tree(Class~A01+A23+A18, data = PD.train)
plot(DTree.small.fit)
text(DTree.small.fit, pretty = 0)
summary(DTree.small.fit)

# Do prediction with test data
DTree.small.fit.predict = predict(DTree.small.fit, PD.test, type = 'class')

# Calculate accuracy
DTree.small.conf = table(actual = PD.test$Class, predicted = DTree.small.fit.predict)
DTree.small.accuracy = (sum(diag(DTree.small.conf ))/sum(DTree.small.conf ))
DTree.small.accuracy

# Calculate AUC
DTree.small.conf = predict(DTree.small.fit, PD.test, type = 'vector')
DTree.small.conf.pred = ROCR::prediction(DTree.small.conf[,2],PD.test$Class)
DTree.small.auc.perf = ROCR::performance(DTree.small.conf.pred, "auc")
DTree.small.auc = round(as.numeric(DTree.small.auc.perf@y.values), 4)
DTree.small.auc

'-------------------- Question 10 ------------------------'
# Find best tree by doing cross validation

#### CV tree
set.seed(32581343)
DTree.cv = cv.tree(DTree.fit, FUN= prune.misclass)
DTree.cv 
# prune using size 5 
DTree.cv.prune = prune.misclass(DTree.fit, best = 5)
summary(DTree.cv.prune)
plot(DTree.cv.prune)
text(DTree.cv.prune, pretty =0)

# check accuracy using the pruned decision tree
DTree.cv.prune.predict = predict(DTree.cv.prune, PD.test, type = 'class')
DTree.cv.prune.conf = table(actual = PD.test$Class, predicted = DTree.cv.prune.predict)
DTree.cv.prune.accuracy = sum(diag(DTree.cv.prune.conf)/sum(DTree.cv.prune.conf))
DTree.cv.prune.accuracy

# CV tree AUC
set.seed(32581343)
DTree.cv.conf = predict(DTree.cv.prune,PD.test, type = "vector")
DTree.cv.conf.pred = ROCR::prediction(DTree.cv.conf[,2],PD.test$Class)
DTree.cv.auc.perf = ROCR::performance(DTree.cv.conf.pred, "auc")
DTree.cve.auc = round(as.numeric(DTree.cv.auc.perf@y.values), 4)
DTree.cve.auc 

#### CV forest 
set.seed(32581343)
# cross validation for 
PD.rfcv = rfcv(trainx=PD.train[,-c(23)], # training attributes (all columns except Class)
               trainy=PD.train[,c(23)],  # target attributes (Class)
               cv.fold=10,step=0.5,      # 10-folds cross validation, number of features reduced at step size of 0.5
               mtry=function(p) max(1,floor(sqrt(p))), # number of features to try at each split, min is 1                         
               recursive=TRUE) # remove the least important features at each steps and build the model again                                              
PD.rfcv$error.cv                                                                                                


# check accuracy using the pruned Random Forest
set.seed(32581343)
RForest.cv = randomForest(Class~., data = PD.train, mtry = 12)
RForest.cv.pred = predict(RForest.cv, PD.test)
RForest.cv.conf = table(actual = PD.test$Class, predicted = RForest.cv.pred )
RForest.cv.accuracy = sum(diag(RForest.cv.conf)/sum(RForest.cv.conf))
RForest.cv.accuracy 

# CV forest AUC
set.seed(32581343)
RForest.cv.conf = predict(RForest.cv, PD.test, type="prob")
RForest.cv.conf.pred = ROCR::prediction(RForest.cv.conf[,2],PD.test$Class)
RForest.cv.auc.perf = ROCR::performance(RForest.cv.conf.pred, "auc")
RForest.cv.auc = round(as.numeric(RForest.cv.auc.perf@y.values), 4)
RForest.cv.auc

#### CV Bagging & Boosting
# Retrieve attributes' name which have importance score > 0
bag.important.attribute = names(bag.sorted.importance[bag.sorted.importance> 0])
boost.important.attribute = names(boost.sorted.importance[boost.sorted.importance> 0])

# Find best number of folds for Bagging and Boosting
find.best.v = function(data, model.type, important.attributes ) {
  best.v = 0
  best.accuracy = 0
  
  for (v in 5:10) {
    set.seed(32581343)
    formula.string = paste(" Class ~", paste(important.attributes , collapse = " + "))
  
    if (model.type == "bagging") {# CV for bagging
      cv.model = bagging.cv(as.formula(formula.string), v = v, data = data)
    } else if (model.type == "boosting") { # CV for boosting
      cv.model = boosting.cv(as.formula(formula.string), v = v, data = data)
    }
    
    # Find accuracy of each created model 
    cv.conf = cv.model$confusion
    cv.accuracy = sum(diag(cv.conf)) / sum(cv.conf)
    
    # If the model's accuracy is better than best accuracy, then it will become the best accuracy
    if (cv.accuracy > best.accuracy) {
      best.accuracy = cv.accuracy
      best.v = v
    }
  }
  return(list(best.v = best.v, best.accuracy = best.accuracy))
}

PD.best.v.bag = find.best.v(PD.train,"bagging", bag.important.attribute)
print(paste("Best v bagging:",PD.best.v.bag$best.v))
print(paste("Best accuracy:", PD.best.v.bag$best.accuracy))

PD.best.v.boost = find.best.v(PD.train, "boosting", boost.important.attribute)
print(paste("Best v boosting:", PD.best.v.boost$best.v))
print(paste("Best accuracy:", PD.best.v.boost$best.accuracy))

# Fit the tuned model for bagging
set.seed(32581343)
Bagging.cv = bagging(Class ~ ., data = PD.train, mfinal = 50)

# Do prediction on PD.test and find accuracy 
Bagging.cv.predict = predict.bagging(Bagging.cv, PD.test, type = "class")
Bagging.cv.conf = Bagging.cv.predict$confusion
Bagging.cv.conf.accuracy = sum(diag(Bagging.cv.conf )) / sum(Bagging.cv.conf )
Bagging.cv.conf.accuracy

# Find AUC for bagging cv
Bagging.cv.conf.pred = ROCR::prediction(Bagging.cv.predict$prob[,2],PD.test$Class)
Bagging.cv.auc.perf = ROCR::performance(Bagging.cv.conf.pred, "auc")
Bagging.cv.auc = round(as.numeric(Bagging.cv.auc.perf@y.values), 4)
Bagging.cv.auc

# Fit the tuned model for boosting
set.seed(32581343)
Boosting.cv = boosting(Class ~ ., data = PD.train, mfinal=150)
# Do prediction on PD.test and find accuracy 
Boosting.cv.predict = predict.boosting(Boosting.cv, PD.test, type = "class")
Boosting.cv.conf = Boosting.cv.predict $confusion
Boosting.cv.conf.accuracy = sum(diag(Boosting.cv.conf )) / sum(Boosting.cv.conf)
Boosting.cv.conf.accuracy

# Find AUC for boosting cv
Boosting.cv.conf.pred = ROCR::prediction(Boosting.cv.predict$prob[,2],PD.test$Class)
Boosting.cv.auc.perf = ROCR::performance(Boosting.cv.conf.pred, "auc")
Boosting.cv.auc = round(as.numeric(Boosting.cv.auc.perf@y.values), 4)
Boosting.cv.auc


'-------------------- Question 11 ------------------------'
set.seed(32581343)
# Fit ANN model, choose only top 8 most important attributes

ANN.fit = neuralnet(Class == 1 ~ A01 + A23 + A22 + A18 + A08 + A24 + A12, PD.train, hidden=6,linear.output = FALSE)

ANN.pred = neuralnet::compute(ANN.fit, PD.test[,c("A01", "A23", "A22", "A18", "A08", "A24", "A12")])
ANN.pred = as.data.frame(round(ANN.pred$net.result,0))

# Calculate ANN accuracy
ANN.conf = table(observed = PD.test$Class, predicted = ANN.pred$V1)
ANN.conf.accuracy = sum(diag(ANN.conf)/sum(ANN.conf))
ANN.conf.accuracy

# Calculate ANN AUC
ANN.conf.pred = ROCR::prediction(ANN.pred[,c(1)],PD.test$Class)
ANN.conf.pred.perf = ROCR::performance(ANN.conf.pred, "auc")
ANN.conf.pred.auc = round(as.numeric(ANN.conf.pred.perf@y.values), 4)
ANN.conf.pred.auc 

# plot ANN diagram
plot(ANN.fit)

'-------------------- Question 12 ------------------------'
PD.xg = PD
set.seed(32581343)

# Transform Class attribute to numeric
label = PD.xg$Class
label = as.numeric(PD.xg$Class) -1
PD.xg$Class = NULL # drop Class attributes

# Split data to training & testing
xg.train.row = sample(1:nrow(PD.xg), 0.7*nrow(PD.xg)) 
PD.xg.train = PD.xg[xg.train.row,]
PD.xg.test = PD.xg[-xg.train.row,] 

PD.train.data = as.matrix(PD.xg.train)
PD.train.label = label[xg.train.row] # create label for performance evaluation  

PD.test.data = as.matrix(PD.xg.test)
PD.test.label = label[-xg.train.row]# create label for performance evaluation  

# Transform train and test data into xgb.Matrix 
xg.train = xgb.DMatrix(data = PD.train.data, label = PD.train.label)
xg.test = xgb.DMatrix(data = PD.test.data, label = PD.test.label)

# Define parameters
params = list(
  booster = "gbtree", # tree based model
  objective = "binary:logistic", # use logistic regression to do binary classification
  eval_metric = "auc"
)
? xgb.train
# Train the model, stop if no improvement after 10 rounds
PD.xg.fit = xgb.train(params = params, data = xg.train, nrounds = 100, watchlist = list(eval = xg.test, train = xg.train), early_stopping_rounds = 10)
# Do prediction
PD.xg.pred = predict(PD.xg.fit,PD.test.data)

# Convert probabilities to binary class labels
PD.xg.pred.labels = ifelse(PD.xg.pred > 0.5, 1, 0)

# Create a confusion matrix
PD.xg.conf = table(Predicted = PD.xg.pred.labels, Actual = PD.test.label)
print(PD.xg.conf)

# Calculate accuracy
accuracy = sum(diag(PD.xg.conf)) / sum(PD.xg.conf)
print(paste("Accuracy:", accuracy))

# Calculate AUC
xg.pred = ROCR::prediction(PD.xg.pred,PD.test.label)
xg.perf = ROCR::performance(xg.pred, "auc")
xg.auc = xg.perf@y.values[[1]]
print(paste("AUC:", xg.auc))

# Plot one of the tree
install.packages("DiagrammeR")
model_trees = xgb.model.dt.tree(model = PD.xg.fit)
length(model_trees)
xgb.plot.tree(model = PD.xg.fit, trees = 0)
