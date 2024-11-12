#load data and library
alcohol<- read.csv('student_xxxxxxdata.csv')
str(alcohol)
install.packages('ggplot2')
library(ggplot2)
install.packages('GGally')
library(GGally)

###################################
############### EDA ###############
###################################

ggpairs(alcohol[,-1],
        upper=list(continuous=wrap("points", alpha=0.1, size=0.5,color="#d73027")), 
        lower="blank", axisLabels="none")

#check for NA values
which(is.na(alcohol))

#split into training, valid and testing
n <-  nrow(alcohol) 
ind1 <- sample(c(1:n),round(n/2)) 
ind2 <- sample(c(1:n)[-ind1],round(n/4))
ind3 <- setdiff(c(1:n),c(ind1,ind2)) 
train.data <-  alcohol[ind1, ]
valid.data <-  alcohol[ind2, ]
test.data <- alcohol[ind3, ]
dim(train.data)
dim(valid.data)
dim(test.data)

##############################################
################CLASSIFICATION################
##############################################


# # # # # # # # # # # # # # # # # # # #
# # # # K nearest neighbors # # # # # #
# # # # # # # # # # # # # # # # # # # #


install.packages('class')
library(class)
#what value of k to choose?
## Running knn for different values of k from 1 to 25 
## and recording the test error rate
class.rate<-numeric(30)
for(k in 1:30)
{
  grid <- expand.grid(x=1:100, y=1:100)
  pred.class<-knn(train.data[,2:12], valid.data[,2:12], train.data[,13], k=k)
  class.rate[k]<-sum((pred.class==valid.data[,13]))/length(pred.class)
}

plot(c(1:30),class.rate,type="l",
     main="Correct Classification Rates for the Test Data for a range of k",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7)
which.max(class.rate)

#apply KNN
pred.class<-knn(train.data[,2:12], valid.data[,2:12], train.data[,13], k=8, prob = TRUE)
class.rate<-sum((pred.class==valid.data[,13]))/length(pred.class)
class.rate
#how good are our results?
confusionMatrix(as.factor(valid.data[,13]),as.factor(pred.class) )

#roc curve
install.packages('pROC')
library(pROC)
install.packages('ROCR')
library(ROCR)

install.packages('smallstuff')
library(smallstuff)
ROCknn(pred.class,valid.data[,13])
#pred.class
#knn.roc <- roc(valid.data[,13], attributes(pred.class)$prob)
#print(knn.roc)
#plot(knn.roc, main="Validation data ROC")

# # # # # # # # # # # # # # # # # # # #
# # # Tree based classification # # # #
# # # # # # # # # # # # # # # # # # # #

install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
first_tree <- rpart(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS, 
                    data = train.data, method = "class") #use method="class" for classification
rpart.plot(first_tree,type=2,extra=4)

#predict based on our validation data
probs<- predict(first_tree,newdata = valid.data[,2:12],type="prob")
predictions<- predict(first_tree,newdata = valid.data[,2:12],type="class")
confusionMatrix(as.factor(valid.data[,13]),as.factor(predictions) )

#try fix the low class error with random forest
install.packages('randomForest')
library(randomForest)
train.data$Class <- as.factor(train.data$Class)
second_tree <- randomForest(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,
                            data=train.data,mtry=6,ntree=200)

second_tree
predictions_bag <- predict(second_tree, valid.data[,2:12],"class")
confusionMatrix(as.factor(valid.data[,13]),as.factor(predictions_bag) )

#roc curve
predict(second_tree, valid.data[2:12], type="prob")
tree.preds <- predict(second_tree, valid.data[2:12], type="prob")[, 2]
library(pROC)
tree.roc <- roc(valid.data[,13], tree.preds)
print(tree.roc)
plot(tree.roc, main="Validation data ROC")



# # # # # # # # # # # # # # # # # # # #
# # # # # #      SVM        # # # # # #
# # # # # # # # # # # # # # # # # # # #

install.packages('e1071')
library(e1071)
# Fit the classification SVM for different values of C 
# and calculate the validation prediction error

pred.error<-function(pred,truth)
{
  1-sum(diag(table(pred,truth)))/length(truth)
}

C.val<-c(0.1,0.5,1,2,5,10)
C.error<-numeric(length(C.val))
for(i in 1:length(C.val))
{
  model<-svm(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS
             ,data=train.data,type="C-classification",kernel="linear",
             cost=C.val[i])
  pred.model<-predict(model, valid.data[,2:12])
  C.error[i]<-pred.error(pred.model,valid.data[,13])
}
C.sel<-C.val[min(which.min(C.error))]
C.sel
plot(C.val,C.error,type="b")
abline(v=C.sel,lty=2)
#fit with the lowest cost value
final.svm<-svm(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,
               data=train.data,kernel="linear",cost=C.sel,
               type="C-classification")
summary(final.svm)
#calculate accuracy
pred.test<-predict(final.svm,valid.data[,2:12])
confusionMatrix(as.factor(valid.data[,13]),as.factor(pred.test) )
#roc curve on validation data
install.packages('pROC')
library(pROC)

fitted.valid<-attributes(predict(final.svm,valid.data[,2:12], decision.values = TRUE))$decision.values
outcome<-as.numeric(valid.data[,13])
svmROC<-roc(outcome,as.numeric(fitted.valid))
plot(svmROC,main="Validation data ROC")
#area under curve
svmROC$auc


#kernalisation
#repeat the above with polinomial kernal

pred.error<-function(pred,truth)
{
  1-sum(diag(table(pred,truth)))/length(truth)
}

C.val<-c(0.1,0.5,1,2,5,10)
C.error<-numeric(length(C.val))
for(i in 1:length(C.val))
{
  model<-svm(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS
             ,data=train.data,type="C-classification",kernel="polynomial",
             cost=C.val[i])
  pred.model<-predict(model, valid.data[,2:12])
  C.error[i]<-pred.error(pred.model,valid.data[,13])
}
C.sel<-C.val[min(which.min(C.error))]
C.sel
plot(C.val,C.error,type="b")
abline(v=C.sel,lty=2)
#fit with the lowest cost value
final.svm<-svm(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,
               data=train.data,kernel="polynomial",cost=C.sel,
               type="C-classification")
summary(final.svm)
#calculate accuracy
pred.test<-predict(final.svm,valid.data[,2:12])
confusionMatrix(as.factor(valid.data[,13]),as.factor(pred.test) )

#roc curve on validation data
install.packages('pROC')
library(pROC)

fitted.valid<-attributes(predict(final.svm,valid.data[,2:12], decision.values = TRUE))$decision.values
outcome<-as.numeric(valid.data[,13])
svmROC<-roc(outcome,as.numeric(fitted.valid))
plot(svmROC,main="Validation data ROC")
#area under curve
svmROC$auc


#######
#Model based classification
#######


# # # # # # # # # # # # # # # # # # # #
# # # # # #      CVA        # # # # # #
# # # # # # # # # # # # # # # # # # # #

install.packages('MASS')
library(MASS)
#set G to the number of unique classes (2)
G<-length(unique(train.data[,13]))

#extract the number of variables
d<-ncol(train.data[,2:12])

#cva model
model.cva<-lda(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,
               data=train.data,prior=c(rep(1/G,G)))
# Absolute Difference in means
mean.diff<- model.cva$means[1,]-model.cva$means[2,]
sort(abs(mean.diff),decreasing = TRUE)
# Absolute loadings
abs(model.cva$scaling)
order(abs(model.cva$scaling))
colnames(train.data[2:12])[order(abs(model.cva$scaling),decreasing=TRUE)][1]
colnames(train.data[2:12])[order(abs(model.cva$scaling),decreasing=TRUE)][2]

#test on valid data
test.pred<-predict(model.cva,valid.data)
test.pred.class<-test.pred$class
test.scores<-as.numeric(test.pred$x)
test.roc<-roc(valid.data[,13],test.scores)
plot(test.roc, main="Validation data ROC")
auc(test.roc)

confusionMatrix(as.factor(valid.data[,13]),as.factor(test.pred$class) )

# # # # # # # # # # # # # # # # # # # #
# # # # # #      LDA        # # # # # #
# # # # # # # # # # # # # # # # # # # #

mod.lda<-lda(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,
             data=train.data)
mod.lda
pred.valid.lda<-predict(mod.lda,valid.data)
confusionMatrix(as.factor(valid.data[,13]),as.factor(pred.valid.lda$class) )

#roc curve
test.pred<-predict(mod.lda,valid.datavalid.data[,2:12])
roc(valid.data[,13],test.pred$posterior[,2],plot=TRUE, legacy.axes = TRUE, 
    percent =TRUE, xlab="Specificity", ylab="Sensitivity", main="Validation data ROC")

# # # # # # # # # # # # # # # # # # # #
# # # # # #      QDA        # # # # # #
# # # # # # # # # # # # # # # # # # # #

mod.qda<-qda(Class~ Age+ Education+X.Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,
             data=train.data)
mod.qda
pred.valid.qda<-predict(mod.qda,valid.data)
confusionMatrix(as.factor(valid.data[,13]),as.factor(pred.valid.qda$class) )

#roc curve
test.pred<-predict(mod.qda,valid.data[,2:12])

roc(valid.data[,13],test.pred$posterior[,2],plot=TRUE, legacy.axes = TRUE, 
    percent =TRUE, xlab="Specificity", ylab="Sensitivity", main="Validation data ROC")



#Best model is KNN.

#apply KNN on testing data
pred.class<-knn(train.data[,2:12], test.data[,2:12], train.data[,13], k=8, prob = TRUE)
class.rate<-sum((pred.class==test.data[,13]))/length(pred.class)
class.rate
#how good are our results?
confusionMatrix(as.factor(test.data[,13]),as.factor(pred.class) )




