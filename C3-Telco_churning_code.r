dataset_temp<-read.csv(file='~/Downloads/Product Analyst Technical challenge/Telco-Customer-Churn.csv')
install.packages('sqldf')
library('sqldf')
sqldf("select * from dataset limit 10")
# total rows 7043
sqldf("select COUNT(*) from dataset ") 
#Checking for NA values
colSums(is.na(dataset))
# There are 11 NA values in the TotalCharges column.
#  NA values constitute 0.1 % [7/7043] of the entire dataset .
# They are filtered because they are negligible.
dataset<-na.omit(dataset_temp)



# Variable Selection and Attribute Importance
# Reducing factor levels from 3 to 2
dataset$MultipleLines<-factor(sub("No phone service","No",dataset$MultipleLines))
dataset$OnlineSecurity<-factor(sub("No internet service","No",dataset$OnlineSecurity))
dataset$OnlineBackup<-factor(sub("No internet service","No",dataset$OnlineBackup))
dataset$DeviceProtection<-factor(sub("No internet service","No",dataset$DeviceProtection))
dataset$TechSupport<-factor(sub("No internet service","No",dataset$TechSupport))
dataset$StreamingTV<-factor(sub("No internet service","No",dataset$StreamingTV))
dataset$StreamingMovies<-factor(sub("No internet service","No",dataset$StreamingMovies))
# Factoring Senior Citizen column from int
dataset$SeniorCitizen<-factor(sub(1,"Yes",dataset$SeniorCitizen))
dataset$SeniorCitizen<-factor(sub(0,"No",dataset$SeniorCitizen))
# Factoring tenure column 
sqldf("Select MIN(tenure),Max(tenure) from dataset ")
install.packages("plyr")
library('plyr')
dataset$tenure <-mapvalues(dataset$tenure,from = c(0,1,2,3,4,5,6,7,8,9,10,11,12),to=c("0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12","0 thru 12"))
dataset$tenure <-mapvalues(dataset$tenure,from = c(13,14,15,16,17,18,19,20,21,22,23,24),to=c("13 thru 24","13 thru 24","13 thru 24","13 thru 24","13 thru 24","13 thru 24","13 thru 24","13 thru 24","13 thru 24","13 thru 24","13 thru 24","13 thru 24"))
dataset$tenure <-mapvalues(dataset$tenure,from = c(25,26,27,28,29,30,31,32,33,34,35,36),to=c("24 thru 36","24 thru 36","24 thru 36","24 thru 36","24 thru 36","24 thru 36","24 thru 36","24 thru 36","24 thru 36","24 thru 36","24 thru 36","24 thru 36"))
dataset$tenure <-mapvalues(dataset$tenure,from = c(37,38,39,40,41,42,43,44,45,46,47,48),to=c("37 thru 48","37 thru 48","37 thru 48","37 thru 48","37 thru 48","37 thru 48","37 thru 48","37 thru 48","37 thru 48","37 thru 48","37 thru 48","37 thru 48"))
dataset$tenure <-mapvalues(dataset$tenure,from = c(49,50,51,52,53,54,55,56,57,58,59,60),to=c("49 thru 60","49 thru 60","49 thru 60","49 thru 60","49 thru 60","49 thru 60","49 thru 60","49 thru 60","49 thru 60","49 thru 60","49 thru 60","49 thru 60"))
dataset$tenure <-mapvalues(dataset$tenure,from = c(61,62,63,64,65,66,67,68,69,70,71,72),to=c("61 thru 72","61 thru 72","61 thru 72","61 thru 72","61 thru 72","61 thru 72","61 thru 72","61 thru 72","61 thru 72","61 thru 72","61 thru 72","61 thru 72"))
dataset$tenure <- factor(dataset$tenure)


# Splitting data Train [70%] Test [30%]
adding_index=sample(1:nrow(dataset), size=0.3*nrow(dataset))
test = dataset[adding_index,]
train=dataset[-adding_index,]

# finding correlation 
Newset<-sqldf("Select TotalCharges ,  MonthlyCharges from train")
library(corrplot)
correlation_matrix<-cor(Newset)
corrplot(correlation_matrix,method="number")

# Runing Logistic Regression 
Logistic_regression <- glm(Churn ~ gender+	SeniorCitizen+	Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges,family=binomial(link="logit"),data=train)
anova(Logistic_regression, test="Chisq")
# Testing Logistic Regression 
 Fit_Output <- predict(Logistic_regression,newdata=test,type='response')
 Fit_Output <- ifelse(Fit_Output > 0.5,1,0)
 mis_Classification <- mean(Fit_Output != test$Churn)
print(paste('Logistic Regression Accuracy',1-mis_Classification))

# Confusion Matrix  of the logistic Regression
c_matrix<-table(test$Churn, fitted.results > 0.5)
install.packages('caret')
library("caret")
install.packages('e1071', dependencies=TRUE)

# ROC Curve 
install.packages("ROCR")
library("ROCR")
pr<-prediction(Fit_Output,test$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]

# Decision Tree
install.packages("party")
library("party")
decision_tree <-ctree(Churn~tenure+Contract+PaperlessBilling+PaymentMethod,train)
 predicting_tree <- predict(decision_tree, test)
 table(Predicted = predicting_tree, Actual = test$Churn)

  ptree<-predict(decision_tree,test)
 ptree_train<-predict(decision_tree,train)
 tree1<-table(Predicted=ptree_train,Actual=train$Churn)
 tree2<-table(Predicted=ptree,Actual=test$Churn)
 sum(diag(tree2)/sum(tree2))
 




