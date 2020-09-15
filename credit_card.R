if (!require(package)) install.packages('tidyverse')
if (!require(package)) install.packages('readr')
if (!require(package)) install.packages('readr')
if (!require(package)) install.packages('corrplot')
if (!require(package)) install.packages('smotefamily')
if (!require(package)) install.packages('randomForest')
if (!require(package)) install.packages('devtools')
if (!require(package)) install.packages('MASS')
if (!require(package)) install.packages('rpart')
if (!require(package)) install.packages('rpart.plot')
if (!require(package)) install.packages('GGally')
library(tidyverse)
library(readr)
library(readr)
library(corrplot)
library(smotefamily)
library(randomForest)
library(devtools)
library(MASS)
library(rpart)
library(rpart.plot)
library(GGally)

##Reading the downloaded credit card data ###
###Data Exploration####
credit_card <- read.csv("creditcard.csv")
head(credit_card)
str(credit_card)
table(credit_card$Class)
as.numeric(cor(credit_card))


##Plotting histogrm of Amount column ###
credit_card %>% 
  ggplot(aes(Amount)) +
  geom_histogram(bins = 10,color = "white") + 
  ggtitle("Distribution of Amount") +
  xlab("Amount") +
  ylab("Number of Amount") 

##Filtering and plotting Amount less than 100 ###
amount_filter <- credit_card %>% 
  filter(Amount<=100) 

amount_filter %>% 
  ggplot(aes(Amount)) +
  geom_histogram(bins = 10,color = "white") + 
  ggtitle("Distribution of Amount") +
  xlab("Amount") +
  ylab("Number of Amount")

summary(credit_card$Amount)
length(credit_card$Amount)
length(credit_card$Amount)
max(credit_card$Amount)
mean(credit_card$Amount <=5)
mean(credit_card$Amount <=100)
mean(credit_card$Amount <= 200)

## Plotting histogram of Time column ###
credit_card %>% 
  ggplot(aes(Time)) +
  geom_histogram(bins = 10,color = "white") + 
  ggtitle("Distribution of Time") +
  xlab("Time") +
  ylab("Number of Time") 


cor(credit_card$Time,credit_card$Amount)## Correlation of Amount and Time

##Plotting correlation matrix of the data
ggcorr(credit_card)

credit_card$Class <- factor(credit_card$Class)## Redefining class column as factor
credit_card$Amount <-  scale(credit_card$Amount)## Scaling Amount Column

###Data Modelling###
set.seed(1234)
test_index <- createDataPartition(y=credit_card$Class, p = 0.2, times = 1, list = F)
test_set <- credit_card[test_index,]
train_set <- credit_card[-test_index,]
dim(test_set)
dim(train_set)

### MODEL TRAINING####

###LDA#########
lda <- train(Class ~., data = train_set, method = "lda")
predict_lda <- predict(lda,test_set)
lda_CM <-  confusionMatrix(predict_lda,test_set$Class)

#### Applying SMOTE ###
### Setting positive class to be 65% of the data and rest to be 35%#####
set.seed(1234)
n0 <- nrow(subset(credit_card,Class==0))
n1 <- nrow(subset(credit_card,Class==1))
r0 <- 0.9
ntimes <- ((1 -r0) / r0) * (n0/n1) - 1

###Creating Smote datset###
set.seed(1234)
smote_output <- SMOTE(X = credit_card[ , -c(1, 31)], target=credit_card$Class, K = 5, dup_size = ntimes)
credit_smote <- smote_output$data
colnames(credit_smote)[30] <- "Class"
table(credit_smote$Class)
prop.table(table(credit_smote$Class))

### Plotting datasets after and before the Smote####
credit_card %>%
  ggplot(aes(V1,V2,color=Class)) +
  geom_point()

credit_smote %>%
  ggplot(aes(V1,V2,color=Class)) +
  geom_point()

dim(credit_smote)

#####Partitioning the SMOTE datset to train and test_set#####

credit_smote$Class <-  factor(credit_smote$Class)
set.seed(1234)
test_index_smote <- createDataPartition(y=credit_smote$Class, times=1,p = 0.2, list = F)
train_smote <- credit_smote[-test_index_smote,]
test_smote <- credit_smote[test_index_smote,]
dim(train_smote)
dim(test_smote)
prop.table(table(credit_smote$Class))

###LDA.......###############

train_smote$Amount <- as.vector(train_smote$Amount)
test_smote$Amount <- as.vector(test_smote$Amount)
lda_smote <- train(Class ~., data = train_smote, method = "lda")
predict_lda_smote <- predict(lda_smote,test_smote,na.action = na.pass)
lda_CM_smote <-  confusionMatrix(table(predict_lda_smote,test_smote$Class))

####GLM###

glm_smote <- glm(Class ~., data = train_smote, family = binomial())
glm_predict_smote <- predict(glm_smote, test_smote)
prediction <- if_else(glm_predict_smote>=0,1,0)
glm_CM_Smote <- confusionMatrix(as.factor(prediction),as.factor(test_smote$Class))
glm_CM_Smote

##### Decision Tree Model ###

decision_tree <- rpart(Class ~ .,data=credit_smote, method = "class")
decision_tree_predict <- predict(decision_tree, test_smote,type = 'class',na.action = na.pass)
rpart.plot(decision_tree)
decision_tree_CM <- confusionMatrix(factor(decision_tree_predict), test_smote$Class)
DT_Smote_CM <- confusionMatrix(factor(test_smote$Class), factor(decision_tree_predict))

#####Random Forest####
rf_smote <- randomForest(Class ~ ., data = train_smote)
rf_predict_smote <- predict(rf_smote, test_smote)
rf_CM_smote <- confusionMatrix(rf_predict_smote, test_smote$Class)
length(rf_predict_smote) 
rf_CM_smote








