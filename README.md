# Charger les bibliothèques nécessaires
library(tidyverse)
library(caret)
library(dplyr)
library(corrplot)
library(ggplot2)
library(pROC)

#charger les données
getwd()

train <- read.table("/Users/nausicaa/Desktop/train.csv",header = T,sep =",", stringsAsFactors =T)
score <- read.table("/Users/nausicaa/Desktop/score.csv",header = T,sep =",", stringsAsFactors = T)

l_train <- colnames(train)
l_score <- colnames(score)


setdiff(l_train,l_score)
setdiff(l_score,l_train)

# nettoyer les données
sum(is.na(train))
donneesmanq<-sapply(train,function(x)sum(is.na(x))/length(x))
donneesmanq

seuil_na <- 0.9
delete<-which(donneesmanq>seuil_na)
train<-train[,c(-delete)]

colonne_na<-which(donneesmanq>0 & donneesmanq< 0.9)
donneesmanq[colonne_na]

median(train$phone_price, na.rm = TRUE)
train[is.na(train$phone_price),"phone_price"]<- median(train$phone_price, na.rm = TRUE)

median(train$time_since_technical_problems , na.rm = TRUE)
train[is.na(train$time_since_technical_problems),"time_since_technical_problems"]<- median(train$time_since_technical_problems , na.rm = TRUE)

median(train$time_since_complaints, na.rm = TRUE)
train[is.na(train$time_since_complaints),"time_since_complaints"]<- median(train$time_since_complaints, na.rm = TRUE)


sum(is.na(train))

#valeur extremes 
# Define a function to detect outliers using the IQR method
detect_outliers <- function(column) {
  if (is.numeric(column))
    Q1 <- quantile(column, 0.25, na.rm = TRUE) # First quartile
    Q3 <- quantile(column, 0.75, na.rm = TRUE) # Third quartile
    IQR <- Q3 - Q1                            # Interquartile range
    lower_bound <- Q1 - 1.2 * IQR             # Lower bound
    upper_bound <- Q3 + 1.2 * IQR             # Upper bound
    return(column < lower_bound | column > upper_bound)} #
train$phone_price[which(detect_outliers(train$phone_price))]
detect_outliers(train$phone_price)

p <- ggplot(train, aes(x=phone_price)) + 
  geom_boxplot()
p

#fonction pour déterminer la moyenne de churn par groupe
seuil_phone_price <- unique(quantile(train$phone_price, probs = seq(0, 1, 0.1)))
train$phone_price_cat <- cut(train$phone_price, seuil_phone_price)``

seuil_age <- unique(quantile(train$age, probs = seq(0, 1, 0.1)))
train$age_cat <- cut(train$age, seuil_age)


train$phone_price_cat

train$churn_in_12_num <-as.numeric(train$churn_in_12=="True")

grouped_data <- train %>%
  group_by(age_cat, promo) %>%
  summarise(Mean_churn = mean(churn_in_12_num))

#split train test
set.seed(123)
trainIndex <- createDataPartition(train$churn_in_12_num, p = .80,
                                  list = FALSE,
                                  times = 1)
train_data <- train[trainIndex,]
test_data <- train[-trainIndex,]

# Construire un modèle de régression logistique
columns_to_remove<-c("id","family_id", "active", "unique_id", "phone_price_cat", "churn_in_12_num", "unlimited_text")
train_data2 <- train_data[,setdiff(names(train_data), columns_to_remove)]
factor_indices <- sapply(train_data2, is.numeric)
sapply(train_data2[,factor_indices],function(x) length(unique(x)))
factor_indices <- sapply(train_data2, is.factor)
sapply(train_data2[,factor_indices],function(x) length(levels(x)))
model <- glm(churn_in_12 ~ ., data = train_data2, family = binomial)
sum(is.na(train_data))


# Faire des prédictions sur l'ensemble de test
predictions <- predict(model, newdata = test_data, type = "response")
# si point de coupure 0.5
test_data$predicted <- ifelse(predictions > 0.5, 1, 0)

library(ROCR)
roc_curve <- prediction(predictions,test_data$churn_in_12)
curve_R <- performance(roc_curve, "tpr", "fpr")
plot(curve_R, main ="Courbe ROC - Validation Externe", col= "darkblue")
abline(a=0, b=1, col ="red")

# Calcul de l'AUC
auc_vecor <- performance(roc_curve, measure = "auc")
auc_value <- auc_vecor@y.values[[1]]
auc_value

train_data_complet <- train[,setdiff(names(train), columns_to_remove)]
model_complet <- glm(churn_in_12 ~ ., data = train_data_complet, family = binomial)

summary(model_complet)

#voir les na dans une data
sapply(score,function(x) sum(is.na(x)))

inpute_median <- function(x){
  median_value <- median(x, na.rm = TRUE)
  x[is.na(x)] <- median_value
  return(x)
}


sum(is.na(inpute_median(score$voice_minutes)))

score$time_since_technical_problems <- inpute_median(score$time_since_technical_problems)
score$phone_price <- inpute_median(score$phone_price)
score$time_since_complaints <- inpute_median(score$time_since_complaints)
score$time_since_voice_overage <- inpute_median(score$time_since_voice_overage)
score$time_since_voice_overage <- inpute_median(score$time_since_voice_overage)
score$time_since_data_overage <- inpute_median(score$  time_since_data_overage)
score$time_since_overage <- inpute_median(score$time_since_overage)
score$voice_minutes <- inpute_median(score$voice_minutes)

#Application model prediction à score 
predictions_score <- predict(model_complet, newdata = score, type = "response")
score$pred <- predictions_score

temp <- score[score$unique_family==1000012079,]
result <- score %>%
  group_by(unique_family) %>%
  summarise(nb = n(),
            meanpred = mean(pred,na.rm = TRUE),
            maxpred = max(pred,na.rm = TRUE),
            minphoneprice = min(phone_price,na.rm = TRUE)
            , .groups = 'drop')

nrow(result[result$maxpred > 0.15,])

(result[result$maxpred > 0.3,])

result %>%
  filter(maxpred > 0.1485) %>%
  filter(minphoneprice<1200) %>%
  summarise(count_individu = sum(nb, na.rm = TRUE))

result_csv <-result %>%
filter(maxpred > 0.1485) %>%
  filter(minphoneprice<1200) %>%
  select(unique_family)
write.csv(result_csv, "/Users/nausicaa/Desktop/resultpromo.csv", row.names = FALSE)
