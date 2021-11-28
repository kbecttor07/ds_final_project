
#import data
setwd('/Users/meizhuchen/Desktop/Columbia-Semester1/5200/Kaggle/')
data = read.csv('analysisData.csv',dec=",")
scoringData = read.csv('scoringData.csv',dec=",")
data2 = scoringData

#Explore the data
glimpse(data) #41330samples and 91 variables 
#Define the variables
# Missing data
#1 Change the blank cells to "NA"
data[data == "" | data == " "] <- NA
#2 Change N/A cells to "NA"
data[data == "" | data == "N/A"] <- NA
data
#3 N/A data
missing<-which(colSums(is.na(data))>0)
miss_data<-data[,missing]
pna<-function(x){sum(is.na(x)/length(x))}
apply(miss_data,2,pna)#算出missing_data表中各字段含有缺失值的比例
library('VIM')
aggr(miss_data, labels=names(miss_data),cex.axis=0.4)
md.pattern(miss_data)
#Delete the variable lost more than 50% data ("notes","square_feet","weekly_price","monthly_price","license","jurisdiction_names")
library(tidyverse)
data<-data%>%
subset(select=-c(notes, square_feet , weekly_price,monthly_price,license,jurisdiction_names))

#fillup the N/A value


#Change data format
#host(host_response_rate,host_acceptance_rate, host_is_superhost, neighbourhood,property_type,room_type,host_has_profile_pic,beds,guests,security_deposit,cleaning_fee,extra_people, minimum_nights, has_availability,number_of_reviews,review_scores_rating,review_scores_value)
data$host_response_rate <- gsub("%", "", data$host_response_rate) 
data$host_response_rate=as.numeric(data$host_response_rate)
data[which(is.na(data[,'host_response_rate'])),'host_response_rate']<-mean(data$host_response_rate,na.rm=T)
data$host_acceptance_rate <- gsub("%", "", data$host_acceptance_rate)
data$host_acceptance_rate=as.numeric(data$host_acceptance_rate)
data[which(is.na(data[,'host_acceptance_rate'])),'host_acceptance_rate']<-mean(data$host_acceptance_rate,na.rm=T)
data$host_is_superhost[data$host_is_superhost=="f"] <-"1"
data$host_is_superhost[data$host_is_superhost=="t"] <-"0"
data[which(is.na(data[,'host_is_superhost'])),'host_is_superhost'] <-"0"
data$host_is_superhost=as.numeric(data$host_is_superhost)
data[which(is.na(data[,'neighbourhood'])),'neighbourhood'] <-"Others"
data$neighbourhood=as.factor(data$neighbourhood)
data$property_type=as.factor(data$property_type)
data$room_type=as.factor(data$room_type)
data$host_has_profile_pic[data$host_has_profile_pic=="f"] <-"1"
data$host_has_profile_pic[data$host_has_profile_pic=="t"] <-"0"
data[which(is.na(data[,'host_has_profile_pic'])),'host_has_profile_pic'] <-"0"
data$host_has_profile_pic=as.numeric(data$host_has_profile_pic)
data$has_availability[data$has_availability=="f"] <-"1"
data$has_availability[data$has_availability=="t"] <-"0"
data$has_availability=as.numeric(data$has_availability)
data$cancellation_policy[data$cancellation_policy=="flexible"] <-"6"
data$cancellation_policy[data$cancellation_policy=="moderate"] <-"5"
data$cancellation_policy[data$cancellation_policy=="strict"] <-"4"
data$cancellation_policy[data$cancellation_policy=="strict_14_with_grace_period"] <-"3"
data$cancellation_policy[data$cancellation_policy=="super_strict_30"] <-"2"
data$cancellation_policy[data$cancellation_policy=="super_strict_60"] <-"1"
data$cancellation_policy=as.numeric(data$cancellation_policy)
data$beds=as.numeric(data$beds)
data[which(is.na(data[,'beds'])),'beds']<-mean(data$beds,na.rm=T)
data$guests_included=as.numeric(data$guests_included)
data$security_deposit=as.numeric(data$security_deposit)
data[which(is.na(data[,'security_deposit'])),'security_deposit']<-mean(data$security_deposit,na.rm=T)
data$cleaning_fee=as.numeric(data$cleaning_fee)
data[which(is.na(data[,'cleaning_fee'])),'cleaning_fee']<-mean(data$cleaning_fee,na.rm=T)
data$extra_people=as.numeric(data$extra_people)
data$number_of_reviews=as.numeric(data$number_of_reviews)
data$review_scores_value=as.numeric(data$review_scores_value)
data$review_scores_rating=as.numeric(data$review_scores_rating)
data$review_scores_accuracy=as.numeric(data$review_scores_accuracy)
data$accommodates=as.numeric(data$accommodates)
data$bedrooms=as.numeric(data$bedrooms)
data$bathrooms=as.numeric(data$bathrooms)
data$review_scores_value=as.numeric(data$review_scores_value)
data$availability_365=as.numeric(data$availability_365)
data$instant_bookable[data$instant_bookable=="f"] <-"1"
data$instant_bookable[data$instant_bookable=="t"] <-"0"
data$instant_bookable=as.numeric(data$instant_bookable)
data$instant_bookable=as.numeric(data$review_scores_cleanliness)
data[which(is.na(data[,'host_total_listings_count'])),'host_total_listings_count']<-mean(data$host_total_listings_count,na.rm=T)
data$host_total_listings_count=as.numeric(data$host_total_listings_count)
data$is_location_exact[data$is_location_exact=="f"] <-"1"
data$is_location_exact[data$is_location_exact=="t"] <-"0"
data$is_location_exact=as.numeric(data$is_location_exact)
data$reviews_per_month=as.numeric(data$reviews_per_month)
data[which(is.na(data[,'reviews_per_month'])),'reviews_per_month']<-mean(data$reviews_per_month,na.rm=T)




#Change scoring data
#fillup the N/A value
#host(host_response_rate,host_acceptance_rate, host_is_superhost, neighbourhood,property_type,room_type,host_has_profile_pic,beds,guests,security_deposit,cleaning_fee,extra_people, minimum_nights, has_availability,number_of_reviews,review_scores_rating,review_scores_value)
data2$host_response_rate <- gsub("%", "", data2$host_response_rate) 
data2$host_response_rate=as.numeric(data2$host_response_rate)
data2[which(is.na(data2[,'host_response_rate'])),'host_response_rate']<-mean(data2$host_response_rate,na.rm=T)
data2$host_acceptance_rate <- gsub("%", "", data2$host_acceptance_rate)
data2$host_acceptance_rate=as.numeric(data2$host_acceptance_rate)
data2[which(is.na(data2[,'host_acceptance_rate'])),'host_acceptance_rate']<-mean(data2$host_acceptance_rate,na.rm=T)
data2$host_is_superhost[data2$host_is_superhost=="f"] <-"1"
data2$host_is_superhost[data2$host_is_superhost=="t"] <-"0"
data2[which(is.na(data2[,'host_is_superhost'])),'host_is_superhost'] <-"0"
data2$host_is_superhost=as.numeric(data2$host_is_superhost)
data2[which(is.na(data2[,'neighbourhood'])),'neighbourhood'] <-"Others"
data2$neighbourhood=as.factor(data2$neighbourhood)
data2$property_type=as.factor(data2$property_type)
data2$room_type=as.factor(data2$room_type)
data2$host_has_profile_pic[data2$host_has_profile_pic=="f"] <-"1"
data2$host_has_profile_pic[data2$host_has_profile_pic=="t"] <-"0"
data2[which(is.na(data2[,'host_has_profile_pic'])),'host_has_profile_pic'] <-"0"
data2$host_has_profile_pic=as.numeric(data2$host_has_profile_pic)
data2$has_availability[data2$has_availability=="f"] <-"1"
data2$has_availability[data2$has_availability=="t"] <-"0"
data2$has_availability=as.numeric(data2$has_availability)
data2$cancellation_policy[data2$cancellation_policy=="flexible"] <-"6"
data2$cancellation_policy[data2$cancellation_policy=="moderate"] <-"5"
data2$cancellation_policy[data2$cancellation_policy=="strict"] <-"4"
data2$cancellation_policy[data2$cancellation_policy=="strict_14_with_grace_period"] <-"3"
data2$cancellation_policy[data2$cancellation_policy=="super_strict_30"] <-"2"
data2$cancellation_policy[data2$cancellation_policy=="super_strict_60"] <-"1"
data2$cancellation_policy=as.numeric(data2$cancellation_policy)
data2$beds=as.numeric(data2$beds)
data2[which(is.na(data2[,'beds'])),'beds']<-mean(data2$beds,na.rm=T)
data2$guests_included=as.numeric(data2$guests_included)
data2$security_deposit=as.numeric(data2$security_deposit)
data2[which(is.na(data2[,'security_deposit'])),'security_deposit']<-mean(data2$security_deposit,na.rm=T)
data2$cleaning_fee=as.numeric(data2$cleaning_fee)
data2[which(is.na(data2[,'cleaning_fee'])),'cleaning_fee']<-mean(data2$cleaning_fee,na.rm=T)
data2$extra_people=as.numeric(data2$extra_people)
data2$number_of_reviews=as.numeric(data2$number_of_reviews)
data2$review_scores_value=as.numeric(data2$review_scores_value)
data2$review_scores_rating=as.numeric(data2$review_scores_rating)
data2$review_scores_accuracy=as.numeric(data2$review_scores_accuracy)
data2$accommodates=as.numeric(data2$accommodates)
data2$bedrooms=as.numeric(data2$bedrooms)
data2$bathrooms=as.numeric(data2$bathrooms)
data2$review_scores_value=as.numeric(data2$review_scores_value)
data2$availability_365=as.numeric(data2$availability_365)
data2$instant_bookable[data2$instant_bookable=="f"] <-"1"
data2$instant_bookable[data2$instant_bookable=="t"] <-"0"
data2$instant_bookable=as.numeric(data2$instant_bookable)
data2$instant_bookable=as.numeric(data2$review_scores_cleanliness)
data2[which(is.na(data2[,'host_total_listings_count'])),'host_total_listings_count']<-mean(data2$host_total_listings_count,na.rm=T)
data2$host_total_listings_count=as.numeric(data2$host_total_listings_count)
data2$is_location_exact[data2$is_location_exact=="f"] <-"1"
data2$is_location_exact[data2$is_location_exact=="t"] <-"0"
data2$is_location_exact=as.numeric(data2$is_location_exact)
data2$reviews_per_month=as.numeric(data2$reviews_per_month)
data2[which(is.na(data2[,'reviews_per_month'])),'reviews_per_month']<-mean(data2$reviews_per_month,na.rm=T)

#feature selection
model_f = lm(price~host_response_rate+host_acceptance_rate+host_is_superhost+neighbourhood+property_type+room_type+host_has_profile_pic+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+has_availability+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness+host_total_listings_count+is_location_exact+reviews_per_month,train)
library(broom)
library(tidyverse)
summary(model_f) %>%
  tidy()
start_mod = lm(price~host_response_rate+host_acceptance_rate+host_is_superhost+neighbourhood+property_type+room_type+host_has_profile_pic+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+has_availability+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness+host_total_listings_count+is_location_exact+reviews_per_month,train)
empty_mod = lm(price~host_response_rate+host_acceptance_rate+host_is_superhost+neighbourhood+property_type+room_type+host_has_profile_pic+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+has_availability+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness+host_total_listings_count+is_location_exact+reviews_per_month,train)
full_mod = lm(price~host_response_rate+host_acceptance_rate+host_is_superhost+neighbourhood+property_type+room_type+host_has_profile_pic+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+has_availability+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness+host_total_listings_count+is_location_exact+reviews_per_month,train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')
summary(hybridStepwise)
options(max.print=1000000) 
#remove host_response_rate, host_acceptance_rate, host_has_profile_pic, has_availability

#regression tree
library(rpart); library(rpart.plot); library(caret)
set.seed(5000)
split = createDataPartition(y = data$price, p = 0.75, list = F)
train = data[split,]
test = data[-split,]
tree = rpart(price~host_is_superhost+neighbourhood+property_type+room_type+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness,data = train, method = 'anova')
pred_train2 = predict(tree)
rmse_train_tree = sqrt(mean((pred_train2 - train$price)^2)); rmse_train_tree
#79.07548

#lasso
#Step 1: Load the Data
x = model.matrix(price~host_response_rate+host_acceptance_rate+host_is_superhost+neighbourhood+property_type+room_type+host_has_profile_pic+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+has_availability+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness,data=train)
y = train$price
#Step 2: Fit the Lasso Regression Model
library(glmnet)
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)
#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #0.07542592
#Step 3: Analyze Final Model
#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)
#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
#ind R-Squared
rsq <- 1 - sse/sst
rsq
#0.5592837

#ranger
library(randomForest)
library(ranger)
set.seed(5000)
forest_ranger = ranger(price~host_is_superhost+neighbourhood+property_type+room_type+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness++host_total_listings_count+is_location_exact+reviews_per_month,
                      train, 
                      num.trees  = 5000)
pred_train = predict(forest_ranger, data = train, num.trees = 5000)
rmse_train_forest_ranger = sqrt(mean((pred_train$predictions - train$price)^2)); rmse_train_forest_ranger
#34.43504
pred = predict(forest_ranger, data = test, num.trees = 5000)
rmse_forest_ranger = sqrt(mean((pred$predictions - test$price)^2)); rmse_forest_ranger
#68.1915
pred2 = predict(forest_ranger,data2)$predictions
submissionFile = data.frame(id = scoringData$id, price = pred2)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)
#remove has_availability


#tuned ranger
trControl=trainControl(method="cv",number=5)
tuneGrid = expand.grid(mtry=1:ncol(train)-1, 
                       splitrule = c('variance','extratrees','maxstat'), 
                       min.node.size = c(2,5,10,15,20,25))
set.seed(5000)
cvModel = train(price~host_is_superhost+neighbourhood+property_type+room_type+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness,
                data=train,
                method="ranger",
                num.trees=1000,
                trControl=trControl,
                tuneGrid=tuneGrid)
cvModel$bestTune

#gbm
library(gbm)
set.seed(5000)
boost = gbm(price~host_is_superhost+neighbourhood+property_type+room_type+beds+guests_included+security_deposit+cleaning_fee+extra_people+minimum_nights+number_of_reviews+review_scores_rating+review_scores_value+review_scores_accuracy+accommodates+bathrooms+bedrooms+availability_365+instant_bookable+review_scores_cleanliness,
            data=train,
            distribution="gaussian",
            n.trees = 5000,
            interaction.depth = 2,
            shrinkage = 0.05)
pred_train2 = predict(boost, n.trees=5000)
rmse_train_boost = sqrt(mean((pred_train2 - train$price)^2)); rmse_train_boost
#62.44769


#confirm if there is any missing data
sum(is.na(data$host_response_rate))
sum(is.na(data$host_acceptance_rate))
sum(is.na(data$host_is_superhost))
sum(is.na(data$neighbourhood))
sum(is.na(data$property_type))
sum(is.na(data$room_type))
sum(is.na(data$host_has_profile_pic))
sum(is.na(data$beds))
sum(is.na(data$guests_included))
sum(is.na(data$security_deposit))
sum(is.na(data$cleaning_fee))
sum(is.na(data$extra_people))
sum(is.na(data$has_availability))
sum(is.na(data$number_of_reviews))
sum(is.na(data$review_scores_rating))
sum(is.na(data$review_scores_value))
sum(is.na(data$review_scores_accuracy))
sum(is.na(data$accommodates))
sum(is.na(data$bathrooms))
sum(is.na(data$bedrooms))
sum(is.na(data$review_scores_value))
sum(is.na(data$availability_365))
sum(is.na(data$instant_bookable))
sum(is.na(data$review_scores_cleanliness))
sum(is.na(data$host_total_listings_count))
sum(is.na(data$host_identity_verified))
sum(is.na(data$reviews_per_month))

#confirm the data typ
str(data$host_response_rate) 
str(data$host_acceptance_rate) 
str(data$host_is_superhost) 
str(data$neighbourhood) 
str(data$property_type) 
str(data$room_type) 
str(data$host_has_profile_pic) 
str(data$beds) 
str(data$guests_included)
str(data$security_deposit) 
str(data$cleaning_fee) 
str(data$extra_people)
str(data$has_availability)
str(data$number_of_reviews)
str(data$review_scores_rating)
str(data$review_scores_value)
str(data$review_scores_accuracy)
str(data$accommodates)
str(data$bathrooms)
str(data$bedrooms)
str(data$review_scores_value)
str(data$availability_365)
str(data$instant_bookable)
str(data$review_scores_cleanliness)
str(data$host_total_listings_count)
str(data$host_identity_verified)




host_is_superhost
+neighbourhood
+property_type
+room_type
+beds
+guests_included
+security_deposit
+cleaning_fee
+extra_people
+minimum_nights
+number_of_reviews
+review_scores_rating
+review_scores_value
+review_scores_accuracy
+accommodates
+bathrooms
+bedrooms
+availability_365
+instant_bookable
+review_scores_cleanliness

