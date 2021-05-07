heart<-read.csv("C:/Users/slandge/Downloads/heart.csv",stringsAsFactors = T)
library(dplyr)
library(rattle)
library(rpart)
str(heart)
summary(heart)
heart_group<-group_by(heart, age)
heart_group<-summarise(heart_group, avg_heart=mean(age))
sorted_data<-filter(heart,heart$output==1)
sorted_data2<-filter(heart, heart$output==0)
sorted_data2<-sorted_data2[order(sorted_data2$age),]
sorted_data<-sorted_data[order(sorted_data$age),]
sorted_data$age_group<-ifelse(sorted_data$age<=29,1,ifelse(sorted_data$age<=39,2,ifelse(sorted_data$age<=49,3,ifelse(sorted_data$age<=59,4,5))))
sorted_data2$age_group<-ifelse(sorted_data2$age<=29,1,ifelse(sorted_data2$age<=39,2,ifelse(sorted_data2$age<=49,3,ifelse(sorted_data2$age<=59,4,5))))
library(ggplot2)
library(mgcv)
t1<-data.frame(sorted_data$age_group, sorted_data$output)
t1<-table(t1)
barplot(t1,xlab = "age group", main = "No. of people effected in age group", names.arg = c("0-29","30-39","40-49","50-59","60+"),cex.names = 0.6,legend = rownames(t1), col = c("red","green","blue","yellow","orange"),beside = T)
t2<-data.frame(sorted_data2$age_group, sorted_data2$output)
t2<-table(t2)
barplot(t2,xlab = "age group", main = "No. of people  did not effected by age group", names.arg = c("30-39","40-49","50-59","60+"),cex.names = 0.6,legend = rownames(t2), col = c("darkblue","pink","purple","darkgreen"),beside = T)
t3<-data.frame(sorted_data$sex,sorted_data$output)
t3<-t3[order(t3),]
t3<-table(t3)
barplot(t3, xlab = "gender", main = "gender affecting heart disease", names.arg = c("Female","Male"), cex.names = 0.6, legend = rownames(t3), col = c("skyblue","pink"), beside = T)
t4<-data.frame(sorted_data$age_group,sorted_data$cp)
t4<-table(t4)
barplot(t4, xlab = "cholestrol", main = "cholestrol affecting heart disease", names.arg = c("0-29","30-39","40-49","50-59","60+"), cex.names = 0.1, legend = rownames(t3), col = c("red","green","blue","yellow","orange"),beside = T)
library(Amelia)
missmap(heart)
library(DMwR2)
heart_sex_zero<-filter(heart,heart$sex==1)
heart_sex_one<-filter(heart,heart$sex==0)
heart_nums<-heart[sapply(heart, is.numeric)]
names(heart_num)
for (i in 1:13) {
  heart_num[,i]<-ifelse(heart_num[,i]<quantile(heart_num[,i],c(0.01)),quantile(heart_num[,i],c(0.01)),heart_num[,i])
  
}
for (i in 1:13) {
 heart_num[,i]<-ifelse(heart_num[,i]>quantile(heart_num[,i],c(0.99)),quantile(heart_num[,i],c(0.99)),heart_num[,i])
}
library(caret)
heart_nzv<-(nearZeroVar(heart))
library(caTools)
set.seed(123)
sample<-sample.split(heart_num$output, SplitRatio = 0.7)
train_data<-subset(heart_num,sample ==T)
validate<-subset(heart_num, sample ==F)
str(validate)
str(train_data)
names(train_data)
logistic_heart<-step(glm(output~., data = train_data))

pred <- predict(logistic_heart,validate,type="response")
validate$pred_log <- pred
cutoff <- 0 
F1_calculated <- 0 
cutoff_output <- data.frame(cutoff,F1_calculated) 
cutoff <- 0.1  



while(cutoff <= 0.8){
  
  t1 <- table(validate$output,validate$pred_log >cutoff)  
  
  t1
  prop.table(t1)
  tp <- t1[4]
  fp <- t1[3]
  tn <- t1[1]
  fn <- t1[2]
  pre <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  
  f1 <- 2*((pre*recall)/(pre+recall)) 
  
  cutoff <- cutoff  
  F1_calculated <- f1 
  new_result <- data.frame(cutoff,F1_calculated) 
  cutoff_output <- rbind(cutoff_output,new_result) 
  cutoff <- cutoff + 0.01 
}

cutoff_output <- as.data.frame(cutoff_output) 
cutoff_output <- cutoff_output[order(-cutoff_output$F1_calculated),] 
head(cutoff_output)  

best_cutoff <- cutoff_output[1,1]

for (i in 1:13) {
  heart_sex_zero[,i]<-ifelse(heart_sex_zero[,i]<quantile(heart_sex_zero[,i],c(0.01)),quantile(heart_sex_zero[,i],c(0.01)),heart_sex_zero[,i])
  
}
for (i in 1:13) {
  heart_sex_zero[,i]<-ifelse(heart_sex_zero[,i]>quantile(heart_sex_zero[,i],c(0.99)),quantile(heart_sex_zero[,i],c(0.99)),heart_sex_zero[,i])
}
library(caret)
heart_nzv_zero<-(nearZeroVar(heart_sex_zero))
library(caTools)
set.seed(123)
sample1<-sample.split(heart_sex_zero$output, SplitRatio = 0.7)
train_data_zero<-subset(heart_sex_zero,sample ==T)
validate_zero<-subset(heart_sex_zero, sample ==F)
str(validate)
str(train_data)
names(train_data)
logistic_heart_zero<-step(glm(output~., data = train_data_zero))

pred_zero<- predict(logistic_heart_zero,validate_zero,type="response")
validate_zero$pred_log <- pred_zero
cutoff_zero<- 0 
F1_calculated_zero<- 0 
cutoff_output_zero<- data.frame(cutoff_zero,F1_calculated_zero) 
cutoff_zero <- 0.1  



while(cutoff_zero <= 0.8){
  
  t1 <- table(validate_zero$output,validate_zero$pred_log >cutoff_zero)  
  
  t1
  prop.table(t1)
  tp <- t1[4]
  fp <- t1[3]
  tn <- t1[1]
  fn <- t1[2]
  pre <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  
  f1 <- 2*((pre*recall)/(pre+recall)) 
  
  cutoff_zero <- cutoff_zero  
  F1_calculated_zero <- f1 
  new_result <- data.frame(cutoff_zero,F1_calculated_zero) 
  cutoff_output_zero <- rbind(cutoff_output_zero,new_result) 
  cutoff_zero<- cutoff_zero + 0.01 
}

cutoff_output_zero <- as.data.frame(cutoff_output_zero) 
cutoff_output_zero <- cutoff_output_zero[order(-cutoff_output_zero$F1_calculated),] 
head(cutoff_output_zero)  

best_cutoff_zero <- cutoff_output_zero[1,1]

for (i in 1:13) {
  heart_sex_one[,i]<-ifelse(heart_sex_one[,i]<quantile(heart_sex_one[,i],c(0.01)),quantile(heart_sex_one[,i],c(0.01)),heart_sex_one[,i])
  
}
for (i in 1:13) {
  heart_sex_one[,i]<-ifelse(heart_sex_one[,i]>quantile(heart_sex_one[,i],c(0.99)),quantile(heart_sex_one[,i],c(0.99)),heart_sex_one[,i])
}
library(caret)
library(caTools)
set.seed(123)
sample2<-sample.split(heart_sex_one$output, SplitRatio = 0.7)
train_data_one<-subset(heart_sex_one,sample ==T)
validate_one<-subset(heart_sex_one, sample ==F)
str(validate)
str(train_data)
names(train_data)
logistic_heart_one<-step(glm(output~., data = train_data_one))

pred_one<- predict(logistic_heart_one,validate_one,type="response")
validate_one$pred_log <- pred_one
cutoff_one<- 0 
F1_calculated_one<- 0 
cutoff_output_one<- data.frame(cutoff_one,F1_calculated_one) 
cutoff_one <- 0.1  



while(cutoff_one <= 0.8){
  
  t1 <- table(validate_one$output,validate_one$pred_log >cutoff_one)  
  
  t1
  prop.table(t1)
  tp <- t1[4]
  fp <- t1[3]
  tn <- t1[1]
  fn <- t1[2]
  pre <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  
  f1 <- 2*((pre*recall)/(pre+recall)) 
  
  cutoff_one <- cutoff_one  
  F1_calculated_one <- f1 
  new_result <- data.frame(cutoff_one,F1_calculated_one) 
  cutoff_output_one <- rbind(cutoff_output_one,new_result) 
  cutoff_one<- cutoff_one + 0.01 
}

cutoff_output_one <- as.data.frame(cutoff_output_one) 
cutoff_output_one <- cutoff_output_one[order(-cutoff_output_one$F1_calculated),] 
head(cutoff_output_one)  

best_cutoff_one <- cutoff_output_one[1,1]

