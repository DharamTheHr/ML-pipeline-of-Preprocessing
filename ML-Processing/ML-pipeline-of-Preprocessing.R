library(breakDown)

#Extracting the Data 
data_set = HR_Data
View(data_set) 

#Cheeck Missing values  
sum(is.na(data_set))

summary(data_set)

#Renaming the irrilavent variable 
library(plyr)
data_set<-rename(data_set,c("sales" = "designation"))
data_set<-rename(data_set,c("time_spend_company" = "Work_Exp"))
head(data_set,20)

Exploring the data 
dim(data_set)

#check the type of features
 str(data_set)
company_attrition<-as.factor(data_set$left) 
summary(attrition)
sum(data_set$left)
perc_company_attrition_rate<-sum(data_set$left/length(data_set$Work_accident))*100
print(perc_company_attrition_rate)

#Approx 76% of employees stayed and 24% of employees left

# Overview of summary (Turnover V.S. Non-turnover) 

Dharam<-data_set[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","Work_Exp","Work_accident","left","promotion_last_5years")]
aggregate(Dharam[,c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "Work_Exp", "Work_accident","left", "promotion_last_5years")], by=list(Category =Dharam$left), FUN = mean)

#Correlation Matrix 
library(reshape2)
library(ggplot2)

Dharam<-data_set[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","Work_Exp","Work_accident","left","promotion_last_5years")]
cor(Dharam)
trans<-cor(Dharam)
melted_cormat<-melt(trans)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile() + theme(axis.title.x = element_text(angle = 90, hjust = 1))

######findings#####
#projectCount vs evaluation:
#projectCount vs averageMonthlyHours:
#averageMonthlyHours vs evaluation:

#employees who spent more hours and did more projects were evaluated highly

#What features affect our target variable the most (turnover)?
#What features have strong correlations with each other?
#Can we do a more in depth examination of these features?

#Statistical Test
#One-Sample T-Test (Measuring Satisfaction Level)
#checks whether a sample mean differs from the population mean. 

#whether the average satisfaction level of employees that had a turnover differs from the entire employee population

#Hypothesis Testing: Is there significant difference in the means of satisfaction level between employees 
#who had a turnover and the entire employee population?

emp_population_satisfaction <-mean(data_set$satisfaction_level)
left_pop<-subset(data_set,left==1)

emp_turnover_satisfaction <-mean(left_pop$satisfaction_level)
print( c('The mean for the employee population is: ', emp_population_satisfaction) )
print( c('The mean for the employees that had a turnover is: ' ,emp_turnover_satisfaction) )
t.test(left_pop$satisfaction_level,mu=emp_population_satisfaction)

# Employee Population satisfaction mean

#p-value < 2.2e-16 at a 5% confidence level is a good indicator to reject the null hypothesis.

#Distribution Plot 
par(mfrow=c(1,4))
hist(data_set$satisfaction_level, col="violet")
hist(data_set$last_evaluation, col="red")
hist(data_set$average_montly_hours, col="pink")

vis_1<-table(data_set$salary,data_set$left)
d_vis_1<-as.data.frame(vis_1)
print(d_vis_1)
library(ggplot2)
p<-ggplot(d_vis_1, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') + coord_flip()

print(p)

#Department V.S. Turnover
vis_2<-table(data_set$role,data_set$left)
d_vis_2<-as.data.frame(vis_2)
d_vis_2<-subset(d_vis_2,Var2==1)
library(ggplot2)
d_vis_2$Var1 <- factor(d_vis_2$Var1, levels = d_vis_2$Var1[order(-d_vis_2$Freq)])
p<-ggplot(d_vis_2, aes(x=Var1,y=Freq,fill=Var1)) +
  geom_bar(stat='identity') +theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p)

#Turnover V.S. ProjectCount
vis_3<-table(data_set$number_project,data_set$left)
d_vis_3<-as.data.frame(vis_3)
#print(d_vis_1)
library(ggplot2)
p<-ggplot(d_vis_3, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') + coord_flip()

print(p)

#Turnover V.S. Evaluation
# Kernel Density Plot
left_data<-subset(data_set,left==1)
stay_data<-subset(data_set,left==0)
ggplot() + geom_density(aes(x=last_evaluation), colour="red", data=left_data) + 
  geom_density(aes(x=last_evaluation), colour="blue", data=stay_data)

#Turnover V.S. AverageMonthlyHours
#KDEPlot: Kernel Density Estimate Plot

ggplot() + geom_density(aes(x=average_montly_hours), colour="red", data=left_data) + 
  geom_density(aes(x=average_montly_hours), colour="blue", data=stay_data)

#Turnover V.S. Satisfaction
#KDEPlot: Kernel Density Estimate Plot
ggplot() + geom_density(aes(x=satisfaction_level), colour="red", data=left_data) + 
  geom_density(aes(x=satisfaction_level), colour="blue", data=stay_data)

#ProjectCount VS AverageMonthlyHours
library(ggplot2)
p<-ggplot(data_set, aes(x = factor(number_project), y = average_montly_hours, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("yellow", "orange"))
print(p)

#ProjectCount VS Evaluation
p<-ggplot(data_set, aes(x = factor(number_project), y = last_evaluation, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("yellow", "orange"))
print(p) 

#Satisfaction VS Evaluation
library(ggplot2)
ggplot(data_set, aes(satisfaction_level, last_evaluation, color = left)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") 

#Feature Extraction
install.packages("Boruta")
library(Boruta)
data_set$left<-as.factor(data_set$left)
boruta.train <- Boruta(left~., data = data_set, doTrace = 2)

print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7) 

#Modeling - Logistic Regression
#Creating training and test sets for the logistic regression
smp_size <- floor(0.75 * nrow(data_set)) 
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_set)), size = smp_size)

train <- data_set[train_ind, ]
test <- data_set[-train_ind, ]
dim(test)
dim(train)
library(gmodels)
library (Hmisc)
library (caTools)
library (ROCR)
logit_model<-glm(left~satisfaction_level+last_evaluation+average_montly_hours+salary+role+number_project,data=train,binomial())

summary(logit_model)

test$logit_model<-predict(logit_model,test)

colAUC(test$logit_model,test$left, plotROC=TRUE)

#Now using that threshold created the predicted values for each record
test$prediction<-ifelse(test$logit_model>=-.95,1,0)

conf_mat<-table(test$left,test$prediction)

accuracy<-(conf_mat[1,1]+conf_mat[2,2])/(conf_mat[1,1]+conf_mat[2,2]+conf_mat[1,2]+conf_mat[2,1])
recall<-(conf_mat[2,2])/(conf_mat[1,2]+conf_mat[2,2])
precision<-(conf_mat[2,2])/(conf_mat[2,2]+conf_mat[2,1])

print(c("Accuracy:",accuracy))
print(c("Precision:",precision))
print(c("Recall:",recall))
