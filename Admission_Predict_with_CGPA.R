getwd()
rm(list = ls())

#This dataset is created for prediction of Graduate Admissions from an Indian perspective.
#The dataset contains several parameters which are considered important during the 
#application for Masters Programs. 

#Features in the dataset:

#1. GRE Scores ( 290 to 340 )
#2. TOEFL Scores ( 92 to 120 ) 
#3. University Rating ( 1 to 5 ) 
#4. Statement of Purpose / SOP ( 1 to 5 ) 
#5. Letter of Recommendation Strength / LOR (1 to 5)
#5. Undergraduate GPA / CGPA ( 6.8  to 9.92 ) 
#6. Research Experience ( either 0 or 1 ) 
#7. Chance of Admit - Target Variable ( 0.34 to 0.97 )


#This dataset was built with the purpose of helping students in shortlisting universities 
#with their profiles. The predicted output gives them a fair idea about their chances 
#for a particular university.


``

# reading data set from the repository 
admission = read.csv("Admission_Predict.csv")

#rm(adimission)

#basic EDA.
head(admission)

#Checking for missing values
sum(is.na(admission))

# checking duplicate rows
length(unique(admission$Serial.No.)) == nrow(admission)
# Serial.No.is the primary key with no duplicate values

#Finding variable type
str(admission)

#Knowing more about variables
summary(admission)

#All variables are almost normally disturbuted except "Research"

library(dplyr)
colnames(admission)

#removing invalid or not reqired variables
admission = admission%>%select(-Serial.No.)

#Creating a variable on the basis of target variable 
admission = admission%>%mutate(Admit=ifelse(Chance.of.Admit>=0.50,1,0))


#The 3 most important features for 
#admission to the Master: CGPA, GRE SCORE, and TOEFL SCORE

#The 3 least important features for admission to the Master: Research, LOR, and SOP

#Scatter plot, Frequency curve and Correlation values in one graph

library(GGally)
ggpairs(admission)

#Having Research or not:

#The majority of the candidates in the dataset have research experience.

#Therefore, the Research will be a unimportant feature for the Chance of Admit. 

#The correlation between Chance of Admit and Research was already lower than 
#other correlation values.

admission$Research = as.factor(admission$Research)
admission$Research = as.numeric(admission$Research)
summary(admission)

#Not Having Research: 181
#Having Research: 219

plot(admission$Research,col="Blue")

#TOEFL Score:

# The lowest TOEFL score is 92 and the highest Toefl score is 120. The average is 107.41.
library(ggplot2)
x= c("Bad","Average","Good")
y=c(min(admission$TOEFL.Score),median(admission$TOEFL.Score),max(admission$TOEFL.Score))
#TOEFL_Score$x=as.integer(TOEFL_Score$x)
TOEFL_Score = data.frame(x,y)
str(TOEFL_Score)
TOFEL_Score$y=arrange(TOEFL_Score$y)
ggplot(TOEFL_Score,aes(x=x,y=y))+ggtitle("TOEFL Scores")+geom_histogram(fill="blue")
hist(TOEFL_Score)
plot(TOEFL_Score)

#Chekcing for outliers 
#GRE.Score
hist(admission$GRE.Score)
#TOEFL.Score
hist(admission$TOEFL.Score)
#CGPA
hist(admission$CGPA)


# Analysing variable 'Research'
summary(factor(admission$Research))
admission$Research <- as.factor(admission$Research)

# Analysing variable 'University.Rating'
summary(factor(admission$University.Rating))
admission$University.Rating <- as.factor(admission$University.Rating)

# Analysing variable 'SOP'
summary(factor(admission$SOP))
admission$SOP <- as.factor(admission$SOP)

# Analysing variable 'LOR'
summary(factor(admission$LOR))
admission$LOR <- as.factor(admission$LOR)

#University Rating
ggplot(admission, aes(x=University.Rating, y = Chance.of.Admit))+ geom_boxplot(outlier.colour = "red")

#From the boxplots it has clearly observed that chance of admission is high when somebody 
#belongs to high ranking university. Although some students are from average rating 
#university, still they have a chance to get admitted.

#Statement of Purpose (SOP)
ggplot(admission, aes(x=SOP, y = Chance.of.Admit))+ geom_boxplot(outlier.colour = "red")

#Here we can see that "high degree of Statement of Purpose means high probability of getting admission. 
#There is some rare cases where you have some less chance to get admission.

#Letter of Recommendation (LOR)
ggplot(admission, aes(x=LOR, y = Chance.of.Admit))+ geom_boxplot(outlier.colour = "red")

#Letter of Recommendation has great influnce towards getting admission in University.

#Research   
ggplot(admission, aes(x=Research, y = Chance.of.Admit))+ geom_boxplot(outlier.colour = "red")

#Its definite who have done some reserch works, 
#they have good chance of getting admission in the university.


library(caTools)
set.seed(212)
sample <- sample.split(admission, SplitRatio = 0.7)
train <- subset(admission, sample == TRUE)
test <- subset(admission, sample == FALSE)

str(train)

str(test)

model1<-lm(Chance.of.Admit~.,data = train)
summary(model1)

model2<-lm(Chance.of.Admit~.-SOP,data=train)
summary(model2)

#model3 = lm(formula = Chance.of.Admit~University.Rating+LOR+CGPA+Research+Admit,data = train)
#summary(model3)

model3 = lm(formula = Chance.of.Admit~University.Rating+GRE.Score+LOR+CGPA+Research+Admit,data = train)
summary(model3)

pred<-predict(model3,newdata=test)
View(res)
res<-test$Chance.of.Admit-pred
modelOutput <- data.frame("obs" = test$Chance.of.Admit,"pred" = pred)
test$pred_CGPA<-ifelse(pred>=0.50,1,0)

install.packages("forecast")
library(forecast)
accuracy(pred,test$Chance.of.Admit)


#predict(model2,data.frame(GRE.Score=316,TOEFL.Score=103,University.Rating=4,SOP=3.5,LOR=3.5,CGPA=7.07,Research=0))

#predict.lm(model2, data.frame(GRE.Score = 316, TOEFL.Score = 103, University.Rating = 4, SOP = 3.5, LOR = 3.5, CGPA = 7.07,Research = 0))

#model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)

install.packages("Metrics")

# load Metrics package

library(Metrics)

# calculate auc, accuracy, clasification error
auc <- auc(test$Admit,test$pred_CGPA)
accuracy <- accuracy(test$Admit,test$pred_CGPA)
classification_error <- ce(test$Admit,test$pred_CGPA)

# print out the metrics on to screen
print(paste("AUC=", auc))
print(paste("Accuracy=", accuracy))
print(paste("Classification Error=", classification_error))

# confusion matrix
table(test$Admit,test$pred_CGPA, dnn=c('True Status','Predicted Status')) # confusion matrix
