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

#Finding values of target variable 
admission = admission%>%mutate(Admit=ifelse(Chance.of.Admit>=0.5,1,0))

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

# Analysing variable 'GRE.Score'
summary(admission$GRE.Score)
quantile(admission$GRE.Score, seq(0,1,0.01))
q1 <- quantile(admission$GRE.Score, c(0.25))
q3 <- quantile(admission$GRE.Score, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(admission[admission$GRE.Score > upper_range,])
nrow(admission[admission$GRE.Score < lower_range,])
# No Outliers

# Analysing variable 'TOEFL.Score'
summary(admission$TOEFL.Score)
quantile(admission$TOEFL.Score, seq(0,1,0.01))
q1 <- quantile(admission$TOEFL.Score, c(0.25))
q3 <- quantile(admission$TOEFL.Score, c(0.75))
IQR <- q3 - q1  
hupper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(admission[admission$TOEFL.Score > upper_range,])
nrow(admission[admission$TOEFL.Score < lower_range,])
# No Outliers

# Analysing variable 'CGPA'
summary(admission$CGPA)
quantile(admission$CGPA, seq(0,1,0.01))
q1 <- quantile(admission$CGPA, c(0.25))
q3 <- quantile(admission$CGPA, c(0.75))
IQR <- q3 - q1  
upper_range <- q3 + 1.5*IQR  
lower_range <- q1 - 1.5*IQR
nrow(admission[admission$CGPA > upper_range,])
nrow(admission[admission$CGPA < lower_range,])
# 1 outliers in upper range of 'CGPA'
# Treating outliers
admission$CGPA[which(admission$CGPA < lower_range)] <- lower_range


# Analysing variable 'University.Rating'
summary(factor(admission$University.Rating))
admission$University.Rating <- as.factor(admission$University.Rating)

# Analysing variable 'SOP'
summary(factor(admission$SOP))
admission$SOP <- as.factor(admission$SOP)

# Analysing variable 'LOR'
summary(factor(admission$LOR))
admission$LOR <- as.factor(admission$LOR)

# Analysing variable 'Research'
summary(factor(admission$Research))
admission$Research <- as.factor(admission$Research)


#Create a new vriable 
#Classify data with greater than 0.72 because of 0.5 gives un-leveled data division
table(admission$Chance.of.Admit > 0.5) # False = 35, True = 365
admission$get_admission = as.factor(ifelse(admission$Chance.of.Admit > 0.72,1,0)) #False = 196, True = 204
table(admission$Chance.of.Admit > 0.72)


# Selecting all Numeric Variables
admission_Numeric_Variable <- select_if(admission, is.numeric)
admission_Numeric_Variable


# Correlation of Numeric variables with chance of admit
corr <- cor(admission_Numeric_Variable)
corrplot(corr,method = "number",type = "full")
#Exam Scores are highly correlated.



#####   Dummy Variables ######

# University.Rating
length(levels(admission$University.Rating))

dummy_University.Rating <- data.frame(model.matrix( ~University.Rating, data = admission))
dummy_University.Rating <- dummy_University.Rating[,-1]
length(dummy_University.Rating)

?cbind
admission_1 <- cbind(select(admission, -'University.Rating'), dummy_University.Rating)
ncol(admission_1)

dummy_SOP <- data.frame(model.matrix( ~SOP, data = admission))
dummy_SOP <- dummy_SOP[,-1]
length(dummy_SOP)

admission_2 <- cbind(select(admission_1, -'SOP'), dummy_SOP)
ncol(admission_2)

dummy_LOR <- data.frame(model.matrix( ~LOR, data = admission))
dummy_LOR <- dummy_LOR[,-1]
length(dummy_LOR)

admission_3 <- cbind(select(admission_2, -'LOR'), dummy_LOR)
ncol(admission_3)

admission_3$Chance.of.Admit = NULL
admission_3



#Splitting the data set
set.seed(1000)
indx= sample(1:nrow(admission_3), 0.7*nrow(admission_3))
train = admission_3[indx,]
test = admission_3[-indx,]


model1 = glm(get_admission ~ ., data = train, family = "binomial")
summary(model1) #AIC = 186.62, Null deviance =  387.65


model1.0 <- glm( formula = get_admission ~ SOP1.5+SOP2+SOP3.5+SOP4.5+LOR2+LOR2.5+LOR4+LOR4.5+University.Rating2+University.Rating3+University.Rating5,family = "binomial", data = train)
summary(model1.0)#AIC = 260.65, Null deviance =  387.65

model1.1 <- glm( formula = get_admission ~ TOEFL.Score + CGPA + Research + SOP1.5+SOP2+SOP3.5+SOP4.5+LOR2+LOR2.5+LOR4+LOR4.5+University.Rating2+University.Rating3+University.Rating5,family = "binomial", data = train)
summary(model1.1)#AIC = 178.7, Null deviance =  387.65

model1.2 <- glm( formula = get_admission ~ GRE.Score + CGPA + Research + SOP1.5+SOP2+SOP3.5+SOP4.5+LOR2+LOR2.5+LOR4+LOR4.5+University.Rating2+University.Rating3+University.Rating5,family = "binomial", data = train)
summary(model1.2) #AIC = 177.01, Null deviance =  387.65

model1.3 <- glm( formula = get_admission ~ GRE.Score + CGPA + Research +SOP2+SOP3.5+SOP4.5+LOR2+LOR2.5+LOR4+University.Rating2+University.Rating3,family = "binomial", data = train)
summary(model1.3) #AIC = 173.83, Null deviance =  387.65

model1.4 <- glm( formula = get_admission ~ GRE.Score + CGPA + Research+SOP3.5+SOP4+LOR4+LOR4.5+University.Rating2,family = "binomial", data = train)
summary(model1.4)#AIC = 170.11, Null deviance =  387.65

model1.5 <- glm( formula = get_admission ~ GRE.Score + CGPA + Research+SOP3+SOP4+LOR3+LOR4.5+University.Rating2,family = "binomial", data = train)
summary(model1.5)#AIC = 168.65, Null deviance =  387.65

model1.6 <- glm( formula = get_admission ~ GRE.Score + CGPA + Research+SOP2+SOP3+SOP4+LOR3+LOR4.5+University.Rating2,family = "binomial", data = train)
summary(model1.6) #AIC =  170.51, Null deviance =  387.65

model1.7 <- glm( formula = get_admission ~ GRE.Score + CGPA + Research + LOR5+LOR4.5+University.Rating2 + SOP2 + SOP4 + LOR4,family = "binomial", data = train)
summary(model1.7)#AIC =  168.59, Null deviance =  387.65

model1.8 <- glm( formula = get_admission ~ GRE.Score + CGPA + Research + LOR5+SOP4.5+University.Rating2 + SOP2 + SOP4 + LOR4,family = "binomial", data = train)
summary(model1.8)#AIC =  168.34, Null deviance =  387.65

model1.9 <- glm( formula = get_admission ~ GRE.Score + CGPA + Research + LOR1.5+SOP4.5+University.Rating2 + LOR3.5 + LOR2.5 + LOR4,family = "binomial", data = train)
summary(model1.9) #AIC = 171.74, Null deviance =  387.65

model1.10 <- glm( formula = get_admission ~ GRE.Score + Research + CGPA + University.Rating2 + SOP2 + SOP4 + LOR4,family = "binomial", data = train)
summary(model1.10) #AIC =170.86, Null deviance =  387.65



#Model1.8 has less AIC as compared to all other models.
#Create Confusion matrix for Model1.5
predictTrain = predict(model1.8, type="response")
summary(predictTrain)
table(train$get_admission, predictTrain > 0.5)# Accuracy = 87.86%, FP = 11%, FN = 13%
table(train$get_admission, predictTrain > 0.4)# Accuracy = 87.86%, FP = 14%, FN = 9%
#Finalize threshold as 0.5 due to less proportion between False Positive and False Negative

#Build ROC curve for train Set
pred1 <- prediction(predictTrain,train$get_admission)
roc.perf = performance(pred1, measure = "tpr", x.measure = "fpr")
plot(roc.perf,colorize=TRUE)


#Test the Model
predictTest = predict(model1.10, type = "response", newdata = test)
table(test$get_admission,predictTest >= 0.5)# Accuracy = 87.5%, FP = 7%, FN = 15%

#Build ROC curve for test Set
pred2 <- prediction(predictTest,test$get_admission)
roc.perf2 = performance(pred2, measure = "tpr", x.measure = "fpr")
plot(roc.perf2,colorize=TRUE)


###### My build model is 87.5% accurate to predict admission status of a student. 
##### I have used logistic regression.##################