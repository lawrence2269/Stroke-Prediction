library(dplyr)
library(caret)
library(ggpubr)
library(e1071)
library(ggplot2)
library(ROSE)
library(caTools)
library(Metrics)
library(pROC)
library(randomForest)
library(AUC)

#Loading the data
data = read.csv("train.csv",header=TRUE,na.strings=c(""," ","NA"))
data = data[,!(names(data) %in% c("id"))]

#Filtering the data
data = filter(data,data$gender!="Other"&data$work_type!="children"&data$age>=18)

#converting hypertension and heart disease to factors
data$hypertension = as.factor(data$hypertension)
data$heart_disease = as.factor(data$heart_disease)

data$gender = as.factor(data$gender)
data$gender = ifelse(data$gender=="Male",0,1)

data$ever_married = as.factor(data$ever_married)
data$ever_married = ifelse(data$ever_married=="Yes",1,0)

data$work_type = as.factor(data$work_type)
data$work_type = ifelse(data$work_type=="Govt_job",1,ifelse(data$work_type=="Never_worked",2,
                                                            ifelse(data$work_type=="Private",3,4)))

data$Residence_type = as.factor(data$Residence_type)
data$Residence_type = ifelse(data$Residence_type=="Urban",1,0)

data$smoking_status = as.factor(data$smoking_status)
data$smoking_status = ifelse(data$smoking_status=="never smoked",0,
                             ifelse(data$smoking_status=="smokes",1,2))
data$stroke = as.factor(data$stroke)

#Checking for missing values
colSums(is.na(data))

#Before imputing missing values,let us remove the outliers in the data using IQR
boxplot(data$age,horizontal = TRUE,xlab="Age")
boxplot(data$avg_glucose_level,horizontal = TRUE,xlab="Avg_Glucose Level")
boxplot(data$bmi,horizontal = TRUE,xlab="BMI")

cols = c("bmi")
#"bmi","avg_glucose_level"
outliers = c()

for(i in cols){
  #Getting the min and max values
  max = quantile(data[,i],0.75, na.rm=TRUE) + (IQR(data[,i], na.rm=TRUE) * 1.5 )
  min = quantile(data[,i],0.25, na.rm=TRUE) - (IQR(data[,i], na.rm=TRUE) * 1.5 )
  
  # Get the id's using which
  idx <- which(data[,i] < min | data[,i] > max)
  
  # Output the number of outliers in each variable
  print(paste(i, length(idx), sep=''))
  
  # Append the outliers list
  outliers = c(outliers, idx) 
}

# Sort, I think it's always good to do this
outliers = sort(outliers)

# Remove the outliers
data = data[-outliers,]
boxplot(data$bmi,horizontal = TRUE,xlab="BMI")
boxplot(data$avg_glucose_level,horizontal = TRUE,xlab="Avg_Glucose Level")

#Missing value imputation
mean_bmi = mean(data$bmi,na.rm = T)
data = data %>% mutate(bmi = ifelse(is.na(bmi),mean_bmi,bmi))
data$bmi = round(data$bmi,2)
  #Note: Again check for outliers in bmi attribute

#Checking again for missing values
colSums(is.na(data))

#Removing the NA Categorical variables
data = na.omit(data)

#Checking Normality of the data
skewness(data$age) #0.14
skewness(data$avg_glucose_level) #0.40
skewness(data$bmi) #0.40

hist(data$age,probability = TRUE)
lines(density(data$age), col="blue", lwd=2)

hist(data$avg_glucose_level,probability = TRUE)
lines(density(data$avg_glucose_level), col="blue", lwd=2)

hist(data$bmi,probability = TRUE)
lines(density(data$bmi), col="blue", lwd=2)

skewness((data$avg_glucose_level)^(1/3))
hist((data$avg_glucose_level)^(1/3),probability = TRUE)
lines(density((data$avg_glucose_level)^(1/3)), col="blue", lwd=2)

skewness((data$bmi)^(1/3))
hist((data$bmi)^(1/3),probability = TRUE)
lines(density((data$bmi)^(1/3)), col="blue", lwd=2)

data$avg_glucose_level = (data$avg_glucose_level)^(1/3)
data$bmi = (data$bmi)^(1/3)

#Rounding up of continous variables to speed up the calculations
data$avg_glucose_level = round(data$avg_glucose_level,2)
data$bmi = round(data$bmi,2)

#Checking dataset whether it is balanced or not
prop.table(table(data$stroke))
#0          1 
#0.98258663 0.01741337 
#We can observe that data is imbalanced

#Using ROSE Technique to balance the data
balanced.data = ovun.sample(stroke~.,data=data,p=0.5,seed=1,
                            method="both")$data

#After ROSE Technique
prop.table(table(balanced.data$stroke))
#0         1 
#0.5035439 0.4964561 

temp = data
data = balanced.data

#Checking for outliers
boxplot(data$age,horizontal = TRUE,xlab="Age")
boxplot(data$avg_glucose_level,horizontal = TRUE,xlab="Avg_Glucose Level")
boxplot(data$bmi,horizontal = TRUE,xlab="BMI")

  #BMI has some outliers and removing it using IQR
cols = c("bmi")
#"bmi","avg_glucose_level"
outliers = c()

for(i in cols){
  #Getting the min and max values
  max = quantile(data[,i],0.75, na.rm=TRUE) + (IQR(data[,i], na.rm=TRUE) * 1.5 )
  min = quantile(data[,i],0.25, na.rm=TRUE) - (IQR(data[,i], na.rm=TRUE) * 1.5 )
  
  # Get the id's using which
  idx <- which(data[,i] < min | data[,i] > max)
  
  # Output the number of outliers in each variable
  print(paste(i, length(idx), sep=''))
  
  # Append the outliers list
  outliers = c(outliers, idx) 
}

# Sort, I think it's always good to do this
outliers = sort(outliers)

# Remove the outliers
data = data[-outliers,]
boxplot(data$bmi,horizontal = TRUE,xlab="BMI")

#Checking the data for Normality
skewness(data$age)
skewness(data$avg_glucose_level)
skewness(data$bmi)

hist(data$age,probability = TRUE)
lines(density(data$age), col="blue", lwd=2)

hist(data$avg_glucose_level,probability = TRUE)
lines(density(data$avg_glucose_level), col="blue", lwd=2)

hist(data$bmi,probability = TRUE)
lines(density(data$bmi), col="blue", lwd=2)

data$avg_glucose_level = round((data$avg_glucose_level)^(1/3),2)
data$bmi = round((data$bmi)^(1/3),2)

                  ################## EDA ###################

#Frquency plot for both males and females
tb1 = as.data.frame.table(table(data$gender))
tb1 = tb1[-c(3),]
colnames(tb1) = c("Gender","Frequency")
genderLen = c(paste(as.character(round(table(data$gender)[1]/1000,2)),"K"),
              paste(as.character(round(table(data$gender)[2]/1000,2)),"K"))
library(ggplot2)
ggplot(tb1,aes(Gender,Frequency,label = genderLen,fill=Gender))+
  geom_bar(stat = "identity")+
  labs(title="No. of Males and Females")+
  geom_text(vjust = 5,color = "Black",size=10)

#From the above graph, we can see that data contains more female than male

#Grouping by Stroke
NoStrokeData = filter(data,data$stroke=="0")
StrokeData = filter(data,data$stroke=="1")

#To find mode
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Understanding the data

#StrokeData
#Age
round(mean(StrokeData$age),2) #68.2
#Below Average
round((nrow(StrokeData[StrokeData$age<68.2,])/nrow(StrokeData))*100,2) # 41.17%
#Above Average
round((nrow(StrokeData[StrokeData$age>68.2,])/nrow(StrokeData))*100,2) # 58.83%

getmode(StrokeData$age) #79

#So,people who are above average age and especially who are 79 years are female who, 
#are having strokes which can be observed from the given data.

#Gender
tb3 = as.data.frame.table(table(StrokeData$gender))
tb3 = tb3[-c(3),]
colnames(tb3) = c("Gender","Frequency")
genderLenStrokeData = c(paste(as.character(round(table(StrokeData$gender)[1]/1000,2)),"K"),
                        paste(as.character(round(table(StrokeData$gender)[2]/1000,2)),"K"))

ggplot(tb3,aes(Gender,Frequency,label = genderLenStrokeData,fill=Gender))+
  geom_bar(stat = "identity")+
  labs(title="No. of Males and Females")+
  geom_text(vjust = 5,color = "Black",size=10)

#Again, here female are more in number and have stroke than male.

#Hypertension
tb4 = as.data.frame.table(with(StrokeData,table(StrokeData$hypertension,StrokeData$gender)))
colnames(tb4) = c("HyperTension","Gender","Frequency")
strokeDataHyperTensionLen = c(paste(as.character(round(tb4$Frequency[1]/1000,2)),"K"),
                              paste(as.character(round(tb4$Frequency[2]/1000,2)),"K"),
                              paste(as.character(round(tb4$Frequency[3]/1000,2)),"K"),
                              paste(as.character(round(tb4$Frequency[4]/1000,2)),"K"))

ggplot(tb4,aes(HyperTension,Frequency,label = strokeDataHyperTensionLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person having and not having hypertension")+
  geom_text(aes(label=strokeDataHyperTensionLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart, We can observe that female are high in numbers having and not 
#having hypertension than male

#Heart Disease
tb5 = as.data.frame.table(with(StrokeData,table(StrokeData$heart_disease,StrokeData$gender)))
colnames(tb5) = c("HeartDisease","Gender","Frequency")
strokeDataHeartDiseaseLen = c(paste(as.character(round(tb5$Frequency[1]/1000,2)),"K"),
                              paste(as.character(round(tb5$Frequency[2]/1000,2)),"K"),
                              paste(as.character(round(tb5$Frequency[3]/1000,2)),"K"),
                              paste(as.character(round(tb5$Frequency[4]/1000,2)),"K"))

ggplot(tb5,aes(HeartDisease,Frequency,label = strokeDataHeartDiseaseLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person having and not having HeartDisease")+
  geom_text(aes(label=strokeDataHeartDiseaseLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart,we can observe that, female are more in number who are not
# having heart disease when compared with male, but male are more in number who
# are having heart disease when compared with female.

#Marital Status
tb6 = as.data.frame.table(with(StrokeData,table(StrokeData$ever_married,StrokeData$gender)))
colnames(tb6) = c("Married","Gender","Frequency")
strokeDataMaritalLen = c(paste(as.character(round(tb6$Frequency[1]/1000,2)),"K"),
                         paste(as.character(round(tb6$Frequency[2]/1000,2)),"K"),
                         paste(as.character(round(tb6$Frequency[3]/1000,2)),"K"),
                         paste(as.character(round(tb6$Frequency[4]/1000,2)),"K"))

ggplot(tb6,aes(Married,Frequency,label = strokeDataMaritalLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person who are married and not married")+
  geom_text(aes(label=strokeDataMaritalLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart, we can observe that people are who are having stroke are female
# who are married as well as not married when compared with male.

#Work type
tb7 = as.data.frame.table(with(StrokeData,table(StrokeData$work_type,StrokeData$gender)))
colnames(tb7) = c("WorkType","Gender","Frequency")
strokeDataWorkTypeLen = c(paste(as.character(round(tb7$Frequency[1]/1000,2)),"K"),
                          paste(as.character(round(tb7$Frequency[2]/1000,2)),"K"),
                          paste(as.character(round(tb7$Frequency[3]/1000,2)),"K"),
                          paste(as.character(round(tb7$Frequency[4]/1000,2)),"K"),
                          paste(as.character(round(tb7$Frequency[5]/1000,2)),"K"),
                          paste(as.character(round(tb7$Frequency[6]/1000,2)),"K"))

ggplot(tb7,aes(WorkType,Frequency,label = strokeDataWorkTypeLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person based on Work Type")+
  geom_text(aes(label=strokeDataWorkTypeLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart we can see that, females who are working in private firms are 
# high in numbers suffering from strokes when compared with self-employment 
# and govt jobs. The same applies to males and another thing to note here is
# female who are not working also suffering from strokes.

#Residence Type
tb8 = as.data.frame.table(with(StrokeData,table(StrokeData$Residence_type,StrokeData$gender)))
colnames(tb8) = c("ResidenceType","Gender","Frequency")
strokeDataResidenceTypeLen = c(paste(as.character(round(tb8$Frequency[1]/1000,2)),"K"),
                               paste(as.character(round(tb8$Frequency[2]/1000,2)),"K"),
                               paste(as.character(round(tb8$Frequency[3]/1000,2)),"K"),
                               paste(as.character(round(tb8$Frequency[4]/1000,2)),"K"))

ggplot(tb8,aes(ResidenceType,Frequency,label = strokeDataResidenceTypeLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person based on Residence Type")+
  geom_text(aes(label=strokeDataResidenceTypeLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart, we can observe that females are who are living in rural as well
#as in urban are more in number are suffering from stroke and there is no much
#difference between females in rural and urban. The same applies to males as well.

#Glucose Level
round(mean(StrokeData$avg_glucose_level),2) #1.64

#Below Average
round((nrow(StrokeData[StrokeData$avg_glucose_level<1.64,])/nrow(StrokeData))*100,2) #45.83%
#Above Average
round((nrow(StrokeData[StrokeData$avg_glucose_level>1.64,])/nrow(StrokeData))*100,2) #44.66%

getmode(StrokeData$avg_glucose_level) #1.62

#From the data, we can observe that, people who are having glucose level less than
#1.64, are mostly females who are working in private firms and not having
#hyper tension and living in Urban and never smoked are suffering from strokes

#There are more number of people who get stroke with the average glucose level 
#of 1.62 which consists of females who are living in urban and working
#in private firms and are married with the smoking status as never smoked and 
#formely smoked

#BMI
round(mean(StrokeData$bmi),2) #1.45

#Below Average
round((nrow(StrokeData[StrokeData$bmi<1.45,])/nrow(StrokeData))*100,2) #40.31%
#Above Average
round((nrow(StrokeData[StrokeData$bmi>1.45,])/nrow(StrokeData))*100,2) #34.48%

getmode(StrokeData$bmi) #1.45

#From the data, we can see that 2811 persons who are having bmi of 1.45 are female
#mostly in the age group of 76 and are married living in Urban who had formerly
#smoked and working in private firms with no hypertension and heart disease, but
#as per the litreature the normal bmi level would be between 2.64 to 2.92, but here
#people are less than normal bmi are suffering from strokes.

#smoking Status
tb9 = as.data.frame.table(with(StrokeData,table(StrokeData$smoking_status,StrokeData$gender)))
colnames(tb9) = c("SmokingStatus","Gender","Frequency")
strokeDataSmokingStatusLen = c(paste(as.character(round(tb9$Frequency[1]/1000,2)),"K"),
                               paste(as.character(round(tb9$Frequency[2]/1000,2)),"K"),
                               paste(as.character(round(tb9$Frequency[3]/1000,2)),"K"),
                               paste(as.character(round(tb9$Frequency[4]/1000,2)),"K"),
                               paste(as.character(round(tb9$Frequency[5]/1000,2)),"K"),
                               paste(as.character(round(tb9$Frequency[6]/1000,2)),"K"))

ggplot(tb9,aes(SmokingStatus,Frequency,label = strokeDataSmokingStatusLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person based on Residence Type")+
  geom_text(aes(label=strokeDataSmokingStatusLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart, we can observe that, female are mostly affected by stroke who
#never smoked when compared with females from formerly smoked and smokes category.
#Males are also have similar pattern of females.

#NoStroke Data
#Age
round(mean(NoStrokeData$age),2) #47.3
#Below Average
round((nrow(NoStrokeData[NoStrokeData$age<47.3,])/nrow(NoStrokeData))*100,2) #51.41%
#Above Average
round((nrow(NoStrokeData[NoStrokeData$age>47.3,])/nrow(NoStrokeData))*100,2) #48.59%

getmode(NoStrokeData$age) #47

#So,people who are below average age and especially who are 47 years, 
#are having strokes are more in number which can be observed from the given data.

#Gender
tb10 = as.data.frame.table(table(NoStrokeData$gender))
tb10 = tb10[-c(3),]
colnames(tb10) = c("Gender","Frequency")
genderLenNoStrokeData = c(paste(as.character(round(table(NoStrokeData$gender)[1]/1000,2)),"K"),
                          paste(as.character(round(table(NoStrokeData$gender)[2]/1000,2)),"K"))
library(ggplot2)
ggplot(tb10,aes(Gender,Frequency,label = genderLenNoStrokeData,fill=Gender))+
  geom_bar(stat = "identity")+
  labs(title="No. of Males and Females")+
  geom_text(vjust = 5,color = "Black",size=10)

#Again, here female are more in number and have not stroke than male.

#Hypertension
tb14 = as.data.frame.table(with(NoStrokeData,table(NoStrokeData$hypertension,NoStrokeData$gender)))
colnames(tb14) = c("HyperTension","Gender","Frequency")
NoStrokeDataHyperTensionLen = c(paste(as.character(round(tb14$Frequency[1]/1000,2)),"K"),
                                paste(as.character(round(tb14$Frequency[2]/1000,2)),"K"),
                                paste(as.character(round(tb14$Frequency[3]/1000,2)),"K"),
                                paste(as.character(round(tb14$Frequency[4]/1000,2)),"K"))

library(ggplot2)
ggplot(tb14,aes(HyperTension,Frequency,label = NoStrokeDataHyperTensionLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person having and not having hypertension")+
  geom_text(aes(label=NoStrokeDataHyperTensionLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart, We can observe that female are high in numbers having and not 
#having hypertension than male

#Heart Disease
tb15 = as.data.frame.table(with(NoStrokeData,table(NoStrokeData$heart_disease,NoStrokeData$gender)))
colnames(tb15) = c("HeartDisease","Gender","Frequency")
NoStrokeDataHeartDiseaseLen = c(paste(as.character(round(tb15$Frequency[1]/1000,2)),"K"),
                                paste(as.character(round(tb15$Frequency[2]/1000,2)),"K"),
                                paste(as.character(round(tb15$Frequency[3]/1000,2)),"K"),
                                paste(as.character(round(tb15$Frequency[4]/1000,2)),"K"))

library(ggplot2)
ggplot(tb15,aes(HeartDisease,Frequency,label = NoStrokeDataHeartDiseaseLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person having and not having HeartDisease")+
  geom_text(aes(label=NoStrokeDataHeartDiseaseLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart,we can observe that, female are more in number who are not
# having heart disease when compared with male, but male are more in number who
# are having heart disease when compared with female.

#Marital Status
tb16 = as.data.frame.table(with(NoStrokeData,table(NoStrokeData$ever_married,NoStrokeData$gender)))
colnames(tb16) = c("Married","Gender","Frequency")
NoStrokeDataMaritalLen = c(paste(as.character(round(tb16$Frequency[1]/1000,2)),"K"),
                           paste(as.character(round(tb16$Frequency[2]/1000,2)),"K"),
                           paste(as.character(round(tb16$Frequency[3]/1000,2)),"K"),
                           paste(as.character(round(tb16$Frequency[4]/1000,2)),"K"))

library(ggplot2)
ggplot(tb16,aes(Married,Frequency,label = NoStrokeDataMaritalLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person who are married and not married")+
  geom_text(aes(label=NoStrokeDataMaritalLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart, we can observe that people are who are having stroke are female
# who are married as well as not married when compared with male.

#Work type
tb17 = as.data.frame.table(with(NoStrokeData,table(StrokeData$work_type,StrokeData$gender)))
colnames(tb17) = c("WorkType","Gender","Frequency")
NoStrokeDataWorkTypeLen = c(paste(as.character(round(tb17$Frequency[1]/1000,2)),"K"),
                            paste(as.character(round(tb17$Frequency[2]/1000,2)),"K"),
                            paste(as.character(round(tb17$Frequency[3]/1000,2)),"K"),
                            paste(as.character(round(tb17$Frequency[4]/1000,2)),"K"),
                            paste(as.character(round(tb17$Frequency[5]/1000,2)),"K"),
                            paste(as.character(round(tb17$Frequency[6]/1000,2)),"K"))

library(ggplot2)
ggplot(tb17,aes(WorkType,Frequency,label = NoStrokeDataWorkTypeLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person based on Work Type")+
  geom_text(aes(label=NoStrokeDataWorkTypeLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart we can see that, females who are working in private firms are 
# high in numbers suffering from strokes when compared with self-employment 
# and govt jobs. The same applies to males and another thing to note here is
# female who are not working also suffering from strokes.

#Residence Type
tb18 = as.data.frame.table(with(NoStrokeData,table(NoStrokeData$Residence_type,NoStrokeData$gender)))
colnames(tb18) = c("ResidenceType","Gender","Frequency")
NoStrokeDataResidenceTypeLen = c(paste(as.character(round(tb18$Frequency[1]/1000,2)),"K"),
                                 paste(as.character(round(tb18$Frequency[2]/1000,2)),"K"),
                                 paste(as.character(round(tb18$Frequency[3]/1000,2)),"K"),
                                 paste(as.character(round(tb18$Frequency[4]/1000,2)),"K"))

library(ggplot2)
ggplot(tb18,aes(ResidenceType,Frequency,label = NoStrokeDataResidenceTypeLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person based on Residence Type")+
  geom_text(aes(label=strokeDataResidenceTypeLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)

#From the chart, we can observe that females are who are living in rural as well
#as in urban are more in number are suffering from stroke and there is no much
#difference between females in rural and urban. The same applies to males as well.

#Glucose Level
round(mean(NoStrokeData$avg_glucose_level),2) #1.64

#Below Average
round((nrow(NoStrokeData[NoStrokeData$avg_glucose_level<1.64,])/nrow(NoStrokeData))*100,2) #42.69%
#Above Average
round((nrow(NoStrokeData[NoStrokeData$avg_glucose_level>1.64,])/nrow(NoStrokeData))*100,2) #48.15%

getmode(NoStrokeData$avg_glucose_level) #1.63

#From the data, we can observe that, people who are having glucose level less than
#1.64, are mostly females in the age group of 51 who are working in private firms and 
#are married.They are not having hyper tension and heart diseases
#and living in Urban and never smoked with glucose level 87.73 and
#bmi level in 23.7 and 35.1 are suffering from strokes

#There are more number of people who get stroke with the average glucose level 
#of 74.95 which consists of females who are living in urban and working
#in private firms and are married with the smoking status as never smoked and 
#formely smoked

#BMI
round(mean(NoStrokeData$bmi),2) #1.45

#Below Average
round((nrow(NoStrokeData[NoStrokeData$bmi<1.45,])/nrow(NoStrokeData))*100,2) #41.81%
#Above Average
round((nrow(NoStrokeData[NoStrokeData$bmi>1.45,])/nrow(NoStrokeData))*100,2) #42.84%

getmode(NoStrokeData$bmi) #1.45

#From the data, we can see that 4743 persons who are having bmi of 1.45 are females
#mostly in the age group of 29. They are married and living in Urban who had 
#never smoked and working in private firms with no hypertension and heart disease,
#but as per the litreature the normal bmi level would be between 2.64 to 2.92, 
#but here people are having less than normal bmi are suffering from strokes.

#smoking Status
tb19 = as.data.frame.table(with(NoStrokeData,table(NoStrokeData$smoking_status,NoStrokeData$gender)))
colnames(tb19) = c("SmokingStatus","Gender","Frequency")
NoStrokeDataSmokingStatusLen = c(paste(as.character(round(tb19$Frequency[1]/1000,2)),"K"),
                                 paste(as.character(round(tb19$Frequency[2]/1000,2)),"K"),
                                 paste(as.character(round(tb19$Frequency[3]/1000,2)),"K"),
                                 paste(as.character(round(tb19$Frequency[4]/1000,2)),"K"),
                                 paste(as.character(round(tb19$Frequency[5]/1000,2)),"K"),
                                 paste(as.character(round(tb19$Frequency[6]/1000,2)),"K"))

library(ggplot2)
ggplot(tb19,aes(SmokingStatus,Frequency,label = NoStrokeDataSmokingStatusLen,fill=Gender))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(title="No. of person based on Residence Type")+
  geom_text(aes(label=NoStrokeDataSmokingStatusLen),position = position_dodge(0.9),
            vjust = -0.2,color = "black",size=5)


#From the chart, we can observe that, female are mostly affected by stroke who
#never smoked when compared with other females from formerly smoked and smokes category.
#Males are also have similar patterns of females.

                    ############## Model Building ##############

#Splitting the data into train and test
set.seed(123)
sample = sample.split(data,SplitRatio = 0.75)
train = subset(data,sample==TRUE)
test = subset(data,sample==FALSE)

test = test[-11]

#Applying decision tree model
target = stroke~gender+age+hypertension+heart_disease+ever_married+work_type+
         Residence_type+avg_glucose_level+bmi+smoking_status
forest = randomForest(target,data=train)
forest_Pred = predict(forest, test[-11])
table(test$stroke,forest_Pred)
summary(forest)

#Accuracy
round(mean(forest_Pred==test$stroke),2)  #0.98 i.e. 98%

cm = as.matrix(table(Actual = test$stroke, Predicted = forest_Pred)) 
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

accuracy = sum(diag) / n 
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1) 

plot(roc(forest_Pred, test$stroke), main = "ROC", cex.axis=0.6, cex.main=0.8, cex.lab=0.8)
print(paste0("AUC = ", round(Metrics::auc(actual = test$stroke, predicted = forest_Pred), 4)))
