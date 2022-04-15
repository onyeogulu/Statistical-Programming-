# Install packages 
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages('plyr')
library(plyr)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
#==============================================================================================================================================
#Read in the Hepatitis C csv file as a dataframe in R
HepatitisCdata <- read.csv("HepatitisCdata.csv")
head(HepatitisCdata)

# Data cleaning of the hepatitis C dataset

#drop all missing values in the dataframe
HepatitisCdata_new<-HepatitisCdata %>% drop_na()

head(HepatitisCdata_new) # view the head of the dataframe


# recoding the various categories in the Category attribute and combing all the different stages of hapatitis with one code
HepatitisCdata_new$Category<- revalue(HepatitisCdata_new$Category, c(
  "0=Blood Donor"="Blood Donor", 
  "0s=suspect Blood Donor"="Hepatitis", 
  "1=Hepatitis"="Hepatitis",
  "2=Fibrosis"="Hepatitis",
  "3=Cirrhosis"="Hepatitis"
))

# recode the sex atribute with m = Male and f = Female
HepatitisCdata_new$Sex<- revalue(HepatitisCdata_new$Sex, c(
  "m"="Male", 
  "f"="Female"
))

# Drop the serial number attribute in the dataframe.
HepatitisCdata_new<-subset(HepatitisCdata_new, select = c('Category', 'Age',
                                                          'Sex', 'ALB', 'ALP', 'ALT',
                                                          'AST', 'BIL', 'CHE','CHOL'))

# Descriptive statistcs of the  cleaned hepatitis C dataset

#Use the summary function to obtain basic descriptive statistics for all attribute in the dataframe
summary(HepatitisCdata_new) 

#===============================================================================================================================================

# Question one 
# Subseting age for blood donors patients 
blood_donor_age <- subset(HepatitisCdata_new, Category=='Blood Donor',select = c('Age')) 

# Subseting age for hepatitis patients
Hepatitis_age <- subset(HepatitisCdata_new, Category=='Hepatitis',select = c('Age')) 

# Shapiro test to check if the ages for blood donors follows a normal distribution
shapiro.test(blood_donor_age$Age) 
# Shapiro test to check if the ages for hepatitis patient follows a normal distribution
shapiro.test(Hepatitis_age$Age)

# Wilcox test to check if a difference exist between the ages for blood donors and hepatitis patients
wilcox.test(blood_donor_age$Age,Hepatitis_age$Age)
#===================================================================================================================================================
#Question two
# table function to count the number of male and female in blood donors and hepatitis patients
proptable<-table(HepatitisCdata_new$Category,HepatitisCdata_new$Sex)
# test the proportiob of females in blood donors and hepatitis patients 
prop.test(proptable)
#===================================================================================================================================================
#Question three
# Subseting the ALP values for male patients
ALP_male <- subset(HepatitisCdata_new, Sex=='Male',select = c('ALP'))
# Subseting the ALP values for female patients
ALP_female <- subset(HepatitisCdata_new, Sex=='Female',select = c('ALP'))

# Shapiro test to check if the ALP values for male patient follows a normal distribution
shapiro.test(ALP_male$ALP)
# Shapiro test to check if the ALP values for female patient follows a normal distribution
shapiro.test(ALP_female$ALP)
# Wilcox test to check if a difference exist between the ALP values for male and female patients
wilcox.test(ALP_male$ALP,ALP_female$ALP)
#===================================================================================================================================================
# Question four
# Subseting the ALP values for blood donors patients
ALP_blooddonor <- subset(HepatitisCdata_new, Category=='Blood Donor',select = c('ALP'))
# Subseting the ALP values for hepatitis patients
ALP_hepatitis <- subset(HepatitisCdata_new, Category=='Hepatitis',select = c('ALP'))
# Shapiro test to check if the ALP value for blood donors follows a normal distribution
shapiro.test(ALP_blooddonor$ALP)
# Shapiro test to check if the ALP values for hepatitis patient follows a normal distribution
shapiro.test(ALP_hepatitis$ALP)
# Wilcox test to check if a difference exist between the CHE values for blood donors and hepatitis patients
wilcox.test(ALP_blooddonor$ALP,ALP_hepatitis$ALP)
#====================================================================================================================================================
# Question five
# Subseting the CHE values for male patients
CHE_male <- subset(HepatitisCdata_new,Sex=='Male',select = c('CHE'))
# Subseting the CHE values for female patients
CHE_female <- subset(HepatitisCdata_new,  Sex=='Female',select = c('CHE'))
# Shapiro test to check if the CHE value for male follows a normal distribution
shapiro.test(CHE_male$CHE)
# Shapiro test to check if the CHE values for female patient follows a normal distribution
shapiro.test(CHE_female$CHE)
# Wilcox test to check if a difference exist between the CHE values for male and female patients
wilcox.test(CHE_male$CHE,CHE_female$CHE)
#===================================================================================================================================================
#Question Six
# Subseting the CHE values for blood donors patients
CHE_blooddonor <- subset(HepatitisCdata_new, Category=='Blood Donor',select = c('CHE'))
# Subseting the CHE values for hepatitis patients
CHE_hepatitis <- subset(HepatitisCdata_new, Category=='Hepatitis',select = c('CHE'))
# Shapiro test to check if the CHE value for blood donors follows a normal distribution
shapiro.test(CHE_blooddonor$CHE)
# Shapiro test to check if the CHE values for hepatitis patient follows a normal distribution
shapiro.test(CHE_hepatitis$CHE)
# Wilcox test to check if a difference exist between the CHE values for blood donors and hepatitis patients
wilcox.test(CHE_blooddonor$CHE,CHE_hepatitis$CHE)
#===================================================================================================================================================
#Question Seven
# Two way anova to test if sex and category have an effect on the ALT laboratory value
summary(aov(ALT~Sex+Category, data =HepatitisCdata_new ))
#===================================================================================================================================================
# Graphs to support the statistical test carried out.
# Graph one
# ggplot of show if age has any influence on the category of patient.
ggplot(HepatitisCdata_new, aes(x=Category, y =Age ))+geom_boxplot()+ylab("Age of patient")

# Graph two
# ggplot of category of patient and frequecy legend with sex
# convert the proptable to a dataframe
proptable_frame<-as.data.frame(proptable)
proptable_frame
# column names for the proptable_frame dataframe
colnames(proptable_frame) <- c('Category', 'Sex', 'Frequency')
# Barchart to show the distribution of sex and category variable
ggplot(proptable_frame, aes(Category,Frequency)) + geom_bar(aes(fill=Sex), stat = "identity", position = "dodge")

# Graph three
# Box plot graph to show if Category or Sex have an influence on the ALP laboratoty value using ggplot
p1<-ggplot(HepatitisCdata_new, aes(x=Sex, y = ALP))+geom_boxplot()
p1+facet_grid(~Category)+ylab("Alkaline phosphatase (ALP)")

# Graph four
# Box plot graph to show if Category or Sex have an influence on the CHE laboratoty value using ggplot
p2<-ggplot(HepatitisCdata_new, aes(x=Sex, y = CHE))+geom_boxplot()
p2+facet_grid(~Category)+ylab("Acetylcholinesterase (CHE)")

# Graph five
# Box plot graph to show if Category or Sex have an influence on the ALT laboratoty value using ggplot
p3<-ggplot(HepatitisCdata_new, aes(x=Sex, y = ALT))+geom_boxplot()
p3+facet_grid(~Category)+ylab("Alanine Transaminase (ALT)")
#===================================================================================================================================================
