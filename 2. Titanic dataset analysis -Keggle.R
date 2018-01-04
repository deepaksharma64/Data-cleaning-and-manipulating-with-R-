#--------------------------Titanic------------------------------------------
library(ggplot2)
dim(titanic) #891x12
names(titanic)#PassengerId,Survived,Pclass,Name,Sex,Age,SibSp,Parch,Ticket,Fare,Cabin,Embarked
#Identify the categorical predictors,even though R might interpret it as numeric 
#we will not use them for any mathematical manuplations.
#Survived,Pclass,Sex,Embarked are categorical(factor)
#Age has some missing data.

attach(titanic)
titanic$Pclass<-as.factor(titanic$Pclass)
titanic$Survived<-as.factor(titanic$Survived)
titanic$Sex<-as.factor(titanic$Sex)
titanic$Embarked<-as.factor(titanic$Embarked)

#First Question - What was the survival rate?

ggplot(titanic,aes(x=Survived))+geom_bar()
#percentage
prop.table(table(titanic$Survived))#0=0.61,1=38
#Add some customization for labels and theme.
ggplot(titanic,aes(x=Survived))+
  theme_bw()+geom_bar()+
  labs(y="Passenger Count",
  title="Titanic Survival Rates")

#Second question - What was the survival rate by gender

#We can use color to look at two aspects (i.e.,dimensions)
#of the data simultaneously
ggplot(titanic,aes(x=Sex,fill=Survived))+ #if you use fill command and put catagorecal variable,you get nice color coding.
 theme_bw()+
 geom_bar()+
 labs(y="Passenger Count",title="Titanic Survival Rates by Sex")

#Third question - what was survival rate by class of tickets?

ggplot(titanic,aes(x=Pclass,fill=Survived))+ #if you use fill command and put catagorecal variable,you get nice color coding.
  theme_bw()+
  geom_bar()+
  labs(y="Passenger Count",title="Titanic Survival Rates by Ticket Class")

#Fourth question - What was the survival rate by class of ticket and gender ?

ggplot(titanic,aes(x=Sex,fill=Survived))+ #if you use fill command and put catagorecal variable,you get nice color coding.
  theme_bw()+
  facet_wrap(~Pclass)+ #to add one more 
  geom_bar()+
  labs(y="Passenger Count",title="Titanic Survival Rates by Ticket Class and Gender")

#Now we will plot continuous data and not categorical variables.

#Fifth question- What is the distribution of passengers age ?

#The histogram is a staple of visualizing numeric data as it very 
#powerfully communicates the distribution of a variable.

ggplot(titanic,aes(x=Age))+
  theme_bw()+
  geom_histogram(binwidth=5)+  #binwidth changes the overall shape.
  labs(y="Passenger Count",
       x="Age (binwidth=5)",
       title="Titanic age distribution")
#177 missing values were automatically removed.

#Sixth question- What are survival rates by age?
ggplot(titanic,aes(x=Age,fill=Survived))+
  theme_bw()+
  geom_histogram(binwidth=5)+  #binwidth changes the overall shape.
  labs(y="Passenger Count",
       x="Age (binwidth=5)",
       title="Titanic age distribution and survival rates")
#177 missing values were automatically removed.

#Another way q-6 and be analyzed is by box-and-whisker plot

ggplot(titanic,aes(x=Survived,y=Age))+
  theme_bw()+
  geom_boxplot()+  #binwidth changes the overall shape.
  labs(y="Age",
       x="Survived",
       title="Titanic age distribution and survival rates")

#Question 7 - What is the survival rates by age when segmented by 
#gender and class of ticket

#A related visualization to the histogram is a density plot. Think of 
#a density plot as smoothed version of the histogram. Using ggplot2
#we can use facets to allow for visual drill-down via density plots.

ggplot(titanic,aes(x=Age,fill=Survived))+
  theme_bw()+
  facet_wrap(Sex~Pclass)+
  geom_density(alpha=0.5)+  #alpha is for transparancy
  labs(y="Survived",
       x="Age",
       title="Titanic age distribution and survival rates")
#same thing can be done using histogram plot
ggplot(titanic,aes(x=Age,fill=Survived))+
  theme_bw()+
  facet_wrap(Sex~Pclass)+
  geom_histogram(binwidth=5)+  #alpha is for transparancy
  labs(y="Survived",
       x="Age",
       title="Titanic age distribution and survival rates")
#-------------------------------------------------------------------------------------------------------------------------
