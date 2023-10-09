library(tidyverse)
library(dplyr)
library(mosaic)
library(ggplot2)

Accident_casualities <- read.csv("~/R programming class/Accident_causalities.csv")
#Data from http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#Bootstrap. 
Accident_casualities<-Accident_casualities%>%
  select(Age_of_Casualty)

Sample_of_ages_for_accident_casualities<-Accident_casualities%>%
  slice_sample(n=10000, replace = T)

nboot=10000
UQ<-vector("numeric",10000)
Q3<-vector("numeric",10000)
iqr<-vector("numeric",10000)
Q1<-vector("numeric",10000)
LQ<-vector("numeric",10000)


for(i in 1:nboot){
  bootsample<-Sample_of_ages_for_accident_casualities%>%
    slice_sample(n=1000, replace = T)
  
  UQ[i]<-quantile(bootsample$Age_of_Casualty,0.95)
  Q3[i]<-quantile(bootsample$Age_of_Casualty,0.75)
  iqr[i]<-IQR(bootsample$Age_of_Casualty)
  Q1[i]<-quantile(bootsample$Age_of_Casualty,0.25)
  LQ[i]<-quantile(bootsample$Age_of_Casualty,0.05)
 
}
mean(iqr)
mean(UQ)
mean(LQ)
mean(Q3)
mean(Q1)

IQR(Accident_casualities$Age_of_Casualty)
quantile(Accident_casualities$Age_of_Casualty,0.975)
quantile(Accident_casualities$Age_of_Casualty,0.1)
quantile(Accident_casualities$Age_of_Casualty,0.25)
quantile(Accident_casualities$Age_of_Casualty,0.75)
quantile(Accident_casualities$Age_of_Casualty,1)



#Probability of person in a fatal accident being younger than the age of 22

#Why the beta distribution? 

Data_on_fatalities_by_death<-tibble(
  Age = c(20,30,40, 50, 60, 70, 80),
  Deaths =c(5623, 6548, 5117, 4958, 5347, 3658, 3556)
)

Data_driven_distribution<-ggplot(data = Data_on_fatalities_by_death, aes(x = Age, y = Deaths)) +
  geom_line()+
  geom_point()+
  labs(title= "Distibution based on data powered by autoinsurance.org")
Data_driven_distribution

xpbeta(10000,2,5)

age_scale_container<-vector("numeric",10000)
Proportion_container<-vector("logical",10000)

for(i in 1:10000){
  age_scale_container[i]<-rbeta(1,2,5)
  Proportion_container[i]<-ifelse(age_scale_container[i]<=0.1777777,T,F)
}


Probability_of_a_person_in_a_collision_being_younger_than_22<-sum(Proportion_container)/10000
Probability_of_a_person_in_a_collision_being_younger_than_22

#Probability of both drivers being young. 

age_scale_container_p1<-vector("numeric", 10000)
age_scale_container_p2<-vector("numeric", 10000)
Proportion_container_<-vector("logical",10000)

for(i in 1:10000){
  age_scale_container_p1[i]<-rbeta(1,2,5)
  age_scale_container_p2[i]<-rbeta(1,2,5)
  
  Proportion_container_[i]<-ifelse(age_scale_container_p1[i]<=0.1777777 & age_scale_container_p2[i]<=0.1777777 ,T,F)
  
 
}

Probability_of_both_being_young<-sum(Proportion_container_)/10000
Probability_of_both_being_young


#Number of miles until you are expected to get into an accident.Probability from Esurance. 
#Average life of a car 
number_of_1000mi_until_accident<-vector("numeric",10000)

for(i in 1:10000){
  number_of_1000mi_until_accident[i]<-rgeom(1,(1/336))
}

number_of_1000mi_until_accident
mean(number_of_1000mi_until_accident)

#Number of 1000 miles until getting into two accidents while speeding only 10 mph over speed limit. 
#According to automotive fleet and hale law firm.  
number_of_1000mi_until_2accidents_Speed<-vector("numeric",10000)

new_prob<-1.31*(1/366)
new_prob

for(i in 1:10000){
  number_of_1000mi_until_2accidents_Speed[i]<-rnbinom(1,2,new_prob)
}

number_of_1000mi_until_2accidents_Speed
mean(number_of_1000mi_until_2accidents_Speed)

number_of_1000mi_until_2accidents_Speednorm<-vector("numeric",10000)


for(i in 1:10000){
  number_of_1000mi_until_2accidents_Speednorm[i]<-rnbinom(1,2,1/366)
}

number_of_1000mi_until_2accidents_Speednorm
mean(number_of_1000mi_until_2accidents_Speednorm)

