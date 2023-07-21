##Required libraries

library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotrix)

##importing dataset in rstudio
cause_of_deaths <- read.csv("C:/Users/mihir/Downloads/Proj/cause_of_deaths.csv")
data<-cause_of_deaths
summary(data)
head(data)
glimpse(data)

##Checking Dimensions of data
dim(data)

##Checking NA values from our dataset
summarise(data,count=sum(is.na(data)))

##Finding the data type in each Column
sapply(data,typeof)

##Renaming Column from Country/Territory
colnames(data)[1]<-"Country"
colnames(data)

##Checking the number of distinct countries
n_distinct(data$Country)## gives there are 204 distinct countries

##Checking number of distinct data in all Columns
sapply(data,n_distinct)

##Finding row wise sum and adding that as a new column in our data set
total_death<-rowSums(data[,-c(1,2,3)])
data<-data%>%mutate(total_death)
head(data)

##total death by each disease over the whole period of time
death_by_cause<-colSums(data[,-c(1,2,3,35)])
death_by_cause

##Cause of highest number of death over the time 1990 to 2019
names(which(death_by_cause==max(death_by_cause)))

##Arranging data year wise
year_wise_data<-arrange(data,Year)
year_wise_data

##Correlation matrix
cor(data[-c(1,2,35)])


##Total number of death Country wise over 1990 to 2019 
sum_by_country<-aggregate(total_death~Country,data,sum)
sum_by_country    ##Alternative Method: sum_by_country=data%>%group_by(Country)%>%summarize(sum=sum(total_death))

##Top 10 Countries with highest number of death over whole period of time
Death10<-head(sum_by_country[order(sum_by_country$total_death,decreasing = T),],10)
Death10
#China and India Topped the list taking first and second spot respectively

##Top 5 causes of death in Year 2019
data_2019<-colSums(subset(data,Year==2019)[-c(1,2,3,35)])
data_2019
top_5<-head(sort(data_2019,decreasing = T),5)
top_5
#Cardiovascular Diseases has been the topmost reason for death over the world in the year 2019

##For top 5 Countries with highest number of death
top5_data<-year_wise_data%>%filter(Country %in% c("China","India","United States", "Russia", "Indonesia"))
top5_data<-top5_data[c(1,3,35)]
top5_data<-arrange(top5_data, Country)

##Top 5 causes of death over the year 1990-2019
top_5_cause<-head(death_by_cause[order(death_by_cause,decreasing = T)],5)
top_5_cause
top_5_cause_df<-data.frame(cause=names(top_5_cause),death=unname(top_5_cause))

#Cardiovascular Disease has been the reason for the highest number of death over the time period 1990-2019

##Visualisation

##line chart showing Year Vs Total Death for top 5 countries with highest number of Death
ggplot(top5_data,aes(x=Year,y=total_death,group=Country,color=Country))+
  geom_line()+
  geom_point(size=2)+
labs(title ="Year Vs Total Death " ,x= "Year", y="Total Death")

## Barchart showing top 5 causes of death
ggplot(top_5_cause_df,aes(x=cause,y=death))+
  geom_col(fill=c("#FEC5BB","#FCD5CE","#FAE1DD","#F8F0F2","#D0E0EB"))+
  theme_bw()

##For India
india<-year_wise_data%>%filter(Country%in%"India")
india
#Top 10 reason of death 
india_10<-head(sort(colSums(india[-c(1,2,3,35)]),decreasing=T),10)
india_10_df<-data.frame(cause=names(india_10),death=unname(india_10))
india_10_df

ggplot(india_10_df,aes(x=cause,y=death))+
  geom_col(fill="blue")+
  theme_bw()+
  coord_flip()

pie3D(india_10_df$death,labels = india_10_df$cause,
      start=pi/2,labelcex = 0.8,theta = 1,radius=1.2,explode=0.1,
      height=0.25,mar=c(3,4,3,3),border="black",shade=0.5,
      main="Top 10 causes of death in India over the time 1990-2019")

##Analysing top 5 cause of disease
top_5_cause_df

#1.Cardiovascular disease
death_data5<-data%>%filter(Country %in% c("China","India","United States", "Russia", "Indonesia"))
death_data5
ggplot(death_data5,aes(x=Year,y=Cardiovascular.Diseases,group=Country,fill=Country))+
  geom_area(alpha=0.5)+
  labs(title ="Year Vs Death due to Cardiovascular disease " ,x= "Year", y="Death due to Cardiovascular disease")+
  theme_classic()
 

#2.Neoplasm
ggplot(death_data5,aes(x=Year,y=Neoplasms,group=Country,fill=Country))+
  geom_area(alpha=0.5)+
  labs(title ="Year Vs Death due to Neoplasm " ,x= "Year", y="Death due to Neoplasm")+
  theme_gray()

#3.Chronic Respiratory Diseases
ggplot(death_data5,aes(x=Year,y=Chronic.Respiratory.Diseases,group=Country,fill=Country))+
  geom_area(alpha=0.5)+
  labs(title ="Year Vs Death due to Chronic Respiratory Diseases " ,x= "Year", y="Death due to Chronic Respiratory Diseases")+
  theme_bw()

#4.Lower Respiratory Infections
ggplot(death_data5,aes(x=Year,y=Lower.Respiratory.Infections,group=Country,fill=Country))+
  geom_area(alpha=0.5)+
  labs(title ="Year Vs Death due to Lower Respiratory Infections " ,x= "Year", y="Death due to Lower Respiratory Infections")+
  theme_get()

#5.Neonatal Disorders
ggplot(death_data5,aes(x=Year,y=Neonatal.Disorders,group=Country,fill=Country))+
  geom_area(alpha=0.5)+
  labs(title ="Year Vs Death due to Neonatal Disorders " ,x= "Year", y="Death due to Neonatal Disorders")+
  theme_linedraw()


