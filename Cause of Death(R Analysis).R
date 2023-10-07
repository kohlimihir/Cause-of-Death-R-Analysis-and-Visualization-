#' ---
#' title: "Analysis of Cause of Death(1990-2019)"
#' author: "Mihir Kohli"
#' date: ""
#' ---


##Required libraries

library(tidyverse)
library(plotrix)
library(knitr)
#The knitr package is a general-purpose tool for dynamic report generation in R using literate programming techniques

##importing data set in rstudio
cause_of_deaths <- read.csv("cause_of_deaths.csv")
data<-cause_of_deaths

##Summary of Data
kable(t(summary(data)),T)

##Header of data
kable(t(head(data)),T)

glimpse(data)

##Checking Dimensions of data
dim(data)

##Checking NA values from our dataset
summarise(data,count=sum(is.na(data)))

##Finding the data type in each Column
sapply(data,typeof)

##Renaming Column from Country/Territory to Country
colnames(data)[1]<-"Country"
colnames(data)

##Checking the number of distinct countries
n_distinct(data$Country)## gives there are 204 distinct countries

##Checking number of distinct data in all Columns
sapply(data,n_distinct)

##Finding row wise sum and adding that as a new column in our data set
total_death<-rowSums(data[,-c(1,2,3)])
data<-data%>%mutate(total_death)
#show the transpose of the header of the dataframe
kable(t(head(data)),T)

##total death by each disease over the whole period of time
death_by_cause<-colSums(data[,-c(1,2,3,35)])
death_by_cause

##Cause of highest number of death over the time 1990 to 2019
names(which(death_by_cause==max(death_by_cause)))

##Arranging data year wise
year_wise_data<-arrange(data,Year)
kable(t(head(year_wise_data)),T)

##Correlation matrix
cordata<-kable(cor(data[-c(1,2,3,35)]),linebreak=T)


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
##
##For top 5 Countries with highest number of death
top5_data<-year_wise_data%>%filter(Country %in% c("China","India","United States", "Russia", "Indonesia"))
top5_data<-top5_data[c(1,3,35)]
top5_data<-arrange(top5_data, Country)
top5_data

##Top 5 causes of death over the year 1990-2019 in top 5 Countries with highest number of death
top_5_cause<-head(death_by_cause[order(death_by_cause,decreasing = T)],5)
top_5_cause
top_5_cause_df<-data.frame(cause=names(top_5_cause),death=unname(top_5_cause))
top_5_cause_df

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
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))


##For India
india<-year_wise_data%>%filter(Country%in%"India")
#Top 10 reason of death 
india_10<-head(sort(colSums(india[-c(1,2,3,35)]),decreasing=T),10)
india_10_df<-data.frame(cause=names(india_10),death=unname(india_10))
india_10_df

ggplot(india_10_df,aes(x=cause,y=death))+
  geom_col(fill="blue")+
  theme_bw()+
  coord_flip()

label <-paste(round(india_10_df[,2]*100/sum(india_10_df[,2]),2),"%",sep="")
pie3D(india_10_df$death,explode=0.1,labels=label,col = heat.colors(length(india_10_df$cause)),
      main="Top 10 causes of death in India over the time 1990-2019")
par(xpd = TRUE)
legend(0.2,0.9, legend = india_10_df$cause, cex=0.7, yjust=0.6, xjust = -0.1, 
       fill = heat.colors(length(india_10_df$cause)))        


##Analysing top 5 cause of disease
top_5_cause_df

#1.Cardiovascular disease
death_data5<-data%>%filter(Country %in% c("China","India","United States", "Russia", "Indonesia"))
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



