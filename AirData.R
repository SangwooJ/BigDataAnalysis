library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)

setwd("C:/Users/linc/Desktop/미세먼지/2017")
FirstData <- read.csv("2017-1.csv", header = TRUE)
SecondData <- read.csv("2017-2.csv", header = TRUE)

##NA처리
NaData1<-na.omit(FirstData)
NaData2<-na.omit(SecondData)

##지역별 월별 추출 PM10 median (측정소별 월)
MonthlyData1 <-NaData1 %>%
  filter(month(as.Date(as.character(NaData1$측정일시),"%Y%m%d"))==1) %>%
  group_by(지역) %>%
  summarize(mean(PM10))

MonthlyData2 <-NaData1 %>%
  filter(month(as.Date(as.character(NaData1$측정일시),"%Y%m%d"))==2) %>%
  group_by(지역) %>%
  summarize(mean(PM10))

MonthlyData3 <-NaData1 %>%
  filter(month(as.Date(as.character(NaData1$측정일시),"%Y%m%d"))==3) %>%
  group_by(지역) %>%
  summarize(mean(PM10))

MonthlyData4 <-NaData2 %>%
  filter(month(as.Date(as.character(NaData2$측정일시),"%Y%m%d"))==4) %>%
  group_by(지역) %>%
  summarize(mean(PM10))

MonthlyData5 <-NaData2 %>%
  filter(month(as.Date(as.character(NaData2$측정일시),"%Y%m%d"))==5) %>%
  group_by(지역) %>%
  summarize(mean(PM10))


##열이름 재정의
colnames(MonthlyData1)=c("area","pm10")
colnames(MonthlyData2)=c("area","pm10")
colnames(MonthlyData3)=c("area","pm10")
colnames(MonthlyData4)=c("area","pm10")
colnames(MonthlyData5)=c("area","pm10")

#시도로 나누기 (도시별 월별평균)
tempCityName<-str_split_fixed(MonthlyData1$area, " ", 2)
CityDo<-as.vector(tempCityName[,1])
MonthlyData1<- cbind(MonthlyData1, city=CityDo)
MonthlyData1C<-MonthlyData1 %>%
  group_by(city) %>%
  summarize(mean(pm10))

tempCityName<-str_split_fixed(MonthlyData2$area, " ", 2)
CityDo<-as.vector(tempCityName[,1])
MonthlyData2<- cbind(MonthlyData2, city=CityDo)
MonthlyData2C<-MonthlyData2 %>%
  group_by(city) %>%
  summarize(mean(pm10))

tempCityName<-str_split_fixed(MonthlyData3$area, " ", 2)
CityDo<-as.vector(tempCityName[,1])
MonthlyData3<- cbind(MonthlyData3, city=CityDo)
MonthlyData3C<-MonthlyData3 %>%
  group_by(city) %>%
  summarize(mean(pm10))

tempCityName<-str_split_fixed(MonthlyData4$area, " ", 2)
CityDo<-as.vector(tempCityName[,1])
MonthlyData4<- cbind(MonthlyData4, city=CityDo)
MonthlyData4C<-MonthlyData4 %>%
  group_by(city) %>%
  summarize(mean(pm10))

tempCityName<-str_split_fixed(MonthlyData5$area, " ", 2)
CityDo<-as.vector(tempCityName[,1])
MonthlyData5<- cbind(MonthlyData5, city=CityDo)
MonthlyData5C<-MonthlyData5 %>%
  group_by(city) %>%
  summarize(mean(pm10))



#월정보 추가
monthval=rep(1,17)
MonthlyData1C<- cbind(MonthlyData1C, month=monthval)
monthval=rep(2,17)
MonthlyData2C<- cbind(MonthlyData2C, month=monthval)
monthval=rep(3,17)
MonthlyData3C<- cbind(MonthlyData3C, month=monthval)
monthval=rep(4,17)
MonthlyData4C<- cbind(MonthlyData4C, month=monthval)
monthval=rep(5,17)
MonthlyData5C<- cbind(MonthlyData5C, month=monthval)


totalMonthlyCity<-rbind(MonthlyData1C,MonthlyData2C,MonthlyData3C,MonthlyData4C,MonthlyData5C)

colnames(totalMonthlyCity)=c("city","pm10","month")
ggplot(totalMonthlyCity, aes(month, pm10)) + geom_point(aes(color=city))

#str(NaData
#month(as.Date(as.character(NaData$측정일시),"%Y%m%d"))

#library(lubridate)
#x="2017030202"
#y="2014024342"
#month(as.Date(c("2019020202","2014020302"),"%Y%m%d"))
# number of days between 6/22/07 and 2/13/04 
#month(x)