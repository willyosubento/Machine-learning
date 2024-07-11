#load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(skimr)
library(gridExtra)
library(caTools)
library(corrplot)
library(ggcorrplot)
library(naniar)
library(readr)
#load data
HPV<-read_csv("C:\\Users\\user\\Desktop\\RDocuments\\Analysis of HPV data\\HPV_DATA.csv")
dim(HPV)
names(HPV)

#Rename variables to be the correct format for analysis
HPV_renamed<-HPV %>%
  rename(Education_level="Education level",
         Monthly_income="Monthly income",
         Marital_status="Marital status",
         use_any_drugs="Use of any drugs",
         History_cancer="History of cancer",
         History_STIs="History of STIs" ,
         HPV_status="HPV Status") %>%
  view()
unique(HPV_renamed$Education_level)
#select variables and remove missing variables
df_HPV<-HPV_renamed %>%
  select(Age,Education_level,Monthly_income,Marital_status,
         use_any_drugs,History_cancer,History_STIs,HPV_status) %>%
  mutate(Education_level=recode(Education_level,primary="Primary")) %>%
  filter(complete.cases(.)) %>%
  distinct() %>%
  view()
#describe and explore the data
summary(df_HPV)
glimpse(df_HPV)
skim(df_HPV)#summarize the data structure of HPV
#check if we have values like unknown,N/A and other in the data
miss_scan_count(data = df_HPV,search = c("Unknown","N/A","other"))
#check for duplicates
sum(duplicated(df_HPV))
#Number of HPV women ranked according to Education level
df_HPV %>% count(Education_level) 
#convert non numeric variables to factor
df_HPV$Education_level<-as.factor(df_HPV$Education_level)
df_HPV$Monthly_income<-as.factor(df_HPV$Monthly_income)
df_HPV$Marital_status<-as.factor(df_HPV$Marital_status)
df_HPV$use_any_drugs<-as.factor(df_HPV$use_any_drugs)
df_HPV$History_cancer<-as.factor(df_HPV$History_cancer)
df_HPV$History_STIs<-as.factor(df_HPV$History_STIs)
df_HPV$HPV_status<-as.factor(df_HPV$HPV_status)
glimpse(df_HPV)
#=========================================================================================================================================
#Build prediction models
b1 <- df_HPV %>%
  ggplot(aes(x = Education_level, fill = Education_level)) +
  geom_bar(fill = c("red", "blue","green","purple")) +
  ggtitle("Education level Distribution") +
  geom_text(aes(label=..count..), stat = "Count", vjust = 1.0)

b2<-df_HPV %>%
  ggplot(aes(x=Monthly_income,fill=Monthly_income)) +
  geom_bar(fill=c("red","blue")) +
  ggtitle("Monthly income Distribution") +
  geom_text(aes(label=..count..),stat = "count",vjust=1.0)

b3<-df_HPV %>%
  ggplot(aes(x=Marital_status,fill=Marital_status)) +
  geom_bar(fill=c("red","blue","green","purple","brown")) +
  ggtitle("Marital status Distribution") +
  geom_text(aes(label=..count..),stat = "count",vjust=1.0)

b4<-df_HPV %>%
  ggplot(aes(x=use_any_drugs,fill=use_any_drugs)) +
  geom_bar(fill=c("red")) +
  ggtitle("Use of any drugs Distribution") +
  geom_text(aes(label=..count..),stat = "count",vjust=1.0)

b5<-df_HPV %>%
  ggplot(aes(x=History_cancer,fill=History_cancer)) +
  geom_bar(fill=c("red","blue")) +
  ggtitle("History of cancer Distribution") +
  geom_text(aes(label=..count..),stat = "count",vjust=1.0)

b6<-df_HPV %>%
  ggplot(aes(x=History_STIs,fill=History_STIs)) +
  geom_bar(fill=c("red","blue")) +
  ggtitle("History of STI's Distribution") +
  geom_text(aes(label=..count..),stat = "count",vjust=1.0)

b7<-df_HPV %>%
  ggplot(aes(x=HPV_status,fill=HPV_status)) +
  geom_bar(fill=c("red","blue")) +
  ggtitle("HPV status Distribution") +
  geom_text(aes(label=..count..),stat = "count",vjust=1.0)
#put all the graphs in one grid
grid.arrange(b1,b2,b3,b4,b5,b6,b7,ncol=2)
#compare cancer and HPV status
c1<-df_HPV %>%
  ggplot(aes(x = History_cancer, fill = HPV_status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("aquamarine3",
                             "blueviolet")) +
  ggtitle("History of cancer vs. HPV status") 
#compare History of STI's and HPV status
c2<-df_HPV %>%
  ggplot(aes(x = History_STIs, fill = HPV_status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("aquamarine3",
                             "blueviolet")) +
  ggtitle("History of STI's vs. HPV status")
#compare income and HPV_status
c3<-df_HPV %>%
  ggplot(aes(x = Monthly_income, fill = HPV_status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("aquamarine3",
                             "blueviolet")) +
  ggtitle("Monthly income vs. HPV status")
#compare Education level and HPV status
c4<-df_HPV %>%
  ggplot(aes(x = Education_level, fill = HPV_status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("aquamarine3",
                             "blueviolet")) +
  ggtitle("Education level vs. HPV status")

c5<-df_HPV %>%
  ggplot(aes(x = Marital_status, fill = HPV_status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("aquamarine3",
                             "blueviolet")) +
  ggtitle("Marital status vs. HPV status")


c6<-df_HPV %>%
  ggplot(aes(x = Age, fill = HPV_status)) +
  geom_density(position = "fill") +
  scale_fill_manual(values=c("aquamarine3",
                             "blueviolet")) +
  ggtitle("age vs. HPV status")

c7<-df_HPV %>%
  ggplot(aes(x = Age, fill = HPV_status)) +
  geom_histogram(position = "fill",bins = 5) +
  scale_fill_manual(values=c("aquamarine3",
                             "blueviolet")) +
  ggtitle("Age vs. HPV status")

c8<-df_HPV %>%
  ggplot(aes(x = use_any_drugs, fill = HPV_status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("aquamarine3",
                             "blueviolet")) +
  ggtitle("Use of drugs vs. HPV status")
#arrange the grids
grid.arrange(c1,c2,c3,c4,c5,c6,c7,c8,ncol=3)
#Evaluate and select the model
sample.split(df_HPV$HPV_status,SplitRatio = 0.80)->split_tag
train<-subset(df_HPV,split_tag==TRUE)
test<-subset(df_HPV,split_tag==FALSE)
dim(train)
dim(test)
#Deploy the prediction model
set.seed(123)
rf <- randomForest(formula = HPV_status~.,data = train)
rf
plot(rf)
confusionMatrix(predict(rf,test),test$HPV_status)




































 