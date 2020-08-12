rm(list=ls())
setwd("C:/Users/Vinayak/Desktop/Business Data Analytics/Project")
#Loading required packages
library(dplyr)
library(ggplot2)
library(stringr)
library(rlang)
library(tidyverse)
library(caret)
library(reshape2)
library(Information)
library(woe)
library(MASS)
library(fuzzyjoin)
library(ROCR)
library(woeBinning)
library(corrplot)
library(gridExtra)
library(CombMSC)
library(class)

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
##################
# DATA CLEANING  #
##################

#Accessing the two datasets
credit <- read.csv("Credit_Bureau.csv",na.strings = c("NA",""))
dem <- read.csv("demogs.csv",na.strings = c("NA",""))

#reassigning coloumn names
colnames(dem)<-c("ID","Age","Gender","Marital_Status","Dependents","Income",
                 "Education","Profession","Residence","Months_in_residence",
                 "Months_in_company","Performance_Tag")
colnames(credit)<-c("ID","90DPD-6M","60DPD-6M","30DPD-6M","90DPD-12M",
                    "60DPD-12M","30DPD-12M","AVGAS.CC","Open_Trades_6M",
                    "Open_Trades_12M","Open_PL_Trades_6M","Open_PL_Trades_12M",
                    "Inquiries_6M","Inquiries_12M","Open_Home_Loan","Outstanding_Balance",
                    "Total_Trades","Open_Auto_Loan","Performance_Tag")

#Summary of datasets
summary(dem)
summary(credit)

#checking for duplicates
dem %>%
  group_by(ID)%>%
  filter(n()>1)

credit %>%
  group_by(ID)%>%
  filter(n()>1)
#3 duplicates found#


#selecting only unique ID's
credit <- credit[!credit$ID %in% c(765011468, 653287861, 671989187),]
dem <- dem[!dem$ID %in% c(765011468, 653287861, 671989187),]

#Identifyig NA's in coloumns

#total number of NA's in dem and credit
Tot_NA<-as.data.frame(t(sapply(list(dem,credit), function(x) length(which(is.na(x))))))
colnames(Tot_NA)<-c("dem","credit")
rownames(Tot_NA)<-"NA.count"
print(Tot_NA)

#no. of NA's in dem by coloumn
colSums(is.na(dem))

#no. of NA's in credit by coloumn
colSums(is.na(credit))

#checking for NA in Performance.Tag i.e dependent categorical variable
#missing values in dependent variable cannot be practically solved
#thus we resort to removing them from the dataset
dem$Performance_Tag %>%
  is.na()%>%
  sum()

credit$Performance_Tag %>%
  is.na()%>%
  sum()

#1425 missing entries for Performance in both datasets

#removing rows with missing entries of Performance_Tag
credit<-credit[!(is.na(credit$Performance_Tag)),]
dem<-dem[!(is.na(dem$Performance_Tag)),]

merged_data <- merge(dem[,-12], credit, by ="ID",all=F)

########################
# UNIVARIATE ANALYSIS  #
########################

#Exploratory plots and statistics of demographic data

boxplot(merged_data$Age,main="Age Boxplot")
median(merged_data$Age)

ggplot(merged_data,aes(x=Gender))+geom_bar(fill="blue")+ggtitle("Gender Barplot")

ggplot(merged_data,aes(x=Marital_Status))+geom_bar(fill="blue")+ggtitle("Marital Status Barplot")

boxplot(merged_data$Dependents,main="Boxplot on No. of Dependents")
median(merged_data$Dependents,na.rm = T)

boxplot(merged_data$Income,main="Income Boxplot")
median(merged_data$Income)

ggplot(merged_data,aes(x=Education))+geom_bar(fill="blue")+ggtitle("Education Barplot")

ggplot(merged_data,aes(x=Profession))+geom_bar(fill="blue")+ggtitle("Profession Barplot")

ggplot(merged_data,aes(x=Residence))+geom_bar(fill="blue")+ggtitle("Type of Residence Barplot")

boxplot(merged_data$Months_in_residence,main="Boxplot on No. of months in current residence")
median(merged_data$Months_in_residence)

boxplot(merged_data$Months_in_company,main="Boxplot on No. of months in current company")
median(merged_data$Months_in_company)

#Exploratory plots and statistics of credit bureau data

boxplot(merged_data$`90DPD-6M`,main="Boxplot of 90DPD in 6 Months")
median(merged_data$`90DPD-6M`,na.rm = T)

boxplot(merged_data$`60DPD-6M`,main="Boxplot of 60DPD in 6 Months")
median(merged_data$`60DPD-6M`,na.rm = T)

boxplot(merged_data$`30DPD-6M`,main="Boxplot of 30DPD in 6 Months")
median(merged_data$`30DPD-6M`,na.rm = T)

boxplot(merged_data$`90DPD-12M`,main="Boxplot of 90DPD in 12 Months")
median(merged_data$`90DPD-12M`,na.rm = T)

boxplot(merged_data$`60DPD-12M`,main="Boxplot of 60DPD in 12 Months")
median(merged_data$`60DPD-12M`,na.rm = T)

boxplot(merged_data$`30DPD-12M`,main="Boxplot of 30DPD in 12 Months")
median(merged_data$`30DPD-12M`,na.rm = T)

boxplot(merged_data$AVGAS.CC,main=c("Boxplot of Avgas CC Utilization in last 12 months"))
median(merged_data$AVGAS.CC,na.rm = T)

boxplot(merged_data$Open_Trades_6M,main="Boxplot of Open Trades in 6 Months")
median(merged_data$Open_Trades_6M,na.rm = T)

boxplot(merged_data$Open_Trades_12M,main="Boxplot of Open Trades in 12 Months")
median(merged_data$Open_Trades_12M,na.rm = T)

boxplot(merged_data$Open_PL_Trades_6M,main="Boxplot of Open PL Trades in 6 Months")
median(merged_data$Open_PL_Trades_6M,na.rm = T)

boxplot(merged_data$Open_PL_Trades_12M,main="Boxplot of Open Trades in 12 Months")
median(merged_data$Open_PL_Trades_12M,na.rm = T)

boxplot(merged_data$Inquiries_6M,main="Boxplot of No. of Inquiries in 6 Months")
median(merged_data$Inquiries_6M,na.rm = T)

boxplot(merged_data$Inquiries_12M,main="Boxplot of No. of Inquiries in 12 Months")
median(merged_data$Inquiries_12M,na.rm = T)

ggplot(merged_data,aes(x=factor(Open_Home_Loan)))+geom_bar(stat="count",fill="brown")+ggtitle("Open Home Loan")

boxplot(merged_data$Outstanding_Balance,main="Boxplot of Outstanding Balance")
median(merged_data$Outstanding_Balance,na.rm = T)

boxplot(merged_data$Total_Trades,main="Total No. of Trades")
median(merged_data$Total_Trades,na.rm = T)

ggplot(merged_data,aes(x=factor(Open_Auto_Loan)))+geom_bar(stat="count",fill="brown")+ggtitle("Open Auto Loan")

#######################
# BIVARIATE ANALYSIS  #
#######################

#Demographic data analysis

ggplot(merged_data,aes(y=Age,x=factor(Performance_Tag)))+geom_boxplot()+
  ggtitle("Age")
#There is no difference in median values. 

ggplot(merged_data,aes(x=Gender,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Gender")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
#No significant pattern found in defaulter rates for gender values

ggplot(merged_data,aes(x=Marital_Status,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Marital Status at the time of Application")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))
#No significant pattern found in defaulter rates for differnt marital status

ggplot(merged_data,aes(x=Dependents,fill=factor(Performance_Tag)))+geom_bar()+
  ggtitle("No. of Dependents")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
#No significant pattern found in defaulter rates for differnt No.of.dependents values.

ggplot(merged_data,aes(y=Income,x=factor(Performance_Tag)))+geom_boxplot()+
  ggtitle("Income")
#The median values for income of defaulters are lower than that of non defaulters

ggplot(merged_data,aes(x=Education,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Education")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
#No significant pattern found in defaulter rates for differnt Education except for 'others' type

ggplot(merged_data,aes(x=Profession,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Profession")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
#No significant pattern found in defaulter rates for differnt professions

ggplot(merged_data,aes(x=Residence,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Residence")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
#No significant pattern found in defaulter rates for differnt residence type except for 'others' type

ggplot(merged_data,aes(y=Months_in_residence,x=factor(Performance_Tag)))+geom_boxplot()+
  ggtitle("Months in current residence")
#The median No.of.months.in.current.residence of non defaulters are lower than that of defaulters

ggplot(merged_data,aes(y=Months_in_company,x=factor(Performance_Tag)))+geom_boxplot()+
  ggtitle("Months in current company")
#The median No.of.months.in.current.company of non defaulters are slightly lower than that of defaulters


#Credit Bureau Data

ggplot(merged_data,aes(x=`90DPD-6M`,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("90DPD in 6 Months")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# No of defaulters are increasing with increase in no.of.times 90 DPD in last 6 months

ggplot(merged_data,aes(x=`60DPD-6M`,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("60DPD in 6 Months")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# No of defaulters are increasing for upto 3  times 60 DPD in last 6 months

ggplot(merged_data,aes(x=`30DPD-6M`,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("30DPD in 6 Months")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# No of defaulters are increasing for upto 5 times.30 D.P.D in last 6 months

ggplot(merged_data,aes(x=`90DPD-12M`,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("90DPD in 12 Months")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# No of defaulters are increasing with increase in no.of.times.90.DPD.or worse in last 12 months

ggplot(merged_data,aes(x=`60DPD-12M`,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("60DPD in 12 Months")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# No of defaulters are increasing till 4 times.60.DPD or worse in last 12 months

ggplot(merged_data,aes(x=`30DPD-12M`,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("30DPD in 12 Months")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# # No of defaulters are increasing till 7.5 times.30.DPD in last 12 months

#Binning of values for Modified CC ultilization
merged_data$Binned_CC_Utilization <- as.factor(cut(merged_data$AVGAS.CC, breaks = c(0,10, 20, 30, 40, 50, 60,70,80,90,100,110,120),include.lowest = TRUE))
ggplot(merged_data,aes(x=Binned_CC_Utilization,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("AVGAS.CC Utilization in 12 Months")
#There is no significant pattern found

ggplot(merged_data,aes(x=Open_Trades_6M,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Open Trades in 6 Months")
# Trades which are open 4 times in last 6 months tends to default more

ggplot(merged_data,aes(x=Open_Trades_12M,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Open Trades in 12 Months")
# No appropriate pattern found in no of defaulters with increase in No.of.trades.opened.in.last.12.months

ggplot(merged_data,aes(x=Open_PL_Trades_6M,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Open PL Trades in 6 Months")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# No of defaulters increases till 4th month then decreases with increase in no of PL trades opened in last 6 months 

ggplot(merged_data,aes(x=Open_PL_Trades_12M,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Open PL Trades in 12 Months")
# No of defaulters increases till 6th month then decreases and suddenly increases in 12th month with increase in no of PL trades opened in last 12 months

ggplot(merged_data,aes(x=Inquiries_6M,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Inquiries in 6 Months")
# No appropriate pattern found in no of defaulters 

ggplot(merged_data,aes(x=Inquiries_12M,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Inquiries in 12 Months")
# No appropriate pattern found in no of defaulters 

# Binning outstanding balance
merged_data$Binning.outstanding.Balance <- as.factor(cut(merged_data$Outstanding_Balance, breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000 ,6000000),include.lowest = TRUE))
ggplot(merged_data,aes(x=Binning.outstanding.Balance,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Outstanding Balance")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))

ggplot(merged_data,aes(x=Total_Trades,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Total No. of Trades")
# People who opened 39 No of trades tend to defaut more  

ggplot(merged_data,aes(x=Open_Auto_Loan,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Open Auto Loan")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# No appropriate pattern found in no of defaulters 

ggplot(merged_data,aes(x=Open_Home_Loan,fill=factor(Performance_Tag)))+geom_bar(position="fill")+
  ggtitle("Open Home Loan")+geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.5))
# No appropriate pattern found in no of defaulters 



########################
# WOE AND IV ANALYSIS  #
########################

#Information package registers Performance differently. 
#We use Reverse_Performance_Tag for our purposes. Essentially the complement of Performance_Tag
merged_data$Reverse_Performance_Tag<-ifelse(merged_data$Performance_Tag == 0,1,0)
#Creating bins
merged_data_bins<-merged_data[,-c(1,29,30,31)]
#Obtaining woe_data
woe_data<-woe.binning(merged_data_bins,'Reverse_Performance_Tag',merged_data_bins)
#obtaining woe tables
woe_table<-woe.binning.table(woe_data)
#Merged data+respective bins+woe
merged_data_woe<-woe.binning.deploy(merged_data_bins,woe_data,add.woe.or.dum.var = "woe")
#bins+woe values
merged_data_woeandbin<-merged_data_woe[,-c(1:27)]

#Final WOE data of our merged dataset
merged_data_woe<-merged_data_woeandbin[,-2*c(1:27)]

#assigning appropriate coloumn names
colnames(merged_data_woe)<-c("Reverse_Performance_Tag","Open_PL_Trades_12M","Inquiries_12M",
                             "Open_Trades_12M","AVGAS.CC","30DPD-6M","30DPD-12M",
                             "Open_PL_Trades_6M","90DPD-12M","60DPD-6M","Total_Trades",
                             "60DPD-12M","Inquiries_6M","Open_Trades_6M","90DPD-6M",
                             "Months_in_Residence","Income","Months_in_company",
                             "Open_Home_Loan","Outstanding_Balance","Age","Dependents",
                             "Profession","Open_Auto_Loan","Residence","Education",
                            "Gender","Marital_Status")

#IV data of credit data alone
IV_credit<-create_infotables(data=merged_data[,colnames(merged_data)%in%c(colnames(credit)[-c(1,19)]
                                                                          ,"Reverse_Performance_Tag")],
                             y="Reverse_Performance_Tag", parallel = TRUE)
#Summary of IV credit data
IV_credit$Summary

#IV data of demographic data alone
IV_dem<-create_infotables(data=merged_data[,colnames(merged_data)%in%c(colnames(dem)[-c(1,12)]
                                                                          ,"Reverse_Performance_Tag")],
                             y="Reverse_Performance_Tag", parallel = TRUE)
#Summary of IV dem data
IV_dem$Summary

#IV data of whole merged dataset
IV_whole<-create_infotables(data=merged_data[,-c(29,30,31)], y="Reverse_Performance_Tag", parallel = TRUE)
#summary of IV whole data
IV_whole$Summary


#####################
# CORRELATION PLOT  #
#####################

#creating copy of merged data independent variables
corr_data <- merged_data_woe[,-1]

#correlation matrix of merged data ind. variables
data_correlation <- cor(corr_data)

#correlation plot of the independent variables considered
corrplot(data_correlation, type = "full",tl.pos = "dt",
         method = "circle", tl.cex = .75, tl.col = 'black',
         order = "hclust", diag = FALSE)


######################
# OUTLIER TREATMENT  #
######################


############################
# MODELLING WITH WOE DATA  #
############################

col_IV<- IV_whole$Summary$Variable[IV_whole$Summary$IV>0.2]
col_IV<-append(col_IV,"Reverse_Performance_Tag")
merged_data_promiv <- merged_data_woe[,colnames(merged_data_woe)%in%col_IV]
merged_data_promiv<-merged_data_promiv[,col_IV]

p<-prcomp(merged_data_promiv[,-12])
summary(p)

merged_data_promiv<-merged_data_promiv[,-c(10,11)]
merged_data_promiv$Reverse_Performance_Tag<-dem$Performance_Tag
colnames(merged_data_promiv)[10]<-"Performance_Tag"
merged_data_promiv$Performance_Tag<-as.factor(merged_data$Performance_Tag)
# Split the data into training and test set
set.seed(0)
training.samples <- merged_data_promiv$Performance_Tag %>%
  createDataPartition(p = 0.6, list = FALSE)
train.data  <- merged_data_promiv[training.samples, ]
test.data <- merged_data_promiv[-training.samples, ]

#######
##LDA##
#######

#######################
##Logistic Regression##
#######################

#######
# KNN #
#######


############################
# MODELLING WITH RAW DATA  #
############################

#demcopy-demographic raw data
#demcredit - dem+credit raw data

#replacing coloumns containing NA values with the woe values
#we only perform this for those coloumns with NA values
#the rest remain as is

dem$Gender<-merged_data_woe$Gender
dem$Marital_Status<-merged_data_woe$Marital_Status
dem$Dependents<-merged_data_woe$Dependents
dem$Education<-merged_data_woe$Education
dem$Profession<-merged_data_woe$Profession
dem$Residence<-merged_data_woe$Residence

credit$AVGAS.CC<-merged_data_woe$AVGAS.CC
credit$Open_Trades_6M<-merged_data_woe$Open_Trades_6M
credit$Open_Home_Loan<-merged_data_woe$Open_Home_Loan
credit$Outstanding_Balance<-merged_data_woe$Outstanding_Balance

#creating demcredit data
demcredit <- merge(dem[,-12], credit, by ="ID",all=F)

#removing ID data
demcopy<-dem[,-1]
demcredit<-demcredit[,-1]

#accessing IV values of demcopy and demcredit
IV_demcopy <- create_infotables(data=demcopy, y="Performance_Tag", parallel = FALSE)
IV_demcredit <-create_infotables(data=demcredit, y="Performance_Tag", parallel = FALSE)

IV_demcopy$Summary
IV_demcredit$Summary

#scaling the entries of the two datasets for PCA and modelling purposes
demcopy[,-11] <- as.data.frame(scale(demcopy[,-11]))
demcredit[,-28] <- as.data.frame(scale(demcredit[,-28]))

#arranging coloumns in terms of decreasing IV and performing PCA
demcopy<-demcopy[,c(IV_demcopy$Summary$Variable,'Performance_Tag')]
pca_demcopy <- prcomp(demcopy[,-11])
summary(pca_demcopy)

demcredit <- demcredit[,c(IV_demcredit$Summary$Variable,'Performance_Tag')]
pca_demcredit <- prcomp(demcredit[,-28])
summary(pca_demcredit)

#Creating train and test datasets of demcopy and demcredit
set.seed(0)
training.samples <- demcopy$Performance_Tag %>%createDataPartition(p = 0.75, list = FALSE)
demcopy_train  <- demcopy[training.samples, ]
demcopy_test<- demcopy[-training.samples, ]

demcredit_train  <- demcredit[training.samples, ]
demcredit_test<- demcredit[-training.samples, ]

#Creating factor copy of Performance_Tag for cross validation input
demcopy_train$Performance_Tag1 <-as.factor(demcopy_train$Performance_Tag)
demcredit_train$Performance_Tag1 <-as.factor(demcredit_train$Performance_Tag)

# Define training control
control1 <- trainControl(method="cv", number=7)

#accuracy dataframes
train_acc<-as.data.frame(matrix(0,nrow = 2,ncol = 3),row.names = c("Dem","Dem+Credit"))
colnames(train_acc)<-c("Log","LDA","KNN")
test_acc<-as.data.frame(matrix(0,nrow = 2,ncol = 3),row.names = c("Dem","Dem+Credit"))
colnames(test_acc)<-c("Log","LDA","KNN")

#######
# LDA #
#######

#Demcopy data

# Fit the model
lda_raw_dem <- lda(Performance_Tag1~Months_in_residence+Income+Months_in_company+Education,data=demcopy_train)
summary(lda_raw_dem)
lda_raw_dem

unregister()
#k-fold for linear Discriminant Analysis
set.seed(9)
#Train the model
ldamodel1 <- train(Performance_Tag1~Months_in_residence+Income+Months_in_company+Education,data=demcopy_train, method="lda", trControl=control1)
#Summarize
print(ldamodel1)

# Make predictions
predictions <- ldamodel1 %>% predict(demcopy_test)
# Model accuracy
test_acc$LDA[1]<-mean(predictions==demcopy_test$Performance_Tag)
train_acc$LDA[1]<-ldamodel1$results$Accuracy

#Whole data

# Fit the model
lda_raw_demcredit <- lda(Performance_Tag~Open_Trades_12M+Open_PL_Trades_12M+Inquiries_12M+`30DPD-6M`+Open_PL_Trades_6M+
                       `90DPD-12M`+`60DPD-6M`+Inquiries_6M,data=demcredit_train)
summary(lda_raw_demcredit)
lda_raw_demcredit

unregister()
#k-fold for linear Discriminant Analysis
set.seed(9)
#Train the model
ldamodel2 <- train(Performance_Tag1~Open_Trades_12M+Open_PL_Trades_12M+Inquiries_12M+`30DPD-6M`+Open_PL_Trades_6M+
                     `90DPD-12M`+`60DPD-6M`+Inquiries_6M,data=demcredit_train, method="lda", trControl=control1)
#Summarize
print(ldamodel2)

# Make predictions
predictions <- ldamodel2 %>% predict(demcredit_test)

test_acc$LDA[2]<-mean(predictions==demcredit_test$Performance_Tag)
train_acc$LDA[2]<-ldamodel2$results$Accuracy


############
# Logistic #
############

#Demo model
log_raw_dem<-glm(Performance_Tag~Months_in_residence+Income+Months_in_company+Education,data=demcopy_train,family = binomial)
summary(log_raw_dem)


unregister()
#k-fold for Logistic Regression
set.seed(9)
#Train the model
log_model1 <- train(Performance_Tag1~Months_in_residence+Income+Months_in_company+Education,data=demcopy_train, method='glm',family='binomial', trControl=control1)
#Summarize
print(log_model1)

predictions<-predict(log_model1,demcopy_test)

test_acc$Log[1]<-mean(predictions==demcopy_test$Performance_Tag)
train_acc$Log[1]<-log_model1$results$Accuracy


#Whole model

log_raw_demcredit<-glm(Performance_Tag~Open_Trades_12M+Open_PL_Trades_12M+Inquiries_12M+`30DPD-6M`+Open_PL_Trades_6M+
                         `90DPD-12M`,data=demcredit_train,family = binomial)
summary(log_raw_demcredit)

unregister()
#k-fold for Logistic Regression
set.seed(9)
#Train the model
log_model2 <- train(Performance_Tag1~Open_Trades_12M+Open_PL_Trades_12M+Inquiries_12M+`30DPD-6M`+Open_PL_Trades_6M+
                          `90DPD-12M`,data=demcredit_train, method='glm',family='binomial', trControl=control1)
#Summarize
print(log_model2)

predictions<-predict(log_model2,demcredit_test)

test_acc$Log[2]<-mean(predictions==demcredit_test$Performance_Tag)
train_acc$Log[2]<-log_model2$results$Accuracy

#######
# KNN #
#######

#Demo data

unregister()
#k-fold for KNN
set.seed(9)
#Train the model
knnmodel1 <- train(x=demcopy_train[,-c(11,12)],y=demcopy_train[,12], method="knn", trControl=control1)
#Summarize
print(knnmodel1)

# Fit the model. Choice of k based on cv, k=9 optimal based on accuracy
knn_model_raw1 <- knn(demcopy_train[,-12],demcopy_test,demcopy_train$Performance_Tag1,k=9)
summary(knn_model_raw1)
knn_model_raw1

# Model accuracy
test_acc$KNN[1]<- mean(knn_model_raw1==demcopy_test$Performance_Tag)
train_acc$KNN[1]<-knnmodel1$results$Accuracy[3]


#Whole data

unregister()
#k-fold for KNN
set.seed(9)
#Train the model
knnmodel2 <- train(x=demcredit_train[,-c(28,29)],y=demcredit_train[,29], method="knn", trControl=control1)
#Summarize
print(knnmodel2)

# Fit the optimal model. k=9 based on accuracy
knn_model_raw2 <- knn(demcredit_train[,-11],demcredit_test,demcredit_train$Performance_Tag1,k=9)
summary(knn_model_raw2)

# Model accuracy
test_acc$KNN[2]<- mean(knn_model_raw2==demcredit_test$Performance_Tag)
train_acc$KNN[2]<-knnmodel2$results$Accuracy[3]

#Printing accuracy measures
print(round(train_acc,5))
print(round(test_acc,5))

#Based on test accuracy scores, logistic model seems ideal

##############
WOE MODELLING
##############
#Build the Linear Dicriminant Analysis

 


# Split the data into training and test set
training.samples <- woe_data$Performance.Tag %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- woe_data[training.samples, ]
test.data <- woe_data[-training.samples, ]

 


# Fit the model
model <- lda(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe+ No.of.trades.opened.in.last.12.months_woe, data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class==test.data$Performance.Tag)

 

#Plot LDA
plot(model)

 

lda.data <- cbind(train.data, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Type))

 

 

#k-fold for linear Discriminant Analysis (predictor=1)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
#Train the model
ldamodel1 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe , data=woe_data, method="lda", trControl=control1)
#Summarize
print(ldamodel1)

 

#k-fold for linear Discriminant Analysis (predictor=2)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
#Train the model
ldamodel2 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe, data=woe_data, method="lda", trControl=control1)
#Summarize
print(ldamodel2)

 

#k-fold for linear Discriminant Analysis (predictor=3)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
#Train the model
ldamodel3 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe+ No.of.trades.opened.in.last.12.months_woe , data=woe_data, method="lda", trControl=control1)
#Summarize
print(ldamodel3)

 

#k-fold for linear Discriminant Analysis(predictor=4)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
#Train the model
ldamodel4 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe+ No.of.trades.opened.in.last.12.months_woe +No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, data=woe_data, method="lda", trControl=control1)
#Summarize
print(ldamodel4)

 


#k-fold for linear Discriminant Analysis (predictor=5)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
woe_data$Performance.Tag <- as.character(woe_data$Performance.Tag)
#Train the model
ldamodel5 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe+ No.of.trades.opened.in.last.12.months_woe +No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe, data=woe_data, method="lda", trControl=control1)
#Summarize
print(ldamodel5)


#k-fold for logistic Regression Analysis (predictor=1)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
#Train the model
logmodel1 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe , data=woe_data, method="glm", trControl=control1)
#Summarize
print(logmodel1)

 

#k-fold for logistic Regression Analysis (predictor=2)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
#Train the model
logmodel2 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe, data=woe_data, method="glm", trControl=control1)
#Summarize
print(logmodel2)

 

#k-fold for logistic Regression Analysis (predictor=3)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
#Train the model
logmodel3 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe+ No.of.trades.opened.in.last.12.months_woe , data=woe_data, method="glm", trControl=control1)
#Summarize
print(logmodel3)

 

#k-fold for logistic Regression Analysis(predictor=4)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
#Train the model
logmodel4 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe+ No.of.trades.opened.in.last.12.months_woe +No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe, data=woe_data, method="glm", trControl=control1)
#Summarize
print(logmodel4)

 


#k-fold for Logistic Regression Analysis (predictor=5)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
woe_data$Performance.Tag <- as.character(woe_data$Performance.Tag)
#Train the model
logmodel5 <- train(Performance.Tag~ Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe+ No.of.trades.opened.in.last.12.months_woe +No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe, data=woe_data, method="glm", trControl=control1)
#Summarize
print(logmodel5)
 