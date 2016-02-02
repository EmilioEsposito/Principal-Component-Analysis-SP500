#######################################################
#Homework 3
#Name:  Emilio Esposito
#andrew ID: eesposit
#email: emilio@cmu.edu
#######################################################

#Problem 1

#####################################
#code to download yahoo finance data#
####################################
# source('yahoo_finance_data_loading.R')
# ticker.list <- read.csv('SP500_ticker.csv',header=TRUE)[,1]
# first.day <- "2011-01-01"
# last.day <-"2014-12-31"
# 
# 
# 
# download.data <- NULL
# for (ticker in ticker.list){
#     print(ticker)
#     dataset <- NULL
#     try(dataset<- data.loading(ticker, first.day, last.day)$close)
#     download.data <-cbind(download.data, dataset)
# }
# 
# date <- row.names(download.data)
# download.data <- data.frame(date=date,download.data)
# write.table(download.data,'SP500_close_price_raw.csv',quote=FALSE,sep=",",row.names=FALSE)
# 
# #remove stocks with more than na.thresh missing values
# na.thresh <- 60
# stay.col <- colnames(download.data)[which(colSums(1*(is.na(download.data)))<na.thresh)]
# download.data2 <- download.data[,stay.col]
# write.csv(download.data2, 'SP500_close_price2.csv')

#reference for zoo package
#http://cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
library(zoo)
library(plyr)


import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)
}

##############################
#prepare data for PCA analysis
##############################
mydata <-import.csv('SP500_close_price.csv')
date <- as.Date(as.character(mydata[, 1]), format="%Y-%m-%d") #had to modify date input format
myzoo <- zoo(mydata[,c(-1,-2)], date ) #had to remove redundant date column
#myzoo <- na.omit(myzoo)
myzoo <- na.locf(myzoo) #impute missing values
prices2returns <- function(x) 100*diff(log(x)) #function to covert from price to return
log.return.zoo <- prices2returns(myzoo)
log.return.data <- coredata(log.return.zoo) #data
log.return.date <- time(log.return.zoo) #date

#PROBLEM 1 A 1
options(scipen = 999)
pca_returns <- prcomp(x = scale(as.data.frame(log.return.data)), retx = TRUE, center = TRUE, scale = FALSE)
screeplot(pca_returns, type = 'lines', npcs=10)

#PROBLEM 1 A 2
variance <- pca_returns$sdev^2 #variance aka eignenvalues
total_var <- sum(variance)
perc_var <- variance/total_var*100.0
accum_perc_var <- cumsum(perc_var)
plot(accum_perc_var, main="% Cumulative Variance by N number of Components to keep", xlab="# of Primary Components to keep", ylab="% Variance Retained")
max(variance)


#PROBLEM 1 A 3
k <- min(which(accum_perc_var>=80))
k
#summary(pca_returns) gives same answer

#PROBLEM 1 A 4
total_var
start <- 3
end <- length(variance)
recon_error_var <- sum(variance[start:end])
recon_error_var_perc <- recon_error_var/total_var*100
recon_error_var
recon_error_var_perc

#PROBLEM 1 B 1
x <- pca_returns$x
PC1 <- x[,"PC1"]
#PC1_return <- data.frame(date=date[-1], PC1=PC1)
PC1_return <- zoo(PC1,date[-1]) #make sure to align dats
plot(PC1_return,xlab = "Date", main="PC1 of S&P500 log return over time")
#PC1_return[which.min(PC1_return$PC1),]
PC1_return[which.min(PC1_return)]

#PROBLEM 1 B 2
rotation <- pca_returns$rotation
PC1PC2 <- rotation[, c("PC1","PC2")]

#PROBLEM 1 B 3
tickers <- import.csv("SP500_ticker.csv")

#clean the leading/trailing whitespaces for sector
tickers$sector <- as.character(tickers$sector)
tickers$sector <- gsub("^\\s+|\\s+$", "", tickers$sector)
tickers$sector <- factor(tickers$sector)
levels(tickers$sector)
#replace - with . in ticker
tickers$ticker <- gsub("-", ".", tickers$ticker, fixed = TRUE)

#load plyr
library(plyr)

#add ticker column to PC1PC2
PC1PC2 <- data.frame(ticker=row.names(PC1PC2), PC1PC2)

#join with ticker data
PC1PC2_sector <- join(PC1PC2, tickers, by="ticker")

PC1PC2_sector_agg <-ddply(.data = PC1PC2_sector, ~ sector, summarize, PC1_weight=mean(PC1), PC2_weight=mean(PC2))

#PC1 weights
library(ggplot2)
ggplot(data = PC1PC2_sector_agg, aes(x=sector, y=PC1_weight))+
  geom_bar(na.rm=TRUE, stat ="identity", position = "dodge")+
  ggtitle("PC1 weights by sector")+ coord_flip()+
  theme(text = element_text(size=16))


#PROBLEM 1 B 4
#PC2 weights
ggplot(data = PC1PC2_sector_agg, aes(x=sector, y=PC2_weight))+
  geom_bar(na.rm=TRUE, stat ="identity", position = "dodge")+
  ggtitle("PC2 weights by sector")+ coord_flip()+
  theme(text = element_text(size=16))


#PROBLEM 2 A


filter.features.by.cor <- function(df) {
  
  #get last column index to id the output col
  lc <- ncol(df)
  
  #get value correlations
  cors <- cor(df[,1:lc])
  
  #only keep cors with output column
  cors <- as.data.frame(cors[,lc])
  
  #remove correlatin of output vs output
  output_index <- which(rownames(cors)==names(df[lc]))
  cors <- cors[-output_index,,drop=FALSE]
 
  #reorder in descending abs correlation order
  cors <- as.data.frame(cors[order(abs(cors), decreasing = TRUE),,drop=FALSE])
  
  #add a column equal to rownames just for convenience 
  cors <- data.frame(var=rownames(cors), weight=cors[,1])
  
  return(cors)
}

bmi <- import.csv("bmi.csv")

#get bmi correlations
bmi_cors <- filter.features.by.cor(bmi)

#get top 3 cors
bmi_cors_top3 <- bmi_cors[1:min(nrow(bmi_cors),3),,drop=FALSE]
bmi_cors_top3


#PROBLEM 2 B

library(leaps)

#find subsets wth leaps package
subs <- regsubsets(fatpctg~., data=bmi, nvmax = 3, nbest=1, method  ='exhaustive')

#get summary
subs_summ <- summary(subs)

plot(subs, scale="r2", col=c("blue","red","green"))

#find best 3 subsets
best3 <- subs_summ$which
t(best3)

#PROBLEM 2 C
library(MASS)

#get full lm of bmi
bmi_lm <- lm(fatpctg ~ . ,data = bmi)

#perform backward stepwise regression
stepAIC(object = bmi_lm, direction = "backward")




