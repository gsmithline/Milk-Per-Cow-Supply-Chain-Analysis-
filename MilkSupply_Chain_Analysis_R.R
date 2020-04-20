library(dplyr)
library(stringr)
library(BHH2)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(beeswarm)

my_prac <- read.csv("Rpractice.csv",na.strings = c("", "NA"),stringsAsFactors=FALSE)
#This adds the column names to the data

#Q1 You are trying to forecast production levels of 1976 which is missing. How many months of data are available to help you forcast?
month_count<- count(my_prac,  vars = 'Month')  #gives month count, 168 months of data are available
#Q2 Given the historcal data how is it trending? How is milk production per cow trending?
#Get Scatter plot and line of best fit to tell trend
regres_years<- scatter.smooth(x=my_prac$Year , y=my_prac$ProdLbsPerCow, main="Milk Production Per Cow 1962 to 1975", xlab = "Years 1962 - 1975", ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow ~ my_prac$Year)) 
regres_months62<- scatter.smooth(x=my_prac$Month[1:12] , y=my_prac$ProdLbsPerCow[1:12], main="Milk Production Per Cow 1962", xlab = "Months", 
                                 ylab = "Milk Production per cow 1962")  + abline(lm(my_prac$ProdLbsPerCow[1:12] ~ my_prac$Month[1:12] ))
regres_month63<- scatter.smooth(x=my_prac$Month[13:24] , y=my_prac$ProdLbsPerCow[13:24], main="Milk Production Per Cow 1963", xlab = "Months", 
                                                  ylab = "Milk Production per cow")  + abline(lm(my_prac$ProdLbsPerCow[13:24] ~ my_prac$Month[13:24] ))
regres_month64<- scatter.smooth(x=my_prac$Month[25:36] , y=my_prac$ProdLbsPerCow[25:36], main="Milk Production Per Cow 1964", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[25:36] ~ my_prac$Month[25:36] ))
regres_month65<-  scatter.smooth(x=my_prac$Month[37:48] , y=my_prac$ProdLbsPerCow[37:48], main="Milk Production Per Cow 1965", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[37:48] ~ my_prac$Month[37:48] ))
regres_month66<- scatter.smooth(x=my_prac$Month[49:60] , y=my_prac$ProdLbsPerCow[49:60], main="Milk Production Per Cow 1966", xlab = "Months", 
                                                    ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[49:60] ~ my_prac$Month[49:60] ))
regres_month67<- scatter.smooth(x=my_prac$Month[61:72] , y=my_prac$ProdLbsPerCow[61:72], main="Milk Production Per Cow 1967", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[61:72] ~ my_prac$Month[61:72] ))
regres_month68<- scatter.smooth(x=my_prac$Month[73:84] , y=my_prac$ProdLbsPerCow[73:84], main="Milk Production Per Cow 1968", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[73:84] ~ my_prac$Month[73:84] ))
regres_month69<- scatter.smooth(x=my_prac$Month[85:96] , y=my_prac$ProdLbsPerCow[85:96], main="Milk Production Per Cow 1969", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[85:96] ~ my_prac$Month[85:96] ))
regres_month70<- scatter.smooth(x=my_prac$Month[97:108] , y=my_prac$ProdLbsPerCow[97:108], main="Milk Production Per Cow 1970", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[97:108] ~ my_prac$Month[97:108] ))
regres_month71<- scatter.smooth(x=my_prac$Month[109:120] , y=my_prac$ProdLbsPerCow[109:120], main="Milk Production Per Cow 1971", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[109:120] ~ my_prac$Month[109:120] ))
regres_month72<- scatter.smooth(x=my_prac$Month[121:132] , y=my_prac$ProdLbsPerCow[121:132], main="Milk Production Per Cow 1972", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[121:132] ~ my_prac$Month[121:132] ))
regres_month73<- scatter.smooth(x=my_prac$Month[133:144] , y=my_prac$ProdLbsPerCow[133:144], main="Milk Production Per Cow 1973", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[133:144] ~ my_prac$Month[133:144] ))
regres_month74<- scatter.smooth(x=my_prac$Month[145:156] , y=my_prac$ProdLbsPerCow[145:156], main="Milk Production Per Cow 1974", xlab = "Months", 
                                                ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[145:156] ~ my_prac$Month[145:156] ))
regres_month75<- scatter.smooth(x=my_prac$Month[157:168] , y=my_prac$ProdLbsPerCow[157:168], main="Milk Production Per Cow 1975", xlab = "Months", 
                                                  ylab = "Milk Production per cow") + abline(lm(my_prac$ProdLbsPerCow[157:168] ~ my_prac$Month[157:168] ))
#Q3Get variance in in each year for Milk Production Per Cow  
var62<- var(my_prac$ProdLbsPerCow[1:12])
var63<- var(my_prac$ProdLbsPerCow[13:24])
var64<- var(my_prac$ProdLbsPerCow[25:36])
var65<- var(my_prac$ProdLbsPerCow[37:48])
var66<- var(my_prac$ProdLbsPerCow[49:60])
var67<- var(my_prac$ProdLbsPerCow[61:72])
var68<- var(my_prac$ProdLbsPerCow[73:84])
var69<- var(my_prac$ProdLbsPerCow[85:96])
var70<- var(my_prac$ProdLbsPerCow[97:108])
var71<- var(my_prac$ProdLbsPerCow[109:120])
var72<- var(my_prac$ProdLbsPerCow[121:132])
var73<- var(my_prac$ProdLbsPerCow[133:144])
var74<- var(my_prac$ProdLbsPerCow[145:156])
var75<- var(my_prac$ProdLbsPerCow[157:168])
#Graphing variance in Bar Graph for Each year
totalvar<- data.frame(var62, var63, var64, var65, var66, var67, var68, var69, var70, var71, var72, var73, var74, var75)
totalvar2<- data.frame(melt(totalvar))
names_graph<- data.frame(1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975)
names_graph2<- melt(names_graph)
totalgraph<- cbind.data.frame(totalvar2, names_graph2)
colnames(totalgraph) <- c("Var ID", "Variance #","Year ID","Year")
barpltvar<- barplot(totalgraph$`Variance #`,  names.arg = totalgraph$Year, xlab = "Years", ylab = "Variance", main = "Variance Milk Production Per Cow 1962-1975")
