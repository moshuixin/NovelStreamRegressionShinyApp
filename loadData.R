##########################################################################################
# Novel stream algorithm
# Xinxin Yang
#
#  
# This program is used to read data and preprocess data.
# 


setwd("/Users/xinxin/Documents/Merseburg/Masterarbeit/Novel-Stream-Regression-shiny-app-")
getwd()
# library
library(DT)
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(zoo)
library(Matrix)
library(xtable)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(htmlTable)
library(car)
library(stargazer)
library(gridExtra)
library(olsrr)
library(readr)
library(tidyverse)
library(caret)
library(tidyr)


#-----------------------------------------------------------------------------------------
#TASK ONE: loading the data
#-----------------------------------------------------------------------------------------
# load dataset 1: money data ----
geld2 <- read_excel("data/geld2.xlsx", col_types = c("text", "numeric", "numeric", "numeric"), skip = 11)
# Y Bruttosozialprodukt, M1 MassfuerGeldmenge, P Preisbereinigungsindex

# load dataset 2: oekk data -----
oekk <- read_excel("data/oekkennzd.xlsx", col_types = c("text", "numeric", "numeric", "numeric", "numeric"), skip = 12)

# generate the simulated data sets ----
simulation_data <- function(n,beta0,beta1,beta2)
{
  # simulation regression data
  set.seed(23)
  # beta0 = 1
  # beta1 = 2
  # beta2= -2
  x1 = runif(n, min = 0, max = 10000)
  x2 = runif(n, min = 0, max = 999)
  error =  rnorm(n) 
  y = beta0 + beta1*x1+ beta2*x2 + error
  
  simlationDT = data.frame(y,x1,x2)
  return(simlationDT)
}

# data set 1: data size =1000
simulationDT_1 = rbind(simulation_data(400,1,2,-2), simulation_data(600,4,2,-3))
# data set 2: data size = 10000
temp_2.1 = rbind(simulationDT_1, simulation_data(2000,3,5,-2))
temp_2.2 = rbind(simulation_data(3000,-2,3,-10), simulation_data(4000,3,5,-2))
simulationDT_2 = rbind(temp_2.1, temp_2.2)
# data set 3: data size = 1000000
temp_3.1 = rbind(simulationDT_2, simulation_data(20000,7,1,-2))
temp_3.2 = rbind(simulation_data(20000,4,-0.5,-2), simulation_data(20000,23,5,-2))
temp_3.3 = rbind(simulation_data(10000,3,-0.6,-6), simulation_data(20000,0.11,3,5))
simulationDT_3 = rbind(temp_3.1,temp_3.2)
simulationDT_3 = rbind(simulationDT_3,temp_3.3)

# load the data set 4: Earning and Education ----
dt  <- read_table2("data/TableF4-1.txt", skip = 27)
#choose the data,which were participants in the formal labor market
dt <- dt[dt["WHRS"] != 0,]
#select the features
incomeDT =  dt[2:7]
# add variable: wife's income, in 1975 dollars
WINC = dt$WHRS*dt$WW
incomeDT["WINC"] = WINC
#add variable: Children = how many children unten 18 years old
Children = dt$KL6+dt$K618
incomeDT["Children"] = Children
income =  incomeDT %>% select(4,5,7,8)
#add new variable quarad Wife's age
income["WA2"] = (income$WA)^2
#combine the data as data frame
income = data.frame(income[,c(3,1,2,4,5)])

#-----------------------------------------------------------------------------------------
#TASK two: Formating the data
#-----------------------------------------------------------------------------------------

# regression with seasonaility (just for dataset 1)
# convert data in Quarter 
geld <- str_split_fixed(geld2$Quartal,fixed("-"),2)
colnames(geld) <- c("Year", "Quarter")
Q_Geld <- cbind(geld, geld2[,2:4])

# convert the data in data frame
oekk = data.frame(oekk)

# split the data into quater 
oekk_test = str_split_fixed(oekk$quartal,fixed("-"),2)
colnames(oekk_test) = c("Quarter","Year")
oekk_test <- cbind(oekk_test, oekk[,2:5])
oekk_test$Quartal <- paste(oekk_test$Year, oekk_test$Quarter)

# Normalization the money
Q_Geld = Q_Geld %>% mutate_at(c("Y", "M1","P"), ~(scale(.) %>% as.vector))

# Normalization the oekk.
oekkennzd = oekk %>% mutate_at(c("SP", "CPI","ULC"), ~(scale(.) %>% as.vector))

#normalization the earnings 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
earning = normalize(income)

#-----------------------------------------------------------------------------------------
#TASK three: preprocessing the data for analyzing
#-----------------------------------------------------------------------------------------

# set the money data----
x0 = rep(1, nrow(Q_Geld))
X = as.matrix (cbind(x0, Q_Geld[,4:5]))
y= as.matrix(Q_Geld[,3])

# set the oekk----
DS2_x0 = rep(1, nrow(oekkennzd))
DS2_X =as.matrix(cbind(DS2_x0,oekkennzd[,3:5]))
DS2_y= as.matrix(oekkennzd[,2])

# set the simulated data with data size = 1000 ----
DS3_x0 = rep(1, nrow(simulationDT_1))
DS3_X =as.matrix(cbind(DS3_x0,simulationDT_1[,2:3]))
DS3_y= as.matrix(simulationDT_1[,1])

# set simualted data with data size = 10000----
sim2_DS3_x0 = rep(1, nrow(simulationDT_2))
sim2_DS3_X =as.matrix(cbind(sim2_DS3_x0,simulationDT_2[,2:3]))
sim2_DS3_y= as.matrix(simulationDT_2[,1])

# set simulated data with data size = 100000 ----
sim3_DS3_x0 = rep(1, nrow(simulationDT_3))
sim3_DS3_X =as.matrix(cbind(sim3_DS3_x0,simulationDT_3[,2:3]))
sim3_DS3_y= as.matrix(simulationDT_3[,1])

# set the earning data ----
earning_x0 = rep(1, nrow(earning))
earning_x = as.matrix(cbind(earning_x0, earning %>% select(2,3,4,5)))
print(colnames(earning_x))
earning_y = as.matrix(simulationDT_3[,1])

#-----------------------------------------------------------------------------------
# Task four: building nultiple regression models
#-----------------------------------------------------------------------------------

# Init mutilple model----
# for the money data
money_model  = Y~ M1 + P
# for the oekk data
oekk_model = IR ~ SP+ CPI + ULC
# for the simulated data
simu_model = y ~ x1 + x2
#for the earning data
earning_model = WINC ~  WA + WE  + Children + WA2

# multiple regression model
model1 = lm (money_model ,data = Q_Geld)
model2 = lm (oekk_model ,oekkennzd)
model3 = lm (simu_model ,simulationDT_1)
model4 = lm(earning_model, earning)

# calcauting the mse value
print(sprintf("MSE=%0.3f",  mean(model1$residuals^2)))
print(sprintf("MSE=%0.3f",  mean(model2$residuals^2)))
print(sprintf("MSE=%0.2f",  mean(model3$residuals^2)))
print(sprintf("MSE=%0.3f",  mean(model4$residuals^2)))

# output the latex table
#stargazer(model1, model2, model3, title="Regression Results", align=TRUE)

#-----------------------------------------------------------------------------------
# Task five: computing multicollinearity
#-----------------------------------------------------------------------------------
car::vif(model1)
car::vif(model2)
car::vif(model3)
car::vif(model4)
#------------------------------------------------------------------------------------
# Task six: computing correlation matrix
#------------------------------------------------------------------------------------
Q_dat = Q_Geld[3:5]
cor(Q_dat)
cor(oekkennzd[3:5])
cor(simulationDT_1)
cor(simulationDT_2)
cor(simulationDT_3)
cor(income)
