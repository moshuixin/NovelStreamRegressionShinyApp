setwd("/Users/xinxin/Documents/Merseburg/Masterarbeit/Novel-Stream-Regression-shiny-app-")
getwd()
# library
library(DT)
library(shiny)
library(readxl)
library(ggplot2)
library(stringr)
library(dplyr)
library(data.table)
library(stringr)
library(zoo)
library(Matrix)
library(xtable)
library(ggplot2)

# load package
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

#-----------------------------------------------------------------------------------------
#TASK ONE: load the data
#-----------------------------------------------------------------------------------------

# dataset1: money data
geld2 <- read_excel("data/geld2.xlsx", col_types = c("text", "numeric", "numeric", "numeric"), skip = 11)
# Y Bruttosozialprodukt, M1 MassfuerGeldmenge, P Preisbereinigungsindex

# dataset2: oek data
oekk <- read_excel("data/oekkennzd.xlsx", col_types = c("text", "numeric", "numeric", "numeric", "numeric"), skip = 12)

# dataset3: simulation

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

#dataset 4

# income dataset 
# Earning and Education
dt  <- read_table2("data/TableF4-1.txt", skip = 27)
#choosing the data,which were participants in the formal labor market
dt <- dt[dt["WHRS"] != 0,]
incomeDT =  dt[2:7]

# add variable: woman income,in 1975 dollars
WINC = dt$WHRS*dt$WW
incomeDT["WINC"] = WINC
#add variable: Children = how many children unten 18 years old
#incomeDT <- transform(incomeDT, Children=ifelse(KL6== 0  & K618== 0,0,1))
Children = dt$KL6+dt$K618
incomeDT["Children"] = Children
income =  incomeDT %>% select(4,5,7,8)

#add new variable 
income["WA2"] = (income$WA)^2

income = data.frame(income[,c(3,1,2,4,5)])

#-----------------------------------------------------------------------------------------
#TASK two: Format the data
#-----------------------------------------------------------------------------------------

# regression with seasonaility (just for dataset 1)
# convert data in Quarter 


geld <- str_split_fixed(geld2$Quartal,fixed("-"),2)
colnames(geld) <- c("Year", "Quarter")
Q_Geld <- cbind(geld, geld2[,2:4])


oekk = data.frame(oekk)

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
#TASK three: preprocessing the data for analysing
#-----------------------------------------------------------------------------------------

# set the money data
x0 = rep(1, nrow(Q_Geld))
X = as.matrix (cbind(x0, Q_Geld[,4:5]))
y= as.matrix(Q_Geld[,3])


#set the oekk
DS2_x0 = rep(1, nrow(oekkennzd))
DS2_X =as.matrix(cbind(DS2_x0,oekkennzd[,3:5]))
DS2_y= as.matrix(oekkennzd[,2])

# set the simulationsdata
DS3_x0 = rep(1, nrow(simulationDT_1))
DS3_X =as.matrix(cbind(DS3_x0,simulationDT_1[,2:3]))
DS3_y= as.matrix(simulationDT_1[,1])

# size 10000
sim2_DS3_x0 = rep(1, nrow(simulationDT_2))
sim2_DS3_X =as.matrix(cbind(sim2_DS3_x0,simulationDT_2[,2:3]))
sim2_DS3_y= as.matrix(simulationDT_2[,1])

#size 100000
sim3_DS3_x0 = rep(1, nrow(simulationDT_3))
sim3_DS3_X =as.matrix(cbind(sim3_DS3_x0,simulationDT_3[,2:3]))
sim3_DS3_y= as.matrix(simulationDT_3[,1])

#set the earning data


earning_x0 = rep(1, nrow(earning))
earning_x = as.matrix(cbind(earning_x0, earning %>% select(2,3,4,5)))
print(colnames(earning_x))
earning_y = as.matrix(simulationDT_3[,1])

#---------------------------------
# regression model
#---------------------------------

# Init mutilple model
money_model  = Y~ M1 + P
oekk_model = IR ~ SP+ CPI + ULC
simu_model = y ~ x1 + x2
earning_model = WINC ~  WA + WE  + Children + WA2

# multiple regression model
model1 = lm (money_model ,data = Q_Geld)
model2 = lm (oekk_model ,oekkennzd)
model3 = lm (simu_model ,simulationDT_1)
model4 = lm(earning_model, earning)

#mse
print(sprintf("MSE=%0.3f",  mean(model1$residuals^2)))
print(sprintf("MSE=%0.3f",  mean(model2$residuals^2)))
print(sprintf("MSE=%0.2f",  mean(model3$residuals^2)))
print(sprintf("MSE=%0.3f",  mean(model4$residuals^2)))

# output the latex table
#stargazer(model1, model2, model3, title="Regression Results", align=TRUE)

#----------------------------------
# multicollinearity
#---------------------------------
car::vif(model1)
car::vif(model2)
car::vif(model3)
car::vif(model4)

# correlation 
Q_dat = Q_Geld[3:5]
cor(Q_dat)
cor(oekkennzd[3:5])
cor(simulationDT_1)
cor(simulationDT_2)
cor(simulationDT_3)
cor(income)

# plot
library(tidyr)
plot_money = geld2 %>%
  gather(Variable,value, Y, M1, P) %>%
  ggplot(aes(x= as.yearqtr(Quartal), y=value, colour=Variable)) +
  geom_line()+
  ggtitle("Money data") +
  labs(x = "Quartal Date", y = "Value")

plot_oekk = oekk_test %>%
  gather(Variable,value, IR,SP,CPI,ULC) %>%
  ggplot(aes(x= as.yearqtr(Quartal), y=value, colour=Variable)) +
  geom_line()+
  ggtitle("Oekk. data") +
  labs(x = "Quartal Date", y = "Value")

income$id= 1:nrow(income)

plot_income = income %>%
  gather(Variable,value, WINC, WA, WE, Children) %>%
  ggplot(aes(x= id, y=value, colour=Variable)) +
  geom_line()+
  ylim(0, 3000)+
  ggtitle("Earnings data") +
  labs(x = "Data Points", y = "Value")

simulationDT_1$id= 1:nrow(simulationDT_1)

plot_simu = simulationDT_1 %>%
  gather(Variable,value,y,x1,x2) %>%
  ggplot(aes(x= id, y=value, colour=Variable)) +
  geom_line()+
  ggtitle("Simulated data 1") +
  labs(x = "Data Points", y = "Value")
