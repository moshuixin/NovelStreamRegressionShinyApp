setwd("/Users/xinxin/documents/Merseburg/Masterarbeit/R_code")
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

# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(htmlTable)
library(car)
library(stargazer)
library(gridExtra)


#-----------------------------------------------------------------------------------------
#TASK ONE: load the data
#-----------------------------------------------------------------------------------------

# dataset1: money data
geld2 <- read_excel("geld2.xlsx", col_types = c("text", "numeric", "numeric", "numeric"), skip = 11)
# Y Bruttosozialprodukt, M1 MassfuerGeldmenge, P Preisbereinigungsindex

# dataset2: oek data
oekkennzd <- read_excel("oekkennzd.xlsx", col_types = c("text", "numeric", "numeric", "numeric", "numeric"), skip = 12)

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


#-----------------------------------------------------------------------------------------
#TASK two: Formate the data
#-----------------------------------------------------------------------------------------

# regression with seasonaility (just for dataset 1)
# convert data in Quarter 
geld <- str_split_fixed(geld2$Quartal,fixed("-"),2)
colnames(geld) <- c("Year", "Quarter")
Q_Geld <- cbind(geld, geld2[,2:4])


oekkennzd = data.frame(oekkennzd)

# Normalization the money

Q_Geld = Q_Geld %>% mutate_at(c("Y", "M1","P"), ~(scale(.) %>% as.vector))
# Normalization the oekk.
oekkennzd = oekkennzd %>% mutate_at(c("SP", "CPI","ULC"), ~(scale(.) %>% as.vector))

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

#---------------------------------
# regression model
#---------------------------------

# Init mutilple model
money_model  = Y ~ M1 + P
oekk_model = IR ~ SP+ CPI + ULC
simu_model = y ~ x1 + x2

# multiple regression model
model1 = lm (money_model ,Q_Geld)
model2 = lm (oekk_model ,oekkennzd)
model3 = lm (simu_model ,simulationDT_1)

#mse
print(sprintf("MSE=%0.3f",  mean(model1$residuals^2)))
print(sprintf("MSE=%0.3f",  mean(model2$residuals^2)))
print(sprintf("MSE=%0.2f",  mean(model3$residuals^2)))

# output the latex table
#stargazer(model1, model2, model3, title="Regression Results", align=TRUE)

#----------------------------------
# multicollinearity
#---------------------------------
car::vif(model1)
car::vif(model2)
car::vif(model3)


