library(tidyverse)
library(caret)

# income dataset 
# Earning and Education

dt  <- read_table2("data/TableF4-1.txt", skip = 27)


dt <- dt[dt["WHRS"] != 0,]

incomeDT =  dt[2:7]

# add variable: woman income,in 1975 dollars
WINC = dt$WHRS*dt$WW
incomeDT["WINC"] = WINC
#add variable: Children = a dummy variable = 1 if woman had childre, else 0
incomeDT <- transform(incomeDT, Children=ifelse(KL6== 0  & K618== 0,0,1))
# 

income =  incomeDT %>% select(4,5,7,8)




income["WA2"] = (income$WA)^2



#xtable(income[1:10,], type = "latex")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

nor_income = normalize(income)

xtable(cor(income),"latex")

income_model = lm( WINC ~ WE+ WA + WA2+ Children,earning )
print(income_model$coef)

print(sprintf("MSE=%0.3f",  mean(income_model$residuals^2)))
car::vif(income_model)


res = summary(income_model)$coef 
r =data.frame(res)[1]

coef(income_model)
stargazer(model2, model3, income_model, title="Regression Results", align=TRUE)

timeStream(earning, 5,earning_model)
streamMulti(earning_x,earning_y,5,0.01)
