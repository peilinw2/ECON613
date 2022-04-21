library(dplyr)
library(tidyverse)
library(readr)
library(Hmisc)
library(readr)
library(data.table)
library(plm)
library(stats)
library(ggplot2)
library(AER)
library(panelr)

options(scipen=100)

setwd("~/Desktop/ECON 613/A4")

dat_A4 = read_csv("Data/dat_A4.csv")

#Exercise 1
  #1-1
dat_A4$age = 2019 - dat_A4$KEY_BDATE_Y_1997
dat_A4$work_exp = rowSums(dat_A4[,18:28],na.rm = "TRUE")/52

  #1-2
dat_A4$c1 = dat_A4$YSCH.3113_2019
dat_A4$c1[dat_A4$c1 == 1] = 0 
dat_A4$c1[dat_A4$c1 == 2] = 4
dat_A4$c1[dat_A4$c1 == 3] = 12
dat_A4$c1[dat_A4$c1 == 4] = 14
dat_A4$c1[dat_A4$c1 == 5] = 16
dat_A4$c1[dat_A4$c1 == 6] = 18
dat_A4$c1[dat_A4$c1 == 7] = 23
dat_A4$c1[dat_A4$c1 == 8] = 21
dat_A4$CV_HGC_BIO_DAD_1997[dat_A4$CV_HGC_BIO_DAD_1997 == 95] = 0
dat_A4$CV_HGC_BIO_MOM_1997[dat_A4$CV_HGC_BIO_MOM_1997 == 95] = 0
dat_A4$CV_HGC_RES_DAD_1997[dat_A4$CV_HGC_RES_DAD_1997 == 95] = 0
dat_A4$CV_HGC_RES_MOM_1997[dat_A4$CV_HGC_RES_MOM_1997 == 95] = 0
dat_A4$edu_bio = rowSums(dat_A4[,8:9,33], na.rm = "TRUE")
dat_A4$edu_res = rowSums(dat_A4[,10:11,33], na.rm = "TRUE")

  #1-3
    #1-3-1
dat_income = subset(dat_A4, dat_A4$YINC_1700_2019 > 0)
income_age = dat_income %>% group_by(age) %>% summarise(income = mean(YINC_1700_2019))
income_age$age = as.factor(income_age$age)

income_gender = dat_income %>% group_by(KEY_SEX_1997) %>% summarise(income = mean(YINC_1700_2019))
income_gender$KEY_SEX_1997[income_gender$KEY_SEX_1997 == 1] = "Male"
income_gender$KEY_SEX_1997[income_gender$KEY_SEX_1997 == 2] = "Female"

income_chil = dat_income %>% group_by(CV_BIO_CHILD_HH_U18_2019) %>% summarise(income = mean(YINC_1700_2019))

ggplot(income_age,aes(x = age,y = income)) + geom_bar(stat='identity') + ylab("income_mean") +
  ggtitle("income_age") + theme(plot.title = element_text(size = 15L, hjust = 0.5))
ggplot(income_gender,aes(x = KEY_SEX_1997,y = income)) + geom_bar(stat='identity') + ylab("income_mean") +
  ggtitle("income_gender") + theme(plot.title = element_text(size = 15L, hjust = 0.5))
ggplot(income_chil,aes(x = CV_BIO_CHILD_HH_U18_2019,y = income)) + geom_bar(stat='identity') + ylab("income_mean") +
  ggtitle("income_children") + theme(plot.title = element_text(size = 15L, hjust = 0.5))

      #1-3-2
income0_age = dat_A4 %>% group_by(age) %>% summarise(income0 = length(which(YINC_1700_2019 == 0))/
                                                       length(dat_A4))
income0_gender = dat_A4 %>% group_by(KEY_SEX_1997) %>% summarise(income0 = length(which(YINC_1700_2019 == 0))/
                                                       length(dat_A4))
income0_gender$KEY_SEX_1997[income0_gender$KEY_SEX_1997 == 1] = "Male"
income0_gender$KEY_SEX_1997[income0_gender$KEY_SEX_1997 == 2] = "Female"
income0_chil = dat_A4 %>% group_by(CV_BIO_CHILD_HH_U18_2019) %>% summarise(income0 = length(which(YINC_1700_2019 == 0))/
                                                       length(dat_A4))
income0_mar = dat_A4 %>% group_by(CV_MARSTAT_COLLAPSED_2019) %>% summarise(income0 = length(which(YINC_1700_2019 == 0))/
                                                                            length(dat_A4))
income0_mar$CV_MARSTAT_COLLAPSED_2019[income0_mar$CV_MARSTAT_COLLAPSED_2019 == 0] = "Never-married"
income0_mar$CV_MARSTAT_COLLAPSED_2019[income0_mar$CV_MARSTAT_COLLAPSED_2019 == 1] = "Married"
income0_mar$CV_MARSTAT_COLLAPSED_2019[income0_mar$CV_MARSTAT_COLLAPSED_2019 == 2] = "Separated"
income0_mar$CV_MARSTAT_COLLAPSED_2019[income0_mar$CV_MARSTAT_COLLAPSED_2019 == 3] = "Divorced"
income0_mar$CV_MARSTAT_COLLAPSED_2019[income0_mar$CV_MARSTAT_COLLAPSED_2019 == 4] = "Widowed"

ggplot(income0_age,aes(x = age,y = income0)) + geom_bar(stat='identity') + ylab("share of 0") +
  ggtitle("share of 0 in income_age") + theme(plot.title = element_text(size = 15L, hjust = 0.5))
ggplot(income0_gender,aes(x = KEY_SEX_1997,y = income0)) + geom_bar(stat='identity') + ylab("share of 0") +
  ggtitle("share of 0 in income_gender") + theme(plot.title = element_text(size = 15L, hjust = 0.5))
ggplot(income0_chil,aes(x = CV_BIO_CHILD_HH_U18_2019,y = income0)) + geom_point(stat='identity') + ylab("share of 0") +
  ggtitle("share of 0 in income_chil") + theme(plot.title = element_text(size = 15L, hjust = 0.5))
ggplot(income0_mar,aes(x = CV_MARSTAT_COLLAPSED_2019,y = income0)) + geom_point(stat='identity') + ylab("share of 0") +
  ggtitle("share of 0 in income_mar") + theme(plot.title = element_text(size = 15L, hjust = 0.5))

    #1-3-3
"When income is positive, older people will have slightly high income. But, overall, there is no 
significant differences between different age groups. For gender group, male is more likely to have 
higher income than female. Household with 3 children will have the highest income. The income increases 
at first and then decreases with number of children increases."

"When analyzing the share of income is 0, age group 35 and 38 have larger proportion. Male have the 
higher proportion than female.Married people and household with one child are more likely to have 
high share of 0 income. "

#Exercise 2
  #2-1
reg = lm(YINC_1700_2019 ~ age + work_exp + KEY_SEX_1997 + c1, data = dat_income)
summary(reg)

  ###interpret###
"If increasing one year in age, income will increase 381.16. If increasing work 
experience by one year, income will increase 1055.14. Female will have less income 
(14835.46) than male. If increasing education by one year, income will increase 2375.85."

  ###explain###
"Since only positive income is considered, the unemployed people with high educational 
level and work experience are not taken into account. It will cause bias because proper 
randomization is not achieved."

  #2-2
"The heckman model can be separated into two part. First of all, we run the probit 
model to make estimation. Then, we include IMR in OLS which has the function to reduce
bias (selection bias)."

  #2-3
dat = dat_A4 %>% mutate(income_exist = 0)
dat = subset(dat,dat$YSCH.3113_2019!='NA')
dat$income_exist[which(dat$YINC_1700_2019 > 0)] = 1 
x1 = dat$KEY_SEX_1997
x2 = dat$age
x3 = dat$work_exp
x4 = dat$c1
y = dat$income_exist
prob = glm(y ~ x1+x2+x3+x4,family = binomial(link = "probit"), data = dat)
summary(prob)

dat$intercept = 1
intercept = dat$intercept
set.seed(123)
start = runif(7,-10,10)

prob_like = function(par, intercept, x1, x2, x3, x4,y) {
  yhat = par[1] * intercept + par[2] * x1 + par[3] * x2 + par[4] * x3 + par[5] * x4 
  prob = pnorm(yhat)
  prob[prob > 0.999999] <- 0.999999
  prob[prob < 0.000001] <- 0.000001
  like = y * log(prob) + (1 - y) * log(1 - prob)
  return(-sum(like))
}
res  = optim(start,fn=prob_like,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),intercept = intercept, x1=x1, x2=x2,x3=x3,x4=x4,y = y,hessian=TRUE)
res$par
predictor = function(par, intercept, x1, x2, x3, x4) {
  yhat = par[1] * intercept + par[2] * x1 + par[3] * x2 + par[4] * x3 + par[5] * x4
    
  return(yhat)
}
pred = predictor(res$par, intercept, x1, x2, x3, x4)
IMR = dnorm(pred)/pnorm(pred)
reg_heckman = lm(dat$YINC_1700_2019 ~ x1 + x2 + x3 + x4 + IMR)
summary(reg_heckman)

"If increasing one year in age, income will increase 553.3. If increasing work experience 
by one year, income will idecrease 142.3. Female will have less income (11432.7) than male. 
If increasing education by one year, income will increase 1531.3. Comparing to the results 
from OLS, the work experience is not significant in heckman model."


#Exercise 3
  #3-1
dat_income = subset(dat_income,dat_income$YSCH.3113_2019!='NA')
hist(dat_income$YINC_1700_2019,main = "income histogram", xlab = "income")

"the highest income is $100000."

  #3-2
reg_tobit = tobit(YINC_1700_2019 ~ KEY_SEX_1997 + age + work_exp + c1, left = -Inf, right = 100000,data = dat_income)
summary(reg_tobit)                

  #3-3
dat_income$intercept = 1
dat_income$indictor = 0
dat_income$indictor[which(dat_income$YINC_1700_2019 < 100000)] = 1
tobit_like = function(par, intercept, x1, x2, x3, x4,x5,y){
  yhat = par[1]*intercept + par[2]*x1 + par[3]*x2 + par[4]*x3 + par[5]*x4
  res = y - yhat
  standard = (100000-yhat)/exp(par[6])
  like = x5*log(dnorm(res/exp(par[6]))/exp(par[6])) + (1-x5)*log(1 - pnorm(standard))
  return(-sum(like))
}
par <- as.vector(c(reg_tobit$coefficients,10.47996))
start_1 = runif(6,-1000,1000)
start_1
res_2 = optim(start_1,fn=tobit_like,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),intercept = dat_income$intercept,
               x1=dat_income$KEY_SEX_1997,x2=dat_income$age,x3=dat_income$work_exp,x4=dat_income$c1,x5=dat_income$indictor,y=dat_income$YINC_1700_2019,hessian=TRUE)
res_2$par
  #3-4
"Female will have lower income than male. If age, work experience,or educational 
level increases, the income will increase. "

#Exercise 4
dat_A4_panel = read_csv("Data/dat_A4_panel.csv")

  #4-1
"The ability bias indicates the relation between educational level and innate skills. 
People with innate skills are more likely to go to school. Also, there exists the casual 
relationship between educational level and income."

  #4-2
dat_A4_panel = dat_A4_panel %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1998=CV_HIGHEST_DEGREE_9899_1998) %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1999=CV_HIGHEST_DEGREE_9900_1999) %>% 
  rename(CV_HIGHEST_DEGREE_EVER_EDT_2000=CV_HIGHEST_DEGREE_0001_2000) %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2001=CV_HIGHEST_DEGREE_0102_2001) %>% 
  rename(CV_HIGHEST_DEGREE_EVER_EDT_2002=CV_HIGHEST_DEGREE_0203_2002) %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2003=CV_HIGHEST_DEGREE_0304_2003) %>% 
  rename(CV_HIGHEST_DEGREE_EVER_EDT_2004=CV_HIGHEST_DEGREE_0405_2004) %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2005=CV_HIGHEST_DEGREE_0506_2005) %>% 
  rename(CV_HIGHEST_DEGREE_EVER_EDT_2006=CV_HIGHEST_DEGREE_0607_2006) %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2007=CV_HIGHEST_DEGREE_0708_2007) %>% 
  rename(CV_HIGHEST_DEGREE_EVER_EDT_2008=CV_HIGHEST_DEGREE_0809_2008) %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2009=CV_HIGHEST_DEGREE_0910_2009)
                              
dat_A4_panel_long = long_panel(dat_A4_panel,prefix='_',begin  = 1997, end = 2019,label_location = "end")
dat_A4_panel_long = dat_A4_panel_long %>% rename(edu = CV_HIGHEST_DEGREE_EVER_EDT) %>% rename(year = wave)%>% 
                  rename(marital = CV_MARSTAT_COLLAPSED)                           
colnames(dat_A4_panel_long)[5] = "income"

dat_A4_panel_long$age = dat_A4_panel_long$year - dat_A4_panel_long$KEY_BDATE_Y  
dat_A4_panel_long$work_exp= rowSums(dat_A4_panel_long[,10:16], na.rm = "TRUE")/52 + 
                            rowSums(dat_A4_panel_long[,23:30], na.rm = "TRUE")/52

dat_A4_panel_long$edu[dat_A4_panel_long$edu == 0] = 0 
dat_A4_panel_long$edu[dat_A4_panel_long$edu == 1] = 4
dat_A4_panel_long$edu[dat_A4_panel_long$edu == 2] = 12
dat_A4_panel_long$edu[dat_A4_panel_long$edu == 3] = 14
dat_A4_panel_long$edu[dat_A4_panel_long$edu == 4] = 16
dat_A4_panel_long$edu[dat_A4_panel_long$edu == 5] = 18
dat_A4_panel_long$edu[dat_A4_panel_long$edu == 6] = 23
dat_A4_panel_long$edu[dat_A4_panel_long$edu == 7] = 21
#===============================================================================
#Within Estimator: work_exp/education/marital status 
#===============================================================================
dat_A4_panel_long$mean_income = ave(dat_A4_panel_long$income, dat_A4_panel_long$id, FUN = function(x)mean(x,na.rm = "TRUE"))
dat_A4_panel_long$mean_exp = ave(dat_A4_panel_long$work_exp, dat_A4_panel_long$id, FUN = function(x)mean(x,na.rm = "TRUE"))
dat_A4_panel_long$mean_edu = ave(dat_A4_panel_long$edu, dat_A4_panel_long$id, FUN = function(x)mean(x,na.rm = "TRUE"))
dat_A4_panel_long$mean_mar = ave(dat_A4_panel_long$marital, dat_A4_panel_long$id, FUN = function(x)mean(x,na.rm = "TRUE"))

dat_A4_panel_long$income_diff = dat_A4_panel_long$income - dat_A4_panel_long$mean_income
dat_A4_panel_long$exp_diff = dat_A4_panel_long$work_exp - dat_A4_panel_long$mean_exp
dat_A4_panel_long$edu_diff = dat_A4_panel_long$edu - dat_A4_panel_long$mean_edu
dat_A4_panel_long$mar_diff = dat_A4_panel_long$marital - dat_A4_panel_long$mean_mar

within = lm(income_diff ~ exp_diff + edu_diff + mar_diff, dat_A4_panel_long)
summary(within)

#===============================================================================
#Between Estimator: work_exp/education/marital status 
#===============================================================================
y.1 = dat_A4_panel_long %>% group_by(id) %>% select(mean_income)
y.1 = y.1[!duplicated(y.1$id),]
x.exp = dat_A4_panel_long %>% group_by(id) %>% select(mean_exp)
x.exp = x.exp[!duplicated(x.exp$id),]
x.edu = dat_A4_panel_long %>% group_by(id) %>% select(mean_edu)
x.edu = x.edu[!duplicated(x.edu$id),]
x.mar = dat_A4_panel_long %>% group_by(id) %>% select(mean_mar)
x.mar = x.mar[!duplicated(x.mar$id),]

between = y.1 %>% left_join(x.exp) %>% left_join(x.edu) %>% left_join(x.mar)
between_reg = lm(mean_income ~ mean_edu + mean_mar + mean_exp, data = between)
summary(between_reg)                                                 

#===============================================================================
#Difference Estimator: work_exp/education/marital status 
#===============================================================================
#try to solve by not using package and get weird answer
difference = dat_A4_panel_long %>% group_by(id) %>% mutate(income_diff= income-lag(income)) %>% mutate(edu_diff= edu-lag(edu)) %>% mutate(mar_diff= marital-lag(marital)) %>% mutate(work_diff= work_exp-lag(work_exp))
diff_reg = lm(income_diff~ edu_diff + mar_diff + work_diff, data = difference)

#use package
fd = plm(income ~  edu + marital + work_exp, dat_A4_panel_long, model = "fd")
summary(fd)

  #4-3
"Coefficients have the same sign in these three models, but difference estimation 
gives the smallest coefficients and within model gives the largest coefficients. 
Within estimators indicates the effect on individual level and between estimators 
indicates the effect on different individuals. The first difference estimators relies 
on the fixed effect model."
