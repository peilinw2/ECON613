install.packages("plm")
library(dplyr)
library(tidyverse)
library(readr)
library(Hmisc)
library(readr)
library(data.table)
library(plm)

options(scipen=999)

setwd("~/Desktop/ECON 613/A2")

#Exercise 1
list_files = list.files(path = "~/Desktop/ECON 613/A2/Data",pattern = ".csv",full.names = TRUE,recursive = FALSE)
file_names = gsub(pattern = ".cvs","",list_files)
names = substr(file_names,44,nchar(file_names)-4)
for (i in 1:length(list_files)){
  assign(names[[i]] , fread(list_files[i]))
}
time = seq(2009,2019)
individual_dataset = data.frame()
for (i in 1:length(time))
{
  df1     = get(paste0("datind",time[i]))
  df1$idind = as.character(df1$idind) 
  individual_dataset = rbind(individual_dataset,df1)
}
individual_dataset_1 = individual_dataset[wage != 0] #remove wage = 0
X = individual_dataset_1$age
Y = individual_dataset_1$wage

  #1-1
X = as.matrix(X)
Y = as.matrix(Y)
cor(X,Y,method = "pearson",use = "complete.obs") 

  #1-2
Y1 = Y
beta = solve(t(X) %*% X) %*% t(X) %*% Y1 
print(beta)

  #1-3
beta_1 = as.numeric(beta)
error = Y1 - beta_1*X
#based on the formula, we need to have error's variance.
variance = (t(error) %*% error)/(length(Y1)-1)  
var_beta = variance * solve(t(X) %*% X) #use fomular to get standard error
print(sqrt(diag(var_beta)))
  ##bootstrap with 49
R = 49 #49 replications
num_indi = nrow(individual_dataset_1) #number of individuals
num_vari = 1 #number of variables
out = mat.or.vec(R,num_vari) #set size of matrix
set.seed(123)
for (i in 1:R){
  sampl = sample(1:num_indi,num_indi,rep=TRUE) #sampling data
  dat_sampl = individual_dataset_1[sampl,] #randomly select 
  beta_2     = solve(t(dat_sampl$age) %*% dat_sampl$age) %*% 
                              t(dat_sampl$age) %*% dat_sampl$wage
  out[i] = beta_2
}
mean_est = mean(out)
sd_est   = sd(out)
print(sd_est)
  ##bootstrap with 499
R_2 = 499 #follow the same step as before
num_indi = nrow(individual_dataset_1)
num_vari = 1
out_2 = mat.or.vec(R_2,num_vari)
set.seed(123)
for (i in 1:R_2){
  sampl = sample(1:num_indi,num_indi,rep=TRUE)
  dat_sampl = individual_dataset_1[sampl,]
  beta_2     = solve(t(dat_sampl$age) %*% dat_sampl$age) %*% 
    t(dat_sampl$age) %*% dat_sampl$wage
  out_2[i] = beta_2
}
mean_est_2 = mean(out_2)
sd_est_2   = sd(out_2)
print(sd_est_2)

#Exercise 2
  #2-1
time = seq(2005,2018)
data_2 = data.frame()
for (i in 1:length(time))
{
  df1     = get(paste0("datind",time[i]))
  df1$idind = as.character(df1$idind) 
  data_2 = rbind(data_2,df1)
} #combine data

data_3 = data_2[age >= 18] #remove people less than 18 years old
data_3$ag = as.factor(ifelse(data_3$age >= 18 & data_3$age <= 25, "18-25",
                      ifelse(data_3$age >= 26 & data_3$age <= 30, "26-30",
                      ifelse(data_3$age >= 31 & data_3$age <= 35, "31-35",
                      ifelse(data_3$age >= 36 & data_3$age <= 40, "36-40",
                      ifelse(data_3$age >= 41 & data_3$age <= 45, "41-45",
                      ifelse(data_3$age >= 46 & data_3$age <= 50, "46-50",
                      ifelse(data_3$age >= 51 & data_3$age <= 55, "51-55",
                      ifelse(data_3$age >= 56 & data_3$age <= 60, "56-60",
                      ifelse(data_3$age > 60, "60+",0))))))))))

  #2-2
data_4 = data_3[,10:11] #construct new dataset that includes ag and wage
data_4 = data_4 %>% arrange(ag) #arrange dataset based on age group
ggplot(data_4,aes(x = ag,y = wage, color = ag)) + geom_boxplot()#plot boxplot
  
  #2-3
#method 1/I'm not sure if we can use lm() in this question
df = data_3[wage != 0] #remove individuals with wage = 0
add_effect = lm(wage ~ age + factor(year), data = df) #use lm() and include year effect
summary(add_effect)

#method 2/without using lm()
y_6 = ifelse(df$year == '2006', 1, 0) #create year dummy variables
y_7 = ifelse(df$year == '2007', 1, 0)
y_8 = ifelse(df$year == '2008', 1, 0)
y_9 = ifelse(df$year == '2009', 1, 0)
y_10 = ifelse(df$year == '2010', 1, 0)
y_11 = ifelse(df$year == '2011', 1, 0)
y_12 = ifelse(df$year == '2012', 1, 0)
y_13 = ifelse(df$year == '2013', 1, 0)
y_14 = ifelse(df$year == '2014', 1, 0)
y_15 = ifelse(df$year == '2015', 1, 0) 
y_16 = ifelse(df$year == '2016', 1, 0)
y_17 = ifelse(df$year == '2017', 1, 0)
y_18 = ifelse(df$year == '2018', 1, 0)

X.2 = cbind(rep(1,length(df$age)),df$age,y_6,y_7,y_8,y_9,y_10,y_11,y_12,y_13,y_14,y_15,
            y_16,y_17,y_18) #combine all variables
solve(t(X.2) %*% X.2) %*% t(X.2) %*% df$wage #calculate coefficients

#Exercise 3
  #3-1
time = seq(2007,2019)
data_prob = data.frame()
for (i in 1:length(time))
{
  df1     = get(paste0("datind",time[i]))
  df1$idind = as.character(df1$idind) 
  data_prob = rbind(data_prob,df1)
}
#combine data and remove inactive and retired people
data_prob = data_prob[which(data_prob$empstat != "Inactive")]
data_prob = data_prob[which(data_prob$empstat != "Retired")]
  #3-2
yvar = as.numeric(data_prob$empstat == "Employed") #make dummy variable 
x1 = data_prob$age 
likeli  = function(beta,x1,yvar){
  xbeta = beta[1] + beta[2]*x1 #construct xbeta
  pr = pnorm(xbeta) #returns cdf of xbeta
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001 #set probabilities that close to 1 & 0
  prob = yvar*log(pr) + (1-yvar)*log(1-pr) #expectation
  return(-sum(prob)) #return negative to get maximized value
}

yvar_1 = as.matrix(yvar)
data_prob = cbind(data_prob,yvar_1)
colnames(data_prob)[11] = "yvar.1"#add the dummy variables to dataset

#test if function is correct
start = glm(yvar.1~ age,data = data_prob,family = binomial(link = "probit"))
test_par = start$coefficients
likeli(test_par,x1,yvar)
logLik(start)

  #3-3
#Method 1
n = 100 #set number of entry 
out.1 = mat.or.vec(n,3) #set matrix which need to have space for variables and values
for (i in 1:n){
  start    = runif(2,-10,10) #randomize start value
  result  = optim(start,fn=likeli,method="BFGS",control=list(trace=6,maxit=1000),
                  x1=x1,yvar=yvar) #use optim() function to optimize model
  out.1[i,] = c(result$par,result$value) #store minimized value and parameters
}
out.1 = as.data.frame(out.1)
out.1[which(out.1$V3 == min(out.1$V3)),] 
#Method 2
start_1 = lm(yvar.1~age,data = data_prob) #run ols regression
coeff = start_1$coefficients
fit.1 = optim(coeff,fn=likeli,method = "BFGS",control=list(trace=6,maxit=1000),
              x1=x1,yvar=yvar,hessian =  TRUE) #optimize model based on hessian matrix
fit.1$par

  #3-4
data_prob.1 = data_prob[wage != 0] #remove wage=0 and NAs
data_prob.1 = data_prob[!is.na(wage)]
x.1 = data_prob.1$age
x.2 = data_prob.1$wage
yvar.1 = as.vector(data_prob.1$yvar.1) #three variables 
likeli_2  = function(beta,x.1,x.2,yvar.1){
  xbeta = beta[1] + beta[2]*x.1+beta[3]*x.2
  pr = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  prob = yvar.1*log(pr) + (1-yvar.1)*log(1-pr)
  return(-sum(prob))
}
#method 1
n = 100
out.2 = mat.or.vec(n,4)
for (i in 1:n){
  start    = runif(3,-10,10)
  result  = optim(start,fn=likeli_2,method="BFGS",control=list(trace=6,maxit=1000),
                  x.1=x.1,x.2=x.2,yvar.1=yvar.1)
  out.2[i,] = c(result$par,result$value)
}
out.2 = as.data.frame(out.2)
out.2[which(out.2$V4 == min(out.2$V4)),] 
#method 2
start_2 = lm(yvar.1~x.1+x.2)
coeff_2 = start_2$coefficients
fit.2 = optim(coeff_2,fn=likeli_2,method = "BFGS",control=list(trace=6,maxit=1000),x.1=x.1,x.2=x.2,yvar.1=yvar.1,hessian = TRUE)
fit.2$par

#Exercise 4
  #4-1
#combine datasets and remove inactive&retired individuals 
time = seq(2005,2015)
data_prob.2 = data.frame()
for (i in 1:length(time))
{
  df1     = get(paste0("datind",time[i]))
  df1$idind = as.character(df1$idind) 
  data_prob.2 = rbind(data_prob.2,df1)
}
data_prob.2 = data_prob.2[which(data_prob.2$empstat != "Inactive")]
data_prob.2 = data_prob.2[which(data_prob.2$empstat != "Retired")]
  #4-2
y = as.numeric(data_prob.2$empstat == "Employed") #create dummy variable
age = data_prob.2$age
data_prob.2 = data_prob.2 %>% mutate(y = as.matrix(y)) #combine dummy with original dataset
y.6 = ifelse(data_prob.2$year == '2006', 1, 0) #create year dummy variables
y.7 = ifelse(data_prob.2$year == '2007', 1, 0)
y.8 = ifelse(data_prob.2$year == '2008', 1, 0)
y.9 = ifelse(data_prob.2$year == '2009', 1, 0)
y.10 = ifelse(data_prob.2$year == '2010', 1, 0)
y.11 = ifelse(data_prob.2$year == '2011', 1, 0)
y.12 = ifelse(data_prob.2$year == '2012', 1, 0)
y.13 = ifelse(data_prob.2$year == '2013', 1, 0)
y.14 = ifelse(data_prob.2$year == '2014', 1, 0)
y.15 = ifelse(data_prob.2$year == '2015', 1, 0)
#==========================================
# probit
#==========================================

likeli_3  = function(beta,age,y.6,y.7,y.8,y.9,y.10,y.11,y.12,
                     y.13,y.14,y.15,y){
  xbeta = beta[1] + beta[2]*age + beta[3]*y.6 + beta[4]*y.7 + beta[5]*y.8+
    beta[6]*y.9 + beta[7]*y.10 + beta[8]*y.11 + beta[9]*y.12 + beta[10]*y.13 +
    beta[11]*y.14 + beta[12]*y.15
  pr = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  prob = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(prob))
}

n = 100
out_3 = mat.or.vec(n,13)
for (i in 1:n){
  start    = runif(12,-10,10)
  result  = optim(start,fn=likeli_3,method="BFGS",control=list(trace=6,maxit=1000),age=age,
                  y.6=y.6,y.7=y.7,y.8=y.8,y.9=y.9,y.10=y.10,y.11=y.11,y.12=y.12,y.13=y.13,
                  y.14=y.14,y.15=y.15,y=y)
  out_3[i,] = c(result$par,result$value)
}
out_3 = as.data.frame(out_3)
beta_prob = out_3[which(out_3$V13 == min(out_3$V13)),] 
print(beta_prob)

#==========================================
# logit
#==========================================

likeli_4  = function(beta,age,y.6,y.7,y.8,y.9,y.10,y.11,y.12,
                     y.13,y.14,y.15,y){
  xbeta = beta[1] + beta[2]*age + beta[3]*y.6 + beta[4]*y.7 + beta[5]*y.8+
    beta[6]*y.9 + beta[7]*y.10 + beta[8]*y.11 + beta[9]*y.12 + beta[10]*y.13 +
    beta[11]*y.14 + beta[12]*y.15
  pr = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  prob = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(prob))
}

n = 100
out_4 = mat.or.vec(n,13)
for (i in 1:n){
  start    = runif(12,-5,5) #set the different range because (-10,10) doesn't work
  result  = optim(start,fn=likeli_4,method="BFGS",control=list(trace=6,maxit=1000),age=age,
                  y.6=y.6,y.7=y.7,y.8=y.8,y.9=y.9,y.10=y.10,y.11=y.11,y.12=y.12,y.13=y.13,
                  y.14=y.14,y.15=y.15,y=y)
  out_4[i,] = c(result$par,result$value)
  
}
out_4 = as.data.frame(out_4)
beta_log = out_4[which(out_4$V13 == min(out_4$V13)),]
print(beta_log)

#==========================================
# linear 
#==========================================
linear_prob = lm(y ~ age + as.factor(year),data = data_prob.2)
summary(linear_prob) #linear probability model
#I'm not sure if we can use lm(). So, I use another method which give the same answer.
X.3 = cbind(rep(1,length(data_prob.2$age)),data_prob.2$age,y.6,y.7,y.8,
            y.9,y.10,y.11,y.12,y.13,y.14,y.15)
beta_li = solve(t(X.3) %*% X.3) %*% t(X.3) %*% y
print(beta_li)

  #4-3
#==========================================
# probit 
#==========================================
start = runif(12)
result_prob  = optim(start,fn=likeli_3,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),
                      age=age,y.6=y.6,y.7=y.7,y.8=y.8,y.9=y.9,y.10=y.10,y.11=y.11,y.12=y.12,
                      y.13=y.13,y.14=y.14,y.15=y.15,y=y,hessian=TRUE)
fisher_prob = solve(result_prob$hessian)       
sigma_prob  = sqrt(diag(fisher_prob))
print(sigma_prob)

#==========================================
# logit
#==========================================
start = runif(12)
result_logit  = optim(start,fn=likeli_4,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000), 
                      age=age,y.6=y.6,y.7=y.7,y.8=y.8,y.9=y.9,y.10=y.10,y.11=y.11,y.12=y.12,
                      y.13=y.13,y.14=y.14,y.15=y.15,y=y,hessian=TRUE)

fisher_logit = solve(result_logit$hessian)       
sigma_logit  = sqrt(diag(fisher_logit))
print(sigma_logit)

#==========================================
# linear
#==========================================
beta_li = as.matrix(beta_li)
error_li = y - X.3 %*% beta_li
variance_li = (t(error_li) %*% error_li)/(length(y)-1)  
vari_beta.li = solve(t(X.3) %*% X.3) * as.numeric(variance_li)
print(sqrt(diag(vari_beta.li)))

#Exercise 5
  #5-1
#==========================================
# probit marginal effect
#==========================================
beta_prob.1 = beta_prob[,1:12]
pdf_prob = X.3 %*% t(beta_prob.1)
marginal_prob = mean(dnorm(pdf_prob)) * t(beta_prob.1)
print(marginal_prob)

#==========================================
# logit marginal effect
#==========================================
beta_log.1 = beta_log[,1:12]
pdf_log = X.3 %*% t(beta_log.1)
marginal_log = mean(dnorm(pdf_log)) * t(beta_log.1)
print(marginal_log)

  #5-2 
#trying to figure out how to do this part.
#==========================================
# probit 
#==========================================
R = 49 
num_indi.3 = nrow(data_prob.2) 
num_vari.3 = 13
out.3 = mat.or.vec(R,num_vari.3) 
set.seed(123)
for (i in 1:R){
  sampl.3 = sample(1:num_indi.3,num_indi.3,rep=TRUE)
  dat_sampl.4 = data_prob.2[sampl.3,] 
  beta_3     = likeli_3(dat_sampl.3)
  out.3[i] = beta_3
}
mean_prob = mean(out.3)
sd_prob   = sd(out.3)
print(sd_prob)

#==========================================
# logit
#==========================================
R = 49 
num_indi.3 = nrow(data_prob.2) 
num_vari.3 = 13
out.4 = mat.or.vec(R,num_vari.3) 
set.seed(123)
for (i in 1:R){
  sampl.4 = sample(1:num_indi.3,num_indi.3,rep=TRUE)
  dat_sampl.4 = data_prob.2[sampl.3,] 
  beta_4     = likeli_4(dat_sampl.3)
  out.4[i] = beta_4
}
mean_prob = mean(out.4)
sd_prob   = sd(out.4)
print(sd_prob)