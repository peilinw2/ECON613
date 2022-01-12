#exercise 1
install.packages("Hmisc")
install.packages("gdata")
install.packages("boot")
install.packages("xtable")
install.packages("MASS")
install.packages("moments")
install.packages("snow")
install.packages("mvtnorm")

setwd("~/Desktop/assignment 0")
getwd()

dir()
ls()

678%%9 == 0 
#678 is not a mutiple of 9

save.image()

?mean
?cut2
??cut2

log(-1)

#exercise 2
Titanic
sum(Titanic)
?Titanic
sum(Titanic[,,"Adult",])
sum(Titanic["Crew",,,])
sum(Titanic["3rd",,"Child",])
sum(Titanic["2nd","Female","Adult",])
sum(Titanic["1st","Male","Children",])
sum(Titanic["Crew","Female",,"Yes"])
sum(Titanic["1st","Male","Adult","Yes"])

?prop.table
prop.table(Titanic["1st","Male","Adult",])
prop.table(Titanic["1st","Female","Adult",])
prop.table(Titanic["1st","Male","Child",])
prop.table(Titanic["3rd","Female","Adult",])

#exercise 3
  #1
a1 = seq(1,50,1)
a1 = 1:50
a1 = rev(50:1)

b1 = seq(50,1,-1)
b1 = 50:1
b1 = rev(1:50)

  #2
a2 = rep(c(10,19,7),15)
b2 = rep(c(1,2,5,6),8)

  #3
x = seq(3.1,6,0.1)
vec1 = log(x)*sin(x)

  #4
?sample
x1 = 1:100
sam = sample(x1,90)
mean(sam)
sam = sample(x1,90,replace = TRUE)
mean(sam)

  #5
a3 =  1:20
b3 = t(c(1:15))
sum(((exp(sqrt(a3)))*log(a3^5))/(5+cos(a3)%*%sin(b3)))

a4 = 1:20
ans1 = 0
for (i in a4){
  b4 = 1:i
  for (j in b4){
    ans1 = ans1 + (exp(sqrt(i))*log(i^5))/(5+exp(i*j)*cos(i)*sin(j))
  }
}
print(ans1)

  #6
x2 = seq(3,6,0.1)
vec2 = exp(x2)*cos(x2)

#exercise 4
  #1
data = 0:999
xVec = sample(data,1000,replace = TRUE)
yVec = sample(data,1000,replace = TRUE)

  #2(a)
zVec = yVec[2:length(yVec)]-xVec[1:length(xVec)-1]
print(zVec)

  #2(b)
wVec = sin(yVec[1:length(yVec)-1])/cos(xVec[2:length(xVec)])
print(wVec)

  #2(c)
subX = xVec[xVec >= 200]
print(subX)

  #2(d)
which(yVec >= 600,arr.ind = TRUE,) 

#exercise 5
  #1
A = cbind(c(1,5,-2),c(1,2,-1),c(3,6,-3))
A^3 == 0

c4 = A[,1] + A[,3]
cbind(A,c4)

replace(A[3,],,A[1,]+A[2,])

rowMeans(A)
colMeans(A)

  #2
mat1 = matrix(c(2,1,3,1,1,1,1,3,2),nrow = 3,ncol = 3,byrow = TRUE)
mat2 = cbind(mat1,c(10,6,13))
solve(mat1,c(10,6,13))

#exercise 6
  #1
fun1 = function(a,n){
  return(sum(a^(1:n)/(1/n)))
}

  #2
fun2 = function(x){
  if (x < 0){
    return((x^2)+2*x+abs(x))
  }else if (0 <= x & x < 2){
    return((x^2)+3+log(1+x))
  }else{
    return((x^2)+4*x-14)
  }
}


print(fun2(-3))
print(fun2(0))
print(fun2(3))

#exercise 7
v1 = sample(1:20,36,replace = TRUE)

x[-1]
x[2:length(x)]

v2 = v1 > 5
as.integer(v2)

m1 = matrix(v1,nrow = 6,ncol = 6,byrow = TRUE)

x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))


miss = is.na(x)+is.infinite(x)
subvector = x[!miss]

#exercise 8
install.packages("AER")
library(AER)
data("GSOEP9402",package = "AER")
dat = GSOEP9402

typeof(dat)
nrow(dat)
ncol(dat)
colnames(dat)

install.packages("ggplot2")
install.packages("dplyr")
require(ggplot2)
require(dplyr)
ggplot(dat %>% group_by(year) %>% summarize(mean_income = mean(income)) , aes(x=year,y=mean_income)) + geom_line() + ylab("Average Income")

gender = dat %>% group_by(gender) %>% summarize(mean_income = mean(income))
school = dat %>% group_by(school) %>% summarize(mean_income = mean(income))
memployment = dat %>% group_by(memployment) %>% summarize(mean_income = mean(income))
income_difference = c(gender[1,2]-gender[2,2],school[2,2]-school[1,2],school[3,2]-school[1,2],school[3,2]-school[2,2],memployment[2,2]-memployment[1,2],memployment[2,2]-memployment[3,2],memployment[1,2]-memployment[3,2])

#exercise 9
data("CASchools",package = "AER")
dat1 = CASchools

?lm
reg1 = lm(formula = read ~ .-math,data = dat1 )
summary(reg1)

forumla = y ~ x.lm(formula)
reg2 = lm(formula = read ~ .-math,data = dat1[1:200,] )
summary(reg2)

#exercise 10
  #1
install.packages("actuar")
require(actuar)
lu = rpareto(200, 1, 1)
length(lu[lu > 10])
lu[which(lu>10)] = rlogis(length(lu[lu > 10]),6.5,0.5)
  #2
de = rnorm(200,1,2)
de = log(de)
length(de[is.nan(de)])
install.packages("truncnorm")
require(truncnorm)
de[which(is.nan(de))] = rtruncnorm(length(de[is.nan(de)]),0)
  #3
orig  = runif(200,0,1)
dest = runif(200,0,1)
  #4  
?matrix
hist = matrix(runif(200*200,0,1),nrow=200, ncol=200)
dist = matrix(runif(200*200,0,1),nrow=200, ncol=200)
  #5,6
a = outer(orig,dest,"+")+dist
su  = log(a) / (1+log(a))
se = exp(a) / (1+exp(a))
  #7
r = 0.05
myfunction = function(w){
  one = outer(r+de,r+de,"/")
  two = one*w
  three = lu*log(w)
  four = lu*(1+log(w))
  five = outer(three,four,"-")
  six = one*sum(su)-sum(su)
  seven = one*sum(se)-sum(se)
  return(one*w+five+six+seven)
}
myfunction(9245)
  #8
gridw = seq(9100,55240,50)
  #9
?sapply()
sapply(gridw,FUN = myfunction,simplify = FALSE)
system.time(sapply(gridw,FUN = myfunction,simplify = FALSE))

#exercise 11
  #1
vec3 = c(1,2,3)
is.array(vec3)
is.vector(vec3)
is.matrix(vec3)
  #2
x0 = rnorm(1000)
table(x0>0)[[2]]
table(x0>1)[[2]]
table(x0>2)[[2]]
table(x0>0.5)[[2]]
table(x0<1)[[2]]
table(x0>-1)[[2]]
  #3
require(Hmisc)
x1 = cut2(runif(100,0,1),g=10)
levels(x1)=paste("q",1:10,sep="")
  #4
is.factor(x1)
  #5
table(x1 == "q1")[[2]]
  #6
as.numeric(x1)
  #7
rand = rnorm(1000)
  #8
which(rand>0)
  #9
w = rand[which(rand>0)]
w = subset(rand,rand>0)
w = rand[rand>0]

#exercise 12
u = function(n){
  if (n == 0 | n ==1){
    return(1)
  }else{
    return(u(n-1)+u(n-2))
  }
}
  #1
fun = function(x){
  return(sum(x^2))
}
print(fun(c(1:400)))
  #2
fun = function(x,y){
  return(sum(x*y)) 
}
print(fun(c(1:249),c(2:250)))
  #3
crra = function(c,theta){
  if(theta == 0.97:1.03){
    return(log(c^(1-theta)/(1-theta)))
  }else{
    return((c^(1-theta)/(1-theta)))
  }
}
print(crra(1,0.5))
  #4
fact = function(n){
  if(n == 0|n == 1){
    return(1)
  }else{
    return(prod(1:n))
  }
}

#exercise 13 
  #1
m = matrix(c(rnorm(20,0,10), rnorm(20,-1,10)), nrow = 20, ncol = 2)
apply(m,MARGIN = 1, FUN = mean)
apply(m,MARGIN = 2,FUN = mean)
apply(m,MARGIN = 1,FUN = median)
apply(m,MARGIN = 2,FUN = median)
apply(m,MARGIN = 1,FUN = min)
apply(m,MARGIN = 2,FUN = min)
apply(m,MARGIN = 1,FUN = max)
apply(m,MARGIN = 2,FUN = max)
apply(m,MARGIN = 1,FUN = sd)
apply(m,MARGIN = 2,FUN = sd)
  #2
library(datasets)
data(iris)
iris %>% group_by(Species) %>% summarise(average_sepal.length = mean(Sepal.Length))
iris %>% group_by(Species) %>% summarise(sum_log = sum(log(Sepal.Width)))
  #3
y1 = NULL; for (i in 1:100) y1[i]=exp(i)
y2 = exp(1:100)
y3 = sapply(1:100,exp)
system.time(for (i in 1:100) y1[i]=exp(i))
system.time(exp(1:100))
system.time(sapply(1:100,exp))

#exercise 14
  #1
x = rnorm(10000)
summary(x)
  #2
dsummary = function(vec){
  minimum = summary(vec)[[1]]
  decile1 = quantile(vec,probs = seq(0.1,0.9))[[1]]
  quartile1 = summary(vec)[[2]]
  med = median(vec)
  mean = mean(vec)
  sd = sd(vec)
  quartile3 = summary(vec)[[5]]
  decile9 = quantile(vec,probs = seq(0.1,0.9))[[2]]
  maximum = summary(vec)[[6]]
  return(c(minimum, decile1,quartile1,med,mean,sd,quartile3,decile9,maximum))
}
  #3
dnorm(0.5,mean = 2,sd = 0.25)
pnorm(2.5,mean = 2,sd = 0.25)
qnorm(0.95,mean = 2,sd = 0.25)
  #4
dt(0.5,df = 5)
pt(2.5,df = 5)
qt(0.95,df = 5)
  #5
dpareto(0.5,3,1)
ppareto(2.5,3,1)
qpareto(0.95,3,1)
  
#exercise 15
V = rnorm(100,-2,5)
  #1
n = length(V)
  #2
m = mean(V)
  #3
variance = var(V)
  #4
require(moments)
skewness = skewness(V)
  #5
k = kurtosis(V)

#exercise 16
  #1
dat2 = rbeta(1000*10,2,1)
X =  matrix(data = dat2,nrow = 1000,ncol = 10)
length(X[X<0]) ==0
  #2
sigmasqr = 0.5
beta = rgamma(10,2,1)
  #3
eps = rnorm(1000)
  #4
Y = X%*% beta + sqrt(sigmasqr)*eps
  #5
solve(t(X)%*%X)%*%t(X)%*%Y
  #6
Yhat = X %*% beta
epsHat = Yhat - Y
hist(epsHat, col="gray")
plot(density(epsHat))
  #7
s = t(epsHat) %*% epsHat / (1000-10-1)
v = s[1,1] * solve(t(X)%*%X)
  #8
param = cbind(beta,sqrt(v)) 
fit0 = lm(Y~0+X)
summary(fit0)
  #9
confint(fit0)
  #10
sigmasqr = 0.01
Y = X%*% beta + sqrt(sigmasqr)*eps
fit1 = lm(Y~0+X)
confint(fit1)
