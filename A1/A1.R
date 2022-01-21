install.packages("tidyverse")
install.packages("dplyr")
install.packages("gmodels")
install.packages("MASS")
install.packages("data.table")
install.packages("REAT")

rmarkdown::render("A1.R", "pdf_document")
#set a working library
setwd("~/Desktop/A1")

#import data
library(readr)
library(data.table)
list_files = list.files(path = "~/Desktop/A1/Data",pattern = ".csv",full.names = TRUE,recursive = FALSE)
file_names = gsub(pattern = ".cvs","",list_files)
names = substr(file_names,35,nchar(file_names)-4)
for (i in 1:length(list_files)){
  assign(names[[i]] , fread(list_files[i]))
}

#exercise 1
#1-1
nrow(dathh2007) #count number of rows

#1-2
marital_status = dathh2005[,6] #specific the column of marital_status
length(which(marital_status == "Couple, with Kids")) #count number of households with "couple,with kids"
 
#1-3  
nrow(datind2008) 

#1-4
age_2016 = datind2016[,9] #specific the column of age 
length(which(age_2016 >= 25 & age_2016 <= 35)) 

#1-5
require(gmodels)
CrossTable = table(datind2009$gender,datind2009$profession)
CrossTable

# 1-6
require(dplyr)
wage_2005 = datind2005$wage #specific the wage columns in 2005 & 2019
wage_2019 = datind2019$wage
d_2005 = density(wage_2005[!is.na(wage_2005)]) #make sure to ignore the "NA" in the dataset
plot(d_2005,main = "wage_2005") #lognormal/check the plot
d_2019 = density(wage_2019[!is.na(wage_2019)])
plot(d_2019,main = "wage_2019") #lognormal/check the plot

vec1 = wage_2005[!is.na(wage_2005)] #delete NA and people who don't have wage
vec1 = vec1[vec1 != 0]
vec2 = wage_2019[!is.na(wage_2019)]
vec2 = vec2[vec2 != 0]

myfunction = function(vec){
  mean = mean(vec)
  sd = sd(vec)
  interdecile = (quantile(vec,probs = seq(0.1,0.9,by = 0.1))[[9]])/(quantile(vec,probs = seq(0.1,0.9,by = 0.1))[[1]])
  gini_co = sum(outer(vec, vec, FUN=function(x,y){abs(x-y)})) / (2 * (length(vec))^2 * mean(vec))
  return(c(mean,sd,interdecile,gini_co))
} #construct function which can return results as a string
as.character(print(myfunction(vec1)))
as.character(print(myfunction(vec2)))


#1-7
library(REAT)

age_2010 = datind2010$age
d_2010 = density(age_2010)
plot(d_2010,main = "age_2010") #almost uniform distribution

age_male = datind2010 %>% filter(gender == "Male") #male dataset

hist(as.numeric(age_male$age))
age_female = datind2010 %>% filter(gender == "Female") #female dataset
hist(as.numeric(age_female$age))

#1-8
paris_house = dathh2011 %>% filter(location == "Paris") #come up the dataset that household are lived in Paris
set = paris_house %>% merge(datind2011,by = "idmen") #find out individuals lived in Paris
nrow(set)

#exercise2
#2-1
time = seq(2004,2019)
individual_dataset = data.frame()
for (i in 1:length(time))
{
  df1     = get(paste0("datind",time[i]))
  df1$idind = as.character(df1$idind) 
  individual_dataset = rbind(individual_dataset,df1)
}

#2-2
household_dataset = rbind(dathh2004,dathh2005,dathh2006,dathh2007,dathh2008,
                          dathh2009,dathh2010,dathh2011,dathh2012,dathh2013,
                          dathh2014,dathh2015,dathh2016,dathh2017,dathh2018,
                          dathh2019)
#2-3
a = ls(individual_dataset) #list of variables 
b = ls(household_dataset)
intersect(a,b) 
#2-4
merge_set = merge(individual_dataset,household_dataset,by = c("idmen","year"))

#2-5
myfunction = function(x) {
  family_set = merge_set %>% filter(year == x)
  sum(table(family_set$idmen) > 4)
} #category merge_set by years and check the frequency of the idmen's occurrence. sum up the occurrence is larger than 4. 
year = 2004:2019 #time interval
sum(sapply(year, myfunction)) #since the year is an interval, use sapply() to sum up all answers.
#2-6
unemployed = merge_set %>% filter(empstat == "Unemployed") # individual set that all individuals are unemployed
myfunction2 = function(x){
  a = unemployed %>% filter(x == year)
  sum(as.data.frame(table(a$idmen,a$empstat) >= 1))
}  #category merge_set by years and check how many unemployed in a household. If greater or equal to one, keep it and sum up
sum(sapply(year, myfunction2))

#2-7
profession1 = merge_set[-which(merge_set$profession == ""),] #delete all blank
a = is.na(profession1$profession) #find NAs
profession1 = profession1[!a,] #delete NAs
same_profession = function(x){
  b = profession1 %>% filter(x == year)
  sum(as.data.frame(table(b$idmen,b$profession) >= 2))
} 
sum(sapply(year,same_profession))

#2-8
couple_kids = merge_set %>% filter(mstatus == "Couple, with Kids") 
nrow(couple_kids)

#2-9
from_Paris = merge_set %>% filter(location == "Paris")
nrow(from_Paris)

#2-10
family_members = function(x){
  d = merge_set %>% filter(x == year)
  sum(max(table(d$idmen)))
} 
most_family_members = sapply(year, family_members) 
print(most_family_members) # get the each year's largest family member size
idmen_members = function(x){
  members = merge_set %>% filter(year == x)
  e = as.data.frame(table(members$idmen))
  return(e %>% filter(Freq == "14"))
} #since we know the maximum frequency is 14 and its in 2007 & 2010. I build a function to get the idmen number.
print(idmen_members(2007)) # 2207811124040100
print(idmen_members(2010)) #2510263102990100

#2-11
households = function(x){
  household_year = merge_set %>% filter(year == x)
  return(household_year[!duplicated(household_year$idmen)])
}#figure out the unique idmens in year
household_2010 =households(2010)
household_2011 = households(2011)
length(intersect(household_2010$idmen,household_2011$idmen)) #find out number of household presented both in 2010 & 2011

#exercise 3

#3-1
entry_exist =  as.data.frame.matrix(table(household_dataset$idmen, household_dataset$year))
year_spend = as.data.frame(rowSums(entry_exist))
plot(density(year_spend[,1]))

#3-2
household_dataset$dwelling_moved = household_dataset$year - household_dataset$datent == 0 #check if the household moved
head(household_dataset, n=10) #read first 10 rows
require(dplyr)
require(ggplot2)
#the shares of individuals is number of moved in one year divided by total moved, which is mean
ggplot(household_dataset %>% group_by(year) %>% summarise(share_of_individual = mean(as.integer(dwelling_moved),na.rm = TRUE)),aes(x=year,y=share_of_individual))+geom_line()+ylab("Sharesl")

#3-3
myear_exist = household_dataset %>% filter(year <= "2014") #since myear only exists before year 2015, we need to filter them.
myear_notexist = household_dataset %>% filter(year > "2014")
ans_exist = myear_exist$year - myear_exist$myear == 0 #check if last migration year equals to current year
ans_notexist = myear_notexist$move == 2 # check if household has moved or not
ans_combin = as.data.frame(c(ans_exist,ans_notexist))
set2 = cbind(household_dataset,ans_combin) #combin set of migration with household dataset
colnames(set2)[10] = "migrated"
head(set2, n=10) #report first 10 rows
ggplot(set2 %>% group_by(year) %>% summarize(share_of_individual = mean(migrated,na.rm = TRUE)),aes(x=year,y=share_of_individual))+geom_line()+ylab("Shares2")

#3-4
#two datasets based on 3-3 and 3-4
data_1 = household_dataset %>% group_by(year) %>% summarise(share_of_individual = mean(as.integer(dwelling_moved),na.rm = TRUE))
data_2 = set2 %>% group_by(year) %>% summarize(share_of_individual = mean(migrated,na.rm = TRUE))
mixed_plot = ggplot(NULL)+geom_line(data = data_1,aes(x = year, y = share_of_individual),col = "red") + geom_line(data = data_2,aes(x = year,y = share_of_individual),col = "blue")+xlab("year")+ylab("share")
mixed_plot

#3-5
household_migrate = set2 %>% group_by(idmen) %>% filter(dwelling_moved == "TRUE") # households who has migrated
individual_migrate = household_migrate %>% left_join(individual_dataset,by = "idmen") #merge with individual dataset
colnames(individual_migrate)[13] = "year" #change the colname to remember
vec5 = individual_migrate %>% group_by(idind) %>%
        filter(!is.na(profession),!is.na(empstat)) %>% mutate(profession_past = lag(profession, n = 1, order_by = year))%>%
        mutate(empstat_past = lag(empstat, n = 1, order_by = year)) #filter out NAs and shift the profession/empstat one year back
vec5 = vec5 %>% group_by(idmen,year) %>%  mutate(change_or_not = ifelse(profession != profession_past | empstat != empstat_past,1, 0)) #index the change into 1 and not change into 0    
vec5 = vec5 %>% group_by(idmen) %>%
  mutate(total_change = sum(!is.na(change_or_not) & change_or_not == 1)) %>% filter(total_change >= 1) #get the total change and keep the data with total change equal or greater than 1
vec5 = vec5[!duplicated(vec5$idmen),] #remove duplicated idmen
print(nrow(vec5)) 


#exercise 4d
myfunction = function(x){
  A = merge_set %>% filter(x == year)
  B = length(unique(A$idind))
  return(B)
}
x = 2004:2019
C = sapply(x,myfunction)
C = as.data.frame(C)
vec3 = cbind(C[1:15,],C[2:16,])
colnames(vec3)[2] = "indi_2005_2019"
colnames(vec3)[1] = "indi_2004_2018"  #find out numbers of individual in each year.construct one column that includes data from year 2004 to 2018 and one column from year 2005 to 2019.

for (i in seq(2004,2019,1)){
  D = merge_set[merge_set$year == i,]
  E = merge_set[merge_set$year == i+1,]
  vec4 = length(intersect(D$idind,E$idind))
  print(vec4)
} #numbers of individual stay in the survey in each year
vec4 = c(19148,19391,20483,20034,20265,20904,21392,22472,21268,20530,
         20914,20855,19965,19199,18688)
vec4 = as.data.frame(vec4)
vec3 = cbind(vec3,vec4) #append it to the dataset we get before 
vec3 = vec3 %>% mutate(exit = indi_2004_2018 - vec4) %>% mutate(attrition = (exit / indi_2004_2018)*100) #create one columns of individual exit and another columns of attrition ratio. 

