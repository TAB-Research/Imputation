rm(list=ls())

### R packages
# To install the following packages type ``install.packages9("package name")'' in R window
# This step matches to Step 2 (Test) and Step 3 (Imputation) 
require(mice)

### Imputation Estimator Function
Estimate=function(MI,n,M){
my=matrix(0,M,7)
vy=matrix(0,M,7)

for(i in 1:M){
my[i,]=apply(complete(MI,i),2,mean)[1:7]    
vy[i,]=apply(complete(MI,i),2,var)[1:7]    
}    

mest=apply(my,2,mean)

W=apply(vy,2,mean)/n
B=((M+1)/M)*apply(my,2,var)
V=W+B
vest=sqrt(V)
return(list(mest,vest))
}

### Data
dat=readRDS("working directory address/file name.rds")   # read.csv("file name.csv") for csv file
n=nrow(dat)                                              # data file should in the directory folder.

### Define variable names to be matched the manuscript notation. 
# Propensity score Z are attached in the data file. 
mdat=dat[,c(3:9,12)]
names(mdat)=c("x","y1","y2","y3","y4","y5","y6","z")

### Step 2
# d: missing indicator delta (Kim and Im, 2018)
# Regress d on x and z
d=rep(1,n)
d[is.na(apply(mdat[,2:7],1,prod))==T]=0
reg=glm(d~mdat$x+mdat$z,family=binomial(link='logit'))
summary(reg)

### (Step 3) Multiple imputation estimator
# MI1 does not use the propensity score Z
# MI2 use the propensity score Z
# (Estiamted) standard error (Rubin's variance estimation) can be varied with different imputation size and seed.
M=30
MI1=mice(mdat[,-8],m=M,printFlag = F,seed=1105,defaltMethod="polr")
MI2=mice(mdat,m=M,printFlag = F     ,seed=1105,defaltMethod="polr")

MINZ=Estimate(MI1,n,M)
MIWZ=Estimate(MI2,n,M)





