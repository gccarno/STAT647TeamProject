# Team Project 2
## begin second generation

#library(tmap)
library(dplyr)
library(fields)
library(geoR)
#load data

madrid <- read.csv(file="E:/STAT647/Homework/Data/madrid_2015.csv")
#rename the column with station id to allow merging
colnames(madrid)[14] <- "id"
#transform to date
madrid$date <- as.Date(madrid$date)
#group by id and date
#madrid_date_id <- by(madrid,madrid[,c(1,14)],mean)
#this requires dplyr
madrid_date_id <- madrid %>% group_by(date, id) %>% summarise(BEN=mean(BEN,na.rm=TRUE),
                                                              CO=mean(CO,na.rm=TRUE),
                                                              EBE=mean(EBE,na.rm=TRUE),
                                                              NMHC=mean(NMHC,na.rm=TRUE),
                                                              NO=mean(NO,na.rm=TRUE),
                                                              NO_2=mean(NO_2,na.rm=TRUE),
                                                              O_3=mean(O_3,na.rm=TRUE),
                                                              PM10=mean(PM10,na.rm=TRUE),
                                                              PM25=mean(PM25,na.rm=TRUE),
                                                              SO_2=mean(SO_2,na.rm=TRUE),
                                                              TCH=mean(TCH,na.rm=TRUE),
                                                              TOL=mean(TOL,na.rm=TRUE))
summary(madrid_date_id)
#restrict the data to June 01
madrid_june_01 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-01")),]

#summary data
summary(madrid_june_01)
par(mfrow=c(4,4))
for (i in 3:14){
  mask <- (!is.na(madrid_june_01[,i]) & !is.na(madrid_june_01[,10]))
  plot(as.vector(madrid_june_01[mask,10]),as.vector(madrid_june_01[mask,i]))
}
plot(madrid_june_01$PM10,madrid_june_01$BEN)

#read in station information
stations <- read.csv(file="E:/STAT647/Homework/Data/stations.csv")

#merge the data
madrid_air <- merge(madrid_june_01, stations, by="id")

# run linear models
par(mfrow=c(2,2))
m1 <- lm(PM10 ~ lat + lon, data=madrid_air)
summary(m1)
plot(m1)
m2 <- lm(PM10 ~ SO_2, data=madrid_air)
summary(m2)
plot(m2)

#run vgrams using the residuals from model 1
#running in miles? how to do in KM?
v1 <- vgram(c,m1$residuals,lon.lat = TRUE,dmax=6)
par(mfrow=c(1,2))
plot(v1)
boxplotVGram(v1)

#run ols
D <- rdist.earth(madrid_air[!is.na(madrid_air$PM10),c('lat','lon')])

temp <- vgram(D,m1$residuals,N=5) 
plot(temp)
boxplotVGram(temp)
d <- temp$centers
semi.variogram <- temp$stats[2,]
par <- c(0.1,0.1,.5,6)
ols=function(par){
  
  alpha=par[1]
  beta=par[2]
  nu=par[3]
  nugget=par[4]
  
  S <- alpha*Matern(D, smoothness=nu, range=beta)
  diag(S) <- diag(S) + nugget
  
  SSE=sum((semi.variogram-S))
  
  return(SSE)
}

fit.ols=nlm(ols, c(0.1,0.1,.5,6),print.level=2,iterlim=10000)

# mle function

mle=function(par){
  
  alpha=exp(par[1])
  beta=exp(par[2])
  nu=5*exp(par[3])/(1+exp(par[3]))
  
  delta=exp(par[4])
  
  b0=par[5]
  b1=par[6]
  b2=par[7]
  
  S=alpha*Matern(D, smoothness=nu, range=beta)
  diag(S)= diag(S) + delta
  
  mu=b0+b1*madrid_air$lat +b2*madrid_air$lon
  
  temp=(determinant(S, logarithm=T)$modulus + t(z-mu) %*% solve(S) %*% (z-mu))/2
  return(temp)
}

ini=c( 0,6 ,-2,-5,-0.01, 0.37, -0.12)

fit.mle=nlm(mle, ini,print.level=2,iterlim=10000)

par=fit.mle$estimate

back2 <- function(par){
  result <- c(exp(par[1]),
              exp(par[2]),
              par[3],
              par[4],
              par[5],
              exp(par[6]),
              5*exp(par[7])/(1+exp(par[7])))
  return(result)
}

#transform back MLE

alpha=exp(par[1])
beta=exp(par[2])
nu=5*exp(par[3])/(1+exp(par[3]))

delta=exp(par[4])

b0=par[5]
b1=par[6]
b2=par[7]


### REML

pars <- c(0,5,0,-5)
REML.l.matern=function(pars){
  
  cat("raw parameters",pars,"\n")
  
  pars[1:4] <- exp(pars[1:4])
  pars[3]=5*pars[3]/(1+pars[3])
  
  nu=pars[3]
  beta=pars[2]
  alpha=pars[1]
  
  delta=(pars[4]) ## nugget
  
  cat("transformed covariance parameters",pars[1:4],"\n")
  
  cov_v <- alpha*(D/beta)^nu*besselK(D/beta,nu)/(2^(nu-1)*gamma(nu))
  diag(cov_v) <- alpha+delta
  
  t1 <- madrid_air[!is.na(madrid_air$PM10),c('lat')]
  t2 <- madrid_air[!is.na(madrid_air$PM10),c('lon')]
  t3 <- madrid_air[!is.na(madrid_air$PM10),c('PM10')]
  X=cbind(rep(1, dim(D)[1]), t1,t2)
  Z=matrix(t3, ncol=1)
  
  temp <- chol(cov_v)
  a=forwardsolve(t(temp),X)
  b=t(X) %*% backsolve(temp, a)
  
  a=forwardsolve(t(temp), Z)
  b1=t(X) %*% backsolve(temp, a)
  
  r=Z - X %*% solve(b) %*% b1
  
  temp1 <- 2*sum(log(diag(temp)))
  
  tempp=chol(b)
  
  temp2=2*sum(log(diag(tempp)))
  
  a=forwardsolve(t(temp), r)
  b=t(r) %*% backsolve(temp, a)
  
  temp4 <- (temp1+temp2+b)/2
  cat("reml loglikelihood",-temp4,"\n")
  
  return(temp4)
}

ini=c(0,5,0,-5)

fit.reml=nlm(REML.l.matern, ini,print.level=2,iterlim=10000)