# STAT 647 Final Team Project

#library(tmap)
library(dplyr)
library(fields)
library(geoR)
#load data

#cor(madrid_june_01[2:14], use="pairwise.complete.obs")

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
#make 15 additional days 
madrid_june_02 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-02")),]
madrid_june_03 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-03")),]
madrid_june_04 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-04")),]
madrid_june_05 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-05")),]
madrid_june_06 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-06")),]
madrid_june_07 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-07")),]
madrid_june_08 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-08")),]
madrid_june_09 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-09")),]
madrid_june_10 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-10")),]
madrid_june_11 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-11")),]
madrid_june_12 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-12")),]
madrid_june_13 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-13")),]
madrid_june_14 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-14")),]
madrid_june_15 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-15")),]
madrid_june_16 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-16")),]
#get some measurements from before june, to limit the number of missing
madrid_june_31 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-05-31")),]
madrid_june_30 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-05-30")),]
madrid_june_29 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-05-29")),]
madrid_june_28 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-05-28")),]
madrid_june_27 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-05-27")),]
madrid_june_26 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-05-26")),]
madrid_june_25 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-05-25")),]

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
madrid_air_01 <- merge(madrid_june_01, stations, by="id")
madrid_air_02 <- merge(madrid_june_02, stations, by="id")
madrid_air_03 <- merge(madrid_june_03, stations, by="id")
madrid_air_04 <- merge(madrid_june_04, stations, by="id")
madrid_air_05 <- merge(madrid_june_05, stations, by="id")
madrid_air_06 <- merge(madrid_june_06, stations, by="id")
madrid_air_07 <- merge(madrid_june_07, stations, by="id")
madrid_air_08 <- merge(madrid_june_08, stations, by="id")
madrid_air_09 <- merge(madrid_june_09, stations, by="id")
madrid_air_10 <- merge(madrid_june_10, stations, by="id")
madrid_air_11 <- merge(madrid_june_11, stations, by="id")
madrid_air_12 <- merge(madrid_june_12, stations, by="id")
madrid_air_13 <- merge(madrid_june_13, stations, by="id")
madrid_air_14 <- merge(madrid_june_14, stations, by="id")
madrid_air_15 <- merge(madrid_june_15, stations, by="id")
madrid_air_16 <- merge(madrid_june_16, stations, by="id")
#view the data by date
madrid_air_31 <- merge(madrid_june_31, stations, by="id")
madrid_air_30 <- merge(madrid_june_30, stations, by="id")
madrid_air_29 <- merge(madrid_june_29, stations, by="id")
madrid_air_28 <- merge(madrid_june_28, stations, by="id")
madrid_air_27 <- merge(madrid_june_27, stations, by="id")
madrid_air_26 <- merge(madrid_june_26, stations, by="id")
madrid_air_25 <- merge(madrid_june_25, stations, by="id")

#stack the data together
madrid_air <- rbind(madrid_air_25,
                    madrid_air_26,
                    madrid_air_27,
                    madrid_air_28,
                    madrid_air_29,
                    madrid_air_30,
                    madrid_air_31,
                    madrid_air_01,
                    madrid_air_02,
                    madrid_air_03,
                    madrid_air_04,
                    madrid_air_05,
                    madrid_air_06,
                    madrid_air_07,
                    madrid_air_08,
                    madrid_air_09,
                    madrid_air_10,
                    madrid_air_11)
                    # madrid_air_12,
                    # madrid_air_13,
                    # madrid_air_14,
                    # madrid_air_15,
                    # madrid_air_16)
#count NA's by station ID and date for PM10
#madrid_air %>% filter(group == date & is.na(PM10))
#madrid_air %>% filter(group == id & is.na(PM10))

#On 6/12 the data becomes missing
aggregate(PM10 ~ date, data=madrid_air, function(x) {sum(is.na(x))}, na.action = NULL)
aggregate(PM10 ~ id, data=madrid_air, function(x) {sum(is.na(x))}, na.action = NULL)

#no na
madrid_no_na <- na.omit(madrid_air[,c('lat','lon','PM10')])

#attempting to plot the data 
library(rgdal)
library(maps)
library(ggplot2)
library(ggmap)
library(mapproj)
#this api_key is needed to download maps '
map('state')
#load this from a file instead
fileName <- 'E:/STAT647/TeamProject/api_key.txt'
api_key <- readChar(fileName, file.info(fileName)$size)
get_map(location = "houston",source='osm')
baylor <- qmap(location = "baylor university", zoom = 14, maptype = 15434,
     source = "cloudmade", api_key = api_key)
qmplot(madrid_no_na$lon, madrid_no_na$lat, data = madrid_no_na
       , colour = PM10, size = PM10, darken = .3)

par(mfrow=c(1,1),mai=c(.5,.5,.5,.5))
quilt.plot(madrid_air_01$lon,madrid_air_01$lat,madrid_air_01$PM10)

#AIC function
AIC <- function(mle2,p){
  return(2*p -2*mle2)
}


#Run the non-stationary model

#get distance 

#run the lasso regression
#running the lasso on the full data over many days



### model with spatially varying variance ###

lat <- madrid_air_01[!is.na(madrid_air_01$PM10),'lat']
lon <- madrid_air_01[!is.na(madrid_air_01$PM10),'lon']
PM10 <- madrid_air_01[!is.na(madrid_air_01$PM10),'PM10']
loc0 <- madrid_air_01[!is.na(madrid_air_01$PM10),c('lon','lat')]

#directional variogram
v2 <- variog4(coords = loc0, data = PM10)
plot(v2, main="Directional Variogram of PM10 for Madrid June 01, 2015",
     xlab="Distance, note these distances are not geodesic and therefore are not reasonable",
     ylab="Semi-variance") 
title(main="Directional Variogram of PM10 for Madrid June 01, 2015")

lat.all <- madrid_air[!is.na(madrid_air$PM10),'lat']
lon.all <- madrid_air[!is.na(madrid_air$PM10),'lon']
PM10.all <- madrid_air[!is.na(madrid_air$PM10),'PM10']
loc0.all <- madrid_air[!is.na(madrid_air$PM10),c('lon','lat')]

v2.all <- variog4(coords = loc0.all, data = PM10.all)
plot(v2.all, main="Directional Variogram of PM10 for Madrid May 25 to June 11, 2015",
     xlab="Distance, note these distances are not geodesic and therefore are not reasonable",
     ylab="Semi-variance") 
title(main="Directional Variogram of PM10 for Madrid May 25 to June 11, 2015")

#quilt plot
quilt.plot(madrid_air_01[,c('lon')],madrid_air_01[,c('lat')],madrid_air_01[,c('PM10')])

#variogram
m1 <- lm(PM10 ~ lat + lon, data=madrid_air_01)
summary(m1)
par(mfrow=c(2,2))
plot(m1)

#run ols
loc1 <- madrid_air_01[!is.na(madrid_air_01$PM10),c('lat','lon')]
D <- rdist.earth(madrid_air_01[!is.na(madrid_air_01$PM10),c('lat','lon')], miles=FALSE)

temp <- vgram(D,m1$residuals,N=10,dmax=20) 
plot(temp)
boxplotVGram(temp)
temp$centers

###OLS - Exponential covariance function###
###George OLS
lat <- madrid_air_01[!is.na(madrid_air_01$PM10),'lat']
lon <- madrid_air_01[!is.na(madrid_air_01$PM10),'lon']
PM10 <- madrid_air_01[!is.na(madrid_air_01$PM10),'PM10']
loc0 <- madrid_air_01[,c('lon','lat')]
n <- nrow(D)
SSE.ols1.cv <- rep(NA,n)
krig.ols1.cv <- rep(NA,n)
fit.ols1.pars.cv <- matrix(rep(NA,n*6),ncol=6)
#cross-validated
for (i in 1:n){
#i=12
lat.cv <- lat[-i]
lon.cv <- lon[-i]
PM10.cv <- PM10[-i]
D.cv=rdist.earth(cbind(lat.cv,lon.cv),miles=F)

myreg<-lm(PM10.cv~lat.cv+lon.cv)
residuals<-myreg$resid

temp=vgram(D.cv,residuals,N=7)
d=temp$centers
semi.variogram=temp$stats[2,]

ols1=function(par){
  
  alpha=exp(par[1])
  beta=exp(par[2])
  delta=exp(par[3])
  
  S<-alpha*(1-exp(-d/beta))
  S<-S+delta
  SSE=sum((semi.variogram-S)^2)
  
  return(SSE)
  
}

fit.ols1=nlm(ols1,c(2.812042,1.184425,0), print.level=1, stepmax=2, iterlim=10000)

SSE.ols1.cv[i] <- fit.ols1$minimum
fit.ols1.pars <- exp(fit.ols1$estimate)
fit.ols1.pars.cv[i,c(1,2,3)] <- fit.ols1.pars
fit.ols1.pars.cv[i,c(4,5,6)] <- coef(myreg)

K=fit.ols1.pars[1]*exp(-D.cv/fit.ols1.pars[2])
diag(K) <- diag(K) +fit.ols1.pars[3]
mu=fit.ols1.pars.cv[i,4]+fit.ols1.pars.cv[i,5]*lat[i] +fit.ols1.pars.cv[i,6]*lon[i]
D2=D[-i, i]
k=fit.ols1.pars[1]*exp(-D2/fit.ols1.pars[2])
weight=solve(K) %*% k 
e1=PM10[-i]-mu
krig=t(weight) %*% e1
krig.ols1.cv[i]=krig + mu
}
krig.ols1.cv
fit.ols1.pars.cv
SSE.ols1.cv



### George OLS with Matern ###
######OLS - Matern covariance function######
#cross-validated
SSE.ols2.cv <- rep(NA,n)
krig.ols2.cv <- rep(NA,n)
fit.ols2.pars.cv <- matrix(rep(NA,n*7),ncol=7)
for (i in 1:n){
  #i=12
  lat.cv <- lat[-i]
  lon.cv <- lon[-i]
  PM10.cv <- PM10[-i]
  D.cv=rdist.earth(cbind(lat.cv,lon.cv),miles=F)
  
  myreg<-lm(PM10.cv~lat.cv+lon.cv)
  residuals<-myreg$resid
  
  temp=vgram(D.cv,residuals,N=7)
  d=temp$centers
  semi.variogram=temp$stats[2,]
ols2=function(par){ 
  
  alpha=exp(par[1])
  
  beta=exp(par[2])
  
  nu=5*exp(par[3])/(1+exp(par[3]))
  
  delta=exp(par[4])
  
  S <- alpha*Matern(d, smoothness=nu, range=beta)
  
  S <- S + delta
  
  SSE=sum((semi.variogram-S)^2)
  
  return(SSE)
  
}

fit.ols2=nlm(ols2, c(2, 2.303,  0.4857, -3),print.level=1,stepmax = 2,iterlim=10000)
SSE.ols2.cv[i] <- fit.ols2$minimum
fit.ols2.pars <- fit.ols2$estimate
fit.ols2.pars[c(1,2,4)] <- exp(fit.ols2$estimate[c(1,2,4)])
fit.ols2.pars[3] <- 5*exp(fit.ols2.pars[3])/(1+exp(fit.ols2.pars[3])) 
fit.ols2.pars.cv[i,1:4] <- fit.ols2.pars
fit.ols2.pars.cv[i,5:7] <- coef(myreg)

K=fit.ols2.pars[1]*exp(-D.cv/fit.ols2.pars[2])
diag(K) <- diag(K) +fit.ols1.pars[3]
mu=fit.ols2.pars.cv[i,5]+fit.ols2.pars.cv[i,6]*lat[i] +fit.ols2.pars.cv[i,7]*lon[i]
D2=D[-i, i]
k=fit.ols1.pars[1]*exp(-D2/fit.ols2.pars[2])
weight=solve(K) %*% k 
e1=PM10[-i]-mu
krig=t(weight) %*% e1
krig.ols2.cv[i]=krig + mu
}
krig.ols2.cv
fit.ols2.pars.cv
SSE.ols2.cv

#Isotropic Matern
#LOOCV requires a nested loop
D <- rdist.earth(madrid_air_01[!is.na(madrid_air_01$PM10),c('lat','lon')], miles=FALSE)
n <- nrow(D)
mle.cv <- rep(NA,n)
aic.cv <- rep(NA,n)
krig.cv <- rep(NA,n)
iso.par.cv <- matrix(rep(NA,n*7),ncol=7)
#i <- 1
for (i in 1:nrow(D)){
lat.cv <- lat[-i]
lon.cv <- lon[-i]
PM10.cv <- PM10[-i]
D.cv <- rdist.earth(cbind(lat.cv,lon.cv),miles=FALSE)

par=c( 2,2.3,0,-3304.78, 85.10, 30.45)
mle.1=function(par){
  
  alpha=exp(par[1])
  beta=exp(par[2])
  nu=5*exp(par[3])/(1+exp(par[3]))
  
  #nug=exp(par[4])
  
  b0=par[5]
  b1=par[6]
  b2=par[7]
  
  S=alpha*Matern(D.cv, smoothness=nu, range=beta)
  #diag(S)= diag(S)+ nug
  
  mu=b0+b1*lat.cv +b2*lon.cv
  
  temp=(determinant(S, logarithm=T)$modulus 
        + t(PM10.cv-mu) %*% chol2inv(chol(S)) %*% (PM10.cv-mu))/2
  return(temp)
}
ini=c( 3,2.3,17,0,-3304.78, 85.10, 30.45)
fit.mle1=nlm(mle.1, ini,print.level=1,stepmax=2, iterlim=10000)

#parameter estimates
back.mle2 <- function(par){
  alpha=exp(par[1])
  beta=exp(par[2])
  nu=3*exp(par[3])/(1+exp(par[3]))
  nug=exp(par[4])
  b0=par[5]
  b1=par[6]
  b2=par[7]
  p_list <- c(alpha, beta, nu,nug, b0, b1, b2)
  return(p_list)
}
iso.par <- back.mle2(fit.mle1$estimate)
names(iso.par) <- c('alpha','beta','nu','nugget','b0','b1','b2')
print(iso.par)
iso.par.cv[i,] <- iso.par

#MLE
(mle.cv[i] <- -1/2*log(2*pi) + fit.mle1$minimum)

#AIC 
(aic.cv[i]  <- AIC(mle.cv[i],7))

#Kriging estimates
K=iso.par[1]*Matern(D.cv, smoothness=iso.par[3], range=iso.par[2])
mu=iso.par[5]+iso.par[6]*lat[i] +iso.par[7]*lon[i]
D2=D[-i, i]
k=iso.par[1]*Matern(D2, smoothness=iso.par[3], range=iso.par[2])
weight=solve(K) %*% k 
e1=PM10[-i]-mu
krig=t(weight) %*% e1
krig.cv[i]=krig + mu
}
krig.cv
iso.par.cv
mle.cv
aic.cv

# run with REML instead?
D <- rdist.earth(madrid_air_01[!is.na(madrid_air_01$PM10),c('lat','lon')], miles=FALSE)
n <- nrow(D)
mle.sp.cv <- rep(NA,n)
aic.sp.cv <- rep(NA,n)
krig.sp.cv <- rep(NA,n)
sp.par.cv <- matrix(rep(NA,n*8),ncol=8)
for (i in 1:n){
  lat.cv <- lat[-i]
  lon.cv <- lon[-i]
  PM10.cv <- PM10[-i]
#spatially varying model

mle3=function(par){ 
  
  alpha=exp(par[1])
  beta=exp(par[2])
  nu=3*exp(par[3])/(1+exp(par[3]))
  nug=par[4]
  
  b0=par[5]
  b1=par[6]
  b2=par[7]
  
  #vary over longitude
  alpha=alpha + exp(par[8]) * lon.cv
  alpha= matrix(alpha, ncol=1) %*% matrix(alpha, nrow=1)
  
  S=alpha*Matern(D.cv, smoothness=nu, range=beta)
  diag(S)=diag(S)+nug
  
  mu=b0+b1*lat.cv + b2*lon.cv
  
  temp=(determinant(S, logarithm=T)$modulus + t(PM10.cv-mu) %*% chol2inv(chol(S)) %*% (PM10.cv-mu))/2
  return(temp)
}

ini=c( 1.694,2.3,17,0,-3304,85,30,0)
fit.mle3=nlm(mle3, ini,print.level=1,iterlim=10000,stepmax=2)

#parameter estimates
back.mle2 <- function(par){
  alpha=exp(par[1])
  beta=exp(par[2])
  nu=3*exp(par[3])/(1+exp(par[3]))
  nug=exp(par[4])
  b0=par[5]
  b1=par[6]
  b2=par[7]
  var=exp(par[8])
  p_list <- c(alpha, beta, nu, nug, b0, b1, b2, var)
  return(p_list)
}
spatio.par <- back.mle2(fit.mle3$estimate)
#names(spatio.par) <- c('alpha','beta','nu','b0','b1','b2', 'spatial var')
#print(spatio.par)
print(i)
sp.par.cv[i,] <- spatio.par

#MLE
(mle.sp.cv[i] <- -1/2*log(2*pi) + fit.mle3$minimum)

#AIC 
(aic.sp.cv[i] <- AIC(mle.sp.cv[i],7))

## spatially varying variance
par <- spatio.par
alpha=par[1] + exp(par[8]) * lon[-i]
alpha= matrix(alpha, ncol=1) %*% matrix(alpha, nrow=1)
K=alpha*Matern(D.cv, smoothness=par[3], range=par[2])
diag(K) <- diag(K) + par[4]
mu=par[5]+par[6]*lat[i] +par[7]*lon[i]
D2=D[-i, i]
alpha1=par[1]+par[8] * lon[-i]
alpha2=par[1]+ par[8] * lon[i]
alpha= matrix(alpha1, ncol=1) %*% matrix(alpha2, nrow=1)
k=alpha*Matern(D2, smoothness=par[3], range=par[2])
weight=solve(K) %*% k 
e1=PM10[-i]-mu
krig=t(weight) %*% e1
krig.sp.cv[i]=krig + mu
}
krig.sp.cv
sp.par.cv

### Space-Time Model
### Do k-fold cross validation in this case
### Switch to REML because MLE is having trouble with this data
# Matern 
final <- madrid_air[!is.na(madrid_air$PM10),c('lat','lon','PM10','date')]

x = final$lon
y = final$lat
#sp <- unique(final[,c('lat','lon')])
D1 = rdist.earth(cbind(x,y), miles = F)
#tim <- unique(final$date)
D2 = rdist(final$date)
z = as.matrix(final$PM10,ncol=1)
par = c(1.694,2.3,1,-.5,-3000,80,30,0,0,0)

n <- nrow(final)
M= cbind(rep(1,n), x, y)
y1=(diag(1, n,n)-M %*% solve(t(M) %*% M) %*% t(M)) %*% z

# run 6 fold cross validation on the stations, 12 is an even multiple of 6
krig.reml.cv <- rep(NA,n)
reml.matern.mle.cv <- rep(NA,6)
reml.matern.par.cv <- matrix(rep(NA,6*10),ncol=10)
for (i in 1:6){
mask <- seq(from = i, to = n, by=6)
final.cv <- final[-mask,]
x.cv <- final.cv$lat
y.cv <- final.cv$lon
D1.cv = rdist.earth(cbind(x.cv,y.cv),miles=F)
D2.cv = rdist(final.cv$date)
z.cv = as.matrix(final.cv$PM10,ncol=1)
n.cv <- nrow(final.cv)
M.cv = cbind(rep(1,n.cv), x.cv, y.cv)
y1.cv=(diag(1, n.cv,n.cv)-M.cv %*% solve(t(M.cv) %*% M.cv) %*% t(M.cv)) %*% z.cv
#par <- c(2.34,-.630,1.119,10,1.935,3.53,.889)
reml=function(par){
  
  alpha = exp(par[1])
  beta1 = exp(par[2])
  beta2 = exp(par[3])
  nu = 5*exp(par[4])/(1+exp(par[4]))

  nugget_st = exp(par[5])
  nugget_t = exp(par[6])
  nugget_s = exp(par[7])
  
  D = sqrt((D1.cv/beta1)^2+(D2.cv/beta2)^2)
  
  S = alpha*Matern(D, smoothness=nu)
  diag(S)=diag(S)+nugget_st
  S[which(D2.cv == 0)]<-S[which(D2.cv == 0)]+nugget_t
  S[which(D1.cv == 0)]<-S[which(D1.cv == 0)]+nugget_s
  
  temp1=determinant(S, logarithm=T)$modulus
  S_inv <- chol2inv(chol(S))
  temp2=determinant((t(M.cv) %*% S_inv %*% M.cv), logarithm=T)$modulus
  
  temp3= t(y1.cv) %*% (S_inv - S_inv %*% M.cv %*% 
                      solve( t(M.cv) %*% S_inv %*% M.cv) 
                    %*% t(M.cv) %*% S_inv) %*% y1.cv
  
  temp=temp1+temp2+temp3
  return(temp)
}

fit=nlm(reml, c(2.34,-.630,1.119,10,1.935,3.53,.889), print.level=1, stepmax=5)
#make sure to run this sequentially par is defined above as well. 
#I will try to clean code soon
reml.matern.par <- fit$estimate
reml.matern.mle.cv[i] <- fit$minimum
reml.matern.par[c(1,2,3,4,6,7)] <- exp(reml.matern.par[c(1,2,3,5,6,7)])
reml.matern.par[4] <- 5*exp(reml.matern.par[4])/(1+exp(reml.matern.par[4]))
reml.matern.par.cv[i,1:7] <- reml.matern.par

D.cv = sqrt((D1.cv/reml.matern.par[2])^2+(D2.cv/reml.matern.par[3])^2)

K = reml.matern.par[1]*Matern(D.cv, smoothness=reml.matern.par[4])
diag(K)=diag(K)+reml.matern.par[5]
K[which(D2.cv == 0)]<-K[which(D2.cv == 0)]+reml.matern.par[6]
K[which(D1.cv == 0)]<-K[which(D1.cv == 0)]+reml.matern.par[7]

reg.beta= solve(t(M.cv) %*% solve(K) %*% M.cv) %*% 
  t(M.cv) %*% solve(K) %*% z.cv
reml.matern.par.cv[i,8:10] <- reg.beta

### Run the Kriging

mu=reg.beta[1]+reg.beta[2]*final[-mask,'lat'] +reg.beta[3]*final[-mask,'lon']
D3 = sqrt((D1/reml.matern.par[2])^2+(D2/reml.matern.par[3])^2)
D3=D3[-mask, mask]
k=reml.matern.par[1]*Matern(D3, smoothness=reml.matern.par[4])
weight=solve(K) %*% k 
e1=final[-mask,'PM10']-mu
krig=t(weight) %*% e1
mu.new=reg.beta[1]+reg.beta[2]*final[mask,'lat'] +reg.beta[3]*final[mask,'lon']
krig.reml.cv[mask]=krig + mu.new
}
krig.reml.cv
reml.matern.mle.cv
reml.matern.par.cv

### Exponential Spatial Distance Model ###
krig.reml2.cv <- rep(NA,n)
reml.exp.par.cv <- matrix(rep(NA,6*9),ncol=9)
reml.exp.mle.cv <- rep(NA,6)
for (i in 1:6){
  #i=1
  mask <- seq(from = i, to = n, by=6)
  final.cv <- final[-mask,]
  x.cv <- final.cv$lat
  y.cv <- final.cv$lon
  D1.cv = rdist.earth(cbind(x.cv,y.cv),miles=F)
  D2.cv = rdist(final.cv$date)
  z.cv = as.matrix(final.cv$PM10,ncol=1)
  n.cv <- nrow(final.cv)
  M.cv = cbind(rep(1,n.cv), x.cv, y.cv)
  y1.cv=(diag(1, n.cv,n.cv)-M.cv %*% solve(t(M.cv) %*% M.cv) %*% t(M.cv)) %*% z.cv
#par <- c(2.34,-.630,1.119,1.935,3.53,.889)
reml.exp=function(par){
  alpha = exp(par[1])
  beta1 = exp(par[2])
  beta2 = exp(par[3])
  nugget_st = exp(par[4])
  nugget_t = exp(par[5])
  nugget_s = exp(par[6])
  
  D = sqrt((D1.cv/beta1)^2+(D2.cv/beta2)^2)
  
  S = alpha*exp(-D)
  diag(S)=diag(S)+nugget_st
  S[which(D2.cv == 0)]<-S[which(D2.cv == 0)]+nugget_t
  S[which(D1.cv == 0)]<-S[which(D1.cv == 0)]+nugget_s
  
  temp1=determinant(S, logarithm=T)$modulus
  S_inv <- chol2inv(chol(S))
  temp2=determinant((t(M.cv) %*% S_inv %*% M.cv), logarithm=T)$modulus
  
  temp3= t(y1.cv) %*% (S_inv - S_inv %*% M.cv %*% 
                      solve( t(M.cv) %*% S_inv %*% M.cv) 
                    %*% t(M.cv) %*% S_inv) %*% y1.cv
  
  temp=temp1+temp2+temp3
  return(temp)
}

fit.exp=nlm(reml.exp, c(2.34,-.630,1.119,1.935,3.53,.889), print.level=1, stepmax=2)
#make sure to run this sequentially par is defined above as well. 
#I will try to clean code soon
reml.exp.par <- fit.exp$estimate
reml.exp.par[c(1,2,3,4,5,6)] <- exp(reml.exp.par[c(1,2,3,4,5,6)])
#reml.exp.par[4] <- 5*exp(reml.exp.par[4])/(1+exp(reml.exp.par[4]))
reml.exp.par.cv[i,1:6] <- reml.exp.par
reml.exp.mle.cv[i] <- fit.exp$minimum

D.cv = sqrt((D1.cv/reml.exp.par[2])^2+(D2.cv/reml.exp.par[3])^2)

K = reml.exp.par[1]*exp(-D.cv)
diag(K)=diag(K)+reml.exp.par[4]
K[which(D2.cv == 0)]<-K[which(D2.cv == 0)]+reml.exp.par[5]
K[which(D1.cv == 0)]<-K[which(D1.cv == 0)]+reml.exp.par[6]

reg.beta= solve(t(M.cv) %*% solve(K) %*% M.cv) %*% 
  t(M.cv) %*% solve(K) %*% z.cv
reml.exp.par.cv[i,7:9] <- reg.beta

### Run the Kriging

mu=reg.beta[1]+reg.beta[2]*final[-mask,'lat'] +reg.beta[3]*final[-mask,'lon']
D3 = sqrt((D1/reml.exp.par[2])^2+(D2/reml.exp.par[3])^2)
D3=D3[-mask, mask]
k=reml.exp.par[1]*Matern(D3, smoothness=reml.exp.par[4])
weight=solve(K) %*% k 
e1=final[-mask,'PM10']-mu
krig=t(weight) %*% e1
mu.new=reg.beta[1]+reg.beta[2]*final[mask,'lat'] +reg.beta[3]*final[mask,'lon']
krig.reml2.cv[mask]=krig + mu.new
}
krig.reml2.cv
reml.exp.par.cv
reml.exp.mle.cv


##### Fitted Parameter Value Comparison #####
ols1.pars <- colMeans(fit.ols1.pars.cv)
names(ols1.pars) <- c('alpha','beta','nugget','b0','b1','b2')
ols2.pars <- colMeans(fit.ols2.pars.cv)
matrix(colMeans(fit.ols2.pars.cv[-10,]),ncol=1)
names(ols2.pars) <- c('alpha','beta','nu','nugget','b0','b1','b2')
iso.matern.pars <- colMeans(iso.par.cv)
names(iso.matern.pars) <- c('alpha','beta','nu','nugget','b0','b1','b2')
sp.pars <- colMeans(sp.par.cv)
names(sp.pars) <- c('alpha','beta','nu','nugget','b0','b1','b2','spatial variation')
time1.pars <- colMeans(reml.matern.par.cv)
names(time1.pars) <- c('alpha','beta1','beta2','nu','nugget_st'
                       ,'nugget_t','nugget_s','b0','b1','b2')
time2.pars <- colMeans(reml.exp.par.cv)
names(time2.pars) <- c('alpha','beta1','beta2','nugget_st'
                       ,'nugget_t','nugget_s','b0','b1','b2')
matrix(colMeans(reml.exp.par.cv[c(-1,-4),]),ncol=1)
t(reml.exp.par.cv)

##### Likelihood and SSE Comparisons #####
mean(SSE.ols1.cv)
mean(SSE.ols2.cv)
mean(mle.cv)
mean(mle.sp.cv)

#MLE
(mle.sp.cv[i] <- -1/2*log(2*pi) + fit.mle3$minimum)
mean(-1/2*log(2*pi) + reml.matern.mle.cv)
mean(-1/2*log(2*pi) + reml.exp.mle.cv)

#AIC 

mean(aic.cv)
mean(aic.sp.cv)
mean(AIC(-1/2*log(2*pi) + reml.matern.mle.cv,7))
mean(AIC(-1/2*log(2*pi) + reml.exp.mle.cv,6))

### Compare via AIC ###

##### kriging results comparison  #####

MSPE.ols1=mean((PM10-krig.ols1.cv)^2)
MAE.ols1=mean(abs(PM10-krig.ols1.cv))

MSPE.ols2=mean((PM10-krig.ols2.cv)^2)
MAE.ols2=mean(abs(PM10-krig.ols2.cv))

MSPE.mle1=mean((PM10-krig.cv)^2)
MAE.mle1=mean(abs(PM10-krig.cv))

MSPE.mle2=mean((PM10-krig.sp.cv)^2)
MAE.mle2=mean(abs(PM10-krig.sp.cv))

#Kriging from the time series model on June 01
mask2 <- which(final$date == '2015-06-01')
k_vals <- krig.reml.cv[mask2]
k_vals2 <- krig.reml2.cv[mask2]

#just on june 01
MSPE.reml1=mean((PM10-k_vals)^2)
MAE.reml1=mean(abs(PM10-k_vals))

MSPE.reml2=mean((PM10-k_vals2)^2)
MAE.reml2=mean(abs(PM10-k_vals2))

#all_dates
MSPE.reml1.all=mean((final$PM10-krig.reml.cv)^2)
MAE.reml1.all=mean(abs(final$PM10-krig.reml.cv))

MSPE.reml2.all=mean((final$PM10-krig.reml2.cv)^2)
MAE.reml2.all=mean(abs(final$PM10-krig.reml2.cv))

MSPE <- c(MSPE.ols1, MSPE.ols2,MSPE.mle1,MSPE.mle2
          ,MSPE.reml1,MSPE.reml2,MSPE.reml1.all,MSPE.reml2.all)
MAE <- c(MAE.ols1, MAE.ols2,MAE.mle1,MAE.mle2
         ,MAE.reml1,MAE.reml2,MAE.reml1.all,MAE.reml2.all)
cbind(MSPE,MAE)
## Very bad Kriging results from running REML

## Collect Kriging Results in a table 

ols.exp <- krig.ols1.cv
ols.matern <- krig.ols2.cv
iso.matern <- krig.cv
sp.matern <- krig.sp.cv
st.matern <- k_vals2

krig.results<-data.frame(cbind(lat,lon,PM10,ols.exp
                               ,ols.matern,iso.matern,sp.matern,st.matern))
krig.results2 <- data.frame(krig.results[1:2], stack(krig.results[3:ncol(krig.results)]))

library(ggplot2)
ggplot(krig.results2,aes(lon,lat))+ geom_point() +
  geom_jitter(aes(colour = values,shape=ind),width=.01)+
  scale_colour_gradient2(low = "blue", mid = "yellow",
                         high = "red", midpoint=26) + 
  ggtitle("PM10 Concentrations compared to Krigged Values") +
  labs(x="Longitude",y="Latitude")

ggplot(krig.results2,aes(lon,lat,shape=V3))+ geom_point() +
  geom_jitter()+
  scale_colour_gradient2(low = "blue", mid = "yellow",
                         high = "red", midpoint=26)

### Plot kriging result comparisons ###


#compare fitted variogram
library(gstat)
