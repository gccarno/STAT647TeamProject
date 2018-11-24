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



#Run the non-stationary model

#get distance 

#run the lasso regression
#running the lasso on the full data over many days



### model with spatially varying variance ###

lat <- madrid_air_01[!is.na(madrid_air_01$PM10),'lat']
lon <- madrid_air_01[!is.na(madrid_air_01$PM10),'lon']
PM10 <- madrid_air_01[!is.na(madrid_air_01$PM10),'PM10']
loc0 <- madrid_air_01[,c('lon','lat')]

#quilt plot
quilt.plot(madrid_air_01[,c('lon')],madrid_air_01[,c('lat')],madrid_air_01[,c('PM10')])

#variogram
m1 <- lm(PM10 ~ lat + lon, data=madrid_air_01)
summary(m1)
plot(m1)

#run ols
loc1 <- madrid_air_01[!is.na(madrid_air_01$PM10),c('lat','lon')]
D <- rdist.earth(madrid_air_01[!is.na(madrid_air_01$PM10),c('lat','lon')], miles=FALSE)

temp <- vgram(D,m1$residuals,N=10,dmax=20) 
plot(temp)
boxplotVGram(temp)

#Isotropic Matern
#LOOCV requires a nested loop
D <- rdist.earth(madrid_air_01[!is.na(madrid_air_01$PM10),c('lat','lon')], miles=FALSE)
n <- nrow(D)
mle.cv <- rep(NA,n)
aic.cv <- rep(NA,n)
krig.cv <- rep(NA,n)

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

# run with REML instead?
D <- rdist.earth(madrid_air_01[!is.na(madrid_air_01$PM10),c('lat','lon')], miles=FALSE)
n <- nrow(D)
mle.sp.cv <- rep(NA,n)
aic.sp.cv <- rep(NA,n)
krig.sp.cv <- rep(NA,n)
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
names(spatio.par) <- c('alpha','beta','nu','b0','b1','b2', 'spatial var')
print(spatio.par)

#MLE
(mle.sp.cv[i] <- -1/2*log(2*pi) + fit.mle3$minimum)

#AIC 
(aic.sp.cv[i] <- AIC(mle.sp.cv[i],7))

## spatially varying variance
par <- spatio.par
alpha=alpha + exp(par[8]) * lon[-i]
alpha= matrix(alpha, ncol=1) %*% matrix(alpha, nrow=1)
K=alpha*Matern(D.cv, smoothness=par[3], range=par[2])
diag(K) <- diag(K) + par[4]
mu=par[5]+par[6]*lat[i] +par[7]*lon[i]
D2=D[-i, i]
alpha1=exp(par[1])+exp(par[8]) * lon[-i]
alpha2=exp(par[1])+ exp(par[8]) * lon[i]
alpha= matrix(alpha1, ncol=1) %*% matrix(alpha2, nrow=1)
k=alpha*Matern(D2, smoothness=par[3], range=par[2])
weight=solve(K) %*% k 
e1=PM10[-i]-mu
krig=t(weight) %*% e1
krig.sp.cv[i]=krig + mu
}
krig.sp.cv

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

reml=function(par){
  
  alpha = exp(par[1])
  beta1 = exp(par[2])
  beta2 = exp(par[3])
  nu = 5*exp(par[4])/(1+exp(par[4]))

  nugget_st = exp(par[5])
  nugget_t = exp(par[6])
  nugget_s = exp(par[7])
  
  D = sqrt((D1/beta1)^2+(D2/beta2)^2)
  
  S = alpha*Matern(D, smoothness=nu)
  diag(S)=diag(S)+nugget_st
  S[which(D2 == 0)]<-S[which(D2 == 0)]+nugget_t
  S[which(D1 == 0)]<-S[which(D1 == 0)]+nugget_s
  
  temp1=determinant(S, logarithm=T)$modulus
  S_inv <- chol2inv(chol(S))
  temp2=determinant((t(M) %*% S_inv %*% M), logarithm=T)$modulus
  
  temp3= t(y1) %*% (S_inv - S_inv %*% M %*% 
                      solve( t(M) %*% S_inv %*% M) 
                    %*% t(M) %*% S_inv) %*% y1
  
  temp=temp1+temp2+temp3
  return(temp)
}

fit=nlm(reml, c(2.34,-.630,1.119,10,1.935,3.53,.889), print.level=2, stepmax=5)
#make sure to run this sequentially par is defined above as well. 
#I will try to clean code soon
par <- fit$estimate
alpha = exp(par[1])
beta1 = exp(par[2])
beta2 = exp(par[3])
nu = 5*exp(par[4])/(1+exp(par[4]))

nugget_st = exp(par[5])
nugget_t = exp(par[6])
nugget_s = exp(par[7])

D = sqrt((D1/beta1)^2+(D2/beta2)^2)

K = alpha*Matern(D, smoothness=nu)
diag(K)=diag(K)+nugget_st
K[which(D2 == 0)]<-K[which(D2 == 0)]+nugget_t
K[which(D1 == 0)]<-K[which(D1 == 0)]+nugget_s

K=exp(-D/beta)*alpha

reg.beta= solve(t(M) %*% solve(K) %*% M) %*% 
  t(M) %*% solve(K) %*% z

reml.exp=function(par){
  alpha = exp(par[1])
  beta1 = exp(par[2])
  beta2 = exp(par[3])
  nugget_st = exp(par[4])
  nugget_t = exp(par[5])
  nugget_s = exp(par[6])
  
  D = sqrt((D1/beta1)^2+(D2/beta2)^2)
  
  S = alpha*exp(-D)
  diag(S)=diag(S)+nugget_st
  S[which(D2 == 0)]<-S[which(D2 == 0)]+nugget_t
  S[which(D1 == 0)]<-S[which(D1 == 0)]+nugget_s
  
  temp1=determinant(S, logarithm=T)$modulus
  S_inv <- chol2inv(chol(S))
  temp2=determinant((t(M) %*% S_inv %*% M), logarithm=T)$modulus
  
  temp3= t(y1) %*% (S_inv - S_inv %*% M %*% 
                      solve( t(M) %*% S_inv %*% M) 
                    %*% t(M) %*% S_inv) %*% y1
  
  temp=temp1+temp2+temp3
  return(temp)
}

fit.exp=nlm(reml.exp, c(2.34,-.630,1.119,1.935,3.53,.889), print.level=2, stepmax=2)
#make sure to run this sequentially par is defined above as well. 
#I will try to clean code soon
par <- fit.exp$estimate
alpha = exp(par[1])
beta1 = exp(par[2])
beta2 = exp(par[3])
nugget_st = exp(par[4])
nugget_t = exp(par[5])
nugget_s = exp(par[6])

D = sqrt((D1/beta1)^2+(D2/beta2)^2)

K = alpha*exp(-D)
diag(K)=diag(K)+nugget_st
K[which(D2 == 0)]<-K[which(D2 == 0)]+nugget_t
K[which(D1 == 0)]<-K[which(D1 == 0)]+nugget_s

reg.beta.exp= solve(t(M) %*% solve(K) %*% M) %*% 
  t(M) %*% solve(K) %*% z


##### kriging results comparison  #####

MSPE.mle1=mean((PM10-krig.cv)^2)
MAE.mle1=mean(abs(PM10-krig.cv))

MSPE.mle2=mean((PM10-krig.sp.cv)^2)
MAE.mle2=mean(abs(PM10-krig.sp.cv))

### Plot kriging result comparisons ###


#compare fitted variogram
library(gstat)
