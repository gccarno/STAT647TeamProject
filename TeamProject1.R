#Team Project 1
#Greg Carnovale

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
hist(madrid_june_01$PM10,breaks=8,xlim=c(15,40),main="Histogram of PM10 on June 01, 2015", xlab="PM10")
#does not work because of missing values
density(madrid_june_01$PM10,bw="sj")
qqnorm(madrid_june_01$PM10)
qqline(madrid_june_01$PM10)

shapiro.test(madrid_june_01$PM10)

plot(madrid_june_01)

#read in station information
stations <- read.csv(file="E:/STAT647/Homework/Data/stations.csv")

#merge the data
madrid_air <- merge(madrid_june_01, stations, by="id")

par(mfrow=c(1,1))
quilt.plot(madrid_air$lat,madrid_air$lon,madrid_air$PM10)

#construct variograms?
# data is lon, lat
my_vgram <- vgram(madrid_air[,c(17,18)],madrid_air$PM10,lon.lat = TRUE,dmax=6)

#plot my_vgram
plot(my_vgram)

#boxplot vgram

boxplotVGram(my_vgram)
#boxplot(my_vgram$vgram ~ cut(my_vgram$d,6),xlabel=bins)

#Run GeoR
madrid_no_na <- madrid_air[!is.na(madrid_air$PM10),]
v2 <- variog4(coords = madrid_no_na[,c(17,18)], data = madrid_no_na$PM10)
plot(v2) 

