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
                    madrid_air_11,
                    madrid_air_12,
                    madrid_air_13,
                    madrid_air_14,
                    madrid_air_15,
                    madrid_air_16)
#count NA's by station ID and date for PM10
#madrid_air %>% filter(group == date & is.na(PM10))
#madrid_air %>% filter(group == id & is.na(PM10))

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

get_map(location = "houston")
baylor <- qmap(location = "baylor university", zoom = 14, maptype = 15434,
     source = "cloudmade", api_key = api_key)
qmplot(madrid_no_na$lon, madrid_no_na$lat, data = madrid_no_na
       , colour = PM10, size = PM10, darken = .3)
