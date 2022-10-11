#Time at Depth plots -------

dir <- "./0. Data/TimeAtDepth/"

library(tidyverse)
library(scales)
library(lubridate)
library(sp)
library(maptools)
library(viridis)
library(ggpointdensity)

## Custom theme for plots ----
theme_mw <- function () { 
  theme_bw(base_size=10, base_family="") %+replace% 
    theme(
      axis.text = element_text(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA)
    )
}

# Read in PANGAEA data ----

{
  fn<-"S552016_ros_a_f_12_DTD.tab"
  R1<-read.table(file=paste0(dir,fn), header=TRUE, skip=17, sep="\t")
  head(R1)
  R1$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R1$Date.Time<- as.POSIXct(strptime(as.character(R1$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R1$Date.Time[1:5] #Is it okay now????
  R1$id<-"RossF_12"
  
  
  fn<-"S552016_ros_a_f_02_DTD.tab"
  R2<-read.table(file=paste0(dir,fn), header=TRUE, skip=17, sep="\t")
  head(R2)
  R2$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R2$Date.Time<- as.POSIXct(strptime(as.character(R2$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R2$Date.Time[1:5] #Is it okay now????
  R2$id<-"RossF_2"
  
  fn<-"S552016_ros_a_f_18_DTD.tab"
  R3<-read.table(file=paste0(dir,fn), header=TRUE, skip=17, sep="\t")
  head(R3)
  R3$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R3$Date.Time<- as.POSIXct(strptime(as.character(R3$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R3$Date.Time[1:5] #Is it okay now????
  R3$id<-"RossF_18"
  
  fn<-"PS1112018_ros_a_f_01_DTD.tab"
  R4<-read.table(file=paste0(dir,fn), header=TRUE, skip=18, sep="\t")
  head(R4)
  R4$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R4$Date.Time<- as.POSIXct(strptime(as.character(R4$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R4$Date.Time[1:5] #Is it okay now????
  R4$id<-"RossF_22"
  
  fn<-"PS1112018_ros_a_m_02_DTD.tab"
  R5<-read.table(file=paste0(dir,fn), header=TRUE, skip=18, sep="\t")
  head(R5)
  R5$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R5$Date.Time<- as.POSIXct(strptime(as.character(R5$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R5$Date.Time[1:5] #Is it okay now????
  R5$id<-"RossM_23"
}

TAD <- rbind(R1,R2,R3,R4,R5)
head(TAD)
colnames(TAD) <- c("gmt","lat","lon","WaterDep", "TimeatDep","lc","id")
head(TAD)

# DIELPREP ----

diel.prep <- function(theData) {
  
  theData$gmt<-as.POSIXct(strptime(as.character(theData$gmt),'%Y-%m-%d %H:%M:%S'),tz="GMT")
  
  theData$LOC.DATE <- theData$gmt+(theData$lon/15)*3600
  theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$LOC.DATE, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
  
  theData$JDay <- as.numeric(format(theData$LOC.DATE, '%j'))
  
  theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),dateTime=theData$gmt, direction='sunrise', POSIXct.out=T)$day_frac*24
  theData$sunrise.hr <- theData$sunrise.hr+(theData$lon/15)
  
  theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]), dateTime=theData$LOC.DATE, direction='sunset', POSIXct.out=T)$day_frac*24
  theData$sunset.hr <- theData$sunset.hr+(theData$lon/15)
  
  theData$dusk.hr <- crepuscule(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]), dateTime=theData$LOC.DATE, solarDep = 12, direction='dusk', POSIXct.out=T)$day_frac*24
  theData$dusk.hr <- theData$dusk.hr+(theData$lon/15)
  
  
  theData$dawn.hr <- crepuscule(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]), dateTime=theData$LOC.DATE,solarDep = 12, direction='dawn', POSIXct.out=T)$day_frac*24
  theData$dawn.hr <- theData$dawn.hr+(theData$lon/15)
  theData
}

ho2 <- diel.prep(TAD)
head(ho2)
summary(ho2$dusk.hr)
summary(ho2$dawn.hr)
TAD <- ho2
str(ho2)
ho2$LOC.DATE[1:5]

rm(ho2, R1,R2,R3,R4,R5,fn,diel.prep)

x.lt <-as.POSIXlt(TAD$LOC.TIME)
TAD$LOC.DEC.TIME <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600
head(TAD)
x.lt[1:10]
rm(x.lt)


TAD$daynight <- ifelse(is.na(TAD$sunrise.hr),"day",ifelse(TAD$LOC.DEC.TIME > TAD$sunset.hr | TAD$LOC.DEC.TIME < TAD$sunrise.hr,"night","day"))
table(TAD$daynight)

head(TAD)
tail(TAD)
TAD[450:500,]

head(TAD)

TAD$maand <- month(TAD$LOC.DATE)
TAD$jaar <- year(TAD$LOC.DATE)


dat <- droplevels(subset(TAD,TAD$TimeatDep > 0.5))#Dives longer than 30 sec
qplot(TimeatDep,data=dat,geom="histogram")+theme_bw()
dat2 <- droplevels(subset(dat,dat$WaterDep >5)) #Dives deeper than 5 meters 

ggplot(dat2, aes(x=LOC.TIME, y=TimeatDep)) +
  geom_bin2d(bins = 30) + #geom_bin2d
  scale_fill_continuous(type = "viridis") +
  theme_mw()+
  facet_wrap(~id)+
  # scale_y_reverse()+
  ylab("Time at depth (min/hr)") +
  xlab("Local apparent time")+
  labs(fill = "Count")+
  scale_x_datetime( breaks = date_breaks("6 hours"),date_labels = "%H:%M")

pdf(paste0(getwd(),"/Plots/WaterDep_TimeatDep_boxplot.pdf"),width= 8, height = 5)
(p1 <- ggplot(dat2, aes(x=factor(WaterDep), y=TimeatDep)) +
    geom_boxplot()+
    # scale_fill_continuous(type = "viridis") +
    theme_mw()+
    facet_wrap(~id)+
    # scale_y_continuous(limits = quantile(dat2$TimeatDep, c(0.1, 0.9)))+
    ylab("Time (min)") +
    xlab("Water Depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 14)))
dev.off()

png(file=paste0(getwd(),"/Plots/WaterDep_TimeatDep_boxplot.png"),w=2400,h=1800, res=300)
print(p1)
dev.off()

#AntSci format ----
ggsave("Fig4_WaterDep_TimeatDep_boxplot.tiff",  plot = p1,
       width =9, height = 7.5, dpi=1200) 




