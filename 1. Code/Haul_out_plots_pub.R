# Ross seal time at surface behaviour by Mia Wege (mia.wege@gmail.com)
## Read in AWI Pangea At-surface behaviour DTASR data, rbind, clean and plot ---

library(scales)
library(lubridate)
library(sp)
library(maptools)
library(tidyverse)
library(viridis)
library(ggalt)
library(ggridges)

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(lubridate)

options(tibble.width = Inf)

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
#Read in tracking data ----
tr <- read.csv(paste0(getwd(),"/Ross_SSM_Enviros_forFINALmodels.csv"))
tr <- tr[,c("id","date","lon","lat")]
head(tr)

tr2 <- read.csv(paste0(getwd(),"/Ross_2019_SSM.csv"))
head(tr2)
tr2 <- tr2[,c("id","date","lon","lat")]

tr <- rbind(tr,tr2);rm(tr2)  
str(tr)
head(tr)

tr$date <- as.POSIXct(strptime(as.character(tr$date ), "%Y-%m-%d %H:%M:%S", tz="GMT"))
tr$date[1:5]
tr$id <- as.factor(tr$id)
str(tr)
range(tr$date)
names(tr)[names(tr)=="date"] <- "gmt"

levels(tr$id)

divers <- c("152413", "152414", "152416", "152422", "152423", "164437", "164438", "35940", "35941")

tracks <- droplevels(subset(tr,tr$id %in% divers))
levels(tracks$id)
rm(tr)

#rename factor levels
library(forcats)
tracks$id <- fct_recode(tracks$id, "RossF_18" = "152413", "RossF_12" = "152414", "RossF_2"="152416", "RossM_23" = "35940","RossF_22" =  "35941" , "RossM_21" = "152422","RossF_19" = "152423","RossF_25"="164437","RossF_24"="164438")
levels(tracks$id)
write.csv(tracks,file = paste0(getwd(),"/Ross_Haulout_Divers_SSMTracks.csv"),row.names = FALSE)
saveRDS(tracks, file = paste0(getwd(),"/Ross_Haulout_Divers_SSMTracks.RDS"))

# Read in PANGAEA data ----
{
  fn<-"S552016_ros_a_f_12_DTASR.tab"
  R1<-read.table(file=paste0(getwd(),"/AtSurface/",fn), header=TRUE, skip=15, sep="\t")
  head(R1)
  R1$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R1$Date.Time<- as.POSIXct(strptime(as.character(R1$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R1$Date.Time[1:5] #Is it okay now????
  R1$id<-"RossF_12"
  
  
  fn<-"S552016_ros_a_f_15_DTASR.tab"
  R2<-read.table(file=paste0(getwd(),"/AtSurface/",fn), header=TRUE, skip=15, sep="\t")
  head(R2)
  R2$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R2$Date.Time<- as.POSIXct(strptime(as.character(R2$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R2$Date.Time[1:5] #Is it okay now????
  R2$id<-"RossF_15"
  
  fn<-"S552016_ros_a_f_18_DTASR.tab"
  R3<-read.table(file=paste0(getwd(),"/AtSurface/",fn), header=TRUE, skip=15, sep="\t")
  head(R3)
  R3$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R3$Date.Time<- as.POSIXct(strptime(as.character(R3$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R3$Date.Time[1:5] #Is it okay now????
  R3$id<-"RossF_18"
  
  fn<-"S552016_ros_a_f_19_DTASR.tab"
  R4<-read.table(file=paste0(getwd(),"/AtSurface/",fn), header=TRUE, skip=15, sep="\t")
  head(R4)
  R4$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R4$Date.Time<- as.POSIXct(strptime(as.character(R4$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R4$Date.Time[1:5] #Is it okay now????
  R4$id<-"RossF_19"
  
  fn<-"S552016_ros_a_m_21_DTASR.tab"
  R5<-read.table(file=paste0(getwd(),"/AtSurface/",fn), header=TRUE, skip=15, sep="\t")
  head(R5)
  R5$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
  R5$Date.Time<- as.POSIXct(strptime(as.character(R5$Date.Time),
                                     format = "%Y-%m-%dT%H:%M",tz="GMT"))
  R5$Date.Time[1:5] #Is it okay now????
  R5$id<-"RossM_21"
}

haulout <- rbind(R1,R2,R3,R4,R5)
head(haulout)
colnames(haulout) <- c("gmt","lat","lon","TimeatSurface","lc","id")
head(haulout)

qplot(gmt,TimeatSurface,data=haulout)+theme_bw()+facet_wrap(~id)
qplot(lon,lat,col = TimeatSurface,data=haulout)+theme_bw()+facet_wrap(~id)

#Drop RossF_15
haulout <- droplevels(subset(haulout,haulout$id != "RossF_15"))
unique(haulout$id)
qplot(lon,lat,col = TimeatSurface,data=haulout)+theme_bw()+facet_wrap(~id)

# Add PS111 -----
# fn<-"PS1112018_ros_a_f_01_DSB.tab"
R22 <-read.table("C:/Users/User/OneDrive - University of Pretoria/1. Documents/000. Ross Seal Postdoc/2_Data/10_Ross Other Pangeae Data/TimeAtSurface/PS1112018_ros_a_f_01_DSB.tab", header=TRUE, skip=16, sep="\t")
head(R22)
R22$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
R22$Date.Time<- as.POSIXct(strptime(as.character(R22$Date.Time),
                                    format = "%Y-%m-%dT%H:%M",tz="GMT"))
R22$Date.Time[1:5] #Is it okay now????
R22$id<-"RossF_22"

# fn<-"PS1112018_ros_a_m_02_DSB.tab"
R23<-read.table("C:/Users/User/OneDrive - University of Pretoria/1. Documents/000. Ross Seal Postdoc/2_Data/10_Ross Other Pangeae Data/TimeAtSurface/PS1112018_ros_a_m_02_DSB.tab", header=TRUE, skip=16, sep="\t")
head(R23)
R23$Date.Time[1:5] #See the weird T in the middle. Adjust format accordingly.
R23$Date.Time<- as.POSIXct(strptime(as.character(R23$Date.Time),
                                    format = "%Y-%m-%dT%H:%M",tz="GMT"))
R23$Date.Time[1:5] #Is it okay now????
R23$id<-"RossM_23"

ps <- rbind(R22,R23)
head(ps)

colnames(ps) <- c("gmt","lat","lon","wet","lc","id")

qplot(lon,lat,col = wet,data=ps)+theme_bw()+facet_wrap(~id)
qplot(gmt,wet,data=ps)+theme_bw()+facet_wrap(~id)

ps$tc <- cut(ps$gmt, breaks = "1 hour") 
head(ps)

PS111 <- ps %>% 
  group_by(id,tc) %>%
  summarise(lat=mean(lat,na.rm=TRUE),lon=mean(lon,na.rm=TRUE),lc = first(lc),nr = n(),wet=sum(wet,na.rm = TRUE))%>%
  mutate(TimeSubmerge = (wet/nr)*100, TimeatSurface = 100-TimeSubmerge)

head(PS111)
hist(PS111$TimeatSurface)
head(PS111)
names(PS111)
PS111 <- PS111[,-c(6:8)]
head(PS111)
names(PS111)[names(PS111) == "tc"] <- "gmt"

PS111 <- PS111[,c("gmt","lat","lon","TimeatSurface","lc","id")]
head(PS111)

qplot(gmt,TimeatSurface,data=PS111)+theme_bw()+facet_wrap(~id)
qplot(lon,lat,colour=TimeatSurface,data=test)+theme_bw()+facet_wrap(~id)

str(PS111$gmt)
PS111$gmt <- as.POSIXct(strptime(as.character(PS111$gmt),format = "%Y-%m-%d %H:%M:%S",tz="GMT"))
PS111$gmt[1:5]
rm(test,ps,R22,R23)

#Add SCALE ----
scale <-read.csv(file=paste0(getwd(),"/AtSurface/SCALE_DTASR.csv"))
head(scale)
scale$gmt <- as.POSIXct(strptime(as.character(scale$gmt),format = "%Y-%m-%d %H:%M:%S",tz="GMT"))
scale$gmt[1:5]
range(scale$gmt)
scale$id <- as.factor(scale$id)
levels(scale$id)

qplot(gmt,TimeatSurface,data=scale)+theme_bw()+facet_wrap(~id)
qplot(lon,lat,col = TimeatSurface,data=scale)+theme_bw()+facet_wrap(~id)

#Add Ross 2 -----
Ross2 <-read.csv(file=paste0(getwd(),"/AtSurface/Ross2_DTASR.csv"))
head(Ross2)
Ross2$gmt <- as.POSIXct(strptime(as.character(Ross2$gmt),format = "%Y-%m-%d %H:%M:%S",tz="GMT"))
Ross2$gmt[1:5]
range(Ross2$gmt)
Ross2$id <- as.factor(Ross2$id)
levels(Ross2$id)

qplot(gmt,TimeatSurface,data=Ross2)+theme_bw()+facet_wrap(~id)
qplot(lon,lat,data=Ross2,colour = TimeatSurface) + theme_bw()

# Bind all together
haulout <- rbind(haulout,scale, Ross2, PS111)
rm(scale)
qplot(lon,lat,data=haulout,colour = TimeatSurface) + theme_bw()
qplot(lon,lat,data=haulout,colour = TimeatSurface) + theme_bw()+facet_wrap(~id)

col <- sort(haulout$TimeatSurface)
range(col)
hist(col)
qplot(lon, lat, data=haulout, colour=col) + scale_colour_gradient(low="red", high="blue") + theme_bw()
rm(col)

tapply(haulout$TimeatSurface, haulout$id , summary)

# DIELPREP ----

names(haulout)

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

ho2 <- diel.prep(haulout)
head(ho2)
summary(ho2$dusk.hr)
summary(ho2$dawn.hr)
haulout <- ho2
str(ho2)
ho2$LOC.DATE[1:5]

rm(ho2, R1,R2,R3,R4,R5,fn,diel.prep,Ross2, PS111)


haulout$solarelev <-solarpos(crds=as.matrix(haulout[,match(c('lon', 'lat'), names(haulout))]),haulout$LOC.DATE)[,2]
x.lt <-as.POSIXlt(haulout$LOC.TIME)
haulout$LOC.DEC.TIME <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600
head(haulout)
rm(x.lt)

haulout$daynight <- ifelse(is.na(haulout$sunrise.hr),"day",ifelse(haulout$LOC.DEC.TIME > haulout$sunset.hr | haulout$LOC.DEC.TIME < haulout$sunrise.hr,"night","day"))
head(haulout)
table(haulout$daynight)

head(haulout)
tail(haulout)
haulout[450:500,]
haulout <- droplevels(subset(haulout,haulout$TimeatSurface <= 100))

names(haulout)

haulout$maand <- month(haulout$LOC.DATE)
haulout$jaar <- year(haulout$LOC.DATE)

#Save file ----
write.csv(haulout,file=paste0(getwd(),"/AtSurface/TimeAtSurface_Ross_S55_Scale_PS111.csv"),row.names = FALSE)  # write the file to a csv
head(haulout)


head(haulout)
haulout <- haulout %>% 
  group_by(id,just.date) %>% 
  # nest() %>%
  mutate(tot =n()) %>% 
  filter(tot == 24) %>% 
  droplevels() #%>% 
# unnest(col = c(data))
head(haulout)

summary(haulout$tot)

# Plotting -----
# Density ridges for at surface behaviour ----------

lims <- as.POSIXct(strptime(c("1970-01-01 00:00:00","1970-01-01 23:59:59"), format = "%Y-%m-%d %H:%M:%S"))
str(lims)

dens <- atsurf %>% 
  filter(maand != 5 & maand != 6 & maand != 8) %>% 
  ggplot(., aes(x = LOC.TIME, y = factor(mon))) +
  ggridges::stat_density_ridges(alpha=0.6)+
  theme_mw()+
  xlab("Local apparent time")+
  ylab(NULL)+
  ggplot2::scale_x_datetime(limits = lims, breaks = date_breaks("2 hours"), date_labels = "%H:%M")

pdf(paste0(getwd(),"/2. Results/Plots/Fig5_PercTimeAtSurface_TOD_DensityRidges_month_LATEST.pdf"),width= 9, height = 6)
print(dens)
dev.off()

#AntSci format ----
ggsave("Fig5_PercTimeAtSurface_TOD_DensityRidges_month_LATEST.tiff",  plot = dens,
       width =9, height = 7.5, dpi=1200) 


## BREEDING SEASON -----
BS <- droplevels(subset(BS, BS$just.date <= "1970-12-12"))
head(BS)


unique(haulout$id)
R19 <- droplevels(subset(haulout,haulout$id == "RossF_19"))
R19BS <- droplevels(subset(R19,R19$maand == 10 | R19$maand == 11 | R19$maand == 12)) #R19$maand == 10 | 

R24 <- droplevels(subset(haulout,haulout$id == "RossF_24"))
R24BS <- droplevels(subset(R24,R24$maand == 10 | R24$maand == 11 | R24$maand == 12)) #R19$maand == 10 | 

R25 <- droplevels(subset(haulout,haulout$id == "RossF_25"))
R25BS <- droplevels(subset(R25,R25$maand == 10 | R25$maand == 11| R25$maand == 12)) #R19$maand == 10 | 

RBS <-  droplevels(subset(haulout,haulout$maand == 10 | haulout$maand == 11| haulout$maand == 12))
# qplot(LOC.TIME,TimeatSurface, geom = "line", data=RBS) + theme_mw()

MvsF <- droplevels(subset(haulout,haulout$id == "RossF_19" |haulout$id == "RossM_21" ))
MvsFBS <- droplevels(subset(MvsF,MvsF$maand == 10 | MvsF$maand == 11)) 

BS <- rbind(R19BS,R24BS, R25BS)

str(BS$datums)
BS$just.date <- as.POSIXct(strptime(paste('1970-', format(BS$datums, "%m-%d")), "%Y-%m-%d"), tz='GMT')
BS$just.date[1:5]  

range(BS$just.date)
BS <- droplevels(subset(BS, BS$just.date <= "1970-12-12"))
head(BS)

#Breeding season hauled out proportions ----
foo <- BS %>% 
  group_by(id,just.date) %>% 
  mutate(surfsum =sum(surface,na.rm = TRUE)) %>% 
  mutate(surfprop= surfsum/24) %>% 
  mutate(waterprop=1-surfprop)

summary(foo$waterprop)

BS %>% group_by(id,just.date) %>% summarise(surfsum =sum(surface,na.rm = TRUE)) %>% print(n=Inf)

newBS <- foo %>% 
  gather(type, perc, surfprop:waterprop) 
head(newBS)

head(foo)  
summary(foo$surfsum)
summary(foo$surfprop)
summary(foo$waterprop)

levels(factor(newBS$type))


tyd <- format(newBS$LOC.TIME, format="%H:%M:%S")
tyd[1:10]

as.POSIXct(strptime(as.character(tr$date ), "%Y-%m-%d %H:%M:%S", tz="GMT"))

newBS$datetime <- as.POSIXct(strptime(as.character(paste(newBS$just.date, tyd, sep=" ")),format = "%Y-%m-%d %H:%M:%S", tz="GMT"))
newBS$datetime[1:10]
class(newBS$datetime)


newBS$hour <- hour(newBS$LOC.TIME)
newBS$hour[1:10]
newBS$LOC.TIME[1:10]
newBS$day <- day(newBS$just)
#

brks <-sort(unique(newBS$hour))
brks <- brks[seq(1,length(brks),2)]


png(file=paste0(getwd(),"/AtSurface/BreedingSeason_WaterHauledOut_PercHEATMAP.png"),w=2800,h=1800, res=300)
(BS_heatmap <- ggplot(newBS,aes(hour,just.date,fill=TimeatSurface))+
    geom_tile(color= "white",size=0.1) + 
    scale_fill_viridis(name="% Time at\n    Surface",option ="C")+
    facet_grid(~id)+
    scale_x_continuous(breaks = brks)+
    ylab("")+
    xlab("Hour of day")+
    theme_bw()+ 
    theme(axis.text.x = element_text(colour = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(colour = "grey20", size = 12),
          text = element_text(size = 14)))
dev.off()

pdf(paste0(getwd(),"/AtSurface/BreedingSeason_WaterHauledOut_PercHEATMAP.pdf"),width= 9, height = 6)
BS_heatmap
dev.off()

#AntSci pubication plot ---
ggsave("Fig6_BreedingSeason_WaterHauledOut_PercHEATMAP.tiff",  plot = BS_heatmap,
       width =9, height = 7.5, dpi=1200) 


