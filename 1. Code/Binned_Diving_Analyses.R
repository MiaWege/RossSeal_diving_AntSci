library(lattice)
library(tidyverse)
library(lubridate)
library(purrr)
library(scales)
library(sp)
library(maptools)
library(viridis)
library(ggpointdensity)
options(tibble.width = Inf) #Because tibbles are annoying

################# Mean Dive Depth ################
s55 <- read.csv(paste0(getwd(),"/0. Data/S55_Ross_DiveDepth_Cleaned_ALLSeals.csv"))
head(s55)
unique(s55$HistType)

ps111 <- read.csv(paste0(getwd(),"/0. Data/PS111_Ross_DiveDepth_Cleaned_ALLSeals.csv"))
head(ps111)
unique(ps111$HistType)

names(s55)==names(ps111)

dive <- rbind(s55,ps111)
rm(s55, ps111)

dive$id <- as.factor(dive$id)
dive$gmt<- as.POSIXct(strptime(as.character(dive$gmt), "%Y-%m-%d %H:%M:%S", tz="GMT"))
dive$gmt[1:10]

##Remove two dead animals...

dive.bkp <- dive #create a backup
levels(dive$id)

dive <- droplevels(subset(dive,dive$id != "152417" & dive$id != "152415"))
levels(dive$id)


# dive depth bins
# bin <- c(2, 5, 10, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 600, 600) #The min value here is 2 because I programmed the tag to ignore dives shallower than 2m.
# bin_m <- c(5, 10, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 600, 600)

# bins required when only assessing dives >5m
bin <- c(5, 10, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 600, 600)
bin_m <- c(10, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 600, 600)

# calculates mid points of the bins
(mid <- bin[-length(bin)] + (bin[-1] - bin[-length(bin)])/2)

# function to calculate mean dive depth
mnfun <- function(x) sum(x * mid, na.rm = TRUE)/sum(x, na.rm = TRUE)
sumfun <- function(x) sum(x, na.rm = TRUE)
propfun <- function(x) x/sum(x, na.rm = TRUE)
#function for finding min max value - ie start of max depth bin
# excludes both NAs and zero values in a bin (latter=feb06)
maxfun <- function(x) mid[max(which(!is.na(x) & x!=0), na.rm=TRUE)]  

str(dive)
head(dive)
names(dive)
summary(dive)

### THere are no dive in bin14
dive$Bin14 <- rep(0,length=nrow(dive))

## add dive metrics for dives > 5m (ie last 13 dive bins - exclude first)
#Otherwise I would include bin1

dive$mean <- apply(dive[,6:18], 1, mnfun)
dive$sum <- apply(dive[,6:18], 1, sum, na.rm=T)
dive$max <- apply(dive[,6:18], 1, maxfun)
# not sure that this is returning the correct depth (use dive$max as the max depth)
#dive$max2 <- max(dive[,22:34], na.rm=T)

head(dive)
dim(dive)

hist(dive$max)
hist(dive$mean)
hist(dive$sum)
hist(dive$Sum) #No differences

summary(dive$max)
summary(dive$mean)

summary(dive$Sum)
summary(dive$sum)

qplot(mean,data=dive,geom="histogram")+theme_bw()+facet_wrap(~id)+scale_y_reverse()
qplot(mean,data=dive,geom="density")+theme_bw()+facet_wrap(~id)+scale_y_reverse()

qplot(gmt,sum.t,data=dive) + theme_bw() + facet_wrap(~id)


dive$maand <- as.factor(month(dive$gmt))
dive$mon <- as.factor(months(dive$gmt))
dive$maand[1:50]
dive$gmt[1:50]
dive$mon[1:50]


dive$year <- year(dive$gmt)
dive$year[1:5]
summary(dive$year)

# install.packages("lunar")
library(lunar)
dive$lun <- lunar.illumination(as.Date(dive$gmt))
qplot(lun,data=dive,geom = "histogram") + theme_bw() + facet_wrap(~id)

dive$phase <- lunar.phase(as.Date(dive$gmt), name = TRUE)


head(dive)

boxplot(dive$mean~dive$phase, outline = FALSE)
boxplot(dive$max~dive$phase, outline = FALSE)
boxplot(dive$sum~dive$phase, outline = FALSE)

qplot(lun,mean,data=dive, geom="point") + theme_bw() + facet_wrap(~id)
rm(bin,bin_m,mid, maxfun,propfun,sumfun,mnfun)
save.image("dive.RData")
# write.csv(dive, file = "ROSS_dive_binned_means_withLUNAR.csv",row.names = FALSE)  #

########## Mean Dive Duration ###############
s55 <- read.csv(paste0(getwd(),"/0. Data/S55_Ross_DiveDUR_Cleaned_ALLSeals.csv"))
head(s55)
unique(s55$HistType)

ps111 <- read.csv(paste0(getwd(),"/0. Data/PS111_Ross_DiveDUR_Cleaned_ALLSeals.csv"))
head(ps111)
unique(ps111$HistType)

names(s55)==names(ps111)

dur <- rbind(s55,ps111)
rm(s55, ps111)

dur$id <- as.factor(dur$id)
dur$gmt<- as.POSIXct(strptime(as.character(dur$gmt), "%Y-%m-%d %H:%M:%S", tz="GMT"))
dur$gmt[1:10]

unique(dur$HistType)
levels(dur$id)
str(dur)

dur <- droplevels(subset(dur,dur$id != "152417" & dur$id != "152415"))
levels(dur$id)


# #Ross Seal duration bins..
# # 30, 120, 240, 360, 480, 600, 720, 840, 960, 1080, 1200, 1500, 1800, >1800
# bin <- c(20, 30, 120, 240, 360, 480, 600, 720, 840, 960, 1080, 1200, 1500, 1800, 1800)
# #The min value here is 20sec because I programmed the tag to ignore dives shallower than 2m and shorter than 20sec.

# calculates mid points of the bins
# bins required when only assessing durs >2m
bin <- c(30, 120, 240, 360, 480, 600, 720, 840, 960, 1080, 1200, 1500, 1800, 1800)
(mid <- bin[-length(bin)] + (bin[-1] - bin[-length(bin)])/2)

# function to calculate mean dur depth
mnfun <- function(x) sum(x * mid, na.rm = TRUE)/sum(x, na.rm = T)
sumfun <- function(x) sum(x, na.rm = T)
#function for finding min max value - ie start of max depth bin
# excludes both NAs and zero values in a bin (latter=feb06)
maxfun <- function(x) mid[max(which(!is.na(x) & x!=0), na.rm=T)]  
totdurfun <- function(x) sum(x * mid, na.rm = TRUE)#calculates total time spent diving in 4h bin
names(dur)

dur$Bin14 <- as.numeric(as.character(dur$Bin14))
summary(dur$Bin14)
# dur$Bin14 <- rep(0,length=nrow(dur))
names(dur)
# apply mean depth function
# dur$mean <- apply(dur[,5:18], 1, mnfun)
# dur$sum <- apply(dur[,5:18], 1, sum, na.rm=T)
# dur$max <- apply(dur[,5:18], 1, maxfun)
# dur$tot1 <- apply(dur[,5:18], 1, totdurfun)

## add dur metrics for durs > 2m
# MIA: But is this really true? Only excluding dives shorter than 15sec...not excluding dives deeper than 2m
dur$mean <- apply(dur[,6:18], 1, mnfun)
dur$sum <- apply(dur[,6:18], 1, sum, na.rm=T)
dur$max <- apply(dur[,6:18], 1, maxfun)
dur$tot <- apply(dur[,6:18], 1, totdurfun)
head(dur)
dim(dur)

#### Duration proportions 
propfun <- function(x) x/sum(x, na.rm = T)

pp <- dur[,5:18]
pp$sum <- apply(pp, 1, sum, na.rm=T)
prop <- apply(pp[,1:14], 1, propfun)
prop.t <- as.data.frame(t(prop))
dim(prop.t)
dim(dur)
colnames(prop.t) <-  c("p.1", "p.2", "p.3", "p.4", "p.5", "p.6", "p.7", 
                       "p.8", "p.9", "p.10","p.11", "p.12", "p.13", "p.14") 
prop.t$sum.t <- apply(prop.t, 1, sum, na.rm=T)


## add proportions to main dataframe
dur2 <- cbind(dur, prop.t)
head(dur2)
dur <- dur2
rm(pp,prop,prop.t,dur2,dur.bkp2)

hist(dur$max)
hist(dur$mean)
hist(dur$sum)
hist(dur$Sum) #No differences


summary(dur$max)

summary(dur$mean)

summary(dur$Sum)
summary(dur$sum)
summary(dur$sum2)#No differences

qplot(mean,data=dur,geom="histogram")+theme_bw()+facet_wrap(~id)
qplot(mean,data=dur,geom="density")+theme_bw()+facet_wrap(~id)

#### count proportion of bins with zero dives 
xx <- subset(dur, dur$sum==0)
nrow(xx)
rm(xx)

dur$maand <- as.factor(month(dur$gmt))
dur$mon <- as.factor(months(dur$gmt))
dur$maand[500:510]
dur$mon[500:510]
dur$gmt[500:510]
names(dur)

#Add lunar information 
library(lunar)
dur$lun <- lunar.illumination(as.Date(dur$gmt))
qplot(lun,data=dur,geom = "histogram") + theme_bw() + facet_wrap(~id)

dur$phase <- lunar.phase(as.Date(dur$gmt), name = TRUE)

boxplot(dur$mean~dur$phase, outline = FALSE)
boxplot(dur$max~dur$phase, outline = FALSE)
boxplot(dur$sum~dur$phase, outline = FALSE)


# write.csv(dur, file = "PS111_ROSS_diveDUR_binned_means_NO_LUNAR.csv",row.names = FALSE)  # output data
save.image("duration.RData")
rm(maxfun,mnfun,propfun,sumfun,totdurfun,mid,bin)

################ Link dive depth with Locs #################
tr <- read.csv(paste0(getwd(),"/0. Data/Ross_SSM_Enviros_forFINALmodels.csv"))
head(tr)

tr$date <- as.POSIXct(strptime(as.character(tr$date ), "%Y-%m-%d %H:%M:%S", tz="GMT"))
tr$date[1:5]
tr$id <- as.factor(tr$id)
str(tr)
range(tr$date)
names(tr)[names(tr)=="date"] <- "gmt"
range(dive$gmt)
range(tr$gmt)

summary(dive$Bin1)
summary(dive$Bin2)
summary(dive$Bin3)
summary(dive$Bin4)
summary(dive$Bin5)
summary(dive$Bin6) 
summary(dive$Bin7) 
summary(dive$Bin8) 
summary(dive$Bin9) 
summary(dive$Bin10) 
summary(dive$Bin11) 
summary(dive$Bin12) 
summary(dive$Bin13) #has NAs
summary(dive$Bin14) 

### LOOP B:
## Loop B first finds a min and max time interval from your dive bin file
## Because our splash tags were set up to collect data in 4 hour 
## intervals and the time given by the binned data represents diving 
## behaviour from the prev 4 hours I make tmin is the date-time value 
## minus 4 hours...and tmax just the date-time value given by binned data
names(dive)

names(tr)
levels(dive$id)
levels(tr$id)
tracks <- droplevels(subset(tr,tr$id %in% dive$id))
# length(levels(tracks_backup$id))
length(levels(tracks$id))
length(levels(dive$id))
which(levels(tracks$id) != levels(dive$id))
which(levels(dive$id) != levels(tracks$id))
levels(dive$id)
levels(tracks$id)

#set up empty variables
levels(dive$id)
DiveDepthLocs <-{}

for (j in 1:length(levels(as.factor(dive$id)))) {
  loc1 <- droplevels(subset(tracks,tracks$id==levels(tracks$id)[j]))
  dbin <- droplevels(subset(dive, dive$id == levels(tracks$id)[j]))
  # dbin <- droplevels(subset(dbin, dbin$HistType == "DiveDepth"))
  dbin<- dbin[order(dbin$gmt, decreasing=F),]
  loc1<- loc1[order(loc1$gmt, decreasing=F),]
  i <- {};  latm <- {}; lonm <- {}; ice <- {}; ssh <- {}; sshA  <- {};ssh_grad <- {};     sst <- {}; sstA  <- {}; sst_grad <- {}; disticeedge <- {}; bath <- {}; slope <- {};     eke <- {}; currmag <- {}; windmag <- {}; vmix <- {}; vmix_sd <- {}; shflux <- {};       shflux_sd <- {}
  for(i in 1: (nrow(dbin))) {
    (tmin <- dbin$gmt[i] - (3600*4)) #subtracts 4 hours (bin duration) from location time
    (tmax <- dbin$gmt[i] )
    (mvar <- loc1[loc1$gmt >= tmin & loc1$gmt <= tmax,])
    if (nrow(mvar)==0) { #but for some bin intervals there might not be a location value. 
      t <- which(abs(loc1$gmt-dbin$gmt[i])==min(abs(loc1$gmt-dbin$gmt[i]))) #If there is not, then just find the location that matches the nearest time stamp...
      latm[i] <- loc1$lat[t]
      lonm[i] <- loc1$lon[t]
      ice[i] <- loc1$iceconcentration[t]
      ssh[i] <- loc1$ssh[t]
      sshA[i]  <- loc1$sshA[t]
      ssh_grad[i] <- loc1$ssh.grad[t]
      sst[i] <- loc1$sst[t]
      sstA[i]  <- loc1$sstA[t]
      sst_grad[i] <- loc1$sst.grad[t]
      disticeedge[i] <- loc1$disticeedge[t]
      bath[i] <- loc1$bathym[t]
      slope[i] <- loc1$slope[t]
      eke[i] <- loc1$eke[t]
      currmag[i] <- loc1$currmag[t]
      windmag[i] <- loc1$windmag[t]
      vmix[i] <- loc1$vmix[t]
      vmix_sd <- loc1$vmix_sd[t]
      shflux[i] <- loc1$shflux[t]
      shflux_sd[i] <- loc1$shflux_sd[t]
      
      } else { #but if there are locs for that time interval, avg lons and lats.
      latm[i] <- mean(mvar$lat, na.rm=T)
      lonm[i] <- mean(mvar$lon, na.rm=T)
      ice[i] <- mean(mvar$iceconcentration, na.rm=T)
      ssh[i] <- mean(mvar$ssh, na.rm=T)
      sshA[i]  <- mean(mvar$sshA, na.rm=T)
      ssh_grad[i] <- mean(mvar$ssh.grad, na.rm=T)
      sst[i] <- mean(mvar$sst, na.rm=T)
      sstA[i]  <- mean(mvar$sstA, na.rm=T)
      sst_grad[i] <- mean(mvar$sst.grad, na.rm=T)
      disticeedge[i] <- mean(mvar$disticeedge, na.rm=T)
      bath[i] <- mean(mvar$bathym, na.rm=T)
      slope[i] <- mean(mvar$slope, na.rm=T)
      eke[i] <- mean(mvar$eke, na.rm=T)
      currmag[i] <- mean(mvar$currmag, na.rm=T)
      windmag[i] <- mean(mvar$windmag, na.rm=T)
      vmix[i] <- mean(mvar$vmix, na.rm=T)
      vmix_sd <- mean(mvar$vmix_sd, na.rm=T)
      shflux[i] <- mean(mvar$shflux, na.rm=T)
      shflux_sd[i] <- mean(mvar$shflux_sd, na.rm=T)
    }
  }
  dbin$latm <-latm
  dbin$lonm <- lonm
  dbin$ice <- ice
  dbin$ssh <- ssh
  dbin$sshA  <- sshA
  dbin$ssh_grad <- ssh_grad
  dbin$sst <- sst
  dbin$sstA  <- sstA
  dbin$sst_grad <- sst_grad
  dbin$disticeedge <- disticeedge/1000
  dbin$bath <- bath
  dbin$slope <- slope
  dbin$eke <- eke
  dbin$currmag <- currmag
  dbin$windmag <- windmag
  dbin$vmix <- vmix
  dbin$vmix_sd <- vmix_sd
  dbin$shflux <- shflux
  dbin$shflux_sd <- shflux_sd
  
  
  DiveDepthLocs <- rbind(DiveDepthLocs,dbin)
}

#Just some error checking
nrow(subset(dive,dive$HistType == "DiveDepth" & dive$id %in% levels(tracks$id)))
plot(tracks$lon,tracks$lat,pch=19)
points(DiveDepthLocs$lonm,DiveDepthLocs$latm,pch="*",col="red")

rm(tr,bath,currmag,disticeedge,eke,i,ice, j,latm,lonm,shflux, shflux_sd, slope, ssh,sshA, ssh_grad,sst,sst_grad, sstA,t, tmax,tmin,vmix,vmix_sd,windmag, mvar,loc1)

### Let's just do some plotting and compare the the different locations calculated by the two loops

# Some external vector for the color scale
col <- sort(DiveDepthLocs$Sum)
range(col)
qplot(lonm, latm, data=DiveDepthLocs, colour=col) + scale_colour_gradient(low="red", high="blue")
rm(col)

names(DiveDepthLocs)
plot(DiveDepthLocs$lonm,DiveDepthLocs$latm,pch = 20)
lines(DiveDepthLocs$lon,DiveDepthLocs$lat,pch = "*")

################## Link dive duration with Locations #######
levels(tracks$id)
levels(dur$id)

DiveDurationLocs <-{}

for (j in 1:length(levels(as.factor(dur$id)))) {
  loc1 <- droplevels(subset(tracks,tracks$id==levels(tracks$id)[j]))
  dbin <- droplevels(subset(dur, dur$id == levels(tracks$id)[j]))
  # dbin <- droplevels(subset(dbin, dbin$HistType == "DiveDepth"))
  dbin<- dbin[order(dbin$gmt, decreasing=F),]
  loc1<- loc1[order(loc1$gmt, decreasing=F),]
  i <- {};  latm <- {}; lonm <- {}; ice <- {}; ssh <- {}; sshA  <- {};ssh_grad <- {};     sst <- {}; sstA  <- {}; sst_grad <- {}; disticeedge <- {}; bath <- {}; slope <- {};     eke <- {}; currmag <- {}; windmag <- {}; vmix <- {}; vmix_sd <- {}; shflux <- {};       shflux_sd <- {}
  for(i in 1: (nrow(dbin))) {
    (tmin <- dbin$gmt[i] - (3600*4)) #subtracts 4 hours from location time
    (tmax <- dbin$gmt[i] )
    (mvar <- loc1[loc1$gmt >= tmin & loc1$gmt <= tmax,])
    if (nrow(mvar)==0) { #but for some bin intervals there might not be a location value. 
      t <- which(abs(loc1$gmt-dbin$gmt[i])==min(abs(loc1$gmt-dbin$gmt[i]))) #If there is not, then just find the location that matches the nearest time stamp...
      latm[i] <- loc1$lat[t]
      lonm[i] <- loc1$lon[t]
      ice[i] <- loc1$iceconcentration[t]
      ssh[i] <- loc1$ssh[t]
      sshA[i]  <- loc1$sshA[t]
      ssh_grad[i] <- loc1$ssh.grad[t]
      sst[i] <- loc1$sst[t]
      sstA[i]  <- loc1$sstA[t]
      sst_grad[i] <- loc1$sst.grad[t]
      disticeedge[i] <- loc1$disticeedge[t]
      bath[i] <- loc1$bathym[t]
      slope[i] <- loc1$slope[t]
      eke[i] <- loc1$eke[t]
      currmag[i] <- loc1$currmag[t]
      windmag[i] <- loc1$windmag[t]
      vmix[i] <- loc1$vmix[t]
      vmix_sd <- loc1$vmix_sd[t]
      shflux[i] <- loc1$shflux[t]
      shflux_sd[i] <- loc1$shflux_sd[t]
      
    } else { #but if there are locs for that time interval, avg lons and lats.
      latm[i] <- mean(mvar$lat, na.rm=T)
      lonm[i] <- mean(mvar$lon, na.rm=T)
      ice[i] <- mean(mvar$iceconcentration, na.rm=T)
      ssh[i] <- mean(mvar$ssh, na.rm=T)
      sshA[i]  <- mean(mvar$sshA, na.rm=T)
      ssh_grad[i] <- mean(mvar$ssh.grad, na.rm=T)
      sst[i] <- mean(mvar$sst, na.rm=T)
      sstA[i]  <- mean(mvar$sstA, na.rm=T)
      sst_grad[i] <- mean(mvar$sst.grad, na.rm=T)
      disticeedge[i] <- mean(mvar$disticeedge, na.rm=T)
      bath[i] <- mean(mvar$bathym, na.rm=T)
      slope[i] <- mean(mvar$slope, na.rm=T)
      eke[i] <- mean(mvar$eke, na.rm=T)
      currmag[i] <- mean(mvar$currmag, na.rm=T)
      windmag[i] <- mean(mvar$windmag, na.rm=T)
      vmix[i] <- mean(mvar$vmix, na.rm=T)
      vmix_sd <- mean(mvar$vmix_sd, na.rm=T)
      shflux[i] <- mean(mvar$shflux, na.rm=T)
      shflux_sd[i] <- mean(mvar$shflux_sd, na.rm=T)
    }
  }
  dbin$latm <-latm
  dbin$lonm <- lonm
  dbin$ice <- ice
  dbin$ssh <- ssh
  dbin$sshA  <- sshA
  dbin$ssh_grad <- ssh_grad
  dbin$sst <- sst
  dbin$sstA  <- sstA
  dbin$sst_grad <- sst_grad
  dbin$disticeedge <- disticeedge/1000
  dbin$bath <- bath
  dbin$slope <- slope
  dbin$eke <- eke
  dbin$currmag <- currmag
  dbin$windmag <- windmag
  dbin$vmix <- vmix
  dbin$vmix_sd <- vmix_sd
  dbin$shflux <- shflux
  dbin$shflux_sd <- shflux_sd
  
  
  DiveDurationLocs <- rbind(DiveDurationLocs,dbin)
}

#Just some error checking
nrow(subset(DiveDurationLocs,DiveDurationLocs$HistType == "DiveDuration" & DiveDurationLocs$id %in% levels(tracks$id)))
plot(tracks$lon,tracks$lat,pch=19)
points(DiveDurationLocs$lonm,DiveDurationLocs$latm,pch="*",col="red")

rm(tr,bath,currmag,disticeedge,eke,i,ice, j,latm,lonm,shflux, shflux_sd, slope, ssh,sshA, ssh_grad,sst,sst_grad, sstA,t, tmax,tmin,vmix,vmix_sd,windmag, mvar,loc1,dbin)

######## ADD JULIAN DATE ##############
tmp <- as.Date(DiveDepthLocs$gmt, format = "%Y-%m-%d")
tmp[1:10]
DiveDepthLocs$gmt[1:10]
DiveDepthLocs$jday <- format(tmp, "%j")

tmp <- as.Date(DiveDurationLocs$gmt, format = "%Y-%m-%d")
tmp[1:10]
DiveDurationLocs$gmt[1:10]
DiveDurationLocs$jday <- format(tmp, "%j")
head(DiveDepthLocs)

rm(tmp)

save.image("Ross_Dive_Locs.RData")

#####  Combine Depth and Duration Data ################

plot(DiveDepthLocs$lonm,DiveDepthLocs$latm,pch="*")
points(DiveDurationLocs$lonm,DiveDurationLocs$latm,pch=".",cex=3,col="red")

write.csv(DiveDepthLocs,file = paste0(getwd(),"/0. Data/RossDiveDepthLocs.csv"),row.names = FALSE)
write.csv(DiveDurationLocs,file = paste0(getwd(),"/0. Data/RossDiveDurationLocs.csv"),row.names = FALSE)


DiveDepthLocs$id <- as.factor(DiveDepthLocs$id)
DiveDurationLocs$id <- as.factor(DiveDurationLocs$id)

DiveDepthLocs <- DiveDepthLocs[order(DiveDepthLocs$id, DiveDepthLocs$gmt, decreasing=F), ]
DiveDurationLocs <- DiveDurationLocs[order(DiveDurationLocs$id, DiveDurationLocs$gmt, decreasing=F), ]

which(names(DiveDepthLocs) != names(DiveDurationLocs))
length(which(DiveDepthLocs$gmt != DiveDurationLocs$gmt))
length(which(DiveDepthLocs$gmt == DiveDurationLocs$gmt))

nrow(DiveDepthLocs)
nrow(DiveDurationLocs)

names(DiveDepthLocs); dim(DiveDepthLocs)
names(DiveDurationLocs); dim(DiveDurationLocs)

DiveDurationLocs$maand <- DiveDurationLocs$tot1 <- DiveDurationLocs$tot2 <- NULL
DiveDepthLocs$datum <- DiveDepthLocs$maand <- NULL
which(names(DiveDepthLocs) != names(DiveDurationLocs))

levels(DiveDepthLocs$HistType)
levels(DiveDurationLocs$HistType)

bin <- merge(DiveDepthLocs,DiveDurationLocs,by = c("id","gmt"))

nrow(bin); nrow(DiveDepthLocs); nrow(DiveDurationLocs) #baie minder rye in bin

require(ggplot2)
par(mfrow=c(2,1))
plot(bin$mean.x~bin$gmt)
plot(DiveDepthLocs$mean~DiveDepthLocs$gmt)

par(mfrow=c(1,1))
plot(bin$mean.x~bin$mean.y)
abline(lm(bin$mean.x~bin$mean.y),col="red")

par(mfrow=c(1,1))
plot(bin$mean.x,bin$mean.y,col=as.factor(bin$id))
abline(lm(bin$mean.y~bin$mean.x),col="blue")

levels(bin$id)

plot(bin$mean.x[bin$id=="35940"]~bin$mean.y[bin$id=="35940"],col="red")
abline(lm(bin$mean.x[bin$id=="35940"]~bin$mean.y[bin$id=="35940"]),col="red")

points(bin$mean.x[bin$id=="35941"]~bin$mean.y[bin$id=="35941"],col="blue")
abline(lm(bin$mean.x[bin$id=="35941"]~bin$mean.y[bin$id=="35941"]),col="blue")

qplot(mean.x,data=bin,geom="density",colour=id)+theme_bw()
qplot(mean.y,data=bin,geom="density",colour=id)+theme_bw()

############## Clean up combined sheet #############

plot(bin$latm.x~bin$latm.y)
plot(bin$lonm.x~bin$lonm.y)


#Lats and longs match up, can remove the one.
names(bin)[names(bin)=="latm.x"] <- "latm"
names(bin)[names(bin)=="lonm.x"] <- "lonm"
bin$latm.y<-NULL
bin$lonm.y<-NULL

# Let's rename some of these columns, remove kak and simplify this massive dataset.
# 1) Remove columns I am not going to use again soon.
bkp <- bin #First create a backup of bin
names(bin)

#Create a dataframe with all the raw bin values in.
raw.bins <- bin[,c(1:18,62:77)]
head(raw.bins)
#Now remove these bins from the original bin file.

bin <- bin[,-c(3:18,62:77)]
names(bin)

names(bin)

names(bin)[names(bin)=="mean.x"] <- "meandepth"
names(bin)[names(bin)=="sum.x"] <- "nrdives"
names(bin)[names(bin)=="max.x"] <- "maxdepth"


names(bin)[names(bin)=="mean.y"] <- "meanduration"
names(bin)[names(bin)=="sum.y"] <- "nrdivesdur"
names(bin)[names(bin)=="max.y"] <- "maxduration"

names(bin)

plot(bin$meandepth,bin$meanduration)
plot(bin$maxdepth,bin$maxduration)


# Calculate local time based on loc #######################
library(maptools)
names(bin)

diel.prep <- function(theData) {
  
  theData$gmt<-as.POSIXct(strptime(as.character(theData$gmt),'%Y-%m-%d %H:%M:%S'),tz="GMT")
  
  theData$LOC.DATE <- theData$gmt+(theData$lonm/15)*3600
  theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$LOC.DATE, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
  
  theData$JDay <- as.numeric(format(theData$LOC.DATE, '%j'))
  
  theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('lonm', 'latm'), names(theData))]),dateTime=theData$gmt, direction='sunrise', POSIXct.out=T)$day_frac*24
  theData$sunrise.hr <- theData$sunrise.hr+(theData$lonm/15)
  
  theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('lonm', 'latm'), names(theData))]), dateTime=theData$LOC.DATE, direction='sunset', POSIXct.out=T)$day_frac*24
  theData$sunset.hr <- theData$sunset.hr+(theData$lonm/15)
  
  theData$dusk.hr <- crepuscule(crds=as.matrix(theData[,match(c('lonm', 'latm'), names(theData))]), dateTime=theData$LOC.DATE, solarDep = 12, direction='dusk', POSIXct.out=T)$day_frac*24
  theData$dusk.hr <- theData$dusk.hr+(theData$lonm/15)
  
  
  theData$dawn.hr <- crepuscule(crds=as.matrix(theData[,match(c('lonm', 'latm'), names(theData))]), dateTime=theData$LOC.DATE,solarDep = 12, direction='dawn', POSIXct.out=T)$day_frac*24
  theData$dawn.hr <- theData$dawn.hr+(theData$lonm/15)
  
  # theData$mirror.time <- 43200-abs(as.numeric(theData$LOC.TIME)-43200)
  
  theData
}


bin2 <- diel.prep(bin)
head(bin2)
summary(bin2$dusk.hr)
summary(bin2$dawn.hr)
bin <- bin2
bin2$LOC.DATE[1:5]
summary(bin2$sunset.hr)

bin$solarelev <-solarpos(crds=as.matrix(bin[,match(c('lonm', 'latm'), names(bin))]),bin$LOC.DATE)[,2]

tmp <- as.Date(bin$LOC.DATE)
tmp[1:5]
bin$datum <- as.POSIXct(strptime(paste('1970-', format(tmp, "%m-%d")), "%Y-%m-%d"), tz='GMT')
bin$datum[1:5]
rm(tmp)

write.csv(bin,file=paste0(getwd(),"/0. Data/Ross_combinedbinned_means_lunar_LOCtime_DielPrep.csv"),row.names = FALSE)  # write the file to a csv
head(bin)
summary(bin$solarelev)
plot(bin$solarelev~bin$LOC.TIME)
rm(bin2,bkp,diel.prep)

save.image("Ross_Dive_readyforplots.RData")


#Clean up the file 
names(bin)
duik <- bin[,-c(6:20,45,50:86)]
names(duik)
# library(janitor)
# duik <- clean_names(duik)
# names(duik)

tmp <- str_replace_all(names(duik),".x","")
tmp
tmp[tmp=="lonm"] <- "lon"
tmp[tmp=="latm"] <- "lat"
tmp[tmp=="vm"] <- "vmix"
tmp[tmp=="vm_sd"] <- "vmix_sd"
tmp[tmp=="shfl"] <- "shflux"
tmp[tmp=="shfl_sd"] <- "shflux_sd"
tmp[tmp=="phase"] <- "moonphase"
tmp[tmp=="mdepth"] <- "maxdepth"
tmp[tmp=="mduration"] <- "maxduration"
tmp

colnames(duik) <- tmp
head(duik)

duik$hour.bin <- cut(hour(duik$LOC.TIME), breaks = c(0, 4,8, 12, 16, 20,24), include.lowest = TRUE)
class(duik$hour.bin)
levels(duik$hour.bin)
library(forcats)

duik$hour.bin <- fct_recode(duik$hour.bin, "0-4h" = "[0,4]", "4-8h" = "(4,8]", "8-12h" = "(8,12]", "12-16h" = "(12,16]", "16-20h" = "(16,20]", "20-24h" = "(20,24]")
levels(duik$hour.bin)

levels(duik$id)
duik$id <- fct_recode(duik$id, "RossF_18" = "152413", "RossF_12" = "152414", "RossF_2"="152416", "RossM_23" = "35940","RossF_22" =  "35941" )
levels(duik$id)

#Turn dive duration into minutes - who thinks in seconds in any case?
duik$meanduration <- duik$meanduration/60
duik$maxduration <- duik$maxduration/60

summary(duik$meanduration)
summary(duik$maxduration)


####### #Now let's do some plotting ##########
qplot(meandepth, meanduration, data = duik,colour = id) +theme_bw()
duik$resids <- residuals(lm(duik$meanduration ~ duik$meandepth,na.action = na.exclude))
summary(duik$resids)

names(duik)
names(DiveDepthLocs)
qplot(factor(LOC.TIME), meandepth, data = duik, geom="boxplot") +theme_bw()+ facet_wrap(~id)
qplot(LOC.TIME, meanduration, data = duik) +theme_bw()+ facet_wrap(~id)
qplot(LOC.TIME, nrdives, data = duik) +theme_bw()+ facet_wrap(~id)
qplot(LOC.TIME, maxdepth, data = duik) +theme_bw()+ facet_wrap(~id)
qplot(LOC.TIME, maxduration, data = duik) +theme_bw()+ facet_wrap(~id)
qplot(LOC.TIME, nrdivesdur, data = duik) +theme_bw()+ facet_wrap(~id)
# qplot(LOC.TIME, sum.t, data = duik) +theme_bw()+ facet_wrap(~id)
qplot(solarelev,nrdives, data = duik,colour = id) +theme_bw()+ facet_wrap(~id)

qplot(lun.x,meandepth, data = duik) +theme_bw()
qplot(lun.x,meanduration, data = duik) +theme_bw()
qplot(lun.x,maxdepth, data = duik) +theme_bw()
qplot(lun.x,maxduration, data = duik) +theme_bw()
 
qplot(lun.x,meandepth , data = duik,colour = id) +theme_bw()+facet_wrap(~id)
qplot(lun.x,nrdives , data = duik,colour = id) +theme_bw()+facet_wrap(~id)

qplot(solarelev,maxdepth , data = duik,colour = id) +theme_bw()+facet_wrap(~id)
qplot(solarelev,meanduration , data = duik) +theme_bw() + facet_wrap(~id)

qplot(solarelev,maxduration , data = duik,colour = id) +theme_bw()+ facet_wrap(~id)
qplot(solarelev,nrdives , data = duik,colour = id) +theme_bw()+ facet_wrap(~id)
qplot(solarelev,nrdivesdur , data = duik,colour = id) +theme_bw()+ facet_wrap(~id)


qplot(phase.x,meandepth, data = duik,geom="boxplot") +theme_bw()+ facet_wrap(~id)

m1 <- aov(duik$meandepth~factor(duik$phase.x))
summary(m1) #Nothing
plot(m1)

qplot(phase.x,meanduration, data = duik,geom="boxplot") +theme_bw()+ facet_wrap(~id)

m1 <- aov(duik$meanduration~factor(duik$phase.x))
summary(m1) #Nothing
plot(m1)

qplot(phase.x,maxdepth, data = duik,geom="boxplot") +theme_bw()+ facet_wrap(~id)

qplot(meandepth,meanduration, data = duik,colour=id) +theme_bw() + facet_wrap(~id)

qplot(meandepth, data = duik,fill=id, geom = "histogram")+theme_bw()
qplot(id,meandepth, data = duik, geom = "boxplot")+theme_bw()
qplot(meandepth, data = duik,colour=id, geom = "density")+theme_bw()

qplot(lonm, latm, data = duik,geom = "point", colour = meandepth ) + scale_size_area() +theme_bw()
qplot(lonm, latm, data = duik,geom = "point", colour = meanduration ) + scale_size_area() +theme_bw()
qplot(lonm, latm, data = duik,geom = "point", colour = maxdepth ) + scale_size_area() +theme_bw()
qplot(lonm, latm, data = duik,geom = "point", colour = maxduration ) + scale_size_area() +theme_bw()
qplot(lonm, latm, data = duik,geom = "point", colour = id )  +theme_bw()

qplot(lonm,meandepth,data=duik,geom=c("smooth","point"))+theme_bw()
qplot(latm,meandepth,data=duik,geom=c("smooth","point"))+theme_bw()

qplot(lonm,meandepth,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)
qplot(latm,meandepth,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)

qplot(datum,meandepth,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)
qplot(datum,meanduration,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)

qplot(datum,maxdepth,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)
qplot(datum,maxduration,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)
names(duik)

qplot(datum,nrdives,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)
qplot(datum,tot,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)

qplot(datum,disticeedge.x,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)
qplot(datum,disticeedge.y,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)



qplot(disticeedge.y,meandepth,data=duik,geom=c("smooth","point"))+theme_bw()+facet_wrap(~id)

ggplot(duik, aes(x = datum, y = disticeedge.x)) +
  geom_point() +
  facet_wrap(~id) +
  theme_bw() + 
  geom_hline(yintercept=0, linetype="dashed", color = "dark blue", size=1)+
  labs(x = "Date", y = "Distance from the ice edge (km)")

head(DiveDepthLocs)
ggplot(duik,aes(y = meandepth, x = datum)) +
  geom_point() + geom_smooth(method="lm")+theme_bw()+facet_wrap(~id)

ggplot(duik,aes(y = meandepth, x = LOC.TIME)) +
  geom_point() +
  theme_bw()+
  facet_wrap(~id)+
  scale_x_datetime(date_label = "%H:%M")
  

head(duik)

ggplot(duik,aes(y = meandepth, x = LOC.DATE,colour=id,shape=id)) +
  geom_point() + geom_smooth(method="auto")+theme_bw()

ggplot(duik,aes(y = meanduration, x = LOC.DATE,colour=id,shape=id)) +
  geom_point() + geom_smooth(method="auto")+theme_bw()

ggplot(duik,aes(y = maxdepth, x = LOC.DATE,colour=id,shape=id)) +
  geom_point() + geom_smooth(method="auto")+theme_bw()

ggplot(duik,aes(y = maxduration, x = LOC.DATE,colour=id,shape=id)) +
  geom_point() + geom_smooth(method="auto")+theme_bw()



head(duik)
############ Calculate proportion of bin in Night ################
## Let's calculate what proportion [0-1] of each bin is night.


#The datafile is called bin. bin$LOC.DATE is the date-time column of the animals actual location. But it has a time and a date. I am just interested in the date now. In dataframe 'bin' for each entry (i.e. 4h bin) I have calculate the local sunrise and sunset time too. But the output is in decimal times. So first I need to convert that decimal time to a POSIXct date/time value.
# You don't really need to worry about these first few lines of code, I am just sorting dates and times out

# Get the real date
datum <-strftime(bin$LOC.DATE, format = "%Y-%m-%d 00:00:00", tz = "GMT", usetz = FALSE)
datum[1:5]
bin$posix.dates <- as.POSIXct(datum, format="%Y-%m-%d %H:%M:%S",tz="GMT")
bin$posix.dates[1:10]


bin$sunrisehr.date <- bin$posix.dates + (3600*bin$sunrise.hr) #This is the local sunrise time and date
bin$sunsethr.date <- bin$posix.dates + (3600*bin$sunset.hr) #This is the local sunset time and date
bin$duskhr.date <- bin$posix.dates + (3600*bin$dusk.hr) #This is the local dusk time and date
bin$dawnhr.date <- bin$posix.dates + (3600*bin$dawn.hr) #This is the local dawn time and date

#Just some error checking.
bin$sunrisehr.date[1:40]
bin$sunrise.hr[1:30]
bin$dawnhr.date [1:30]
bin$dawn.hr [1:30]
bin$sunsethr.date[1:30]
bin$sunset.hr[1:30]
bin$duskhr.date [1:30]
bin$dusk.hr [1:30]
summary(bin$dawn.hr)

rm(datum) #remove kak files I don't need

##Now that I have a date-time value for sunrise and sunset I can start the loop.

#Things you need to know.
# bin$LOC.DATE or use$LOC.DATE is the local time and date where the animal is. It represents the END time of the 4hour bin. So to get the start time of the 4h bin, I need to subtract 4 hours from LOC.DATE

#But because this is for Ross seals that are waaay south in the Antarctic, some bins in summer won't have any proportion in night. It's all day! How do I deal with this...well I look at where the NA values are
summary(bin$dawn.hr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.5963  4.1730  5.3060  5.2700  6.7220  7.6570     192 
summary(bin$dusk.hr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 16.28   17.15   18.77   18.89   20.20   24.15     190 
summary(bin$sunrise.hr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.4745  5.0240  6.2130  6.4520  8.0840 11.8500      90 
summary(bin$sunset.hr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 12.06   15.74   17.93   17.70   19.40   23.53      93 
plot(lonm[is.na(bin$dawn.hr)],latm[is.na(bin$dawn.hr)],xlim=range(bin$lonm),ylim=range(bin$latm))
points(lonm[is.na(bin$dusk.hr)],latm[is.na(bin$dusk.hr)],col="green",pch="*")
points(lonm[is.na(bin$sunrise.hr)],latm[is.na(bin$sunrise.hr)],col="red",pch="*")
points(lonm[is.na(bin$sunset.hr)],latm[is.na(bin$sunset.hr)],col="blue",pch="19")

# So everywhere where sunrise, sunset, dusk and and dawn hr is NA then replace that with -99999 to id later. 

bin[is.na(bin$dawn.hr),]

bin$dawn.hr[is.na(bin$dawn.hr)] <- -99999
bin$dusk.hr[is.na(bin$dusk.hr)] <- -99999
bin$sunset.hr[is.na(bin$sunset.hr)] <- -99999
bin$sunrise.hr[is.na(bin$sunrise.hr)] <- -99999
summary(bin$dawn.hr) #No more NA's!
summary(bin$dusk.hr) #No more NA's!
summary(bin$sunrise.hr) #No more NA's!
summary(bin$sunset.hr) #No more NA's!

#Create empty vectors
propnight <- {} #Vector that will have my final values
i <- {} #Just a ticker to tick through rows/observations

#Loop begins
for (i in 1:nrow(bin)) { #nrow is the number of rows in the dataframe
  use <- bin[i,]  #I subset the bin dataframe by creating a new one called use, just with the relevant observation that I am going to do analyses with
  (tmin <- use$LOC.DATE) #start time of bin
  (tmax <- use$LOC.DATE + (3600*4)) # Add 4 hrs  - End time of 4h bin
  
  if(use$dawn.hr == -99999 | use$dusk.hr == -99999) { #This is for those locations so far south in summer where it is day 24h
    temp <- 0
    
  }
  else if( tmin > use$sunrisehr.date & tmin < use$sunsethr.date &
           tmax > use$sunrisehr.date & tmax < use$sunsethr.date) {
    
    temp <- 0 #This is for all day observation. If tmin (start of bin) is larger than sunrise and smaller than sunset and tmax (end of bin) is larger than sunrise and smaller than sunset then give temp (intermediate place holder object) the value of 0. Because 0% of this bin is in night time
    
  } else if(tmin < use$sunrisehr.date & tmax < use$sunrisehr.date) {  
    
    temp <- 1 #else if tmin (start of bin) is smaller than sunrise and tmax (end of bin) is smaller than sunrise then it is night. So temp = 1, because entire bin is in night
    
  } else if(tmin > use$sunsethr.date & tmax > use$sunsethr.date) {
    
    temp <- 1 #else if tmin (start of bin) is larger than sunset and tmax (end of bin) is larger than sunset then it is night. So temp = 1, because entire bin is in night. I had to split night time into two if else conditions because dates change across midnight and it causes kak.
    
  } else if(tmin < use$sunrisehr.date & tmax > use$sunrisehr.date) {
    (temp <- as.numeric(difftime(use$sunrisehr.date, tmin,units="hours"))/4)
    #So here we get to what happens when part of bin is night and other part is day.
    # If tmin (start of bin is smaller than sunrise) BUT (and) the end of the bin (tmax) is larger than sunrise, then temp = sunrise - tmin. Because that is the amount of time that bin is in the dark. I calculate it in hours, and divide by four to get a proportion relative to the bin.
  } else if(tmin < use$sunsethr.date & tmax > use$sunsethr.date) {
    (temp <- as.numeric(difftime(tmax, use$sunsethr.date, units = "hours"))/4)
    
    ##Or what could also happen, if the start of the bin (tmin) is in day (smaller than sunset date time) but end of bin (tmax) is in night (larger than sunset time date) then temp = tmax - sunset becuase that is the amount of time in darkenss. Again calculated in hours and divided by 4 to get proportion
    
  } else  
    temp <- 99999
  #Else if there are some other condition I could not think of, give temp the value of 99999
  # so I can error check those observations.
  (propnight <- c(propnight,temp)) #Now propnight (my final vector of proportions) gets the value of propnight (the previous values from prev observations as it sequentially runs through the loop) + the latest observation, temp.
  print(i) #Print i, the rownumber or obervation the loop is at. This is so I can check the progress of the loop
  
} #end.

summary(propnight) #some error checking, did I get any 99999 values? Nope. Good!
bin <- cbind(bin,propnight) #bind that vector of values to my dataframe 'bin'.
rm(i, propnight, temp) #remove kak objects I don't need anymore.

################## Exploratory Plots ###############

#Influence of Proporiton of night
qplot(propnight, meandepth, geom=c("point","smooth"),data=)+theme_bw()
qplot(propnight, nrdives,geom=c("point","smooth"), data=bin)+theme_bw()
qplot(propnight, maxdepth,geom=c("point","smooth"),data=bin)+theme_bw()
qplot(propnight, meanduration, geom=c("point","smooth"),data= bin)+theme_bw()
qplot(propnight, maxduration, geom=c("point","smooth"),data= bin)+theme_bw()

qplot(propnight, data= bin, geom="density", colour=id)+theme_bw()
qplot(LOC.DATE, propnight, data= bin, colour=id)+theme_bw()

##Calculate proportion of bin in daylight -----
names(duik)

duik$startT <- duik$LOC.DATE - 4*60*60 #4 hours = bin duration
duik$endT <- duik$LOC.DATE
duik$startT[1:10]
duik$endT[1:10]
duik$sunset.hr[1:10]
duik$sunrise.hr[1:10]


daynight <-{}
as.factor(daynight)
for (i in 1:nrow(duik)){
  
  if (duik$endT[i] < duik$sunrise.hr[i] &
      duik$startT[i] > duik$sunset.hr[i]) {
    daynight[i] = "night"
  } else if (duik$startT[i] >= duik$sunrise.hr[i] &
             duik$endT[i] <= duik$sunset.hr[i]){
    daynight[i] = "day"
  } else (daynight[i] == "mid") 
}

i<-2
keep <- duik
head(duik)

summary(daynight)







## Influence of date
qplot(jday, nrdives, data= DiveDepthLocs,geom=c("smooth","point"))+theme_bw()
qplot(jday, meandepth, data= DiveDepthLocs,geom=c("smooth","point"))+theme_bw()
qplot(jday, maxdepth, data= DiveDepthLocs,geom=c("smooth","point"))+theme_bw()
qplot(jday,meanduration, data= DiveDurationLocs,geom=c("smooth","point"))+theme_bw()
qplot(jday,maxduration, data= DiveDurationLocs,geom=c("smooth","point"))+theme_bw()

## Influence of time
names(DiveDepthLocs)
qplot(LOC.TIME,nrdives, data = DiveDepthLocs,geom=c("smooth","point"))+theme_bw()
qplot(LOC.TIME, meandepth, data = DiveDepthLocs,geom=c("smooth","point"))+theme_bw()
qplot(LOC.TIME, maxdepth, data = DiveDepthLocs,geom=c("smooth","point"))+theme_bw()
qplot(LOC.TIME,meanduration, data= DiveDurationLocs,geom=c("smooth","point"))+theme_bw()
qplot(LOC.TIME,maxduration, data= DiveDurationLocs,geom=c("smooth","point"))+theme_bw()

timebin <- as.factor(bin$LOC.TIME)
timebin[1:5]
nlevels(timebin)
unique(bin$LOC.TIME)

## Influence of Lat
qplot(latm, nrdives, data= DiveDepthLocs,geom=c("smooth","point"))+theme_bw()
qplot(latm, meandepth, data= DiveDepthLocs,geom=c("point","smooth"))+theme_bw()
qplot(latm, maxdepth, data= DiveDepthLocs,geom=c("point","smooth"))+theme_bw()

qplot(latm,meanduration, data= DiveDurationLocs,geom=c("point","smooth"))+theme_bw()
qplot(latm,maxduration, data=DiveDurationLocs, geom=c("point","smooth"))+theme_bw()



qplot(jday,lonm, data= DiveDepthLocs,geom=c("point","smooth"), colour=id)+theme_bw()
qplot(jday,lon, data= tr,geom= "point",colour=id)+theme_bw()

qplot(jday,latm, data= DiveDepthLocs,geom= c("point","smooth"), colour=id)+theme_bw()
qplot(jday,lat, data= tr,geom= "point",colour=id)+theme_bw()

plot(tr$lat,col=tr$id)


names(tr)
names(bin)

levels(tr$id)

names(tr)



## Influence of Lon
qplot(lonm, nrdives, data= bin,geom=c("smooth","point"))+theme_bw()
qplot(lonm,meandepth, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(lonm,meanduration, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(lonm,maxdepth, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(lonm,maxduration, data= bin,geom=c("point","smooth"))+theme_bw()

names(bin)
## Influence of Solar Elevation - not really relevant
qplot(solarelev, nrdives, data= bin,geom=c("smooth","point"))+theme_bw()
qplot(solarelev, meandepth, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(solarelev, maxdepth, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(solarelev,meanduration, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(solarelev,maxduration, data= bin,geom=c("point","smooth"))+theme_bw()


## Influence Lunar illumination
qplot(lun, nrdives, data= bin,geom=c("smooth","point"))+theme_bw()
qplot(lun, meandepth, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(lun, maxdepth, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(lun,meanduration, data= bin,geom=c("point","smooth"))+theme_bw()
qplot(lun,maxduration, data= bin,geom=c("point","smooth"))+theme_bw()



plot(bin$lonm[is.na(bin$meandepth)],bin$latm[is.na(bin$meandepth)],
     col=bin$id[is.na(bin$meandepth)])


points(bin$lonm[bin$nrdives==0],bin$latm[bin$nrdives==0],pch="*")


TATlimits
TADlimits
divelimits
durlimits

#####Function to calculate mean and sd of nr of dives in each bin for each individual #####
library(tidyr)
mn <- as.data.frame(aggregate(raw.bins[,5:18],by = list(raw.bins$id),FUN = mean,na.action = na.omit))
head(mn)
mn$var <- "gemid"

sd <- as.data.frame(aggregate(raw.bins[,5:18],by = list(raw.bins$id),FUN = sd))
sd$var <- "sd"
colnames(mn) <- c("id","Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14","var")

data_long1 <- gather(mn, binnr, value,  Bin1:Bin14)
data_long1
colnames(data_long1) <- c("id","var","binnr", "mean")
data_long1$var <- NULL
data_long1

colnames(sd) <- c("id","Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14","var")

data_long2 <- gather(sd, binnr, value,  Bin1:Bin14)
data_long2
colnames(data_long2) <- c("id","var","binnr", "sd")
data_long2$var <- NULL
data_long2

binagvs <- merge(data_long1,data_long2,by=c("id","binnr"))
head(binagvs)
rm(mn,sd,data_long1,data_long2)

str(binagvs)
binagvs$binnr <- as.factor(binagvs$binnr)

levels(binagvs$binnr)
binagvs$binnr <- factor(binagvs$binnr, levels = c("Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14"))
levels(binagvs$binnr)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(1) # move them .05 to the left and right

ggplot(binagvs, aes(x=binnr, y=mean)) + 
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_bar(position=pd,stat="identity")+
  theme_bw()+
  facet_wrap(~id)+
  xlab("Bin number")+
  ylab("Dive depth bins mean +/- sd")


#####Function to calculate mean and sd of nr of dives in each bin for each individual FOR DIVE DURATION #####
library(tidyr)
names(raw.bins)
mn <- as.data.frame(aggregate(raw.bins[,23:36],by = list(raw.bins$id),FUN = mean,na.action = na.omit))
head(mn)
mn$var <- "gemid"

sd <- as.data.frame(aggregate(raw.bins[,23:36],by = list(raw.bins$id),FUN = sd))
sd$var <- "sd"
colnames(mn) <- c("id","Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14","var")
colnames(sd) <- c("id","Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14","var")


data_long1 <- gather(mn, binnr, value,  Bin1:Bin14)
data_long1
colnames(data_long1) <- c("id","var","binnr", "mean")
data_long1$var <- NULL
data_long1

data_long2 <- gather(sd, binnr, value,  Bin1:Bin14)
data_long2
colnames(data_long2) <- c("id","var","binnr", "sd")
data_long2$var <- NULL
data_long2

durbinavgs <- merge(data_long1,data_long2,by=c("id","binnr"))
head(durbinavgs)
rm(mn,sd,data_long1,data_long2)

str(durbinavgs)
durbinavgs$binnr <- as.factor(durbinavgs$binnr)

levels(durbinavgs$binnr)
durbinavgs$binnr <- factor(durbinavgs$binnr, levels = c("Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14"))
levels(durbinavgs$binnr)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(1) # move them .05 to the left and right

ggplot(durbinavgs, aes(x=binnr, y=mean)) + 
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_bar(position=pd,stat="identity")+
  theme_bw()+
  facet_wrap(~id)+
  xlab("Bin number")+
  ylab("Dive duration bins mean +/- sd")

############## RESIDUALS MODELS #################
# install.packages("lme4")
library(lme4)
# names(bin)

bin$meandepth[is.na(bin$meandepth)]<- 0
bin$meanduration[is.na(bin$meanduration)]<- 0


mod1 <- lmer(meanduration~meandepth + (1|id),data=bin, na.action=na.omit)
plot(mod1)
hist(residuals(mod1))
qqnorm(residuals(mod1))
qqline(residuals(mod1),col="red")
bin$resids <- residuals(mod1)


############ Time at Depth ##################
rm(TAD.alive)
levels(TAD$id)
levels(TAD.alive$id)
TAD.all <- TAD
TAD <- droplevels(subset(TAD.all,TAD.all$id %in% bin$id)) 
levels(bin$id)
levels(TAD$id)


library(tidyr)
names(TAD)
mn <- as.data.frame(aggregate(TAD[,5:18],by = list(TAD$id),FUN = mean,na.action = na.omit))
head(mn)
mn$var <- "gemid"

sd <- as.data.frame(aggregate(TAD[,5:18],by = list(TAD$id),FUN = sd))
sd$var <- "sd"
head(sd)
colnames(mn) <- c("id","Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14","var")
colnames(sd) <- c("id","Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14","var")


data_long1 <- gather(mn, binnr, value,  Bin1:Bin14)
data_long1
colnames(data_long1) <- c("id","var","binnr", "mean")
data_long1$var <- NULL
data_long1

data_long2 <- gather(sd, binnr, value,  Bin1:Bin14)
data_long2
colnames(data_long2) <- c("id","var","binnr", "sd")
data_long2$var <- NULL
data_long2

TADbinavgs <- merge(data_long1,data_long2,by=c("id","binnr"))
head(TADbinavgs)
rm(mn,sd,data_long1,data_long2)

str(TADbinavgs)
TADbinavgs$binnr <- as.factor(TADbinavgs$binnr)

levels(TADbinavgs$binnr)
TADbinavgs$binnr <- factor(TADbinavgs$binnr, levels = c("Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14"))
levels(TADbinavgs$binnr)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(1) # move them .05 to the left and right

ggplot(TADbinavgs, aes(x=binnr, y=mean)) + 
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_bar(position=pd,stat="identity")+
  theme_bw()+
  facet_wrap(~id)+
  xlab("Bin number")+
  ylab("TAD bins mean +/- sd")

################ TIME AT TEMPERATURE PLOTS ###########
rm(TAT.alive)
levels(TAT$id)
TAT.all <- TAT
TAT <- droplevels(subset(TAT.all,TAT.all$id %in% bin$id)) 
levels(bin$id)
levels(TAT$id)


library(tidyr)
names(TAT)
mn <- as.data.frame(aggregate(TAT[,5:18],by = list(TAT$id),FUN = mean,na.action = na.omit))
head(mn)
mn$var <- "gemid"

sd <- as.data.frame(aggregate(TAT[,5:18],by = list(TAT$id),FUN = sd))
sd$var <- "sd"
head(sd)
colnames(mn) <- c("id","Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14","var")
colnames(sd) <- c("id","Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14","var")


data_long1 <- gather(mn, binnr, value,  Bin1:Bin14)
data_long1
colnames(data_long1) <- c("id","var","binnr", "mean")
data_long1$var <- NULL
data_long1

data_long2 <- gather(sd, binnr, value,  Bin1:Bin14)
data_long2
colnames(data_long2) <- c("id","var","binnr", "sd")
data_long2$var <- NULL
data_long2

TATbinavgs <- merge(data_long1,data_long2,by=c("id","binnr"))
head(TATbinavgs)
rm(mn,sd,data_long1,data_long2)

str(TATbinavgs)
TATbinavgs$binnr <- as.factor(TATbinavgs$binnr)

levels(TATbinavgs$binnr)
TATbinavgs$binnr <- factor(TATbinavgs$binnr, levels = c("Bin1", "Bin2","Bin3","Bin4", "Bin5", "Bin6", "Bin7", "Bin8", "Bin9", "Bin10", "Bin11","Bin12", "Bin13", "Bin14"))
levels(TATbinavgs$binnr)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(1) # move them .05 to the left and right

ggplot(TATbinavgs, aes(x=binnr, y=mean)) + 
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_bar(position=pd,stat="identity")+
  theme_bw()+
  facet_wrap(~id)+
  xlab("Bin number")+
  ylab("TAT bins mean +/- sd")

sd(bin$meandepth, na.rm = TRUE)


qplot(meandepth,geom="histogram",data=DiveDepthLocs)+theme_bw()+ facet_wrap(~id)
na.omit(range(DiveDepthLocs$maxdepth))

qplot(latm,meandepth,data=DiveDepthLocs,colour=id)+theme_bw()
qplot(lonm,meandepth,data=DiveDepthLocs,colour=id)+theme_bw()

qplot(latm,meanduration,data=DiveDurationLocs)+theme_bw()
qplot(lonm,meanduration,data=DiveDurationLocs,colour=id)+theme_bw()

## Summary table for Ant Sci -----

head(duik)
names(duik)

(sumry <- duik %>% 
  group_by(seal_id) %>% 
  summarise(mn_dep = round(mean(meandepth, na.rm = TRUE)),
            sd_dep = round(sd(meandepth, na.rm = TRUE)),
            mn_max_dep =  round(mean(maxdepth, na.rm = TRUE)),
            sd_maxdep =  round(sd(maxdepth, na.rm = TRUE)),
            mn_dur =  round(mean(meanduration, na.rm = TRUE)),
            sd_dur =  round(sd(meanduration, na.rm = TRUE)),
            mn_max_dur =  round(mean(maxduration, na.rm = TRUE)),
            sd_maxdur =  round(sd(maxduration, na.rm = TRUE)),
            mn_nrdives =  round(mean(nrdives, na.rm = TRUE)),
            sd_nrdives =  round(sd(nrdives, na.rm = TRUE)),
            max_max_depth =  round(max(maxdepth, na.rm = TRUE)),
            max_max_dur =  round(max(maxduration, na.rm = TRUE))
            ))

# getwd()
write.csv(sumry, "DivingSummaryTable_forPaper.csv", row.names = FALSE)
