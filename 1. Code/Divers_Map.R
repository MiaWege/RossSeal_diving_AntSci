library(maptools)
library(ggplot2)
library(raster)
library(viridis)
library(ggthemes)

tr <- read.csv(paste0(getwd(),"/0. Data/Ross_SSM_Enviros_forFINALmodels.csv"))
head(tr)

tr2 <- read.csv(paste0(getwd(),"/0. Data/Ross_2019_SSM.csv"))
head(tr2)

tr <- tr[,c("id","date","lat","lon")]
head(tr)

tr2 <- tr2[,c("id","date","lat","lon")]
head(tr2)

tr <- rbind (tr,tr2)
rm(tr2)

tr$date <- as.POSIXct(strptime(as.character(tr$date ), "%Y-%m-%d %H:%M:%S", tz="GMT"))
tr$date[1:5]
tr$id <- as.factor(tr$id)
names(tr)[names(tr)=="date"] <- "gmt"
range(dive$gmt)
range(tr$gmt)

levels(tr$id)

tr %>%
  group_by(id) %>% 
  summarise(start = min(gmt), end = max(gmt)) %>% 
  mutate(duro = difftime(end,start, "days"))


divers <- c(levels(dive$id), "152419","152422","152423", "164437","164438")

divers

tracks <- droplevels(subset(tr,tr$id %in% divers))
levels(tracks$id)

foo <- dur[which(dur$max == max(dur$max,na.rm = TRUE)),]
nrow(foo)

which(dur$max == max(dur$max,na.rm = TRUE))

#rename factor levels
library(forcats)
tracks$id <- fct_recode(tracks$id, "RossF_18" = "152413", "RossF_12" = "152414", "RossF_2"="152416", "RossM_23" = "35940","RossF_22" =  "35941" ,"RossF_15" = "152419" , "RossM_21" = "152422","RossF_19" = "152423","RossF_25"="164437","RossF_24"="164438")
levels(tracks$id)

# Get Southern Ocean fronts from Park & Durand 2019
# https://doi.org/10.17882/59800

library(ncdf4)
frnts <- nc_open(paste0(getwd(),"/0. Data/62985.nc"))

PF <- data.frame(
  "lat" = ncvar_get(frnts, "LatPF"),
  "lon" = ncvar_get(frnts, "LonPF"),
  "name" = "PF"
)
nc_close(frnts)

frnts <- na.omit(PF)
frnts2 <- droplevels(subset(frnts,frnts$lon >= -35 & frnts$lon <= 35))
rm(PF,frnts)

# # Defines the x and y axes required
# x_lines <- seq(-35,35, by = 10)
# y_lines <- seq(-85,-45, by = 10)

# Antarctica base map of just Weddell Sea
data("wrld_simpl", package = "maptools")    
ant <- crop(wrld_simpl, extent(-35, 35, -90, -45)) 

(y_lines <- seq(-75,-45, by = 10))
(x_lines <- c(-35, -15,0,15,35))

#Colour palettes from https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible

(p3 <- ggplot() +
    geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
    coord_map("azequidistant", orientation = c(-90, 0, 0)) +
    geom_point(data = tracks, aes(x = lon, y = lat, colour = id),size=0.5)+
    # scale_color_colorblind()+
    scale_colour_manual(values = c("#882255", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000","#117733"))+
    geom_line(data = frnts2, aes(x = lon, y = lat), linetype = 1, colour = "grey60")+
    theme_void()   +
    guides(colour = guide_legend(override.aes = list(size=2)))+
    scale_x_continuous(breaks = NULL) +
    xlab("") + 
    ylab("") +
    # Adds labels
    geom_text(aes(x = -40, y = seq(-75, -55, by = 10), hjust = -0.2,
                  label = rev(paste0(seq(55, 75, by = 10), "°S")))) +
    geom_text(aes(x = x_lines, y = -42, label = c("35°W","15°W", "0°", "15°E","35°E"))) +
    # Adds Y axes
    geom_segment(aes(y = y_lines, yend = y_lines, x = -35, xend = 35),
                 linetype = "dashed", colour = "lightgrey") +
    # # Add X axes
    geom_segment(aes(y = -45 , yend = -80, x = x_lines, xend = x_lines),
                 linetype = "dashed", colour = "lightgrey") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.position = c(.1,.25)))
p3

library(egg)

#Inset map -----
ant2 <- map_data("world") %>% filter(region=="Antarctica")

pol<-data.frame(xmin=-35,xmax=35 ,ymin=-75 ,ymax=-45)

(p4 <- ggplot() +
    geom_polygon(data = ant2, aes(x=long, y = lat, group = group), fill="grey", alpha = 0.8)+
    # Convert to polar coordinates
    coord_map("azequidistant", orientation = c(-90, 0, 0)) +
    labs(x=NULL,y=NULL)+
    theme_void()+
    geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="black", size = 0.5, linetype="dashed"))
p4

png(file=paste0(getwd(),"/2. Results/Plots/Ross_Divers_Track_Inset.png"),w=2200,h=1800, res=1200)
# grid.newpage()
vp_inset <- grid::viewport(width = 0.50, height = 0.50, x = 0.6, y = 0.4, just = c("left", "top"))
print(p3)
print(p4, vp = vp_inset)
dev.off()

pdf(paste0(getwd(),"/2. Results/Plots/Ross_Divers_Track_Inset.pdf"), width= 8, height = 6)
# grid.newpage()
vp_inset <- grid::viewport(width = 0.50, height = 0.50, x = 0.6, y = 0.4, just = c("left", "top"))
print(p3)
print(p4, vp = vp_inset)


dev.off()

#Final plot for AntSci resolution and specifications ----

library(patchwork)
s1 <- p3 + inset_element(p4, left = 0.55, bottom = -0.1, right = 1.12, top = 0.40)
ggsave("Fig1_Ross_Divers_Track_Inset.tiff",  plot = s1,
       width =13, height = 8.16, dpi=1200) 


