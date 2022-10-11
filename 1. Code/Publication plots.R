library(cowplot)
library(tidyverse)
library(colorspace)
library(ragg)
library(ggtext)
library(pdftools)
library(scales)
library(lubridate)
library(sp)
library(maptools)
library(tidyverse)
library(viridis)
library(ggalt)
library(ggridges)
options(tibble.width = Inf)
library(patchwork)

#Get SSM tacking data
ross_enviros <- read.csv("./0. Data/Ross_SSM_Enviros_forFINALmodels.csv")

names(ross_enviros)
head(ross_enviros)
ross_enviros$datez <- as.Date(ross_enviros$datez)
ross_enviros$datez[1:10]

str(ross_enviros$date)
ross_enviros$date <- as.POSIXct(strptime(as.character(ross_enviros$date), "%Y-%m-%d %H:%M:%S", tz='GMT'))
ross_enviros$date[1:10]

# Make year for all 1980 so that I remove the inter-annual element which is a pain when you plot chronological data
ross_enviros$just.date <- as.POSIXct(strptime(paste('1980-', format(ross_enviros$date, "%m-%d")), "%Y-%m-%d"), tz='GMT')
ross_enviros$just.date[1:5]  
summary(ross_enviros$just.date)
range(ross_enviros$just.date)

ross_enviros$disticeedge <- ross_enviros$disticeedge/1000 #Change m to km
unique(ross_enviros$id)
ours <- c("152414","152418","152419","152413","152423","152422","152416","35941", "35940") 



ggplot(ross_enviros, aes(x = just.date, y = disticeedge)) +
  geom_point() +
  facet_wrap(~id) +
  theme_bw() + 
  geom_hline(yintercept=0, linetype="dashed", colour = "dark blue", size=1)+
  labs(x = "Date", y = "Distance from the ice edge (km)")+
  theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                   angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
        # strip.text = element_text(face = "italic"),
        text = element_text(size = 14))+
  scale_x_datetime(date_label = "%b")


## Custom theme for plots ----
theme_mw <- function () { 
  theme_bw(base_size=15, base_family="") %+replace% 
    theme(
      axis.text = element_text(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA)
    )
}

(p2.b <- ggplot(duik2,aes(y = meandepth, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean diving depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))


# pdf(paste0(getwd(),"/2. Results/Plots/Depth_TOD_boxplot.pdf"),width= 9, height = 5)
# p2
# dev.off()


(p3 <- ggplot(duik2,aes(y = meanduration, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~id)+
    # xlab(NULL)+
    xlab("Local apparent time")+
    ylab("Mean diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p4 <- ggplot(duik2,aes(y = maxduration, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~id)+
    # xlab(NULL)+
    xlab("Local apparent time")+
    ylab("Mean max diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p5 <- ggplot(duik2,aes(y = maxdepth, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~id)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean max diving depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))


pdf(paste0(getwd(),"/2. Results/Plots/Depth_Duration_TOD_boxplots.pdf"),width= 9, height = 8)
plot_grid(p2,p5,p3,p4, labels = c('A', 'B', 'C','D'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/Depth_Duration_TOD_boxplots.png"),w=2600,h=2300, res=300)
plot_grid(p2,p5,p3,p4, labels = c('A', 'B', 'C','D'))
dev.off()

plot_grid(p2,p5,p6,p3,p4,p7, labels = c('A', 'B', 'C','D','E','F'))

(p6 <- ggplot(duik2,aes(y = nrdives, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~id)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean number of dives")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p7 <- ggplot(duik2,aes(y = resids, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~id)+
    xlab("Local apparent time")+
    ylab("Dive residuals")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

pdf(paste0(getwd(),"/2. Results/Plots/All_TOD_boxplots.pdf"),width= 11, height = 8)
plot_grid(p2,p5,p6,p3,p4,p7, labels = c('A', 'B', 'C','D','E','F'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/All_TOD_boxplots.png"),w=3000,h=2300, res=300)
plot_grid(p2,p5,p6,p3,p4,p7, labels = c('A', 'B', 'C','D','E','F'))
dev.off()

names(duik)


ggplot(duik,aes(y = meandepth, x = factor(maand))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~seal_id)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean diving depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13))


#### Maps #####

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

# Antarctica base map of just Weddell Sea
data("wrld_simpl", package = "maptools")    
ant <- crop(wrld_simpl, extent(-35, 35, -90, -45)) 

(y_lines <- seq(-75,-45, by = 10))
(x_lines <- c(-35, -15,0,15,35))

#Colour palettes from https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible

(m1 <- ggplot() +
    geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
    coord_map("azequidistant", orientation = c(-90, 0, 0)) +
    geom_tile(data = duik, aes(lon, lat, z = meandepth),binwidth = 1, stat = "summary_2d", fun = mean,na.rm = TRUE)+
    geom_line(data = frnts2, aes(x = lon, y = lat), linetype = 1, colour = "grey60")+
    theme_void()   +
    # guides(colour = guide_legend(override.aes = list(size=2)))+
    scale_x_continuous(breaks = NULL) +
    xlab("") + 
    ylab("")  +
    labs(fill = "Mean diving \ndepth (m)")+
    scale_fill_viridis()+
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
          # legend.title = element_text("Mean diving \ndepth (m)"),
          legend.text = element_text(size=10),
          legend.position = c(.1,.25)))
m1

(m2 <- ggplot() +
    geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
    coord_map("azequidistant", orientation = c(-90, 0, 0)) +
    geom_tile(data = duik, aes(lon, lat, z = meanduration),binwidth = 1, stat = "summary_2d", fun = mean,na.rm = TRUE)+
    geom_line(data = frnts2, aes(x = lon, y = lat), linetype = 1, colour = "grey60")+
    theme_void()   +
    # guides(colour = guide_legend(override.aes = list(size=2)))+
    scale_x_continuous(breaks = NULL) +
    xlab("") + 
    ylab("")  +
    labs(fill = "Mean diving \nduration (min)")+
    scale_fill_viridis()+
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
          # legend.title = element_text("Mean diving \nduration (min)"),
          legend.text = element_text(size=10),
          legend.position = c(.1,.25)))

m2

(m3 <- ggplot() +
    geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
    coord_map("azequidistant", orientation = c(-90, 0, 0)) +
    geom_tile(data = duik, aes(lon, lat, z = resids),binwidth = 1, stat = "summary_2d", fun = mean,na.rm = TRUE)+
    geom_line(data = frnts2, aes(x = lon, y = lat), linetype = 1, colour = "grey60")+
    theme_void()   +
    # guides(colour = guide_legend(override.aes = list(size=2)))+
    scale_x_continuous(breaks = NULL) +
    xlab("") + 
    ylab("")  +
    labs(fill = "Diving \nresiduals")+
    scale_fill_viridis()+
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
          # legend.title = element_text("Mean diving \nduration (min)"),
          legend.text = element_text(size=10),
          legend.position = c(.1,.25)))

m3

(m4 <- ggplot() +
    geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
    coord_map("azequidistant", orientation = c(-90, 0, 0)) +
    geom_tile(data = duik, aes(lon, lat, z = nrdives),binwidth = 1, stat = "summary_2d", fun = mean,na.rm = TRUE)+
    geom_line(data = frnts2, aes(x = lon, y = lat), linetype = 1, colour = "grey60")+
    theme_void()   +
    # guides(colour = guide_legend(override.aes = list(size=2)))+
    scale_x_continuous(breaks = NULL) +
    xlab("") + 
    ylab("")  +
    labs(fill = "Number of \ndives")+
    scale_fill_viridis()+
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
          # legend.title = element_text("Mean diving \nduration (min)"),
          legend.text = element_text(size=10),
          legend.position = c(.1,.25)))

m4

pdf(paste0(getwd(),"/2. Results/Plots/Diving_metrics_maps.pdf"),width= 10, height = 8)
plot_grid(m1,m2,m3,m4, labels = c('A', 'B', 'C','D'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/Diving_metrics_maps.png"),w=2800,h=2300, res=300)
plot_grid(m1,m2,m3,m4, labels = c('A', 'B', 'C','D'))
dev.off()


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

png(file=paste0(getwd(),"/2. Results/Plots/Ross_Divers_Track_Inset.png"),w=2200,h=1800, res=300)
# grid.newpage()
vp_inset <- grid::viewport(width = 0.50, height = 0.50, x = 0.6, y = 0.4, just = c("left", "top"))
print(p3)
print(p4, vp = vp_inset)
dev.off()

head(duik2)

names(duik)[1] <- "seal_id"

names(duik)

baaiplot <- 
  duik %>% 
  group_by(factor(seal_id)) %>% 
  summarize(median_dive_depth = median(meandepth, na.rm = T), 
         sd_dive_depth = sd(meandepth, na.rm = T),
         median_dive_duration = median(meanduration, na.rm = T),
         sd_dive_duration = sd(meanduration, na.rm = T))

head(baaiplot)
names(baaiplot)[1] <- "seal_id"

theme_mw2 <- function() {
  theme_minimal(base_size=15, base_family="") %+replace% 
    theme(
      panel.grid.major = element_line(color = "grey92", size = .4),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(color = "grey30"),
      axis.title.y = element_text(color = "grey30",angle = 90,vjust=3),
      axis.text = element_text(color = "grey50"),
      axis.ticks =  element_line(color = "grey92", size = .4),
      axis.ticks.length = unit(.6, "lines"))
}

## baaiplot: Biplot ----

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#882255",  "#0072B2", "#E69F00", "#009E73", "#F0E442"
# "#0072B2", "#D55E00", "#CC79A7","#000000","#117733"
(baai <- ggplot(data=duik)+
  geom_point(aes(x=meandepth, y=meanduration, colour=seal_id),alpha = 0.4, size = 4)+
   # scale_fill_viridis(discrete=TRUE) +
   # scale_color_viridis(option = "magma", discrete=TRUE) +
    # "#56B4E9"
  scale_colour_manual(values = c("#882255",  "#0072B2", "#E69F00", "#009E73", "#F0E442"))+
  geom_errorbar(data = baaiplot,
                aes(x = median_dive_depth,
                    ymin = median_dive_duration - sd_dive_duration,
                    ymax = median_dive_duration + sd_dive_duration,
                    colour = seal_id,
                    colour = after_scale(darken(colour, .5, space = "combined"))
                    ),
                inherit.aes = F,
                width = 8,
                size = 0.8)+
  geom_errorbar(
    data = baaiplot,
    aes(
      y = median_dive_duration,
      xmin = median_dive_depth - sd_dive_depth,
      xmax = median_dive_depth + sd_dive_depth,
      colour = seal_id, 
      colour = after_scale(darken(colour, .5, space = "combined"))
    ),
    inherit.aes = F,
    width = .8,
    size = .8) +
  theme_bw() +
  ylab("Mean dive duration (min)\n")+
  xlab("\nMean dive depth (m)")+
  labs(colour = "")+
  theme(axis.text.x = element_text(colour = "grey20", size = 12,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 14)))



png(file=paste0(getwd(),"/2. Results/Plots/Diving_Depth_Duration_biplot.png"),w=2200,h=1800, res=300)
# grid.newpage()
print(baai)
dev.off()

pdf(paste0(getwd(),"/2. Results/Plots/Diving_Depth_Duration_biplot.pdf"),width= 9, height = 7)
print(baai)
dev.off()

#Save in AntSci specific format
ggsave("Fig3_Diving_Depth_Duration_biplot.tiff",  plot = baai,
       width =9, height = 7.5, dpi=1200)  

duik_noNA <- duik[!is.na(duik$meandepth),]

# names(duik)  
duik %>%
filter(maand != 7) %>%
ggplot(., aes(x = LOC.TIME, y = factor(mon))) +
  stat_density_ridges(alpha=0.6)+
  theme_mw()+
  xlab("Local apparent time")+
  ylab(NULL)+
  scale_x_datetime( breaks = date_breaks("4 hours"), 
                    date_labels = "%H:%M")

    
str(df_peng_summary)

### Boxplot variation in vars by months - suppl fig------
(p2.b <- ggplot(duik2,aes(y = meandepth, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean diving depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))


(p3.b <- ggplot(duik2,aes(y = meanduration, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    # xlab(NULL)+
    xlab("Local apparent time")+
    ylab("Mean diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p4.b <- ggplot(duik2,aes(y = maxduration, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    # xlab(NULL)+
    xlab("Local apparent time")+
    ylab("Mean max diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p5.b <- ggplot(duik2,aes(y = maxdepth, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean max diving depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))


pdf(paste0(getwd(),"/2. Results/Plots/Depth_Duration_TOD_MONTH_boxplots.pdf"),width= 9, height = 8)
plot_grid(p2.b,p5.b,p3.b,p4.b, labels = c('A', 'B', 'C','D'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/Depth_Duration_TOD_MONTH_boxplots.png"),w=2600,h=2300, res=300)
plot_grid(p2,p5,p3,p4, labels = c('A', 'B', 'C','D'))
dev.off()

plot_grid(p2,p5,p6,p3,p4,p7, labels = c('A', 'B', 'C','D','E','F'))

(p6.b <- ggplot(duik2,aes(y = nrdives, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean number of dives")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p7.b <- ggplot(duik2,aes(y = resids, x = factor(hour.bin))) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab("Local apparent time")+
    ylab("Dive residuals")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 90, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

pdf(paste0(getwd(),"/2. Results/Plots/All_TOD_MONTH_boxplots.pdf"),width= 12, height = 9)
plot_grid(p2.b,p5.b,p6.b,p3.b,p4.b,p7.b, labels = c('A', 'B', 'C','D','E','F'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/All_TOD_MONTH_boxplots.png"),w=3000,h=2300, res=300)
plot_grid(p2.b,p5.b,p6.b,p3.b,p4.b,p7.b, labels = c('A', 'B', 'C','D','E','F'))
dev.off()

#Day Night Plots -----

(p2.c <- ggplot(duik,aes(y = meandepth, x = TOD)) +
   geom_boxplot(na.rm = TRUE) +
   theme_bw()+
   # facet_wrap(~mon)+
   xlab(NULL)+
   # xlab("Local apparent time")+
   ylab("Mean diving depth (m)")+
   theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                    angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
         # strip.text = element_text(face = "italic"),
         text = element_text(size = 13)))

duik_noNA_depth <- duik %>% 
  drop_na(meandepth)

(p2.d <- ggplot(duik_noNA_depth, aes(x = TOD, y = meandepth)) + 
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .12, 
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  ## remove white space on the left
  coord_cartesian(xlim = c(1.2, NA))+
  theme_bw()+
  # facet_wrap(~mon)+
  xlab(NULL)+
  # xlab("Local apparent time")+
  ylab("Mean diving depth (m)")+
  theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                   angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
        # strip.text = element_text(face = "italic"),
        text = element_text(size = 13)))

(p3.c <- ggplot(duik,aes(y = meanduration, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p4.c <- ggplot(duik,aes(y = maxduration, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean max diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p4.d <- ggplot(duik_noNA_depth, aes(x = TOD, y = maxduration)) + 
    ## add half-violin from {ggdist} package
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5, 
      ## adjust height
      width = .6, 
      ## move geom to the right
      justification = -.2, 
      ## remove slab interval
      .width = 0, 
      point_colour = NA
    ) + 
    geom_boxplot(
      width = .12, 
      ## remove outliers
      outlier.color = NA ## `outlier.shape = NA` works as well
    ) +
    ## remove white space on the left
    coord_cartesian(xlim = c(1.2, NA))+
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean max diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p3.c <- ggplot(duik,aes(y = meanduration, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))


(p3.d <- ggplot(duik_noNA_depth, aes(x = TOD, y = meanduration)) + 
    ## add half-violin from {ggdist} package
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5, 
      ## adjust height
      width = .6, 
      ## move geom to the right
      justification = -.2, 
      ## remove slab interval
      .width = 0, 
      point_colour = NA
    ) + 
    geom_boxplot(
      width = .12, 
      ## remove outliers
      outlier.color = NA ## `outlier.shape = NA` works as well
    ) +
    ## remove white space on the left
    coord_cartesian(xlim = c(1.2, NA))+
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))



(p5.c <- ggplot(duik,aes(y = maxdepth, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean max diving depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p5.d <- ggplot(duik_noNA_depth, aes(x = TOD, y = maxdepth)) + 
    ## add half-violin from {ggdist} package
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5, 
      ## adjust height
      width = .6, 
      ## move geom to the right
      justification = -.2, 
      ## remove slab interval
      .width = 0, 
      point_colour = NA
    ) + 
    geom_boxplot(
      width = .12, 
      ## remove outliers
      outlier.color = NA ## `outlier.shape = NA` works as well
    ) +
    ## remove white space on the left
    coord_cartesian(xlim = c(1.2, NA))+
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean max diving depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))


pdf(paste0(getwd(),"/2. Results/Plots/Depth_Duration_DuskDawn_boxplots.pdf"),width= 9, height = 8)
plot_grid(p2.c,p5.c,p3.c,p4.c, labels = c('A', 'B', 'C','D'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/Depth_Duration_DuskDawn_boxplots.png"),w=2600,h=2300, res=300)
plot_grid(p2.c,p5.c,p3.c,p4.c, labels = c('A', 'B', 'C','D'))
dev.off()

plot_grid(p2.c,p5.c,p6.c,p3.c,p4.c,p7.c, labels = c('a', 'b', 'c','d','e','f'))

(p6.c <- ggplot(duik,aes(y = nrdives, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean number of dives")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p6.d <- ggplot(duik_noNA_depth, aes(x = TOD, y = nrdives)) + 
    ## add half-violin from {ggdist} package
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5, 
      ## adjust height
      width = .6, 
      ## move geom to the right
      justification = -.2, 
      ## remove slab interval
      .width = 0, 
      point_colour = NA
    ) + 
    geom_boxplot(
      width = .12, 
      ## remove outliers
      outlier.color = NA ## `outlier.shape = NA` works as well
    ) +
    ## remove white space on the left
    coord_cartesian(xlim = c(1.2, NA))+
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean number of dives")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))


(p7.c <- ggplot(duik,aes(y = resids, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    xlab(NULL)+
    # facet_wrap(~mon)+
    # xlab("Local apparent time")+
    ylab("Dive residuals")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p7.d <- ggplot(duik_noNA_depth, aes(x = TOD, y = resids)) + 
    ## add half-violin from {ggdist} package
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5, 
      ## adjust height
      width = .6, 
      ## move geom to the right
      justification = -.2, 
      ## remove slab interval
      .width = 0, 
      point_colour = NA
    ) + 
    geom_boxplot(
      width = .12, 
      ## remove outliers
      outlier.color = NA ## `outlier.shape = NA` works as well
    ) +
    ## remove white space on the left
    coord_cartesian(xlim = c(1.2, NA))+
    theme_bw()+
    # facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Dive residuals")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))



pdf(paste0(getwd(),"/2. Results/Plots/All_DuskDawn_boxplots.pdf"),width= 12, height = 9)
plot_grid(p2.c,p5.c,p6.c,p3.c,p4.c,p7.c, labels = c('A', 'B', 'C','D','E','F'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/All_DuskDawn_boxplots.png"),w=3000,h=2300, res=300)
plot_grid(p2.c,p5.c,p6.c,p3.c,p4.c,p7.c, labels = c('A', 'B', 'C','D','E','F'))
dev.off()


pdf(paste0(getwd(),"/2. Results/Plots/All_DuskDawn_boxplots_halfeye.pdf"),width= 12, height = 9)
plot_grid(p2.d,p5.d,p6.d,p3.d,p4.d,p7.d, labels = c('a', 'b', 'c','d','e','f'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/All_DuskDawn_boxplots_halfeye.png"),w=3000,h=2300, res=300)
bp_he <- plot_grid(p2.d,p5.d,p6.d,p3.d,p4.d,p7.d, labels = c('a', 'b', 'c','d','e','f'))
dev.off()

bp_he
#AntSci format for image in tiff 
ggsave("Fig2_All_DuskDawn_boxplots_halfeye.tiff",  plot = bp_he,
       width =9, height = 7.5, dpi=1200) 

### DAY NIGHT MONTHLY VARIATION -----

(p2.e <- ggplot(duik,aes(y = meandepth, x = TOD)) +
   geom_boxplot(na.rm = TRUE) +
   theme_bw()+
   facet_wrap(~mon)+
   xlab(NULL)+
   # xlab("Local apparent time")+
   ylab("Mean diving depth (m)")+
   theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                    angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
         # strip.text = element_text(face = "italic"),
         text = element_text(size = 13)))


(p3.e <- ggplot(duik,aes(y = meanduration, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p4.e <- ggplot(duik,aes(y = maxduration, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean max diving duration (min)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p5.e <- ggplot(duik,aes(y = maxdepth, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean max diving depth (m)")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))


pdf(paste0(getwd(),"/2. Results/Plots/Depth_Duration_DuskDawn_MONTH_boxplots.pdf"),width= 9, height = 8)
plot_grid(p2.e,p5.e,p3.e,p4.e, labels = c('A', 'B', 'C','D'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/Depth_Duration_DuskDawn_MONTH_boxplots.png"),w=2600,h=2300, res=300)
plot_grid(p2.e,p5.e,p3.e,p4.e, labels = c('A', 'B', 'C','D'))
dev.off()

(p6.e <- ggplot(duik,aes(y = nrdives, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    facet_wrap(~mon)+
    xlab(NULL)+
    # xlab("Local apparent time")+
    ylab("Mean number of dives")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

(p7.e <- ggplot(duik,aes(y = resids, x = TOD)) +
    geom_boxplot(na.rm = TRUE) +
    theme_bw()+
    xlab(NULL)+
    facet_wrap(~mon)+
    # xlab("Local apparent time")+
    ylab("Dive residuals")+
    theme(axis.text.x = element_text(colour = "grey20", size = 10,
                                     angle = 45, hjust = 0.5, vjust = 0.5),axis.text.y = element_text(colour = "grey20", size = 12),
          # strip.text = element_text(face = "italic"),
          text = element_text(size = 13)))

pdf(paste0(getwd(),"/2. Results/Plots/All_DuskDawn_MONTH_boxplots.pdf"),width= 12, height = 9)
plot_grid(p2.e,p5.e,p6.e,p3.e,p4.e,p7.e, labels = c('A', 'B', 'C','D','E','F'))
dev.off()

png(file=paste0(getwd(),"/2. Results/Plots/All_DuskDawn_MONTH_boxplots.png"),w=3000,h=2300, res=300)
plot_grid(p2.e,p5.e,p6.e,p3.e,p4.e,p7.e, labels = c('A', 'B', 'C','D','E','F'))
dev.off()

