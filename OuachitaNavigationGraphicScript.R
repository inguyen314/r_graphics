##############################################################################
## Script Objective: Create the Red River Navigation Graphic for MVK Water Control
## Script Author: Melinda Pullman
## Script Date: 05/14/2020

####################### IMPORT PACKAGES #######################
library(ggplot2); library(numform); 
library(dplyr); library(lubridate); library(rvest); library(tidyverse);library(stringr);
library(stringi); library(readxl)

####################### DEFINE FUNCTIONS #######################
# Define function to Wrap long labels for the graphic
wrapper <- function(x, ...) paste(stri_wrap(x, ...), collapse = "\n")

# Define function to collect images to load into the graphic
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

# Define function for formating change in stage/elev
format_24hr_change <- function(x){
  if(as.numeric(x) >= 0.0){
    change24hr <- paste(paste0("+", sprintf("%.1f",as.numeric(x))), "ft")
  } else{
    change24hr <- paste(sprintf("%.1f", as.numeric(x)), "ft")
  }
  return(change24hr)
}

# Function to clean up HTML
clean_up <- function(x) {
  sp <- stringr::str_replace_all(x, "[\r\t\n]", "")
  rm <- str_split(sp, " +")
  return(rm)
}

# Define Function to webscrape MVK Water Control Website
collect_MVK_WC_web_data <- function(station_names, variables, url_page, stage){
  #stage variable is used to obtain variables. T=will only obtain stage. F=will obtain stage and 24hr change.
  url_table <- data.frame(html_table(html_nodes(read_html(url_page), 'table')[2], fill=T))
  #get indices of stations within table
  mat <- matrix(nrow = length(station_names), ncol = 2)
  for (i in 1:length(station_names)){
    mat[i,] <- which(url_table == station_names[i], arr.ind=T)
  }
  
  if(stage==T){
    # fill data in dataframe
    CurrentElev <- c()
    CurrentElevStr <- c()
    
    for (i in 1:length(mat[,1])){
      CurrentElev[i] <- as.numeric(url_table[mat[i,1], mat[i,2]+2])
      CurrentElevStr[i] <- paste(sprintf("%.1f",as.numeric(url_table[mat[i,1], mat[i,2]+2])), "ft")
    }
    
    station_DF <- data.frame("StationNames" = station_names, CurrentElev, CurrentElevStr,
                             stringsAsFactors = F)
  } else{
    # fill data in dataframe
    CurrentElev <- c()
    CurrentElevStr <- c()
    #Change24Hr <- c()
    
    for (i in 1:length(mat[,1])){
      CurrentElev[i] <- as.numeric(url_table[mat[i,1], mat[i,2]+2])
      CurrentElevStr[i] <- paste(sprintf("%.1f",as.numeric(url_table[mat[i,1], mat[i,2]+2])), "ft")
      Change24Hr[i] <- format_24hr_change(as.numeric(url_table[mat[i,1], mat[i,2]+3]))
    }
    
    station_DF <- data.frame("StationNames" = station_names, CurrentElev, CurrentElevStr,
                             Change24Hr, stringsAsFactors = F)
  }
  
  return(station_DF)
}

####################### DEFINE VARIABLES FOR SCRIPT ####################
# Location to Store Graphics: 
graphicDirectory <- 'C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/MVK_GraphicScripts/'
# Collect the logos to include on the graphic
img1 <- get_png("C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/MVK_GraphicScripts/usacelogo.png")
img2 <- get_png("C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/MVK_GraphicScripts/usacelogo2.png")
img3 <- get_png("C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/MVK_GraphicScripts/armylogo.png")

####################### COLLECT CURRENT L&D DATA #######################
## Collect Ouachita L&D data from our website
# external website
ouach_URL <- 'https://www.mvk-wc.usace.army.mil/ouachita.htm'
station_names <- c("Ouachita River @ Thatcher L&D Upper", 
                   "Ouachita River @ Thatcher L&D Lower", 
                   "Ouachita River @ Felsenthal L&D Upper", 
                   "Ouachita River @ Felsenthal L&D Lower", 
                   "Ouachita River @ Columbia L&D Upper",
                   "Ouachita River @ Columbia L&D Lower",
                   "Black River @ Jonesville L&D Upper",
                   "Black River @ Jonesville L&D Lower")
col_names <-c("Stage")
station_DF <- collect_MVK_WC_web_data(station_names = station_names, variables = col_names,
                                      url_page = ouach_URL, stage=T)

# Collect Monroe Flow Data
ouach_URL2 <- 'https://www.mvk-wc.usace.army.mil/ouachita.htm'
ouach_table <- html_table(html_nodes(read_html(ouach_URL2), 'table')[2], fill=T)
MonroeFlow <- paste(ouach_table[[1]]$X6[23], "cfs")

####################### DEFINE L&D PERTINENT DATA #######################
# Create dataframe with structure pertinent data
ouachita_DF <- data.frame("Station Names" = c("Thatcher L&D", "Felsenthal L&D", "Columbia L&D",
                                              "Jonesville L&D"), 
                          "Service Bridge" = c(110.0,113.0,90.0,77.0),
                          "Lock Wall" = c(85.0,79.0,64.0,42.0),
                          "Top of Gate" = c(78.0,71.0,64.0,42.0),
                          "Normal Pool" = c(77.0,65.0,52.0,34.0),
                          "Navigation Pass" = c(71.0,64.0,50.5,28.5),
                          "Gate Sill" = c(52.0,40.0,27.0,4.0))

# define midpoints to draw dams
dam_x <- c(250,750,1250,1750)

# Define arbitrary extents for graphics (ground)
x <- c(0,0,dam_x[1]-62.5,dam_x[1]+62.5, dam_x[2]-62.5,dam_x[2]+62.5, 
       dam_x[3]-62.5,dam_x[3]+62.5, dam_x[4]-62.5,dam_x[4]+62.5,2000,2000)
y <- c(-8,55,46,46,34,34,21,21,-2,-2,-4,-8)
positions <- data.frame(x,y)

# Define extents for water
water_positions <- data.frame(
  x = c(0,0,dam_x[1],dam_x[1],0,0,dam_x[1],dam_x[1],
        dam_x[1],dam_x[1],dam_x[2],dam_x[2],dam_x[1],dam_x[1],dam_x[2],dam_x[2],
        dam_x[2],dam_x[2],dam_x[3],dam_x[3],dam_x[2],dam_x[2],dam_x[3],dam_x[3],
        dam_x[3],dam_x[3],dam_x[4],dam_x[4],dam_x[3],dam_x[3],dam_x[4],dam_x[4],
        dam_x[4],dam_x[4],2000,2000),
  y= c(0,round(station_DF$CurrentElev[1],digits=1),round(station_DF$CurrentElev[1],digits=1),0,0,ouachita_DF$Normal.Pool[1],ouachita_DF$Normal.Pool[1],0,
       0,round(station_DF$CurrentElev[2],digits=1),round(station_DF$CurrentElev[3],digits=1),0,0,ouachita_DF$Normal.Pool[2],ouachita_DF$Normal.Pool[2],0,
       0,round(station_DF$CurrentElev[4],digits=1),round(station_DF$CurrentElev[5],digits=1),0,0,ouachita_DF$Normal.Pool[3],ouachita_DF$Normal.Pool[3],0,
       -4,round(station_DF$CurrentElev[6],digits=1),round(station_DF$CurrentElev[7],digits=1),-4,-4,ouachita_DF$Normal.Pool[4],ouachita_DF$Normal.Pool[4],-4,
       -4,station_DF$CurrentElev[8],station_DF$CurrentElev[8],-4),
  fill=c(rep("dodgerblue2",4),rep("steelblue4",4),
         rep("dodgerblue2",4),rep("steelblue4",4),
         rep("dodgerblue2",4),rep("steelblue4",4),
         rep("dodgerblue2",4),rep("steelblue4",4),
         rep("dodgerblue2",4)),
  stringsAsFactors = F
)

# use conditional statements to plot either normal pool or current pool first.
# if current pool is higher than normal pool ==> plot current pool first so it appears behind normal pool.
# if normal pool is higher than current pool ==> plot normal pool first so it appears behind current pool
if(water_positions$y[2]>=ouachita_DF$Normal.Pool[1] | water_positions$y[3]>=ouachita_DF$Normal.Pool[1]){
  T_grp <- c(rep(1,4),rep(2,4))
} else{
  T_grp <- c(rep(2,4),rep(1,4))
}

if(water_positions$y[10]>=ouachita_DF$Normal.Pool[2] | water_positions$y[11]>=ouachita_DF$Normal.Pool[2]){
  F_grp <- c(rep(3,4),rep(4,4))
} else{
  F_grp <- c(rep(4,4),rep(3,4))
}

if(water_positions$y[18]>=ouachita_DF$Normal.Pool[3] | water_positions$y[19]>=ouachita_DF$Normal.Pool[3]){
  C_grp <- c(rep(5,4),rep(6,4))
} else{
  C_grp <- c(rep(6,4),rep(5,4))
}

if(water_positions$y[26]>=ouachita_DF$Normal.Pool[4] | water_positions$y[27]>=ouachita_DF$Normal.Pool[4]){
  J_grp <- c(rep(7,4),rep(8,4))
} else{
  J_grp <- c(rep(8,4),rep(7,4))
}

JD_grp <- c(rep(9,4))

grp <- c(T_grp, F_grp, C_grp, J_grp, JD_grp)
water_positions['grp'] <- grp
water_positions$grp <- factor(water_positions$grp, levels=c(1,2,3,4,5,6,7,8,9))
water_positions <- arrange(water_positions,water_positions$grp)

# Define label for datetime
date <- f_date(Sys.Date(), format = "%B %d, %Y")
date <- paste0(date, ",")
time <- f_12_hour(x = Sys.time(), format="%I %p")
date_time <- paste("Current as of", date, "at", time)

# Define locations for normal pool labels
norm_pool_x2 = c(100, 500, 1000, 1500)

# Create Wave Data frame for flows
wave <- data.frame(x=c(0,100))

####################### CREATE THE GRAPHIC ####################### 
p1 <- ggplot() +  
  
  # Draw the water polygons 
  geom_polygon(data=water_positions, aes(x=x,y=y,fill=factor(grp)))+
  scale_fill_manual(values = c("1"=water_positions$fill[1],
                               "2"=water_positions$fill[5],
                               "3"=water_positions$fill[9],
                               "4"=water_positions$fill[13],
                               "5"=water_positions$fill[17],
                               "6"=water_positions$fill[21],
                               "7"=water_positions$fill[25],
                               "8"=water_positions$fill[29],
                               "9"=water_positions$fill[33]))+   
  
  # Annotate Normal Pools
  geom_text(aes(x=norm_pool_x2,y=ouachita_DF$Normal.Pool-4,label=paste(ouachita_DF$Normal.Pool, "ft")),
            size=2.5,hjust=0.5,color="steelblue2")+
  
  # Draw the structure polygons
  geom_rect(aes(xmin=dam_x-35,xmax=dam_x+35,ymin=-4,ymax=ouachita_DF$Service.Bridge),fill="grey67")+
  
  # Draw the ground
  geom_polygon(data = positions, mapping = aes(x=positions$x, y=positions$y), fill="tan") +
  
  # Draw Dam Names
  geom_text(mapping=aes(x=dam_x,y=ouachita_DF$Service.Bridge+10,label=ouachita_DF$Station.Names),size=3.5,
            fontface=2, hjust=0.5)+
  
  # Draw HW
  geom_text(mapping = aes(x = dam_x-45, y = c(station_DF$CurrentElev[1],station_DF$CurrentElev[3],
                                              station_DF$CurrentElev[5],station_DF$CurrentElev[7])-4), label="HW",
            size = 2.5, color = "white", fontface=2, hjust=1) +
  geom_text(mapping = aes(x = dam_x-45, y = c(station_DF$CurrentElev[1],station_DF$CurrentElev[3],
                                              station_DF$CurrentElev[5],station_DF$CurrentElev[7])+6,
                          label=c(paste(round(station_DF$CurrentElev[1],digits=1),"ft"),
                                  paste(round(station_DF$CurrentElev[3],digits=1),"ft"),
                                  paste(round(station_DF$CurrentElev[5],digits=1),"ft"),
                                  paste(round(station_DF$CurrentElev[7],digits=1),"ft"))),
            size = 2.5, color = "dodgerblue2", fontface=2, hjust=1) +
  
  # Draw TW
  geom_text(mapping = aes(x = dam_x+45, y = c(station_DF$CurrentElev[2],station_DF$CurrentElev[4],
                                              station_DF$CurrentElev[6],station_DF$CurrentElev[8])-6), label="TW",
            size = 2.5, color = "white", fontface=2, hjust=0) +
  geom_text(mapping = aes(x = dam_x+45, y = c(station_DF$CurrentElev[2],station_DF$CurrentElev[4],
                                              station_DF$CurrentElev[6],station_DF$CurrentElev[8])+6,
                          label=c(paste(round(station_DF$CurrentElev[2],digits=1),"ft"),
                                  paste(round(station_DF$CurrentElev[4],digits=1),"ft"),
                                  paste(round(station_DF$CurrentElev[6],digits=1),"ft"),
                                  paste(round(station_DF$CurrentElev[8],digits=1),"ft"))),
            size = 2.5, color = "dodgerblue2", fontface=2, hjust=0) +
  
  # Draw elev label inidcators
  geom_segment(aes(y=ouachita_DF$Lock.Wall, yend=ouachita_DF$Lock.Wall, x = dam_x-35,
                   xend=dam_x+35), size = 1, color='#ff484d') +
  geom_segment(aes(y=ouachita_DF$Navigation.Pass, yend=ouachita_DF$Navigation.Pass, x = dam_x-35,
                   xend=dam_x+35), size = 1, color='#d20006') +
  geom_segment(aes(y=ouachita_DF$Gate.Sill, yend=ouachita_DF$Gate.Sill, x = dam_x-35,
                   xend=dam_x+35), size = 1, color='#5c0003') +
  
  # Draw elev labels
  geom_text(mapping=aes(x=dam_x, y=ouachita_DF$Lock.Wall+4,
                        label=paste(as.character(ouachita_DF$Lock.Wall), "ft")),size=2.3,
            color='#ff484d', fontface=2, hjust=0.5)+
  geom_text(mapping=aes(x=dam_x, y=ouachita_DF$Navigation.Pass+4,
                        label=paste(as.character(ouachita_DF$Navigation.Pass), "ft")),size=2.3,
            color='#d20006', fontface=2, hjust=0.5)+
  geom_text(mapping=aes(x=dam_x, y=ouachita_DF$Gate.Sill+4,
                        label=paste(as.character(ouachita_DF$Gate.Sill), "ft")),size=2.3,
            color='#5c0003', fontface=2, hjust=0.5)+
  
  # Draw title 
  annotate("text", x= 1000, y=160, label="Ouachita River Navigation Project", color = "black", size = 7, fontface =2) + 
  annotate("text", x= 1000, y=147, label=date_time, color = "black", size = 5, fontface =2) + 
  
  # Draw Legend
  annotate("text", x=0, y=-12, label='Legend', color="black", size=3, fontface=1, hjust=0)+
  geom_segment(aes(y=-18,yend=-18,x = 0,xend=75),size = 1, color='#ff484d') +
  annotate("text", x=85, y=-18, label='Lock Wall Elevation', size=2.5, hjust=0)+
  geom_segment(aes(y=-24, yend=-24,x=0,xend=75), size = 1, color='#d20006') +
  annotate("text", x=85, y=-24, label='Naviation Pass Elevation', size=2.5, hjust=0)+
  geom_segment(aes(y=-30, yend=-30,x=0,xend=75), size = 1, color='#5c0003') +
  annotate("text", x=85, y=-30, label='Gate Sill Elevation', size=2.5, hjust=0)+
  
  geom_polygon(aes(x=c(500,540,540,500), y=c(-22,-22,-26,-26)), fill="dodgerblue2") +
  annotate("text", x=550,y=-24,label='Current Pool Elevation', size=2.5, hjust=0) +
  geom_polygon(aes(x=c(500,540,540,500),y=c(-28,-28,-32,-32)), fill="steelblue4")+
  annotate("text", x=550,y=-30,label='Normal Pool Elevation', size=2.5, hjust=0) +
  annotate("text", x=500,y=-18,label="River Mile (RM) of Lock and Dam",size=2.5, hjust=0,fontface=1, color="tan4")+
  
  # Draw flow 
  annotate("segment", x=999, xend=999.5, y=35, yend=35, linejoin="round", 
           arrow=arrow(type="closed", length=unit(0.015, "npc")), color="#18115e")+
  annotate("segment", x=999, xend=999.5, y=39, yend=39, linejoin="round", 
           arrow=arrow(type="closed", length=unit(0.015, "npc")), color="#18115e")+
  
  geom_text(mapping = aes(x = 1009.5, y = 37,label = MonroeFlow), 
            size = 2.5, color = "#18115e", fontface=2, hjust=0) +
  
  geom_segment(aes(x=1000,xend=1000,y=30,yend=25), color="#18115e")+
  annotate("text",x=1000,y=20,label="Ouachita River @", hjust=0.5,
           size=2.5, fontface=2, color="#18115e")+
  annotate("text",x=1000,y=16,label="Monroe", hjust=0.5,
           size=2.5, fontface=2, color="#18115e")+
  
  
  stat_function(data=wave,aes(wave$x), fun=function(x){sin(x)+36}, 
                xlim=c(875,990), n=21, color="#18115e")+
  stat_function(data=wave,aes(wave$x), fun=function(x){sin(x)+40}, 
                xlim=c(875,990), n=21, color="#18115e")+
  
  # annotate river miles
  annotate("text",x=250,y=43,label="RM 281.9", fontface=1, hjust=0.5, size=2.5, color="tan4")+
  annotate("text",x=750,y=31,label="RM 226.9",fontface=1, hjust=0.5, size=2.5, color="tan4")+
  annotate("text",x=1250,y=19,label="RM 117.2",size=2.5,fontface=1,hjust=0.5,color="tan4")+
  annotate("text", x=1750,y=-4,label="RM 25.0", size=2.5, fontface=1, hjust=0.5, color="tan4")+
  
  # Set theme and scale for graphic
  theme_void() + 
  theme(legend.position="none") +
  scale_y_continuous(limits = c(-32, 160)) +
  scale_x_continuous(limits = c(0,2000)) +
  theme(panel.background = element_rect(fill='white',color='white'))

# add logos to plot
p1+
  
  annotation_custom(img1, xmin=-360, xmax=510, ymin=138, ymax=166) +
  annotation_custom(img2, xmin=1600, xmax=1900, ymin=-35, ymax=-10) +
  annotation_custom(img3, xmin=1870, xmax=2000, ymin=-35, ymax=-10)


# Save the final graphic
ggsave(filename = paste0(graphicDirectory, "ouachita_structures.png"), plot = last_plot(), dpi = 300, width = 8.5, height = 5.5)
