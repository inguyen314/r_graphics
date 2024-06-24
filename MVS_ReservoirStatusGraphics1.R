## Script Objective: Create the Reservoir Status Graphics for MVK Water Control
## Script Author: Melinda Pullman
## Date Created: 03/03/2020
## Instructions: Change variables Under Line 28 to appropraite variables to run the script.You Will also need to 
## update the webscraping section (beginning in line 42) to grab data from your own data sources

####################### Import Packages #######################
library(ggplot2); library(numform); library(plyr);
library(dplyr); library(lubridate); library(rvest); 
library(stringr);library(stringi); library(RColorBrewer); 

####################### DEFINE RELEVANT FUNCTIONS #######################
# Define function to Wrap long labels for the graphic
wrapper <- function(x, ...) paste(stri_wrap(x, ...), collapse = "\n")

# Function to clean up HTML
clean_up <- function(x) {
  sp <- stringr::str_replace_all(x, "[\r\t\n]", "")
  rm <- str_split(sp, " +")
  return(rm)
}

# Function to obtain images for graphic
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

####################### CHANGE THESE VARIABLES TO RUN SCRIPT #######################
# Location to Store Graphics: 
graphicDirectory <- 'C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/MVK_GraphicScripts/'

# Define Location for Supporting Images
img1 <- get_png("C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/MVK_GraphicScripts/cloud.png")
img2 <- get_png("C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/MVK_GraphicScripts/usacelogo.png")

# Define Important reservoir extents
Streambed <- c(189.3, 204.0, 194.0, 160.0)
BottomofFlood <- c(209.3, 236.0, 230.0, 193.0)
TopofFlood <- c(238.3, 281.4, 268.0, 231.0)
TopofDam <- c(264.3, 311.4, 293.0, 256.0)

####################### WEBSCRAPE DATA FROM MVK WATER MANAGEMENT WEBSITE #######################
# Define Reservoir Names & Table Indices for webscraping data: 
res_indices <- c(3, 4, 5, 6)
reservoir_URL <- 'https://www.mvk-wc.usace.army.mil/resrep.htm'
yazooRes_table <- html_table(html_nodes(read_html(reservoir_URL), 'table')[2], fill=T)

# Collect reservoir data from our website
ReservoirName <- c()
CurrentPool <- c()
CurrentPoolStr <- c()
CurrentTail <- c()
CurrentTailStr <- c()
Inflow <- c()
Outflow <- c()
SpillwayFlow <- c()
Change24 <- c()
GuideCurveElev <- c()

for (i in 1:length(res_indices)){
  ReservoirName[i] <- yazooRes_table[[1]]$X1[res_indices[i]]
  CurrentPool[i] <- sprintf("%0.1f", as.numeric(yazooRes_table[[1]]$X3[res_indices[i]]))
  CurrentPoolStr[i] <- paste0(sprintf("%0.1f", as.numeric(yazooRes_table[[1]]$X3[res_indices[i]])), "'")
  CurrentTail[i] <- sprintf("%0.1f", as.numeric(yazooRes_table[[1]]$X5[res_indices[i]]))
  CurrentTailStr[i] <- paste0(sprintf("%0.1f", as.numeric(yazooRes_table[[1]]$X5[res_indices[i]])), "'")
  Inflow[i] <- paste(yazooRes_table[[1]]$X6[res_indices[i]], "cfs")
  Outflow[i] <- paste(yazooRes_table[[1]]$X7[res_indices[i]], "cfs")
  SpillwayFlow[i] <- paste(yazooRes_table[[1]]$X8[res_indices[i]], "cfs")
  Change24[i] <- sprintf("%0.1f", as.numeric(yazooRes_table[[1]]$X4[res_indices[i]]))
  GuideCurveElev[i] <- as.numeric(yazooRes_table[[1]]$X10[res_indices[i]])
}

Res_data <- data.frame(ReservoirName, CurrentPool, CurrentPoolStr, CurrentTail, CurrentTailStr,
                       Inflow, Outflow, SpillwayFlow, Change24, GuideCurveElev, stringsAsFactors = F)

####################### Create Dataframe with important reservoir extents #######################
Res_data2 <- cbind(Res_data, "Streambed"=Streambed,
                   "BottomofFlood" = BottomofFlood, "TopofFlood" = TopofFlood, 
                   "TopofDam" = TopofDam)

####################### Start building graphic elements to plot #######################
for (i in 1:length(Res_data2$ReservoirName)){
  
  # Define water extent
  pool_x <- c(375, 375, 680, 680)
  pool_y <- as.numeric(c(Res_data2$Streambed[i], Res_data$CurrentPool[i], Res_data$CurrentPool[i], 
                         Res_data2$Streambed[i]))
  pool_water_positions <- data.frame(pool_x, pool_y)
  
  tail_x <- c(1060,1060,1800,1800)
  tail_y <- as.numeric(c(Res_data2$Streambed[i]-5, Res_data2$Streambed[i]+5, Res_data2$Streambed[i]+5, 
                         Res_data2$Streambed[i]-5))
  tail_water_positions <- data.frame(tail_x, tail_y)
  
  # Define reservoir extent
  resx <- c(375,375,680,680,625,625,880,880,840,840,960,1060,1060)
  resy <- c(Res_data2$Streambed[i]-5, Res_data2$Streambed[i], Res_data2$Streambed[i],
            Res_data2$TopofDam[i]-5, Res_data2$TopofDam[i]-5, Res_data2$TopofDam[i], 
            Res_data2$TopofDam[i], Res_data2$TopofDam[i]-5, Res_data2$TopofDam[i]-7, 
            Res_data2$TopofDam[i]-15, Res_data2$Streambed[i]+15, Res_data2$Streambed[i]+15,
            Res_data2$Streambed[i]-5)
  positions <- data.frame(resx,resy)
  
  # Define % full bar
  perc_full <- data.frame("PercFull" = 0:100,
                          "y" = seq(Res_data2$BottomofFlood[i], 
                                    Res_data2$TopofFlood[i], len=101))
  ark_perc_full <- c(209.3, 222.40, 229.3, 234.40, 238.3)
  sard_perc_full <- c(236.0, 255.5, 266.5, 275.0, 281.4)
  enid_perc_full <- c(230.0, 246.0, 255.5, 262.0, 268.0)
  gren_perc_full <- c(193.0, 210.5, 219.5, 226.0, 231.0)
  perc_full_labels2 <- data.frame(ark_perc_full, sard_perc_full, enid_perc_full, gren_perc_full)
  
  # Define mountain extent
  mountx <- c(25,35,55,80,85,150,160,175,190,200)
  mounty <- c(Res_data2$TopofDam[i]+7, Res_data2$TopofDam[i]+10, Res_data2$TopofDam[i]+13,
              Res_data2$TopofDam[i]+15, Res_data2$TopofDam[i]+17, Res_data2$TopofDam[i]+20,
              Res_data2$TopofDam[i]+14, Res_data2$TopofDam[i]+12, Res_data2$TopofDam[i]+11,
              Res_data2$TopofDam[i]+7)
  mountdf <- data.frame(mountx, mounty)
  
  mx2 <- c(45,50,55,90,70,75,85)
  my2 <- c(Res_data2$TopofDam[i]+8, Res_data2$TopofDam[i]+10, Res_data2$TopofDam[i]+12,
           Res_data2$TopofDam[i]+14, Res_data2$TopofDam[i]+11, Res_data2$TopofDam[i]+9,
           Res_data2$TopofDam[i]+8)
  mdf2 <- data.frame(mx2, my2)
  
  mx3 <- c(145,142,130,144,150,160)
  my3 <- c(Res_data2$TopofDam[i]+13, Res_data2$TopofDam[i]+14,Res_data2$TopofDam[i]+18,
           Res_data2$TopofDam[i]+17, Res_data2$TopofDam[i]+14,Res_data2$TopofDam[i]+13)
  mdf3 <- data.frame(mx3, my3)
  
  mx4 <- c(160,155,150,140,180)
  my4 <- c(Res_data2$TopofDam[i]+8, Res_data2$TopofDam[i]+10,Res_data2$TopofDam[i]+10,
           Res_data2$TopofDam[i]+12, Res_data2$TopofDam[i]+8)
  mdf4 <- data.frame(mx4, my4)
  
  # Define elevation scale bar 
  scale_elev_y <- seq(round_any(Res_data2$Streambed[i],5), round_any(Res_data2$TopofDam[i]-3, 5), 5)
  scale_elev_x <- rep(360, length(scale_elev_y))
  scale_elev_xbegin <- scale_elev_x-10
  scale_elev_xend<- scale_elev_x+10
  scale_elev_DF <- data.frame(scale_elev_y, scale_elev_x, scale_elev_xbegin, scale_elev_xend)
  
  # Define inflow/outflow arrows
  point_x <- c(500,950,1015)
  point_y <- c(Res_data2$TopofDam[i]+10, Res_data2$TopofFlood[i], Res_data2$Streambed[i]+10)
  point_label <- c("Inflow", "Spillway", "Outflow")
  point_label_short <- c("IN", "SPWY", "OUT")
  point_flow <- c(Res_data$Inflow[i], Res_data$SpillwayFlow[i], Res_data$Outflow[i])
  point_color <- c("steelblue2", "steelblue2", "steelblue4")
  point_DF <- data.frame(point_x, point_y, point_label, point_label_short,
                         point_flow, point_color, stringsAsFactors = F)
  arrow1x <-c(1000,1000)
  arrow1y <-c(point_DF$point_y[3],point_DF$point_y[3]-10)
  arrow1DF <- data.frame(arrow1x, arrow1y)
  
  # Define datetime
  date <- f_date(Sys.Date(), format = "%B %d, %Y")
  date <- paste0(date, ",")
  time <- f_12_hour(x = Sys.time(), format="%I %p")
  date_time <- paste(date, time, "CST")
  
  # Create the Graphic
  p1 <- ggplot() +  
    
    # Add Supporting Graphics to Plot 
    annotation_custom(img1, xmin=200, xmax=300, ymin=Res_data2$TopofDam[i]+8, ymax=Res_data2$TopofDam[i]+28) +
    annotation_custom(img2, xmin=1625, xmax=1800, ymin=Res_data2$TopofDam[i], ymax=Res_data2$TopofDam[i]+50) +
    
    # Draw the Reservoir Polygon
    geom_polygon(data = positions, mapping = aes(x=resx, y=resy), fill="grey") +
    
    # Draw Flow Circles 
    geom_point(data = point_DF, mapping = aes(x=point_x, y=point_y, color=point_color), linewidth = 10) + 
    scale_colour_manual(values = c("skyblue2", "steelblue4"), 
                        labels = c("Inflow", "Spillway", "Outflow")) +
    
    # Draw Inflow Line Segments/Arrows
    geom_segment(data = point_DF, aes(x=50, xend=point_x[1], y=point_y[1], yend=point_y[1]), linewidth=2, 
                 color="skyblue2") + 
    geom_segment(data = point_DF, aes(x=point_x[1], xend=point_x[1], y=point_y[1], yend=point_y[1]-10), 
                 linewidth=2, color="skyblue2") +
    annotate("segment", x=point_DF$point_x[1], xend=point_DF$point_x[1], y=point_DF$point_y[1], 
             yend=point_DF$point_y[1]-12, linejoin="round", 
             arrow=arrow(type="closed", length=unit(0.03, "npc")), color="skyblue2")+
    
    # Draw Spillway Line Segment/Arrow
    geom_segment(data = point_DF, aes(x=point_x[2], xend=point_x[2]+120, y=point_y[2],
                     yend=point_y[2]), linewidth=2, color="skyblue2") +
    annotate("segment", x=point_DF$point_x[2], xend=point_DF$point_x[2]+140, y=point_DF$point_y[2], 
             yend=point_DF$point_y[2], linejoin="round", 
             arrow=arrow(type="closed", length=unit(0.03, "npc")), color="skyblue2")+
    
    # Draw Outflow Line Segment/Arrow
    geom_segment(data = point_DF, aes(x=point_x[3], xend=point_x[3]+120, y=point_y[3], yend=point_y[3]), 
                 color="steelblue4", linewidth=2) +
    annotate("segment", x=point_DF$point_x[3], xend=point_DF$point_x[3]+140, y=point_DF$point_y[3], 
             yend=point_DF$point_y[3], linejoin="round", 
             arrow=arrow(type="closed", length=unit(0.03, "npc")), color="steelblue4")+
    
    # Draw Flow annotations (text)
    geom_text(data = point_DF, mapping = aes(x=point_x, y=point_y, label=point_label_short), 
              linewidth = 2.5, color = "white", fontface=2) + 
    geom_text(data = point_DF, mapping = aes(x=point_x[1]+50, y=point_y[1], label=point_flow[1]),
              linewidth = 2.5, color= "grey19", hjust=0)+
    geom_text(data = point_DF, mapping=aes(x=point_x[2]+50, y=point_y[2]+5, label=point_flow[2]),
              linewidth = 2.5, color= "grey19", hjust=0)+
    geom_text(data = point_DF, mapping=aes(x=point_x[3]+50, y=point_y[3]+5, label=point_flow[3]),
              linewidth = 2.5, color= "grey19", hjust=0)+
    
    # Draw Pool/Tailwater Polygons
    geom_polygon(data = pool_water_positions, mapping = aes(x=pool_x, y=pool_y), fill="lightskyblue1") +
    geom_polygon(data = tail_water_positions, mapping = aes(x=tail_x, y=tail_y), fill="skyblue2") +
    
    # Draw the Mountain
    geom_polygon(data=mountdf, mapping=aes(x=mountx, y=mounty), fill="grey")+
    geom_polygon(data=mdf2, mapping=aes(x=mx2, y = my2), fill="gray50")+
    geom_polygon(data=mdf3, mapping=aes(x=mx3, y = my3), fill="gray50")+
    geom_polygon(data=mdf4, mapping=aes(x=mx4, y = my4), fill="gray50")+
    
    # Draw the remaining ground connecting Mountain to reservoir
    geom_segment(data = Res_data2, aes(x=375, xend=375, y=Streambed[i]-5, yend=TopofDam[i]+5.5),
               color = "grey", linewidth = 2) +
    geom_curve(data = Res_data2, aes(x=375, xend=350, y=TopofDam[i]+5, yend=TopofDam[i]+7),
             curvature = 0.2, angle = 90, ncp=100,  linewidth = 2, color="grey")+
    geom_segment(data= Res_data2, aes(x=351, xend=25, y=TopofDam[i]+7, yend=TopofDam[i]+7),
               color = "grey", linewidth = 2) +
    
    # Draw % Full scale
    geom_tile(data = perc_full, mapping = aes(y = y,x=750, fill=PercFull),
              width=35)+
    scale_fill_gradient2(low="green", mid="yellow", high="red", midpoint=50)+
    annotate("text", x= 800, y= perc_full_labels2[[i]][1], label= "0%", color = "gray19", linewidth = 2, 
             fontface =1) + 
    annotate("text", x= 800, y= perc_full_labels2[[i]][2], label= "25%", color = "gray19", linewidth = 2, 
             fontface =1) + 
    annotate("text", x= 800, y= perc_full_labels2[[i]][3], label= "50%", color = "gray19", linewidth = 2, 
             fontface =1) + 
    annotate("text", x= 800, y= perc_full_labels2[[i]][4], label= "75%", color = "gray19", linewidth = 2, 
             fontface =1) + 
    annotate("text", x= 800, y= perc_full_labels2[[i]][5], label= "100%", color = "gray19", linewidth = 2, 
             fontface =1) + 
    
    # Draw Elevation Scale
    geom_segment(data = scale_elev_DF, mapping = aes(x=scale_elev_xbegin, xend=scale_elev_xend, 
                                                     y=scale_elev_y, yend=scale_elev_y),
                 color = "grey", linewidth = 0.4) + 
    geom_text(data = scale_elev_DF, mapping = aes(x=scale_elev_xbegin-30, y=scale_elev_y, 
                                                  label=scale_elev_y), 
              linewidth = 2, color = "gray60") + 
    
    # Draw Reservoir Pertinent Elevation Line Segments 
    geom_segment(data=Res_data2, aes(y=Streambed[i], yend=Streambed[i], 
                     x = 250, xend=370), linewidth = 0.5, linetype=8, color='red') + 
    geom_segment(data=Res_data2, aes(y=BottomofFlood[i], yend=BottomofFlood[i], 
                     x = 250, xend=735), linewidth = 0.5, linetype=8, color='red') + 
    geom_segment(data=Res_data2, aes(y=TopofFlood[i], yend=TopofFlood[i], 
                     x = 250, xend=735), linewidth = 0.5, linetype=8, color='red') + 
    geom_segment(data=Res_data2, aes(y=TopofDam[i], yend=TopofDam[i], 
                     x = 250, xend=370), linewidth = 0.5, linetype=8, color='red') + 
    geom_segment(data=Res_data2, aes(y=GuideCurveElev[i], yend=GuideCurveElev[i],
                     x=pool_water_positions$pool_x[1]+8, 
                     xend=pool_water_positions$pool_x[3]), linewidth=0.5, linetype=8, color='lightskyblue4') + 
    
    # Draw the Reservoir Pertinent Elevation Labels
    annotate("text", x= 240, y= Res_data2$TopofDam[i]-1, 
             label= wrapper(paste("Top of Dam", paste0(Res_data2$TopofDam[i], "'")), width=10), 
             color = "red", linewidth = 2, fontface =1, hjust=1) + 
    annotate("text", x= 240, y= Res_data2$TopofFlood[i]-1, 
             label= wrapper(paste("Top of Flood", paste0(Res_data2$TopofFlood[i], "'")), width=15),
             color = "red", linewidth = 2, fontface =1, hjust=1) + 
    annotate("text", x= 240, y= Res_data2$BottomofFlood[i]-1, 
             label= wrapper(paste("Bottom of Flood", paste0(Res_data2$BottomofFlood[i], "'")), width=15), 
             color = "red", linewidth = 2, fontface =1, hjust=1) + 
    annotate("text", x= 240, y= Res_data2$Streambed[i]-1, 
             label= wrapper(paste("Streambed", paste0(Res_data2$Streambed[i], "'")), width=10),
             color = "red", linewidth = 2, fontface =1, hjust=1) + 
    annotate("text", x= 527.5, y= as.numeric(Res_data2$GuideCurveElev[i])+2, 
             label= paste("Guide Curve Elevation", paste0("(", Res_data2$GuideCurveElev[i], "')")),
             color = "lightskyblue4", linewidth = 2, fontface =1, hjust=0.5) + 
    
    # Draw the Pool and Tailwater Labels
    annotate("text", x= 540, y= as.numeric(Res_data$CurrentPool[i])-1.5, 
             label= Res_data$CurrentPoolStr[i], 
             color = "gray19", linewidth = 3, fontface =1, hjust=2) + 
    annotate("text", x= 660, y= as.numeric(Res_data$CurrentPool[i])-1.5, 
             label= paste0("(24 Hr Change: ", Res_data$Change24[i], ")"), 
             color = "gray19", linewidth = 2.2, fontface =1, hjust=1) + 
    
    annotate("text", x= 1798, y= Res_data2$Streambed[i]+3.5, 
             label= Res_data$CurrentTailStr[i], color = "gray19", linewidth = 3, fontface =1, hjust=1) +
    
    # Draw Title/Additional Graphic Text
    annotate("text", x=1600, y= Res_data2$TopofDam[i]+30, 
             label= "FLOOD CONTROL STATUS", color = "black", linewidth = 4.5, fontface =2, hjust=1) + 
    annotate("text", x=1600, y= Res_data2$TopofDam[i]+24, 
             label= paste(Res_data$ReservoirName[i], "Lake and Dam, MS"), 
             color = "gray19", linewidth = 3.5, fontface =1, hjust=1) + 
    annotate("text", x= 1600, y=Res_data2$TopofDam[i]+19, label= date_time, 
             color = "gray19", linewidth = 3.5, fontface =1, hjust=1) + 
    
    # Draw Legend
    annotate("text", x= 1660, y=Res_data2$TopofDam[i]+10, label= "Legend", 
             color = "black", linewidth = 3, fontface =2, hjust=2) + 
    geom_segment(data = Res_data2, aes(y=TopofDam[i]+6, yend=TopofDam[i]+6, 
                     x = 1460, xend=1800), linewidth = 1, color='grey') +
    geom_rect(data= Res_data2, mapping = aes(xmin=1460, xmax=1500, ymin=TopofDam[i], ymax=TopofDam[i]+4),
              fill="lightskyblue1")+
    annotate("text", x= 1520, y=Res_data2$TopofDam[i]+2, label= "Current Lake Level", 
             color = "gray19", linewidth = 2.5, hjust=0) + 
    geom_rect(data = Res_data2, mapping = aes(xmin=1460, xmax=1500, ymin=TopofDam[i]-1.5, ymax=TopofDam[i]-5.5),
              fill="skyblue2")+
    annotate("text", x= 1520, y=Res_data2$TopofDam[i]-3.5, label= "Tail Water", 
             color = "gray19", linewidth = 2.5, hjust=0) + 
    
    geom_point(data = Res_data2, mapping = aes(x=1480, y=TopofDam[i]-9.5), color="skyblue2", linewidth = 6) + 
    annotate("text", x= 1480, y=Res_data2$TopofDam[i]-9.5, label= "P", 
             color = "white", linewidth = 1.5, fontface=2) + 
    annotate("text", x= 1520, y=Res_data2$TopofDam[i]-9.5, label= "Precipitation", 
             color = "gray19", linewidth = 2.5, hjust=0) + 
    
    geom_point(data = Res_data2, mapping = aes(x=1480, y=TopofDam[i]-16.5), color="skyblue2", linewidth = 6) + 
    annotate("text", x= 1480, y=Res_data2$TopofDam[i]-16.5, label= "IN", 
             color = "white", linewidth = 1.5, fontface=2) + 
    annotate("text", x= 1520, y=Res_data2$TopofDam[i]-16.5, label= "Inflow", 
             color = "gray19", linewidth = 2.5, hjust=0) + 
    geom_point(data = Res_data2, mapping = aes(x=1480, y=TopofDam[i]-23.5), color="skyblue2", linewidth = 6) + 
    annotate("text", x= 1480, y=Res_data2$TopofDam[i]-23.5, label= "SPWY", 
             color = "white", linewidth = 1.5, fontface=2) + 
    annotate("text", x= 1520, y=Res_data2$TopofDam[i]-23.5, label= "Spillway", 
             color = "gray19", linewidth = 2.5, hjust=0) + 
    geom_point(data = Res_data2, mapping = aes(x=1480, y=TopofDam[i]-30.5), color="steelblue4", linewidth = 6) + 
    annotate("text", x= 1480, y=Res_data2$TopofDam[i]-30.5, label= "OUT", 
             color = "white", linewidth = 1.5, fontface=2) + 
    annotate("text", x= 1520, y=Res_data2$TopofDam[i]-30.5, label= "Outflow", 
             color = "gray19", linewidth = 2.5, hjust=0) + 
    
    # Set Theme and Scale for Graphic
    theme_void() + 
    theme(legend.position="none") +
    scale_y_continuous(limits = c(Res_data2$Streambed[i]-10, Res_data2$TopofDam[i]+30)) +
    scale_x_continuous(limits = c(25,1800)) +
    #theme(panel.background = element_rect(fill='white',color='white'))
  
  plot(p1)
  
  # Save the Graphics
  ggsave(filename = paste0(graphicDirectory, sprintf("%sReservoirStatus.png", Res_data2$ReservoirName[i])),
         plot = last_plot(), dpi = 300, width = 8.5, height = 5)
  
}