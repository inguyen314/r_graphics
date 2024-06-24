## Script Objective: Create the Reservoir Status Graphics for MVS Water Control
## Script Author: Melinda Pullman Revised By: Ivan Nguyen
## Date Created: 01/25/2024
## Instructions: Change variables Under Line 28 to appropriate variables to run the script.You Will also need to 
## ready in JSON file, NO web scraping
## version 3: replaced location levels in JSON
## version 4: have all data read from JSON

####################### Import Packages #######################
library(ggplot2); library(numform); library(plyr);
library(dplyr); library(lubridate); library(rvest); 
library(stringr);library(stringi); library(RColorBrewer); 

####################### DEFINE RELEVANT FUNCTIONS #######################
# Define function to Wrap long labels for the graphic,
# paste to collapse the wrapped text with a newline separator
# stri_wrap function to wrap the text
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


####################### READ DATA FROM JSON #######################
# Install and load the jsonlite package if not already installed
# install.packages("jsonlite")
library(jsonlite)

# URLs of the JSON files
json_urls <- c(
  "https://wm.mvs.ds.usace.army.mil/r/carlyle.json",
  "https://wm.mvs.ds.usace.army.mil/r/shelbyville.json",
  "https://wm.mvs.ds.usace.army.mil/r/wappapello.json",
  "https://wm.mvs.ds.usace.army.mil/r/mark_twain.json",
  "https://wm.mvs.ds.usace.army.mil/r/rend.json"
)

# Create a list to store the parsed data
json_data_list <- list()


# Create a vector to store data
ReservoirNames <- character(length(json_urls))
CurrentPool <- character(length(json_urls))
CurrentPoolStr <- character(length(json_urls))
CurrentTail <- character(length(json_urls))
CurrentTailStr <- character(length(json_urls))
Inflow <- character(length(json_urls))
Outflow <- character(length(json_urls))
SpillwayFlow <- character(length(json_urls))
Change24 <- character(length(json_urls))
GuideCurveElev <- character(length(json_urls))
Streambed <- character(length(json_urls))
BottomofFlood <- character(length(json_urls))
TopofFlood <- character(length(json_urls))
TopofDam <- character(length(json_urls))


# Read JSON data from each URL and extract "project" dynamically
for (i in seq_along(json_urls)) {
  # Read JSON data
  json_data <- fromJSON(json_urls[i])
  
  ReservoirNames[i] <- json_data[[1]]$project
  CurrentPool[i] <- as.numeric(json_data[[1]]$pool_stage)
  CurrentPoolStr[i] <- json_data[[1]]$pool_stage
  CurrentTail[i] <- as.numeric(json_data[[1]]$tw_stage)
  CurrentTailStr[i] <- json_data[[1]]$tw_stage
  Inflow[i] <- as.numeric(json_data[[1]]$inflow)
  Outflow[i] <- as.numeric(json_data[[1]]$outflow_midnight)
  SpillwayFlow[i] <- as.numeric(json_data[[1]]$outflow_evening)
  Change24[i] <- as.numeric(json_data[[1]]$pool_24hr)
  GuideCurveElev[i] <- as.numeric(json_data[[1]]$guide_curve)
  Streambed[i] <- as.numeric(json_data[[1]]$streambed)
  BottomofFlood[i] <- as.numeric(json_data[[1]]$bottom_of_flood)
  TopofFlood[i] <- as.numeric(json_data[[1]]$top_of_flood)
  TopofDam[i] <- as.numeric(json_data[[1]]$top_of_dam)
}

# Print the result for each URL
for (i in seq_along(json_urls)) {
  cat("Data from URL:", json_urls[i], "\n")
  
  # Print the extracted "project" for each URL
  cat("ReservoirName:", ReservoirNames[i], "\n")
  cat("CurrentPool:", CurrentPool[i], "\n")
  cat("CurrentPoolStr:", CurrentPoolStr[i], "\n")
  cat("CurrentTail:", CurrentTail[i], "\n")
  cat("CurrentTailStr:", CurrentTailStr[i], "\n")
  cat("Inflow:", Inflow[i], "\n")
  cat("Outflow:", Outflow[i], "\n")
  cat("SpillwayFlow:", SpillwayFlow[i], "\n")
  cat("Change24:", Change24[i], "\n")
  cat("GuideCurveElev:", GuideCurveElev[i], "\n")
  cat("Streambed:", Streambed[i], "\n")
  cat("BottomofFlood:", BottomofFlood[i], "\n")
  cat("TopofFlood:", TopofFlood[i], "\n")
  cat("TopofDam:", TopofDam[i], "\n")
  cat("\n")
}


Res_data <- data.frame(
  ReservoirName = ReservoirNames,
  CurrentPool = as.numeric(CurrentPool),
  CurrentPoolStr,
  CurrentTail = as.numeric(CurrentTail),
  CurrentTailStr,
  Inflow = as.numeric(Inflow),
  Outflow = as.numeric(Outflow),
  SpillwayFlow = as.numeric(SpillwayFlow),
  Change24 = as.numeric(Change24),
  GuideCurveElev = as.numeric(GuideCurveElev),
  stringsAsFactors = FALSE
)


####################### Create Dataframe with important reservoir extents #######################
Res_data2 <- cbind(Res_data, Streambed = as.numeric(Streambed), 
                   BottomofFlood = as.numeric(BottomofFlood), 
                   TopofFlood = as.numeric(TopofFlood), 
                   TopofDam = as.numeric(TopofDam))


####################### Start building graphic elements to plot #######################
for (i in 1:length(Res_data2$ReservoirName)){
  # Define water extent for pool
  pool_x <- c(375, 375, 680, 680)
  pool_y <- as.numeric(c(
    Res_data2$Streambed[i],
    Res_data$CurrentPool[i],
    Res_data$CurrentPool[i],
    Res_data2$Streambed[i]
  ))
  pool_water_positions <- data.frame(pool_x, pool_y)
  
  # Define water extent for tail
  tail_x <- c(1060, 1060, 1800, 1800)
  tail_y <- as.numeric(c(
    Res_data2$Streambed[i] - 5,
    Res_data2$Streambed[i] + 5,
    Res_data2$Streambed[i] + 5,
    Res_data2$Streambed[i] - 5
  ))
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
             label= paste(Res_data$ReservoirName[i], "Lake, MVS"), 
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
    scale_x_continuous(limits = c(25,1800))
    #theme(panel.background = element_rect(fill='white',color='white'))
  
  plot(p1)
  #p1
  # Save the Graphics
  ggsave(filename = paste0(graphicDirectory, sprintf("%sReservoirStatus.png", Res_data2$ReservoirName[i])),
         plot = last_plot(), dpi = 300, width = 8.5, height = 5)
  
}

#dev.off() = to restore plots data frame