## Script Objective: Create the Reservoir Status Graphics for MVS Water Control
## Script Author: Melinda Pullman Revised By: Ivan Nguyen
## Date Created: 01/25/2024
## Instructions: Change variables Under Line 28 to appropriate variables to run the script.You Will also need to 
## ready in JSON file, NO web scraping
## version 3: replaced location levels in JSON
## version 4: have all data read from JSON
## version 5. working with MVS JSON files
## version 6. add crest data with new JSON file. Add Precipitation and Generation
## version 7. Add Gen/Turbine for Mark Twain. Fix legend
## version 8. fixed gradient bar issue. you have perc_full_labels2 going from [[i]][1] to [[i]][5].
##            Up above, you define perc_full_labels2 as a data.frame with 4 items (ark_perc_full, sard_perc_full, enid_perc_full, gren_perc_full).
##            You are trying to reference the fifth item in an array that is only holding 4 items, thus your subscript is out of bounds.
## Version 9. Change Mark Twain circle and arrow to the same color (skyblue2) and arrow.
##            Add pdf(file) path to run in task scheduler. 
## Version 10. Fix precip legend. 
## Version 11. Add spill, Total, and Remove Gen. Add envir variables
## Version 12. Add languageserver package and verify spillway.


## TODO: save to pdf, how?
##       perc_full_labels2 values mean?

####################### Import Packages #######################
# List of required packages
required_packages <- c("ggplot2", "numform", "plyr", "dplyr", "lubridate", 
                       "rvest", "stringr", "stringi", "RColorBrewer", "jsonlite", "png")

# Install and load packages if not already installed
for (package_name in required_packages) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  }
}

library(ggplot2); 
library(numform); 
library(plyr);
library(dplyr); 
library(lubridate); 
library(rvest); 
library(stringr);
library(stringi); 
library(RColorBrewer); 
library(jsonlite);
library(png);

####################### DEFINE RELEVANT FUNCTIONS #######################
# Function to Wrap long labels for the graphic
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
if (Sys.getenv("USERNAME") == "B3ECHIHN") {
  graphicDirectory <- 'C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/r_graphics/'
           pdf(file = 'C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/r_graphics/Rplots.pdf')
  
  # Define Location for Supporting Images
  img1 <- get_png("C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/r_graphics/cloud.png")
  img2 <- get_png("C:/Users/B3ECHIHN/Documents/water_control/web/rebuild_internal_web/r_graphics/usace_mvs_logo.png")
} else {
  graphicDirectory <- 'C:/wc/r_graphics/'
  pdf(file = 'C:/wc/r_graphics/Rplots.pdf')
  
  # Define Location for Supporting Images
  img1 <- get_png("C:/wc/r_graphics/cloud.png")
  img2 <- get_png("C:/wc/r_graphics/usace_mvs_logo.png")
}


####################### READ DATA FROM JSON #######################
# URL of the JSON file
json_url <- "https://wm.mvs.ds.usace.army.mil/web_apps/board/public/outputR.json"

# Read JSON data from the URL
json_data <- fromJSON(json_url)

# Initializing empty lists to store data for each reservoir:
ReservoirName <- character()
CurrentPool <- numeric()
CurrentPoolStr <- character()
CurrentPoolDateTimeStr <- character()
CurrentTail <- numeric()
CurrentTailStr <- character()
Inflow <- character()
Outflow <- character()
SpillwayFlow <- character()
TotalFlow <- character()
Change24 <- numeric()
GuideCurveElev <- numeric()
Streambed <- numeric()
BottomofFlood <- numeric()
TopofFlood <- numeric()
TopofDam <- numeric()
Precip <- numeric()
Turbine <- numeric()

# Looping through each reservoir in the JSON data and extracting relevant information:
for (project_id in names(json_data)) {
  reservoir <- json_data[[project_id]]
  ReservoirName <- c(ReservoirName, project_id)
  CurrentPool <- c(CurrentPool, as.numeric(reservoir$pool_stage))
  CurrentPoolStr <- c(CurrentPoolStr, paste0(reservoir$pool_stage, " ft "))
  CurrentPoolDateTimeStr <- c(CurrentPoolDateTimeStr, paste0(substr(reservoir$pool_date_time, start = 1, stop = 5), substr(reservoir$pool_date_time, start = 11, stop = 16)))
  CurrentTail <- c(CurrentTail, as.numeric(reservoir$tw_stage))
  CurrentTailStr <- c(CurrentTailStr, paste0(reservoir$tw_stage, " ft"))
  Inflow <- c(Inflow, paste0(reservoir$inflow, " cfs"))
  Outflow <- c(Outflow, paste0(reservoir$outflow_midnight, " cfs"))
  SpillwayFlow <- c(SpillwayFlow, paste0(reservoir$spillway, " cfs"))
  TotalFlow <- c(TotalFlow, paste0(reservoir$flow_total, " cfs"))
  Change24 <- c(Change24, as.numeric(reservoir$pool_stage24))
  GuideCurveElev <- c(GuideCurveElev, as.numeric(reservoir$rule_curve))
  Streambed <- c(Streambed, as.numeric(reservoir$streambed))
  BottomofFlood <- c(BottomofFlood, as.numeric(reservoir$bottom_of_flood))
  TopofFlood <- c(TopofFlood, as.numeric(reservoir$top_of_flood))
  TopofDam <- c(TopofDam, as.numeric(reservoir$top_of_dam))
  Precip <- c(Precip, paste0(reservoir$precip, " in"))
  Turbine <- c(Turbine, paste0(reservoir$turbine, " cfs"))
}

# Create a data frame
Res_data <- data.frame(
  ReservoirName = ReservoirName,
  CurrentPool = CurrentPool,
  CurrentPoolStr = CurrentPoolStr,
  CurrentPoolDateTimeStr = CurrentPoolDateTimeStr,
  CurrentTail = CurrentTail,
  CurrentTailStr = CurrentTailStr,
  Inflow = Inflow,
  Outflow = Outflow,
  SpillwayFlow = SpillwayFlow,
  TotalFlow = TotalFlow,
  Change24 = Change24,
  GuideCurveElev = GuideCurveElev,
  Streambed = Streambed,
  BottomofFlood = BottomofFlood,
  TopofFlood = TopofFlood,
  TopofDam = TopofDam,
  Precip = Precip,
  Turbine = Turbine,
  stringsAsFactors = FALSE
)

# Print the result for each reservoir
for (i in seq_along(ReservoirName)) {
  cat("ReservoirName:", ReservoirName[i], "\n")
  cat("CurrentPool:", CurrentPool[i], "\n")
  cat("CurrentPoolStr:", CurrentPoolStr[i], "\n")
  cat("CurrentPoolDateTimeStr:", CurrentPoolDateTimeStr[i], "\n")
  cat("CurrentTail:", CurrentTail[i], "\n")
  cat("CurrentTailStr:", CurrentTailStr[i], "\n")
  cat("Inflow:", Inflow[i], "\n")
  cat("Outflow:", Outflow[i], "\n")
  cat("SpillwayFlow:", SpillwayFlow[i], "\n")
  cat("TotalFlow:", TotalFlow[i], "\n")
  cat("Change24:", Change24[i], "\n")
  cat("GuideCurveElev:", GuideCurveElev[i], "\n")
  cat("Streambed:", Streambed[i], "\n")
  cat("BottomofFlood:", BottomofFlood[i], "\n")
  cat("TopofFlood:", TopofFlood[i], "\n")
  cat("TopofDam:", TopofDam[i], "\n")
  cat("Precip:", Precip[i], "\n")
  cat("Turbine:", Turbine[i], "\n")
  cat("\n")
}


####################### Start building graphic elements to plot #######################
for (i in 1:length(Res_data$ReservoirName)) {
  # Print relevant information for debugging
  print(paste("Processing ReservoirName:", Res_data$ReservoirName[i]))
  print(paste("Processing CurrentPool:", Res_data$CurrentPool[i]))
  print(paste("Processing CurrentPoolStr:", Res_data$CurrentPoolStr[i]))
  print(paste("Processing CurrentPoolDateTimeStr:", Res_data$CurrentPoolDateTimeStr[i]))
  print(paste("Processing CurrentTail:", Res_data$CurrentTail[i]))
  print(paste("Processing CurrentTailStr:", Res_data$CurrentTailStr[i]))
  print(paste("Processing Inflow:", Res_data$Inflow[i]))
  print(paste("Processing Outflow:", Res_data$Outflow[i]))
  print(paste("Processing SpillwayFlow:", Res_data$SpillwayFlow[i]))
  print(paste("Processing TotalFlow:", Res_data$TotalFlow[i]))
  print(paste("Processing Change24:", Res_data$Change24[i]))
  print(paste("Processing GuideCurveElev:", Res_data$GuideCurveElev[i]))
  print(paste("Processing Streambed:", Res_data$Streambed[i]))
  print(paste("Processing BottomofFlood:", Res_data$BottomofFlood[i]))
  print(paste("Processing TopofFlood:", Res_data$TopofFlood[i]))
  print(paste("Processing TopofDam:", Res_data$TopofDam[i]))
  print(paste("Processing Precip:", Res_data$Precip[i]))
  print(paste("Processing Turbine:", Res_data$Turbine[i]))
  
  
  # Define water extent for pool
  pool_x <- c(375, 375, 680, 680)
  pool_y <- as.numeric(c(
    Res_data$Streambed[i],
    Res_data$CurrentPool[i],
    Res_data$CurrentPool[i],
    Res_data$Streambed[i]
  ))
  pool_water_positions <- data.frame(pool_x, pool_y)
  
  
  # Define water extent for tail
  tail_x <- c(1060, 1060, 1800, 1800)
  tail_y <- as.numeric(c(
    Res_data$Streambed[i] - 5,
    Res_data$Streambed[i] + 5,
    Res_data$Streambed[i] + 5,
    Res_data$Streambed[i] - 5
  ))
  tail_water_positions <- data.frame(tail_x, tail_y)
  
  
  # Define reservoir extent
  resx <- c(375,375,680,680,625,625,880,880,840,840,960,1060,1060)
  resy <- c(Res_data$Streambed[i]-5, Res_data$Streambed[i], Res_data$Streambed[i],
            Res_data$TopofDam[i]-5, Res_data$TopofDam[i]-5, Res_data$TopofDam[i], 
            Res_data$TopofDam[i], Res_data$TopofDam[i]-5, Res_data$TopofDam[i]-7, 
            Res_data$TopofDam[i]-15, Res_data$Streambed[i]+15, Res_data$Streambed[i]+15,
            Res_data$Streambed[i]-5)
  positions <- data.frame(resx,resy)
  
  
  # Define % full bar
  perc_full <- data.frame("PercFull" = 0:100,
                          "y" = seq(Res_data$BottomofFlood[i], 
                                    Res_data$TopofFlood[i], len=101))
  ark_perc_full <- c(209.3, 222.40, 229.3, 234.40, 238.3)
  sard_perc_full <- c(236.0, 255.5, 266.5, 275.0, 281.4)
  enid_perc_full <- c(230.0, 246.0, 255.5, 262.0, 268.0)
  gren_perc_full <- c(193.0, 210.5, 219.5, 226.0, 231.0)
  ark_perc_full2 <- c(209.3, 222.40, 229.3, 234.40, 238.3)
  perc_full_labels2 <- data.frame(ark_perc_full, sard_perc_full, enid_perc_full, gren_perc_full, ark_perc_full2)
  
  
  # Define mountain extent
  mountx <- c(25,35,55,80,85,150,160,175,190,200)
  mounty <- c(Res_data$TopofDam[i]+7, Res_data$TopofDam[i]+10, Res_data$TopofDam[i]+13,
              Res_data$TopofDam[i]+15, Res_data$TopofDam[i]+17, Res_data$TopofDam[i]+20,
              Res_data$TopofDam[i]+14, Res_data$TopofDam[i]+12, Res_data$TopofDam[i]+11,
              Res_data$TopofDam[i]+7)
  mountdf <- data.frame(mountx, mounty)
  
  mx2 <- c(45,50,55,90,70,75,85)
  my2 <- c(Res_data$TopofDam[i]+8, Res_data$TopofDam[i]+10, Res_data$TopofDam[i]+12,
           Res_data$TopofDam[i]+14, Res_data$TopofDam[i]+11, Res_data$TopofDam[i]+9,
           Res_data$TopofDam[i]+8)
  mdf2 <- data.frame(mx2, my2)
  
  mx3 <- c(145,142,130,144,150,160)
  my3 <- c(Res_data$TopofDam[i]+13, Res_data$TopofDam[i]+14,Res_data$TopofDam[i]+18,
           Res_data$TopofDam[i]+17, Res_data$TopofDam[i]+14,Res_data$TopofDam[i]+13)
  mdf3 <- data.frame(mx3, my3)
  
  mx4 <- c(160,155,150,140,180)
  my4 <- c(Res_data$TopofDam[i]+8, Res_data$TopofDam[i]+10,Res_data$TopofDam[i]+10,
           Res_data$TopofDam[i]+12, Res_data$TopofDam[i]+8)
  mdf4 <- data.frame(mx4, my4)
  
  
  # Define elevation scale bar 
  scale_elev_y <- seq(round_any(Res_data$Streambed[i], 5),
                      round_any(Res_data$TopofDam[i] - 3, 5),
                      by = 5)
  scale_elev_x <- rep(360, length(scale_elev_y))
  scale_elev_xbegin <- scale_elev_x - 10
  scale_elev_xend <- scale_elev_x + 10
  
  
  # Create a data frame for the elevation scale
  scale_elev_DF <- data.frame(scale_elev_y, scale_elev_x, scale_elev_xbegin, scale_elev_xend)
  
  
  if (Res_data$ReservoirName[i] == "Mark Twain Lk-Salt") {
    # Define inflow/outflow arrows
    point_x <- c(500,350,950,1200, 1450)
    point_y <- c(Res_data$TopofDam[i]+10, Res_data$TopofDam[i]+20, Res_data$Streambed[i]+10, Res_data$Streambed[i]+15, Res_data$Streambed[i]+15)
    point_label <- c("Inflow", "Precipitation", "Generate", "Spillway", "Q Total")
    point_label_short <- c("IN", "P", "GEN", "SPWY", "TOT")
    point_flow <- c(Res_data$Inflow[i], Res_data$Precip[i], Res_data$Outflow[i], Res_data$SpillwayFlow[i], Res_data$TotalFlow[i])
    point_color <- c("skyblue2", "skyblue2", "steelblue4", "steelblue4", "steelblue4")
    point_DF <- data.frame(point_x, point_y, point_label, point_label_short,
                           point_flow, point_color, stringsAsFactors = F)
    arrow1x <- c(1000, 1000, 1000, 1000, 1000)
    arrow1y <- c(point_DF$point_y[3], point_DF$point_y[3], point_DF$point_y[3], point_DF$point_y[3], point_DF$point_y[3])
  } else if (Res_data$ReservoirName[i] == "Carlyle Lk-Kaskaskia" | Res_data$ReservoirName[i] == "Lk Shelbyville-Kaskaskia" | Res_data$ReservoirName[i] == "Wappapello Lk-St Francis" | Res_data$ReservoirName[i] == "Rend Lk-Big Muddy") {
    # Define inflow/outflow arrows
    point_x <- c(500,350,1015)
    point_y <- c(Res_data$TopofDam[i]+10, Res_data$TopofDam[i]+20, Res_data$Streambed[i]+10)
    point_label <- c("Inflow", "Precipitation", "Outflow")
    point_label_short <- c("IN", "P", "OUT")
    point_flow <- c(Res_data$Inflow[i], Res_data$Precip[i], Res_data$Outflow[i])
    point_color <- c("skyblue2", "skyblue2", "steelblue4")
    point_DF <- data.frame(point_x, point_y, point_label, point_label_short,
                           point_flow, point_color, stringsAsFactors = F)
    arrow1x <- c(1000, 1000, 1000)
    arrow1y <- c(point_DF$point_y[3], point_DF$point_y[3], point_DF$point_y[3])
  } else {
    # Define inflow/outflow arrows
    point_x <- c(500,1015,350,950,950)
    point_y <- c(Res_data$TopofDam[i]+10, Res_data$TopofDam[i]+20, Res_data$Streambed[i]+10, Res_data$TopofFlood[i], Res_data$TopofDam[i]+0)
    point_label <- c("Inflow", "Precipitation", "Outflow", "Spillway", "Generate")
    point_label_short <- c("IN", "P", "OUT", "SPWY", "GEN")
    point_flow <- c(Res_data$Inflow[i], Res_data$Precip[i], Res_data$Outflow[i], Res_data$SpillwayFlow[i], Res_data$Turbine[i])
    point_color <- c("steelblue2", "steelblue4", "steelblue4", "steelblue2", "steelblue4")
    point_DF <- data.frame(point_x, point_y, point_label, point_label_short,
                           point_flow, point_color, stringsAsFactors = F)
    arrow1x <- c(1000, 1000, 1000,1000,1000)
    arrow1y <- c(point_DF$point_y[3], point_DF$point_y[3], point_DF$point_y[3], point_DF$point_y[3], point_DF$point_y[3])
  }
  
  
  # Update arrow1DF
  arrow1DF <- data.frame(arrow1x, arrow1y)
  
  
  # Define datetime
  date <- f_date(Sys.Date(), format = "%B %d, %Y")
  date <- paste0(date, ",")
  time <- f_12_hour(x = Sys.time(), format="%I:%M %p")
  date_time <- paste(date, time, "CST")
  
  
  # Create the Graphic
  p1 <- ggplot() +  
    # Add Supporting Graphics to Plot
    annotation_custom(img1, xmin=200, xmax=300, ymin=Res_data$TopofDam[i]+8, ymax=Res_data$TopofDam[i]+28) +
    annotation_custom(img2, xmin=1625, xmax=1800, ymin=Res_data$TopofDam[i], ymax=Res_data$TopofDam[i]+50) +
    
    
    # Draw the Reservoir Polygon
    geom_polygon(data = positions, mapping = aes(x=resx, y=resy), fill="grey") +
    
    
    # Draw Flow Circles
    geom_point(data = point_DF, mapping = aes(x = point_x, y = point_y, color = point_color), size = 10) + 
    scale_colour_manual(values = c("skyblue2", "steelblue4", "steelblue4", "skyblue2", "skyblue2"), 
                        labels = c("Inflow", "Precipitation", "Outflow", "Generate", "Spillway")) +
    
    
    # Draw INFLOW Line Segments/Arrows
    geom_segment(data = point_DF, aes(x=50, xend=point_x[1], y=point_y[1], yend=point_y[1]), linewidth=2, color="skyblue2") + 
    geom_segment(data = point_DF, aes(x=point_x[1], xend=point_x[1], y=point_y[1], yend=point_y[1]-14), linewidth=2, color="skyblue2") +
    annotate("segment", x=point_DF$point_x[1], xend=point_DF$point_x[1], y=point_DF$point_y[1], yend=point_DF$point_y[1]-16, linejoin="round", 
             arrow=arrow(type="closed", length=unit(0.03, "npc")), color="skyblue2") +
    
    
    # Draw OUTFLOW Line Segment/Arrow
    geom_segment(data = point_DF, aes(x=point_x[2]+30, xend=point_x[2]+120, y=point_y[2], yend=point_y[2]), color="skyblue2", linewidth=2) +
    annotate("segment", x=point_DF$point_x[2]+25, xend=point_DF$point_x[2]+140, y=point_DF$point_y[2], yend=point_DF$point_y[2], linejoin="round", 
             arrow=arrow(type="closed", length=unit(0.03, "npc")), color="skyblue2") +
    
    
    # Draw PRECIP Line Segment/Arrow
    geom_segment(data = point_DF, aes(x=point_x[3]+30, xend=point_x[3]+120, y=point_y[3], yend=point_y[3]), color="steelblue4", linewidth=2) +
    annotate("segment", x=point_DF$point_x[3], xend=point_DF$point_x[3]+140, y=point_DF$point_y[3], yend=point_DF$point_y[3], linejoin="round", 
             arrow=arrow(type="closed", length=unit(0.03, "npc")), color="steelblue4") +
    
    
    # Draw SPILLWAY Line Segment/Arrow
    geom_segment(data = point_DF, aes(x=point_x[4] + 30, xend=point_x[4] + 100, y=point_y[4], yend=point_y[4]), color="steelblue4", linewidth=2) +
    annotate("segment", x = point_x[4] + 30, xend = point_x[4] + 110, y = point_y[4], yend = point_y[4], linejoin = "round", 
             arrow = arrow(type = "closed", length = unit(0.03, "npc")), color = "steelblue4") +
    
    
    # Draw GENERATE Line Segment/Arrow
    geom_segment(data = point_DF, aes(x=point_x[5] + 30, xend=point_x[5] + 100, y=point_y[5], yend=point_y[5]), color="steelblue4", linewidth=2) +
    annotate("segment", x = point_x[5] + 30, xend = point_x[5] + 110, y = point_y[5], yend = point_y[5], linejoin = "round", 
             arrow = arrow(type = "closed", length = unit(0.03, "npc")), color = "steelblue4") +
  
    
    # Draw Flow annotations (text)
    geom_text(data = point_DF, mapping = aes(x=point_x, y=point_y, label=point_label_short), 
              linewidth = 2.5, size = 2.75, color = "white", fontface=2) + 
    geom_text(data = point_DF, mapping = aes(x=point_x[1]+50, y=point_y[1], label=point_flow[1]),
              linewidth = 2.5, size = 2.5, color= "grey19", hjust=0)+
    geom_text(data = point_DF, mapping=aes(x=point_x[2]+50, y=point_y[2]+5, label=point_flow[2]),
              linewidth = 2.5, size = 2.5, color= "grey19", hjust=0)+
    geom_text(data = point_DF, mapping=aes(x=point_x[3]+50, y=point_y[3]+5, label=point_flow[3]),
              linewidth = 2.5, size = 2.5, color= "grey19", hjust=0)+
    geom_text(data = point_DF, mapping=aes(x=point_x[4]+50, y=point_y[4]+5, label=point_flow[4]),
              linewidth = 2.5, size = 2.5, color= "grey19", hjust=0)+
    geom_text(data = point_DF, mapping=aes(x=point_x[5]+50, y=point_y[5]+5, label=point_flow[5]),
              linewidth = 2.5, size = 2.5, color= "grey19", hjust=0)+
    
    
    # Draw Pool/Tail-water Polygons
    geom_polygon(data = pool_water_positions, aes(x = pool_x, y = pool_y), fill = "lightskyblue1") +
    geom_polygon(data = tail_water_positions, aes(x = tail_x, y = tail_y), fill = "skyblue2") +
    
    
    # Draw the Mountain
    geom_polygon(data = mountdf, aes(x = mountx, y = mounty), fill = "grey") +
    geom_polygon(data = mdf2, aes(x = mx2, y = my2), fill = "gray50") +
    geom_polygon(data = mdf3, aes(x = mx3, y = my3), fill = "gray50") +
    geom_polygon(data = mdf4, aes(x = mx4, y = my4), fill = "gray50") +
    
    
    # Draw the remaining ground connecting Mountain to reservoir
    geom_segment(data = Res_data, aes(x = 375, xend = 375, y = Streambed[i] - 5, yend = TopofDam[i] + 5.5), color = "grey", linewidth = 2) +
    geom_curve(data = Res_data, aes(x = 375, xend = 350, y = TopofDam[i] + 5, yend = TopofDam[i] + 7), curvature = 0.2, angle = 90, ncp = 100, linewidth = 2, color = "grey") +
    geom_segment(data = Res_data, aes(x = 351, xend = 25, y = TopofDam[i] + 7, yend = TopofDam[i] + 7), color = "grey", linewidth = 2) +
    
    
    # Draw % Full scale
    geom_tile(data = perc_full, aes(y = y, x = 750, fill = PercFull), width = 35) +
    scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 50) +
    annotate("text", x = 800, y = perc_full_labels2[[i]], label = c("0%", "25%", "50%", "75%", "100%"), color = "gray19", linewidth = 2, fontface = 1) +
    
    
    # Draw Elevation Scale
    geom_segment(data = scale_elev_DF, aes(x = scale_elev_xbegin, xend = scale_elev_xend, y = scale_elev_y, yend = scale_elev_y), color = "grey", linewidth = 0.4) +
    geom_text(data = scale_elev_DF, aes(x = scale_elev_xbegin - 30, y = scale_elev_y, label = scale_elev_y), linewidth = 2, size = 2.5, color = "gray60") +
    
    
    # Draw Reservoir Pertinent Elevation Line Segments
    geom_segment(data = Res_data, aes(y = Streambed[i], yend = Streambed[i], x = 250, xend = 370), linewidth = 0.5, linetype = 8, color = 'red') +
    geom_segment(data = Res_data, aes(y = BottomofFlood[i], yend = BottomofFlood[i], x = 250, xend = 735), linewidth = 0.5, linetype = 8, color = 'red') +
    geom_segment(data = Res_data, aes(y = TopofFlood[i], yend = TopofFlood[i], x = 250, xend = 735), linewidth = 0.5, linetype = 8, color = 'red') +
    geom_segment(data = Res_data, aes(y = TopofDam[i], yend = TopofDam[i], x = 250, xend = 370), linewidth = 0.5, linetype = 8, color = 'red') +
    geom_segment(data = Res_data, aes(y = GuideCurveElev[i], yend = GuideCurveElev[i], x = pool_water_positions$pool_x[1] + 8, xend = pool_water_positions$pool_x[3] + 100), linewidth = 0.5, linetype = 8, color = 'lightskyblue4') +
    
    
    # Draw the Reservoir Pertinent Elevation Labels
    annotate("text", x = 240, y = Res_data$TopofDam[i] - 1, label = wrapper(paste("Top of Dam", paste0(Res_data$TopofDam[i], "'")), width = 10), color = "red", linewidth = 2, size = 3.0, fontface = 1, hjust = 1) +
    annotate("text", x = 240, y = Res_data$TopofFlood[i] + 1, label = wrapper(paste("Top of Flood", paste0(Res_data$TopofFlood[i], "'")), width = 15), color = "red", linewidth = 2, size = 3.0, fontface = 1, hjust = 1) +
    annotate("text", x = 240, y = Res_data$BottomofFlood[i] - 1, label = wrapper(paste("Bottom of Flood", paste0(Res_data$BottomofFlood[i], "'")), width = 15), color = "red", linewidth = 2, size = 3.0, fontface = 1, hjust = 1) +
    annotate("text", x = 240, y = Res_data$Streambed[i] - 1, label = wrapper(paste("Streambed", paste0(Res_data$Streambed[i], "'")), width = 10), color = "red", linewidth = 2, size = 3.0, fontface = 1, hjust = 1) +
    annotate("text", x = 900, y = as.numeric(Res_data$GuideCurveElev[i]) + 0, label = paste("Rule Curve ", paste0("(", Res_data$GuideCurveElev[i], "')")), color = "lightskyblue4", linewidth = 2, size = 2.5, fontface = 1, hjust = 0.5) +
    
    
    # Draw the Pool and Tail-water Labels
    annotate("text", x = 450, y = as.numeric(Res_data$CurrentPool[i]) - 1, label = paste0(Res_data$CurrentPoolStr[i], "(", Res_data$Change24[i], ")\n", Res_data$CurrentPoolDateTimeStr[1]), color = "gray19", size = 2.5, fontface = 1, hjust = 0, vjust = 1) +
    annotate("text", x = 1795, y = Res_data$Streambed[i]+3.5, label = Res_data$CurrentTailStr[i], color = "gray19", size = 3.0, fontface = 1, hjust = 1) +
    
    # Draw Title/Additional Graphic Text
    annotate("text", x = 1600, y = Res_data$TopofDam[i]+30, label = "FLOOD CONTROL STATUS", color = "black", linewidth = 4.5, fontface = 2, hjust = 1) + 
    annotate("text", x = 1600, y = Res_data$TopofDam[i]+24, label = paste(Res_data$ReservoirName[i], "Lake, MVS"), color = "gray19", linewidth = 3.5, fontface = 1, hjust = 1) + 
    annotate("text", x = 1600, y = Res_data$TopofDam[i]+19, label = date_time, color = "gray19", linewidth = 3.5, fontface = 1, hjust = 1) + 
    
    
    # Draw Legend
    # Adding a text annotation for the legend
    annotate("text", x = 1655, y = Res_data$TopofDam[i] + 10, label = "Legend", color = "black", size = 3, fontface = 2, hjust = 2) +
    # Adding a horizontal line segment
    geom_segment(data = Res_data, aes(y = TopofDam[i] + 6, yend = TopofDam[i] + 6, x = 1460, xend = 1800), size = 1, color = 'grey') +
    
    # Adding a rectangular shape with light blue fill
    geom_rect(data = Res_data, mapping = aes(xmin = 1460, xmax = 1500, ymin = TopofDam[i], ymax = TopofDam[i] + 4), fill = "lightskyblue1") +
    # Adding a text annotation for the current lake level
    annotate("text", x = 1520, y = Res_data$TopofDam[i] + 2, label = "Current Lake Level", color = "gray19", size = 3, hjust = 0) +
  
    
    # Adding a rectangular shape with sky blue fill
    geom_rect(data = Res_data, mapping = aes(xmin = 1460, xmax = 1500, ymin = TopofDam[i] - 1.5, ymax = TopofDam[i] - 5.5), fill = "skyblue2") +
    # Adding a text annotation for tail water
    annotate("text", x = 1520, y = Res_data$TopofDam[i] - 3.5, label = "Tail Water", color = "gray19", size = 3, hjust = 0) +
    
    
    # Adding a point with label "P" for precipitation
    geom_point(data = Res_data, mapping = aes(x = 1480, y = TopofDam[i] - 9.5), color = "skyblue2", size = 6) +
    # Adding a text annotation for precipitation
    annotate("text", x = 1480, y = Res_data$TopofDam[i] - 9.5, label = "P", color = "white", size = 1.5, fontface = 2) +
    # Adding a text annotation for precipitation
    annotate("text", x = 1520, y = Res_data$TopofDam[i] - 9.5, label = "Precipitation", color = "gray19", size = 3, hjust = 0) +
    
    
    # Adding another point with label "IN" for inflow
    geom_point(data = Res_data, mapping = aes(x = 1480, y = TopofDam[i] - 16.5), color = "skyblue2", size = 6) +
    # Adding a text annotation for inflow
    annotate("text", x = 1480, y = Res_data$TopofDam[i] - 16.5, label = "IN", color = "white", size = 1.5, fontface = 2) +
    # Adding a text annotation for inflow
    annotate("text", x = 1520, y = Res_data$TopofDam[i] - 16.5, label = "Inflow", color = "gray19", size = 3, hjust = 0) +
  
    
    # Adding points for various reservoir levels
    geom_point(data=Res_data, mapping=aes(x=1480, y=TopofDam[i]-23.5), color="steelblue4", size=6) +
    # Adding text annotation for the first point
    annotate("text", x=1480, y=Res_data$TopofDam[i]-23.5, label="SPWY", color="white", size=1.5, fontface=2) +
    # Adding text annotation for the first point
    annotate("text", x=1520, y=Res_data$TopofDam[i]-23.5, label="Spillway", color="gray19", size=3, hjust=0) +
    
    
    # Adding points for another reservoir level
    geom_point(data=Res_data, mapping=aes(x=1480, y=TopofDam[i]-30.5), color="steelblue4", size=6) +
    # Adding text annotation for the second point
    annotate("text", x=1480, y=Res_data$TopofDam[i]-30.5, label="OUT", color="white", size=1.5, fontface=2) +
    # Adding text annotation for the second point
    annotate("text", x=1520, y=Res_data$TopofDam[i]-30.5, label="Outflow", color="gray19", size=3, hjust=0) +
    

    # Adding points for another reservoir level
    geom_point(data=Res_data, mapping=aes(x=1480, y=TopofDam[i]-37.5), color="steelblue4", size=6) +
    # Adding text annotation for the third point
    annotate("text", x=1480, y=Res_data$TopofDam[i]-37.5, label="GEN", color="white", size=1.5, fontface=2) +
    # Adding text annotation for the third point
    annotate("text", x=1520, y=Res_data$TopofDam[i]-37.5, label="Generate", color="gray19", size=3, hjust=0) +
    
    # Adding points for another reservoir level
    geom_point(data=Res_data, mapping=aes(x=1480, y=TopofDam[i]-44.5), color="steelblue4", size=6) +
    # Adding text annotation for the third point
    annotate("text", x=1480, y=Res_data$TopofDam[i]-44.5, label="TOT", color="white", size=1.5, fontface=2) +
    # Adding text annotation for the third point
    annotate("text", x=1520, y=Res_data$TopofDam[i]-44.5, label="Flow Total", color="gray19", size=3, hjust=0) +
  
    
    # Set Theme and Scale for Graphic
    theme_void() + 
    theme(legend.position="none") +
    scale_y_continuous(limits = c(Res_data$Streambed[i]-10, Res_data$TopofDam[i]+30)) +
    scale_x_continuous(limits = c(25,1800)) +
    theme(panel.background = element_rect(fill='white',color='white'))+
    theme(legend.position="none",
          panel.background = element_rect(fill='white', color='white', size = 1), # Add border
          
          #panel.grid.major = element_line(color = "gray", size = 0.5),
          #panel.grid.minor = element_line(color = "gray", size = 0.25),
          #panel.grid.major.x = element_line(color = "gray", size = 0.5),
          #panel.grid.minor.x = element_line(color = "gray", size = 0.25),
          #panel.grid.major.y = element_line(color = "gray", size = 0.5),
          #panel.grid.minor.y = element_line(color = "gray", size = 0.25),
          
          plot.background = element_rect(color = "black", size = 1), # Add border to the entire plot
          plot.margin = margin(10, 10, 10, 10))+ # Adjust margins as needed
    # Add vertical line at x = 0
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    # Add horizontal line at y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1)
  
  plot(p1)
  # Print information for debugging
  print(paste("Generated ReservoirName:", Res_data$ReservoirName[i]))
  print(paste("Generated CurrentPool:", Res_data$CurrentPool[i]))
  print(paste("Generated CurrentPoolStr:", Res_data$CurrentPoolStr[i]))
  print(paste("Generated CurrentPoolDateTimeStr:", Res_data$CurrentPoolDateTimeStr[i]))
  print(paste("Generated CurrentTail:", Res_data$CurrentTail[i]))
  print(paste("Generated CurrentTailStr:", Res_data$CurrentTailStr[i]))
  print(paste("Generated Inflow:", Res_data$Inflow[i]))
  print(paste("Generated Outflow:", Res_data$Outflow[i]))
  print(paste("Generated SpillwayFlow:", Res_data$SpillwayFlow[i]))
  print(paste("Generated Change24:", Res_data$Change24[i]))
  print(paste("Generated GuideCurveElev:", Res_data$GuideCurveElev[i]))
  print(paste("Generated Streambed:", Res_data$Streambed[i]))
  print(paste("Generated BottomofFlood:", Res_data$BottomofFlood[i]))
  print(paste("Generated TopofFlood:", Res_data$TopofFlood[i]))
  print(paste("Generated TopofDam:", Res_data$TopofDam[i]))
  print(paste("Generated Precip:", Res_data$Precip[i]))
  
  # Save the Graphics
  ggsave(filename = paste0(graphicDirectory, sprintf("%sReservoirStatus.png", Res_data$ReservoirName[i])),
         plot = last_plot(), dpi = 300, width = 8.5, height = 5)
}