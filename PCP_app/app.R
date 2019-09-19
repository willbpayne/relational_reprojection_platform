#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(sp) # might not need
library(ggplot2)
library(ggforce) # to plot circles
library(dplyr)
library(tidyverse)
library(tools) #to learn file extension
library(jsonlite) # for loading in json files
library(geojson) # for loading geojson (need this for geojsonio to work)
library(geojsonio) # for loading geojson
# 
library(geosphere) # where we get bearing
library(gmt) # actually for geodist
library(useful) # for cartesian conversions

# NB: to check later as we re-visit new contours
library(akima) # for irregular grid
library(reshape2) # for contor/grid
library(contoureR) # for contours w/o grid
library(deldir) # CHECK: trying for TIN generation, see sec. 11
library(lawn) # CHECK:  trying for TIN generation, see sec. 11

# dataFile <- "IND_remittances.csv"

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Pseudo-Spatial Chart Program"),
   
   ##### NB: WIDGET: Close/med/far text entry
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       fileInput("uploadFile", "Data File", multiple = FALSE, accept = NULL
       ),  
       plotOutput("distPlot", height = "250px", width="100%"),
       # radio buttons
       radioButtons("interpMeth", "Interpolation Method", 
                    choices = c("Great Circle Distances","Square Root", "Logarithmic", "Exponential", "Custom"), 
                    inline = FALSE, width = "100%"),
       ## if radio button custom
       conditionalPanel(
         condition = "input.interpMeth == 'Custom'", 
          sliderInput("manualCutPoints", 
                   "Cut Points", 0, 500, #to be parametarized
                   c(150,250), step = NULL, 
                   # value is 1 if it is a single value or 2 if it is a vector of 2 eg, [x,y]
                   round = FALSE, 
                   format = "#,##0.#####", 
                   locale = "us", 
                   ticks = TRUE, animate = FALSE)
       )
     ),

      # Show a plot of the generated distribution
      mainPanel(
       
         ##### NB: This should be our map instead
         plotOutput("geoPlot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##### NB: Function-ize map data and put it here 
  
   output$distPlot <- renderPlot({
      
      if (is.null(input$uploadFile) == TRUE){
        df <- read.csv(file = "IND_remittances.csv")
        plot(df$lon, df$lat)
      }
     else{
       uploadFileData <- input$uploadFile
       df <- read.csv(file = uploadFileData$datapath)
       plot(df$lon, df$lat)
     }

   })
   
   output$geoPlot <- renderPlot({
     
     if (is.null(input$uploadFile) == TRUE){
       df <- read.csv(file = "IND_remittances.csv")
       plot(df$lon, df$lat)
     }
     else{
       uploadFileData <- input$uploadFile
       df <- read.csv(file = uploadFileData$datapath)
       plot(df$lon, df$lat, ann=FALSE, axes=FALSE,xaxt="n", yaxt="n",xlab='',ylab='')
     }
     
     ###################################
     #        PARSE COLUMNS            #
     ###################################
     
     colListOrig <- colnames(df) # store column names for later
     latNames <- list("lat","Lat","LAT", "latitude", "Latitude", "LATITUDE", "y","Y", "coords.x2") # add as they come up
     lonNames <- list("lon","Lon","LON","long","Long","LONG","longitude", "Longitude", "LONGITUDE", "x","X", "coords.x1")
     valNameChoices <- list()
     valChoices <- list()
     
     df2 <- df # cloning df for non-destructive editing

     latflag <- 0 # need these here for the column detection
     lonflag <- 0
     ctrBinflag <- 0
     valNameflag <- 0
     valflag <- 0

     for (col in 1:ncol(df)) {
       if (typeof(df[[col]]) == "double"
           && latflag == 0
           && max(as.numeric(df[[col]]), na.rm = T) <= 90.0
           && min(as.numeric(df[[col]]), na.rm = T) >= -90.0
           && names(df)[[col]] %in% latNames) # lat
       { 
         # print(paste("Found a lat column: ", names(df)[[col]]))
         df2$lat <- df[[col]]
         latflag <- 1}
       else{
         if (typeof(df[[col]]) == "double"
             && lonflag == 0
             && max(as.numeric(df[[col]]), na.rm = T) <= 180.0
             && min(as.numeric(df[[col]]), na.rm = T) >= -180.0
             && names(df)[col] %in% lonNames) # lon
         { 
           # print(paste("Found a lon column: ", names(df)[[col]]))
           df2$lon <- df[[col]]
           lonflag <- 1}
         else{
           if (typeof(df[[col]]) != "character"
               && typeof(df[[col]]) != "list"
               && min(as.numeric(df[[col]]), na.rm = T) == 0
               && max(as.numeric(df[[col]]), na.rm = T) == 1
               && sum(as.numeric(df[[col]]), na.rm = T) == 1) # ctrBin
           { df2$ctrBin <- as.logical(df[[col]])
           # print(paste("Found a ctrBin column: ", names(df)[[col]]))
           ctrBinflag <- 1}
           else{
             if (typeof(df[[col]]) == "character" # catches name and name_long
                 || is.factor(df[[col]]) == T) # valName
             { if (valNameflag == 0)
             {df2$valName <- as.character(df[[col]])
             # print(paste("Found a valName column: ", names(df)[[col]]))
             valNameflag <- 1}
               else{
                 # print(paste("Found an alternate valName column: ", names(df)[[col]]))
                 valNameChoices <- c(valNameChoices, names(df)[[col]])}
               # NOTE this isn't storing anywhere yet, might need to later for input
             }
             else{
               if (typeof(df[[col]]) == "integer" || typeof(df[[col]]) == "double" # val
                   && !(names(df)[col] %in% lonNames)
                   && !(names(df)[col] %in% latNames)
                   && names(df[col]) != "geometry")
               { if (valflag == 0)
               {df2$val <- as.double(df[[col]])
               # print(paste("Found a val column: ", names(df)[[col]]))
               valflag <- 1}
                 else{
                   # print(paste("Found an alternate val column: ", names(df)[[col]]))
                   valChoices <- c(valChoices, names(df)[[col]])}
                 # NB: NOTE this isn't storing anywhere yet, might need to later for input
               }
             }
           }
         }
       }
     }

     ###################################
     #        SET CENTER POINT         #
     ###################################
     
     if("ctrBin" %in% names(df)[col]) {
       ctrPtName <- df2$valName[df2$ctrBin == TRUE]
       ctrPt <- c(df2$lat[df2$ctrBin == TRUE],df2$lon[df2$ctrBin == TRUE]) # get lat/lon
       # print("you prepared your data soooooo well!")

     } else {
       ctrPtName <- "Default Center"
       ctrPt <- c(median(df2$lat), median(df2$lon))
     }
     # cat("Your center point, ",ctrPtName,", is located at", ctrPt[1],"longitude and", ctrPt[2], "latitude. ")


     ###################################
     #        CREATE DF2               #
     ###################################
     
     if(df_ext == "csv"){
       df2 <- dplyr::select(df2, valName, val, lat, lon) # just the fields we want
     } else {
       if(df_ext == "geojson"){
         df2 <- dplyr::select(df2, valName, val, lat, lon) #something else
       } else {
         # print(paste("What even is this?"))
       }
     }
     df2$distance <- geodist(ctrPt[1], ctrPt[2], df2$lat, df2$lon, units = "km")
     maxdist <- max(df2$distance) # max great circle distance

     # print( # print out both the longest axial and great circle distances
     #   paste0("The great circle distance between your center point, ",
     #          ctrPtName, ", and farthest point, ",
     #          df2$valName[df2$distance == max(df2$distance)], ", is ",
     #          round(maxdist, 0), " km.")
     # )
     # if(maxdist > 20000){
     #   print("Your maximum distance is more than half of the Earth's circumference; things might get a little squirrely!")
     # } else {
     #   print("Your maximum distance is less than half of the Earth's circumference! Nice!")
     # }

     df2 <- df2 %>% mutate(ctrPtGeobearing = geosphere::bearing(c(ctrPt[2],ctrPt[1]), cbind(lon, lat), a=6378137, f=1/298.257223563)) # get bearing of all points to center
     # 
     # # THIS NEXT PART SHOULD BE A FUNCTION TOO
     # 
     for (row in 1:nrow(df2)) # convert from geographic bearings to polar coordinates
     {if(df2$ctrPtGeobearing[row] <= 0)
       # if geobearing is 0 or negative, mathbearing is 90 plus bearing
       df2$ctrPtMathbearing[row] <- abs(df2$ctrPtGeobearing[row]) + 90
     if(df2$ctrPtGeobearing[row] > 0 & df2$ctrPtGeobearing[row] < 90)
       # if geobearing is positive and equal to or under 90, mathbearing is 90 - bearing
       df2$ctrPtMathbearing[row] <- (90 - df2$ctrPtGeobearing[row])
     if(df2$ctrPtGeobearing[row] >= 90 & df2$ctrPtGeobearing[row] <= 180)
       # if geobearing is positive and between 90 and 180 (inclusive), mathbearing is 360 - (geobearing - 90)
       df2$ctrPtMathbearing[row] <- 360 - (df2$ctrPtGeobearing[row] - 90)}
     rm(row)

     df2 <- dplyr::select(df2,-starts_with("ctrPtGeo"))

     maxdist <- max(df2$distance) # max great circle distance

     plot05 <- ggplot(df2,
                      aes(df2$lon,
                          df2$lat,
                          color = df2$distance)) +
       geom_point() +
       geom_point(data = (as.data.frame(ctrPt)), aes(ctrPt[2], ctrPt[1]), color = "orange")

     plot05
     
     df2$valTrans <- log(df2$val) + 1 # manual one I did for NH data to see what we're looking for
     #df2$valTrans <- (df2$val / 400)^1.5 # manual one I did for NH data to see what we're looking for
     
     hist(df2$val, breaks = 20) # just for giggles
     hist(sqrt(df2$val), breaks = 20)
     hist(log(df2$val), breaks = 20)
     
     #df2$val[df2$val == 0] <- NA # turn zeros to NAs for our purposes
     
     print(min(df2$val, na.rm = T)) # find lowest non-zero value (that will be radius 1)
     print(max(df2$val, na.rm = T)) # find highest non-zero value (that will be radius X)
     print(median(df2$val, na.rm = T)) # find median value (that could be radius 1 + X / 2)
     
     # Run polar to cartesian conversion on x and y axes
     df2 <- df2 %>% mutate(circdistancex =  (useful::pol2cart(distance,ctrPtMathbearing,degrees = TRUE)[[1]]))
     df2 <- df2 %>% mutate(circdistancey = (useful::pol2cart(distance,ctrPtMathbearing,degrees = TRUE)[[2]]))
     
     # Combine x and y into a matrix, add as a column, remove x and y columns
     df2$circcoords <- cbind(df2$circdistancex,df2$circdistancey)
     df2 <- dplyr::select(df2,-starts_with("circdistance"))
     
     # Lets make some circles starting at the center point going to maxdist
     circles <- data.frame(
       x0 = 0,
       y0 = 0,
       r = seq(0, maxdist,length.out = 11)
     )
     circles <- circles[-1,] # remove zero-radius circle to help with log transformation
     
     # Use coord_fixed to ensure true circularity
     
     plot06 <- ggplot(df2, aes(
       df2$circcoords[,1], 
       df2$circcoords[,2], 
       color = df2$distance)) +
       geom_circle(aes(x0 = x0, y0 = y0, r = r),
                   colour = "orange", 
                   data = circles, 
                   show.legend = NA, 
                   inherit.aes = FALSE) +
       geom_point(stroke = 1, size = df2$valTrans) +
       geom_point(data = (as.data.frame(ctrPt)), aes(0, 0), color = "orange") +
       coord_fixed() + labs(color = paste0("Distance from ",ctrPtName," (km)"), x = NULL, y = NULL) +
       guides(colour = "colorbar",size = "legend") +
       theme(panel.background = element_blank())
     
     plot06
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

