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
#
library(RColorBrewer) # for graph colors

# NB: to check later as we re-visit new contours
library(akima) # for irregular grid
library(reshape2) # for contor/grid
library(contoureR) # for contours w/o grid
library(deldir) # CHECK: trying for TIN generation, see sec. 11
library(lawn) # CHECK:  trying for TIN generation, see sec. 11

# dataFile <- "IND_remittances.csv"

########################################
##          UI PARTY TONIGHT!         ##
########################################

ui <- fluidPage(
   
   # Application title
   div(style = "padding: 10px",
     titlePanel("Pseudo-Spatial Chart Program")
     ),
   ##### NB: WIDGET: Close/med/far text entry
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       div(style = "font-size: 14px; padding: 0px; margin-top: -5px",
           fileInput("uploadFile", "Upload Data File", multiple = FALSE, accept = NULL)
       ),
       
       # div(style = "font-size: 14px; padding: 10px 0px; margin-top: -25px",
       #     fluidRow(
       #       ###
       #       ###
       #       ###
       #       ###
       #       column(8,selectInput("dataColumn", label = "Select Data",
       #                            choices = textOutput("dataCols"), 
       #                            selected = "Light", 
       #                            multiple = FALSE,
       #                            selectize = TRUE, 
       #                            width = "100%", size = NULL))
       #     )
       #     ###
       #     ### I think a solid guide to this is here: https://stackoverflow.com/questions/47248534/dynamically-list-choices-for-selectinput-from-a-user-selected-column
       # ),
       div(style = "font-size: 14px; padding: 10px 0px; margin-top: -20px",
         fluidRow(
           column(6,checkboxInput("labelsOn", "Labels", value = FALSE, width = NULL)),
           column(6,checkboxInput("centerOn", "Show Center", value = TRUE, width = NULL))
           )
       ),
       div(style = "font-size: 14px; padding: 10px 0px; margin-top: -25px",
           fluidRow(
             column(8,selectInput("plotTheme", label = NULL, c("Light Theme", "Dark Theme", "Mono Theme"), selected = "Light", multiple = FALSE,
                                  selectize = TRUE, width = "100%", size = NULL))
           )
       ),
       div(style = "margin-top: -60px",
        plotOutput("distPlot", height = "250px", width="100%")
       ),
       # Radio buttons for interpolation method
       div(style = "font-size: 14px; padding: 10px 0px; margin:3%; margin-top: -15px",
         fluidRow(
           column(6,radioButtons("valTransMeth", "Value Interpolation", 
                                 choices = c("Raw","Scaled","Square Root","Log Scale","Custom"), 
                                 inline = FALSE, width = "100%",
                                 selected = "Log Scale")),
           column(6,radioButtons("interpMeth", "Distance Interpolation", 
                                 choices = c("Lat & Long","Great Circles","Square Root", "Logarithmic", "Custom"), 
                                 inline = FALSE, width = "100%",
                                 selected = "Great Circles"))
           )
       ),
       ## If radio button is on "Custom", show cut point slider
       conditionalPanel(
         condition = "input.interpMeth == 'Custom'", 
          sliderInput("manualCutPoints", "Distance Cut Points", 0, 10000, 
                   #  FOR TEST DATA! NOT DYNAMIC YET THOUGH 
                   c(1500,5000), step = NULL, 
                   round = FALSE, 
                   format = "#,##0.#####", 
                   locale = "us", 
                   ticks = TRUE, animate = FALSE)
       )
       # ,
       # div(style = "font-size: 14px; padding: 10px 0px; margin-top: -25px",
       #     downloadButton("downloadSVG", label = "Export SVG")
       # )
     ),

      # Show a plot of the generated distribution
      mainPanel(
         #textOutput("dataCols"),
         #textOutput("ctr"),
         #textOutput("centerpoint_selected"),
         #textOutput("namefield_selected"),
         #textOutput("valuefield_selected"),
        #tableOutput("selectedData")),
         plotOutput("geoPlot", height = "550px"),
         htmlOutput("df"),
         htmlOutput("circledist"),
         htmlOutput("centerpoint_name"),
         htmlOutput("centerpoint_latlong"),
         htmlOutput("valuecolumn_name"),
         htmlOutput("valuecolumn_min"),
         htmlOutput("valuecolumn_max"),
         htmlOutput("newdfparser") #this is where we put all text outputs that require data from df2
      )
   )
)

########################################
##        STOP, SERVER TIME!          ##
########################################

server <- function(input, output) {
  

  p2 <- reactive ({
    #uploadFileData <- input$uploadFile
    #df <- read.csv(file = uploadFileData$datapath)
    df <- read.csv(file = "IND_remittances.csv")
    plot(df$lon, df$lat)
  })
  
  output$selectedData <- renderTable(
     # First reactive function!
    if(is.null(input$uploadFile) == TRUE){
      read.csv(file = "IND_remittances.csv")
    } else {
      n <- input$uploadFile
      read.csv(file = n$datapath)
    }
  
  )
  
   output$distPlot <- renderPlot({ # the basic dot plot for sidebar
     
      if (is.null(input$uploadFile) == TRUE){
        df <- read.csv(file = "IND_remittances.csv")
      }
     else{
       uploadFileData <- input$uploadFile
       df <- read.csv(file = uploadFileData$datapath)
     }

     par(bg = '#f5f5f5',
         mgp=c(1.75,0.5,0))
     plot(df$lon, df$lat, 
          col = "#000000", 
          xlab = "Longitude", 
          ylab = "Latitude",
          tck = -.04,
          cex.axis = 0.7)
     
   })
   
   # output$downloadSVG <- downloadHandler(
   #   filename = function() {
   #     paste("test", ".svg", sep = "")
   #   },
   #   content = function(file) {
   #     ggsave("test.svg", plot = p2, scale = 1, device = "svg", dpi = 150)
   #     # png(file = file)
   #     # p2()
   #     #dev.off()
   #   }
   # )
   
   # I think what we want to do is chunk out all the earlier parts of the
   # code into their own little input-output sections here, including
   # reactivity to the UI, so that the plot code is really only drawing
   # the plot. 
   
   dataframefinder <- function() { # First reactive function!
     if(is.null(input$uploadFile) == TRUE){
       found_df <- read.csv(file = "IND_remittances.csv")
     } else {
       n <- input$uploadFile
       found_df <- read.csv(file = n$datapath)
     }
     return(found_df)
   }

   dfvalues <- reactive(dataframefinder()) # Sets up the output of our function to be reactive
   
   output$newdfparser <- renderText({ # New place to store reactive output
     #paste("test: ", colnames(dfvalues())),
     
     
     
     
     #### fix column names

     #renderText(colnames(dataframefinder()), outputArgs = list())
     
     paste(#paste("<i>Column names: ", as.list(colnames(dfvalues())),  "<br>"),
          "Circle spacing: ", round((dfparser(dataframefinder())[[2]] / 10),2), "km", "</br>",
           "Center point name: ", dfparser(dataframefinder())[[5]], "</br>",
           "Center point coordinates: ", dfparser(dataframefinder())[[3]], ", ", dfparser(dataframefinder())[[4]], "</br>",
           "Value column name: ", colnames(dataframefinder())[[4]], "</br>",
           "Min value is: ", min(dfvalues()[[4]]), "<br>", #italic just to remember which function which
           "Max value is:", max(dfvalues()[[4]]), "</i>") #this is hard-coded--needs to find value column
      ### this is where we put all reactive text
   })

   dfparser <- function(selected_dataframe) { # First non-reactive function! We copied a bunch o code for this
     # ^^ works in concert with newdfparser
     df <- selected_dataframe
     df_ext <- ".csv"
     
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
     
     if("ctrBin" %in% colnames(df2)) {
       ctrPtName <- df2$valName[df2$ctrBin == TRUE]
       ctrPt <- c(df2$lat[df2$ctrBin == TRUE],df2$lon[df2$ctrBin == TRUE]) # get lat/lon
       
     } else {
       ctrPtName <- "Default Center"
       ctrPt <- c(median(df2$lat), median(df2$lon)) # 
     }
     
     # if(df_ext == "csv"){
     #   df2 <- dplyr::select(df2, valName, val, lat, lon) # just the fields we want
     # } else {
     #   if(df_ext == "geojson"){
     #     df2 <- dplyr::select(df2, valName, val, lat, lon) #something else
     #   } else {
     #     # print(paste("What even is this?"))
     #   }
     # }
     ### THIS OMISSION MIGHT BREAK MORE COMPLEX DATA, OR AT LEAST GET SLOW AND LEAKY
     
     df2$distance <- geodist(ctrPt[1], ctrPt[2], df2$lat, df2$lon, units = "km")
     maxdist <- max(df2$distance) # max great circle distance
     
     my_list <- list(df2, maxdist, ctrPt[1], ctrPt[2], ctrPtName)
     return(my_list)
   }


   ###
# output$dfparsertext <- 
  # renderText(
  #   paste("<b>Column Names: </b>", 
  #   paste(colnames(dataframefinder()), 
  #   paste("<br><b>Circle Spacing: </b>", 
  #   paste(((dfparser(dataframefinder())[[1]] / 10)),2),
  #   paste("<b>Center Point Name: </b>", 
  #   paste((ctrPtFinder(dataframefinder())[[3]])))),
  #   collapse=", ")
  #   ))
  #  
# paste("<b>Center Point Lat-Long: </b>", paste((ctrPtFinder(dataframefinder())[[1]])), ", ", paste((ctrPtFinder(dataframefinder())[[2]])) ))

  
  output$geoPlot <- renderPlot({ 

     ###################################
     #        PARSE COLUMNS            #   
     ###################################
    #let's make this a function then make it an input selection, 
    #then df2 takes user input w a sensible default
    
    
     df <- dataframefinder()
     df_ext <- ".csv"
     
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
     
     if("ctrBin" %in% colnames(df2)) {
       ctrPtName <- df2$valName[df2$ctrBin == TRUE]
       ctrPt <- c(df2$lat[df2$ctrBin == TRUE],df2$lon[df2$ctrBin == TRUE]) # get lat/lon

     } else {
       ctrPtName <- "Default Center"
       ctrPt <- c(median(df2$lat), median(df2$lon)) # 
     }

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

     ###################################
     #    DATA VALUE TRANSLATION       #
     ###################################   
     
     
     # min(my_data_frame[my_data_frame$my_column_number>0,my_column_number])
     
     valMin <- min(df2$val[df2$val != 0], na.rm = TRUE) # find lowest non-zero value (that will be radius 1)
     valMax <- max(df2$val, na.rm = T) # find highest non-zero value (that will be radius X)
     valMed <- median(df2$val, na.rm = T) # find median value (that could be radius 1 + X / 2)
   
     minRadius <- 1 # change this with the UI later?
     maxRadius <- 15 # change this with the UI later?
     
     if(input$valTransMeth == "Raw"){
       df2$valTrans <- df2$val
       # only works if we have data at a specific scale, for comparison only
     } else if(input$valTransMeth == "Scaled"){
       df2$valTrans <- sqrt(df2$val/valMax) * maxRadius
       # not sure of the exact math here but realized that linear scale
       # doesn't work since we want the areas to be proportionate, not
       # the radii. think the square root works because area is pi(r)2
       # so the pi doesn't matter if we're rescaling to maxRadius at 
       # maxValue anyway. Right? it's weekend, will come back to this later
     } else if(input$valTransMeth == "Log Scale"){
       df2$valTrans <- log(df2$val) + minRadius
     } else if(input$valTransMeth == "Square Root"){
       df2$valTrans <- sqrt(df2$val) + minRadius
       # this is basically "raw square root" since it's not scaled at all (had plus minRadius before)
     } else if(input$valTransMeth == "Custom"){
       df2$valTrans <- df2$val
       # raw right now. time for another slider? or is that too extra?
     }
      
     #df2$valTrans <- (df2$val / 400)^1.5 # manual one I did for NH data to see what we're looking for
     
     # hist(df2$val, breaks = 20) # just for giggles
     # hist(sqrt(df2$val), breaks = 20)
     # hist(log(df2$val), breaks = 20)
     
     #df2$val[df2$val == 0] <- NA # turn zeros to NAs for our purposes
     
     ###################################
     #        GREAT CIRCLE             #
     ###################################     
     
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
     circles <- circles[-1,] # remove zero-radius circle
     
     ###################################
     #      LOGARITHMIC SCALE          #
     ###################################      
     
     # Replot coordinates on log distance scale
     df2 <- df2 %>% mutate(
       logdistancex =  (useful::pol2cart(log(distance),ctrPtMathbearing,degrees = TRUE)[[1]]), 
       logdistancey = (useful::pol2cart(log(distance),ctrPtMathbearing,degrees = TRUE)[[2]])
     )
     
     # Combine x and y into a matrix, add as a column, remove x and y columns
     df2$logcoords <- cbind(df2$logdistancex,df2$logdistancey)
     df2 <- dplyr::select(df2,-starts_with("logdistance"))
     
     # NEED TO OVERRIDE CENTER POINT INFINITE VALUES WITH 0,0
     
     # Plot it
     df2 <- dplyr::arrange(df2, -val) # sorting for draw order below
     df2$num <- ave(df2$val, FUN = seq_along) # also sorting?
     
     
     ###################################
     #        SQUARE ROUTE             #
     ################################### 
     
     # Replot coordinates on square root distance scale
     df2 <- df2 %>% mutate(
       sqrtdistancex =  (useful::pol2cart(sqrt(distance),ctrPtMathbearing,degrees = TRUE)[[1]]), 
       sqrtdistancey = (useful::pol2cart(sqrt(distance),ctrPtMathbearing,degrees = TRUE)[[2]])
     )
     
     # Combine x and y into a matrix, add as a column, remove x and y columns
     df2$sqrtcoords <- cbind(df2$sqrtdistancex,df2$sqrtdistancey)
     df2 <- select(df2,-starts_with("sqrtdistance"))

     ###################################
     #        LAGRANGE DIST            #
     ################################### 
    
     # Automated route (could also just have user specify neardist and fardist)
     # hist(df2$distance)
     distancemedian <-median(df2$distance)
     geogdist <- c(0, distancemedian, ((distancemedian + distancemedian + maxdist)/3), maxdist) # let's say under 500 km is close, over 2000km is far
     neardist <- geogdist[2] # these two are the output of geogdist above
     fardist <- geogdist[3]
     chartdist <- c(0, 400, 800, 1200) # this just gets us equal intervals on the graph for the different segments of the lines
     
     # Simpler Automated route (just median)
     hist(df2$distance)
     distancemedian <-median(df2$distance)
     geogdist <- c(0, distancemedian, maxdist) # let's say under 500 km is close, over 2000km is far
     neardist <- geogdist[2] # these two are the output of geogdist above
     fardist <- geogdist[3]
     
     neardist <- input$manualCutPoints[1]
     fardist <- input$manualCutPoints[2]
     
     chartdist <- c(0, 800, 1200) # this just gets us equal intervals on the graph for the different segments of the lines
     
     # plot(geogdist, chartdist)
     
     a <- function(x){ # here is some example code for the piecewise funciton
       ifelse(( x >= 0 & x < neardist),(x * 400/neardist),ifelse((neardist <= x & x < fardist),((x * 400)/(fardist - neardist) + (400-((400)/(fardist - neardist)*neardist))), ifelse((fardist <= x & x <= maxdist),((x * 400)/(maxdist - fardist) + (800-((400)/(maxdist - fardist)*fardist))), NA))) 
     } 
     plot(a,xlim=c(0,maxdist), ylim = c(0, 1200), col = "red") 
     
     df2$lagrangedistcircstep <- a(df2$distance) # set new lagrange great circle distances using new stepwise function
     
     # function to make new circles with any stepwise function set above
     lagrange_predictstep <- function(circlesdataframe) { 
       lagrangecirclesdataframe <- circlesdataframe
       for (row in 1:nrow(circlesdataframe)){
         lagrangecirclesdataframe[row,"r"]<- a(circlesdataframe[row,"r"])}
       return(lagrangecirclesdataframe)
     }
     lagrangecircles <- lagrange_predictstep(circles) # this projects the circles
     
     # Replot coordinates on custom distance scale using piecewise function
     df2 <- df2 %>% mutate(
       customdistancex = (useful::pol2cart(a(distance),ctrPtMathbearing,degrees = TRUE)[[1]]), 
       customdistancey = (useful::pol2cart(a(distance),ctrPtMathbearing,degrees = TRUE)[[2]])
     )
     df2$customcoords <- cbind(df2$customdistancex,df2$customdistancey)
     df2 <- select(df2,-starts_with("customdistance"))
     
     ###################################
     #            PLOT CALL            #
     ################################### 
     
     #initating variables
     circleColor = "red" #default value for circle color. If they are red, something is broken
     ctrPtColor = "red"  #default value for center point color. If it is red, something is broken
     themeText = "red"
     
     # PLOT STYLE TEST
     darkPlot <- list(
      scale_color_viridis_c(option = "plasma"),
       theme(panel.background = element_rect(fill = "grey50", linetype = "blank"),
             plot.background = element_rect(fill= "grey50"),
             axis.ticks = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             panel.grid = element_blank(),
             legend.background = element_rect(fill = "grey50"),
             legend.text = element_text(color = "white"),
             legend.title = element_text(color = "white")
             ),
       coord_fixed(),
       labs(color = paste0("Distance from ", '\n',ctrPtName," (km)"), x = NULL, y = NULL),
       geom_point(stroke = 1, size = df2$valTrans),
       guides(colour = "colorbar",size = "legend")
     )
     
     lightPlot <- list(
       scale_color_viridis_c(option = "D"),
       theme(panel.background = element_blank(),
             axis.ticks = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank()),
       coord_fixed(),
       geom_point(stroke = 1, size = df2$valTrans),
       labs(color = paste0("Distance from", '\n', ctrPtName," (km)"), x = NULL, y = NULL),
       guides(colour = "colorbar",size = "legend")
      )
    
    monoPlot <- list(
      scale_color_gradient(low = "gray80", high = "#000000"),
      theme(panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()),
      coord_fixed(),
      geom_point(stroke = 1, size = df2$valTrans),
      labs(color = paste0("Distance from ", '\n',ctrPtName," (km)"), x = NULL, y = NULL),
      #labs(size = paste0("Distance from ", '\n',df2$valTrans," (km)"), x = NULL, y = NULL),
      # guides(colour = "colorbar",size = "legend")
      guides(size = guide_legend())
    ) 
      
     
     #this is where all the crazy themes go
     if(input$plotTheme == "Light Theme"){
       selectedPlotTheme <- lightPlot
       circleColor <- "gold"
       ctrPtColor <- "gold"
       themeText = "gray75"
     } else if (input$plotTheme == "Dark Theme"){
       selectedPlotTheme <-darkPlot
       circleColor <- "gray95"
       ctrPtColor <- "gray95"
       themeText = "white"
     } else if (input$plotTheme == "Mono Theme"){
       selectedPlotTheme <-monoPlot
       circleColor <- "gray50"
       ctrPtColor <- "gray50"
       themeText = "black"
     }
     
     # Figure out which plot to show
     plot_circles <- circles # set default for great circle/logarithmic
     if(input$interpMeth == "Great Circles"){
       plot_coordinates <- df2$circcoords
     } else if(input$interpMeth == "Square Root"){
       plot_coordinates <- df2$sqrtcoords
       plot_circles <- sqrt(circles)
     } else if(input$interpMeth == "Logarithmic"){
       plot_coordinates <- df2$logcoords
     } else if(input$interpMeth == "Custom"){
       plot_coordinates <- df2$customcoords
       plot_circles <- lagrangecircles
     }
     
     # For all the circular plots, plug in variables
     if(input$interpMeth != "Lat & Long"){
     plot <- ggplot(df2 %>% arrange(desc(val)), aes(
       plot_coordinates[,1], 
       plot_coordinates[,2],
       color = df2$distance) ) +
       selectedPlotTheme
     
     if(input$interpMeth == "Logarithmic"){ # sneaky way to add circles below
      plot$layers <- c(geom_circle(aes(x0 = x0, y0 = y0, r = log(r)),
                                  colour = circleColor,
                                  data = plot_circles, 
                                  show.legend = NA, 
                                  inherit.aes = FALSE), plot$layers)
      }
      else{
      plot$layers <- c(geom_circle(aes(x0 = x0, y0 = y0, r = r),
                    colour = circleColor,
                    data = plot_circles, 
                    show.legend = NA, 
                    inherit.aes = FALSE), plot$layers)
      }
     
     ###
     ###
     ### This works now, but nicer to shove into a list to change by theme
      if(input$labelsOn == TRUE){
        plot <- plot + geom_text(data = df2,
                                 aes(plot_coordinates[,1],
                                     plot_coordinates[,2],
                                     label = df2$valName),
                                 size = 3,
                                 check_overlap = TRUE,
                                 color = themeText)
      }
     
     
      if(input$centerOn == TRUE){
       plot <- plot + geom_point(data = (as.data.frame(ctrPt)), aes(0, 0), colour = ctrPtColor, shape = 10, size = 3)
      }
      
      plot
      
      }
     # Use simpler plot for Lat-Long (don't need circles, valTrans, etc.) 
     else{ 
       plot_latLon <- ggplot(df2, aes(df2$lon, df2$lat, color = df2$distance)) + 
         geom_point() + 
         geom_point(data = (as.data.frame(ctrPt)), aes(ctrPt[2], ctrPt[1]), color = ctrPtColor, shape = 10, size = 3) +
         scale_color_viridis_c(option = "plasma") +
         selectedPlotTheme
       plot_latLon
     }
     
     # ###
     # geoPlot4svg <- reactive({
     #   plot
     # })
     
   })
   
   valNameChoices <- reactive(
     c("Name1","Name2","Name3")
   )
  
   output$centerpoint_selected <- renderText({ 
     paste("You have selected the following center point:",ctrPt[1],", ",ctrPt[2])
   })
   
   output$namefield_selected <- renderText({ 
     paste0("These are the potential name fields:", valNameChoices)
   })
   
   output$valuefield_selected <- renderText({ 
     paste("These are the potential value fields:", valChoices)
   })
}

# "You have selected the following value field:") + 
# Run the application 
shinyApp(ui = ui, server = server)

