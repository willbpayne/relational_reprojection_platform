# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. Find out more about Shiny here: http://shiny.rstudio.com/

library(base)
library(datasets)
library(dplyr)
library(forcats)
library(geojsonsf) # read in GeoJSON for background
library(geosphere) # where we get bearing
library(ggforce) # to plot circles
library(ggplot2)
library(ggrepel)
library(gmt) # actually for geodist
library(graphics)
library(grDevices)
library(httpuv) # ditto
library(lubridate)
library(methods)
library(pracma)
library(purrr)
library(readr)
library(rsconnect) # for web hosting
library(scico) # for newer graph colors (colorblind friendly, better for continuous)
library(sf) # for geometry transformations
#library(shiny)
#library(shinylive) # to host this on GitHub Pages
library(stats)
library(stringr)
library(svglite)
library(tibble)
library(tidyr)
library(tidyverse)
library(useful) # for cartesian conversions
library(utils)

options(ggrepel.max.overlaps = Inf) 

# libraries we thought we needed but don't use yet

#library(sp) # might not need
#library(tools) #to learn file extension
#library(jsonlite) # for loading in json files
#library(geojson) # for loading geojson (need this for geojsonio to work)
#library(geojsonio) # for loading geojson

dataFile <- "data/Katrina_Migration.csv" # for testing

########################################
##          UI PARTY TONIGHT!         ##
########################################

ui <- fluidPage(theme = "RRP_style.css",
                div(style = "padding: 10px", h1("Relational Reprojection Platform (BETA)")), # Application title
                sidebarLayout(sidebarPanel(width=5,
                  div(class = "panel",
                    div(style = "font-size: 14px; padding: 0px; margin-top: -5px;",
                        fluidRow(
                          column(5,fileInput("uploadFile"," 1. Select Data File (CSV)", multiple = FALSE, accept = NULL)),
                          column(2,checkboxInput("basemap","Show Polygons?", value = TRUE, width = NULL)),
                          column(5,fileInput("uploadPolygon"," 2. Select Polygons (GeoJSON)", multiple = FALSE, accept = NULL))
                        )),
                    div(style = "font-size: 14px; padding: 10px 0px; margin-top: -35px",
                        fluidRow(
                          column(12,radioButtons("interpMeth", "3. Choose Distance Interpolation Method", choices = c("Great Circle","Square Root","Cube Root","Log","Custom"), inline = TRUE, width = "100%", selected = "Square Root"))
                        )),
                    div(style = "font-size: 14px; padding: 10px 0px; margin-top: -20px",
                        fluidRow(
                          column(6,  uiOutput("ValChoicesFromServer")),
                          column(6,sliderInput("SymbolSizeRange", "Point Size Range", 0, 50, c(1, 25), ticks = TRUE)
                          ))),
                    div(style = "font-size: 14px; padding: 10px 0px; margin-top: -30px",
                        fluidRow(
                          column(6,  uiOutput("NameChoicesFromServer")),
                          column(6,  sliderInput("label_size", "Label Size", 1, 10, 3, ticks = TRUE)
                          ))),
                    div(style = "font-size: 14px; padding: 10px 0px; margin-top: -40px",
                        fluidRow(
                          column(4,checkboxInput("labelsOn", "Show Labels?", value = TRUE, width = NULL)),
                          column(4,checkboxInput("removeZeroes","Remove Zero Values?", value = TRUE, width = NULL)),
                          column(4,checkboxInput("latlon","Show Lat/Lon Plot", value = FALSE, width = NULL))
                        )),
                    div(style = "font-size: 14px; padding: 10px 0px; margin-top: -25px",
                        fluidRow(
                          column(6,selectInput("plotTheme", 
                                               label = NULL, c("Light Theme", "Dark Theme", "Mono Theme"), 
                                               selected = "Light", multiple = FALSE, selectize = TRUE, width = "100%", size = NULL)),
                          column(6,
                                 div(style = "margin-bottom:15px",
                                     downloadButton('downloadPlot','Export SVG'))), 
                        )),
                    conditionalPanel(condition = "input.interpMeth == 'Custom'",
                                     uiOutput("CustomDistanceSlider")),
                    conditionalPanel(condition = "input.latlon == 1",
                                     div(style = "font-size: 14px; padding: 10px 0px; margin-top: -35p", 
                                         fluidRow(
                                           column(12, plotOutput("distPlot", height = "250px")), #plot size
                                         )))
                    )),
                #end of sidebar panel, end of class panel div
                mainPanel(width=7,div(class = "mainP", htmlOutput("newdfparser"), plotOutput("geoPlot", height = "800px")
                ))
                )
                )            
                    
                    #div(style = "font-size: 14px; padding: 10px 0px; margin-top: -50px",
                    #    fluidRow(
                    #      column(6,checkboxInput("centerOn", "Show Center?", value = FALSE, width = NULL)),
                    #      column(6, checkboxInput("HideOverlappingLabels","Hide Overlapping Labels?", value = TRUE, width = NULL))
                    #    )),
                    
                    
                    #div(style = "font-size: 14px; padding: 10px 0px; margin:3%; margin-top: -35px",
                     #   fluidRow(
                      #    column(6,sliderInput("SymbolSizeRange", "Point Size Range", 0, 50, c(1, 25), ticks = TRUE)
                      #  ))),
                    # Radio buttons for interpolation method
                    #div(style = "font-size: 14px; padding: 10px 0px; margin:3%; margin-top: -25px",
                    #    fluidRow(
                    #      column(12,radioButtons("valTransMeth","Value (Symbol Size) Interpolation", choices = c("None", "Square Root", "Log"), inline = TRUE, width = "100%", selected = "Square Root"))
                    #    )),

                    #div(style = "font-size: 14px; padding: 10px 0px; margin:3%; margin-top: -40px",
                    #    fluidRow(
                    #      column(12,radioButtons("interpMeth", "Distance Interpolation", choices = c("Great Circle","Square Root","Cube Root","Log","Custom"), inline = TRUE, width = "100%", selected = "Square Root"))
                    #    )),
                    
                    ## If distance transformation radio button is on "Custom", show cut point slider
                    
                    


########################################
##        STOP, SERVER TIME!          ##
########################################

server <- function(input, output) {
  
  output$distPlot <- renderPlot({ # the basic dot plot for sidebar
    
    if (is.null(input$uploadFile) == TRUE){
      df <- read.csv(file = "data/Katrina_Migration.csv")
    }
    else{
      uploadFileData <- input$uploadFile
      df <- read.csv(file = uploadFileData$datapath)
    }
    
    if (is.null(input$uploadPolygon) == TRUE){
      polygon <- geojson_sf("data/us-states.geojson")
    }
    else{
      uploadPolygonData <- input$uploadPolygon
      polygon <- geojson_sf(file = uploadPolygonData$datapath)
    }
    
    par(bg = "#F0F0F0", #default color is #f5f5f5
        mgp=c(1.75,0.5,0),
        mar=c(4,4,4,4),
        col.lab="#404040",
        col.axis="#404040",
        fg="#404040")
    
    df3 <- df # cloning for non-destructive editing and with a different name than in dataframefinder() below
    latNames2 <- c("lat","Lat","LAT", "latitude", "Latitude", "LATITUDE", "y","Y", "coords.x2") # add as they come up
    lonNames2 <- c("lon","Lon","LON","long","Long","LONG","longitude", "Longitude", "LONGITUDE", "x","X", "coords.x1")

    for (col in 1:ncol(df)) {
      if (max(as.numeric(df[[col]]), na.rm = T) <= 90.0
          && min(as.numeric(df[[col]]), na.rm = T) >= -90.0
          && names(df)[[col]] %in% latNames2) # lat
      { 
        df3$latitude <- as.numeric(df[[col]])
        print("I found latitude!")
        }
      else{
        if (max(as.numeric(df[[col]]), na.rm = T) <= 180.0
            && min(as.numeric(df[[col]]), na.rm = T) >= -180.0
            && names(df)[col] %in% lonNames2) # lon
        { 
          df3$longitude <- as.numeric(df[[col]])
          }
      }
      }
        
    plot(df3$longitude, df3$latitude, 
         col = "#404040", 
         xlab = "Longitude", 
         ylab = "Latitude",
         tck = -.04,
         cex.axis = 0.7
    )
    
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste("testPlot",'.svg',sep='')},
    content = function(file){
      ggsave(file, plot = last_plot(),
             height = 4000,
             width = 4000,
             units = "px") #distPlot the right thing to call here or p2?
    })
  
  dataframefinder <- function() { # First reactive function!
    if(is.null(input$uploadFile) == TRUE){
      found_df <- read.csv(file = dataFile)
    } else {
      n <- input$uploadFile
      found_df <- read.csv(file = n$datapath)
    }
    return(found_df)
  }
  
  polygonfinder <- function() { # Second reactive function!
    if(is.null(input$uploadPolygon) == TRUE){
      polygon <- geojson_sf("data/us-states.geojson")
    } else {
      n <- input$uploadPolygon
      polygon <- geojson_sf(geojson = n)
    }
    return(polygon)
  }
  
  output$newdfparser <- renderText({ # New place to store reactive output
    parserOutputs <- dfparser(dataframefinder()) # run it once!
    if (parserOutputs[[2]] > 20037.5){
      distanceWarning <- " (over half Earth's circumference; plot may be unreliable!)"
    }
    else {
      distanceWarning <- ""
    }
    paste("<b> Maximum distance in dataset: </b>", round(parserOutputs[[2]],0), "km",distanceWarning,"<br><b> Concentric circle spacing: </b>", round((parserOutputs[[2]] / 10),2), "km", "</br>",
      "<b> Center point: </b>", parserOutputs[[5]], " ",
      " (Latitude: ", round(parserOutputs[[3]], 5), ", Longitude: ", round(parserOutputs[[4]], 5), ")</br>", sep='',collapse = "") 
  })
  
  dfparser <- function(selected_dataframe) { # First non-reactive function! We copied a bunch o code for this
    # ^^ works in concert with newdfparser
    df <- selected_dataframe
    df_ext <- ".csv"
    
    colListOrig <- colnames(df) # store column names for later
    latNames <- list("lat","Lat","LAT", "latitude", "Latitude", "LATITUDE", "y","Y", "coords.x2") # add as they come up
    lonNames <- list("lon","Lon","LON","long","Long","LONG","longitude", "Longitude", "LONGITUDE", "x","X", "coords.x1")
    nameChoices <- c() # changed from a list to a vector
    valChoices <- c() # ditto
    
    df2 <- df # cloning df for non-destructive editing
    
    latflag <- 0 # need these here for the column detection
    lonflag <- 0
    ctrBinflag <- 0
    nameFlag <- 0
    valflag <- 0
    
    for (col in 1:ncol(df)) {
      if (typeof(df[[col]]) == "double"
          && latflag == 0
          && max(as.numeric(df[[col]]), na.rm = T) <= 90.0
          && min(as.numeric(df[[col]]), na.rm = T) >= -90.0
          && names(df)[[col]] %in% latNames) # lat
      { 
        df2$lat <- df[[col]]
        latflag <- 1}
      else{
        if (typeof(df[[col]]) == "double"
            && lonflag == 0
            && max(as.numeric(df[[col]]), na.rm = T) <= 180.0
            && min(as.numeric(df[[col]]), na.rm = T) >= -180.0
            && names(df)[col] %in% lonNames) # lon
        { 
          df2$lon <- df[[col]]
          lonflag <- 1}
        else{
          if (typeof(df[[col]]) != "character"
              && typeof(df[[col]]) != "list"
              && min(as.numeric(df[[col]]), na.rm = T) == 0
              && max(as.numeric(df[[col]]), na.rm = T) == 1
              && sum(as.numeric(df[[col]]), na.rm = T) == 1) # ctrBin
          { df2$ctrBin <- as.logical(df[[col]])
          ctrBinflag <- 1}
          else{
            if (typeof(df[[col]]) == "character" # catches name and name_long
                || is.factor(df[[col]]) == T) # valName
            { if (nameFlag == 0)
            {df2$valName <- as.character(df[[col]])
            nameChoices <- c(nameChoices, names(df)[[col]])
            nameFlag <- 1}
              else{
                nameChoices <- c(nameChoices, names(df)[[col]])}
            }
            else{
              if (typeof(df[[col]]) == "integer" || typeof(df[[col]]) == "double" # val
                  && !(names(df)[col] %in% lonNames)
                  && !(names(df)[col] %in% latNames)
                  && names(df[col]) != "geometry")
              { if (valflag == 0)
              {df2$val <- as.double(df[[col]])
              ValColNametoprint <- names(df)[[col]]
              valChoices <- c(valChoices, names(df)[[col]])
              valflag <- 1}
                else{
                  valChoices <- c(valChoices, names(df)[[col]])}
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
    
    valChoicesList <- as.list(valChoices)
    
    nameChoicesList <- as.list(nameChoices)
    
    # if(df_ext == "csv"){
    #   df2 <- dplyr::select(df2, valName, val, lat, lon) # just the fields we want
    # } else {
    #   if(df_ext == "geojson"){
    #     df2 <- dplyr::select(df2, valName, val, lat, lon) #something else
    #   } else {
    #     # print(paste("What even is this?"))
    #   }
    # }

    df2$distance <- geodist(ctrPt[1], ctrPt[2], df2$lat, df2$lon, units = "km")
    maxdist <- max(df2$distance) # max great circle distance in kilometers
    
    maxvaltoprint <- max(df2$val)
    minvaltoprint <- min(df2$val)
    
    my_list <- list(df2, maxdist, ctrPt[1], ctrPt[2], ctrPtName, ValColNametoprint, maxvaltoprint, minvaltoprint, valChoicesList, nameChoicesList)
    return(my_list)
  }
  
  output$CustomValueSlider <-  renderUI({
    valuesfromparser <- dfparser(dataframefinder())
    maxvalue_forslider <- valuesfromparser[[7]]
    minvalue_forslider <- valuesfromparser[[8]]
    sliderInput("manualValueCutPoints", "Value Cut Points", min = round(minvalue_forslider), max = round(maxvalue_forslider), value = c((maxvalue_forslider/3),(maxvalue_forslider*2)/3), step = NULL)
  })
  
  output$CustomDistanceSlider <-  renderUI({
    maxdist_forslider <- dfparser(dataframefinder())[[2]]
    default_dist_low <- maxdist_forslider/3
    default_dist_high <- (maxdist_forslider*2)/3
    if (maxdist_forslider < 10) # keep round slider numbers only for big distances
    {
    sliderInput("manualCutPoints", "Custom Distance Cut Points", min = 0, max = round(maxdist_forslider,2), value = c(round(default_dist_low,2),round(default_dist_high,2)), step = NULL, animate = FALSE, post = ' km', round = FALSE)
    }
    else
    {
    sliderInput("manualCutPoints", "Custom Distance Cut Points", min = 0, max = round(maxdist_forslider,-1), value = c(round(default_dist_low,-1),round(default_dist_high,0)), step = NULL, animate = FALSE, post = ' km', round = TRUE)
  }
    })
  
  output$ValChoicesFromServer <- renderUI({ # serve up a list of value columns
    choicesForDropdown <- dfparser(dataframefinder())[[9]]
    selectInput("valSelection", "4. Select Point Size Column",
                multiple = FALSE,
                choices = choicesForDropdown,
                selected = choicesForDropdown[[1]],
                selectize = TRUE,
                width = "100%", size = NULL)
  })
  
  output$NameChoicesFromServer <- renderUI({ # serve up a list of value columns
    namechoicesForDropdown <- dfparser(dataframefinder())[[10]]
    selectInput("nameSelection", "5. Select Label Column",
                multiple = FALSE,
                choices = namechoicesForDropdown,
                selected = namechoicesForDropdown[1],
                selectize = TRUE,
                width = "100%", size = NULL)
  })
  
  output$geoPlot <- renderPlot({ 
    
    ###################################
    #        PARSE COLUMNS            #   
    ###################################
    
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
    nameFlag <- 0
    valFlag <- 0
    
    for (col in 1:ncol(df)) {
      if (typeof(df[[col]]) == "double"
          && latflag == 0
          && max(as.numeric(df[[col]]), na.rm = T) <= 90.0
          && min(as.numeric(df[[col]]), na.rm = T) >= -90.0
          && names(df)[[col]] %in% latNames) # lat
      { 
        df2$lat <- df[[col]]
        latflag <- 1}
      else{
        if (typeof(df[[col]]) == "double"
            && lonflag == 0
            && max(as.numeric(df[[col]]), na.rm = T) <= 180.0
            && min(as.numeric(df[[col]]), na.rm = T) >= -180.0
            && names(df)[col] %in% lonNames) # lon
        { 
          df2$lon <- df[[col]]
          lonflag <- 1}
        else{
          if (typeof(df[[col]]) != "character"
              && typeof(df[[col]]) != "list"
              && min(as.numeric(df[[col]]), na.rm = T) == 0
              && max(as.numeric(df[[col]]), na.rm = T) == 1
              && sum(as.numeric(df[[col]]), na.rm = T) == 1) # ctrBin
          { df2$ctrBin <- as.logical(df[[col]])
          ctrBinflag <- 1}
          else{
            if (typeof(df[[col]]) == "character" # catches name and name_long
                || is.factor(df[[col]]) == T) # valName
            { if (nameFlag == 0)
            {df2$valName <- as.character(df[[col]])
            nameFlag <- 1}
              else{
                valNameChoices <- c(valNameChoices, names(df)[[col]])}
            }
            else{
              if (typeof(df[[col]]) == "integer" || typeof(df[[col]]) == "double" # val
                  && !(names(df)[col] %in% lonNames)
                  && !(names(df)[col] %in% latNames)
                  && names(df[col]) != "geometry")
              { if (valFlag == 0)
              {df2$val <- as.double(df[[col]])
              LegendValName <- names(df)[[col]]
              valFlag <- 1}
                else{
                  valChoices <- c(valChoices, names(df)[[col]])}
              }
            }
          }
        }
      }
    }
    
    valColumn <- input$valSelection # override with UI selection
    LegendValName <- input$valSelection # update in the legend too
    df2$val <- df[[valColumn]]
    
    nameColumn <- input$nameSelection # override with UI selection
    df2$valName <- df[[nameColumn]]
    
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
    df2$distance <- geodist(ctrPt[1], ctrPt[2], df2$lat, df2$lon, units = "km")*1000
    maxdist <- max(df2$distance) # max great circle distance in meters
    
    df2 <- df2 %>% mutate(ctrPtGeobearing = geosphere::bearing(c(ctrPt[2],ctrPt[1]), cbind(lon, lat), a=6378137, f=1/298.257223563)) # get bearing of all points to center
    
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
    #       POLYGON BACKGROUND        #
    ###################################     
    
    polygon <- polygonfinder()
    
    # fix multipolygon issue
    polygon_cast <- st_cast(st_cast(polygon,"MULTIPOLYGON"),"POLYGON")
    print("CAST THE BACKGROUND TO POLYGONS!")
    
    # this converts it to a dataframe of just points
    polygon_dataframe = data.frame(st_coordinates(st_cast(polygon_cast$geometry,"MULTIPOINT")))
    
    # fix data type issue
    polygon_dataframe$L1_Chr <- as.character(polygon_dataframe$L1)
    
    # add geographic distance to center point
    polygon_dataframe$distance <- geodist(ctrPt[1], ctrPt[2], polygon_dataframe$Y, polygon_dataframe$X, units = "km")*1000
    
    # add bearing
    polygon_dataframe_bearing <- polygon_dataframe %>% mutate(ctrPtGeobearing = geosphere::bearing(c(ctrPt[2],ctrPt[1]), cbind(X, Y), a=6378137, f=1/298.257223563)) # get bearing of all points to center
    
    # convert to polar coordinates
    for (row in 1:nrow(polygon_dataframe_bearing)) # convert from geographic bearings to polar coordinates
    {if(polygon_dataframe_bearing$ctrPtGeobearing[row] <= 0)
      # if geobearing is 0 or negative, mathbearing is 90 plus bearing
      polygon_dataframe_bearing$ctrPtMathbearing[row] <- abs(polygon_dataframe_bearing$ctrPtGeobearing[row]) + 90
    if(polygon_dataframe_bearing$ctrPtGeobearing[row] > 0 & polygon_dataframe_bearing$ctrPtGeobearing[row] < 90)
      # if geobearing is positive and equal to or under 90, mathbearing is 90 - bearing
      polygon_dataframe_bearing$ctrPtMathbearing[row] <- (90 - polygon_dataframe_bearing$ctrPtGeobearing[row])
    if(polygon_dataframe_bearing$ctrPtGeobearing[row] >= 90 & polygon_dataframe_bearing$ctrPtGeobearing[row] <= 180)
      # if geobearing is positive and between 90 and 180 (inclusive), mathbearing is 360 - (geobearing - 90)
      polygon_dataframe_bearing$ctrPtMathbearing[row] <- 360 - (polygon_dataframe_bearing$ctrPtGeobearing[row] - 90)}
    rm(row) # remove counter
    polygon_dataframe_polar <- dplyr::select(polygon_dataframe_bearing,-starts_with("ctrPtGeo"))
    
    ###################################
    #    DATA VALUE TRANSLATION       #
    ###################################   
    
    valMin <- min(df2$val[df2$val != 0], na.rm = TRUE) # find lowest non-zero value (that will be radius 1)
    valMax <- max(df2$val, na.rm = T) # find highest non-zero value (that will be radius X)
    valMed <- median(df2$val, na.rm = T) # find median value (that could be radius 1 + X / 2)
    
    minRadius <- input$SymbolSizeRange[1] # change this with the UI later?
    maxRadius <- input$SymbolSizeRange[2] # change this with the UI later?
    
    # if(input$valTransMeth == "None"){
    #   df2$valTrans <- (df2$val/valMax) * maxRadius + (minRadius - 1)
    #   # proportion of maximum value without scaling
    # } else if(input$valTransMeth == "Square Root"){
    #   df2$valTrans <- (sqrt(df2$val/valMax)) * maxRadius + (minRadius - 1)
    # } else if(input$valTransMeth == "Log"){
    #   df2$valTrans <- (log(df2$val)/log(valMax) * maxRadius) + (minRadius - 1)
    #   df2$valTrans[is.infinite(df2$valTrans)]<-0
    # } else if(input$valTransMeth == "Custom"){
    #   df2$valTrans <- df2$val
    # }

    df2$valTrans <- (sqrt(df2$val/valMax)) * maxRadius + (minRadius - 1)
    
    if(input$removeZeroes == TRUE){
      df2$val[df2$val == 0] <- NaN
      df2<-subset(df2, val >= 0)
    }
    if(input$removeZeroes == FALSE){
      df2$val[df2$val == 0] <- NA
    }
    
    df2$labelNames <- df2$valName # initialize empty strings for labels
    df2$labelNames[df2$val == 0] <- " " # add label text for non-zero values
    df2$labelNames[df2$isCTR == FALSE & is.na(df2$val) == TRUE] <- " " # add label text for non-zero values
        
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
    circles <- circles[-1,] # Remove zero-radius circle
 
    # set up great circle basemap coords
    polygon_dataframe_polar <- polygon_dataframe_polar %>% mutate(
      circdistancex = (useful::pol2cart(distance,ctrPtMathbearing,degrees = TRUE)[[1]]),
      circdistancey = (useful::pol2cart(distance,ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    polygon_dataframe_polar$circcoords <- cbind(polygon_dataframe_polar$circdistancex,polygon_dataframe_polar$circdistancey)
    polygon_dataframe_polar <- select(polygon_dataframe_polar,-starts_with("circdistancex"))
    
    ###################################
    #      LOGARITHMIC SCALE          #
    ###################################      
    
    # Replot coordinates on log distance scale
    df2 <- df2 %>% mutate(
      logdistancex =  (useful::pol2cart(log(distance + 1),ctrPtMathbearing,degrees = TRUE)[[1]]), 
      logdistancey = (useful::pol2cart(log(distance + 1),ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    
    # Overrides infinite values
    df2$logdistancex[is.nan(df2$logdistancex)] <- 0 
    df2$logdistancey[is.nan(df2$logdistancey)] <- 0
    
    # Combine x and y into a matrix, add as a column, remove x and y columns
    df2$logcoords <- cbind(df2$logdistancex,df2$logdistancey)
    df2 <- dplyr::select(df2,-starts_with("logdistance"))
    
    # Plot it
    df2 <- dplyr::arrange(df2, -val) # sorting for draw order below
    df2$num <- ave(df2$val, FUN = seq_along) # also sorting?
    
    # set up logarithmic basemap values
    polygon_dataframe_polar <- polygon_dataframe_polar %>% mutate(
      logdistancex =  (useful::pol2cart(log(distance + 1),ctrPtMathbearing,degrees = TRUE)[[1]]), 
      logdistancey = (useful::pol2cart(log(distance + 1),ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    
    # Overrides infinite values
    polygon_dataframe_polar$logdistancex[is.nan(polygon_dataframe_polar$logdistancex)] <- 0 
    polygon_dataframe_polar$logdistancey[is.nan(polygon_dataframe_polar$logdistancey)] <- 0
    
    # Combine x and y into a matrix, add as a column, remove x and y columns
    polygon_dataframe_polar$logcoords <- cbind(polygon_dataframe_polar$logdistancex,polygon_dataframe_polar$logdistancey)
    polygon_dataframe_polar <- dplyr::select(polygon_dataframe_polar,-starts_with("logdistance"))
    
    ###################################
    #        SQUARE ROUTE             #
    ################################### 
    
    # Replot coordinates on square root scale, combine x and y into a matrix,
    # add as a column, remove x and y columns
    df2 <- df2 %>% mutate(
      sqrtdistancex =  (useful::pol2cart(sqrt(distance),ctrPtMathbearing,degrees = TRUE)[[1]]), 
      sqrtdistancey = (useful::pol2cart(sqrt(distance),ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    df2$sqrtcoords <- cbind(df2$sqrtdistancex,df2$sqrtdistancey)
    df2 <- select(df2,-starts_with("sqrtdistance"))
    
    # Set up square root basemap coords
    polygon_dataframe_polar <- polygon_dataframe_polar %>% mutate(
      sqrtdistancex = (useful::pol2cart(sqrt(distance),ctrPtMathbearing,degrees = TRUE)[[1]]), 
      sqrtdistancey = (useful::pol2cart(sqrt(distance),ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    polygon_dataframe_polar$sqrtcoords <- cbind(polygon_dataframe_polar$sqrtdistancex,polygon_dataframe_polar$sqrtdistancey)
    polygon_dataframe_polar <- select(polygon_dataframe_polar,-starts_with("sqrtdistance"))
    
    ###################################
    #           CUBE ROOT             #
    ################################### 
    
    # Replot coordinates on cube root distance scale
    # Combine x and y into a matrix, add as a column, remove x and y columns
    df2 <- df2 %>% mutate(
      cuberootdistancex = (useful::pol2cart(pracma::nthroot(distance, 3),ctrPtMathbearing,degrees = TRUE)[[1]]), 
      cuberootdistancey = (useful::pol2cart(pracma::nthroot(distance, 3),ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    df2$cuberootcoords <- cbind(df2$cuberootdistancex,df2$cuberootdistancey)
    df2 <- select(df2,-starts_with("cuberootdistance"))
    
    # Set up cube root basemap coords
    polygon_dataframe_polar <- polygon_dataframe_polar %>% mutate(
      cuberootdistancex = (useful::pol2cart(pracma::nthroot(distance, 3),ctrPtMathbearing,degrees = TRUE)[[1]]), 
      cuberootdistancey = (useful::pol2cart(pracma::nthroot(distance, 3),ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    polygon_dataframe_polar$cuberootcoords <- cbind(polygon_dataframe_polar$cuberootdistancex,polygon_dataframe_polar$cuberootdistancey)
    polygon_dataframe_polar <- select(polygon_dataframe_polar,-starts_with("cuberootdistancex"))
    
    ###################################
    #          CUSTOM DIST            #
    ################################### 
    # atm, more just a piecewise linear function
    neardist <- input$manualCutPoints[1]*1000
    fardist <- input$manualCutPoints[2]*1000
    # 
    chartdist <- c(0, 800, 1200) # this just gets us equal intervals on the graph for the different segments of the lines
    # 
    # 
    a <- function(x){ # here is some example code for the piecewise funciton
      ifelse((x >= 0 & x < neardist),
             (x * 400/neardist),
      ifelse((neardist <= x & x < fardist),
            ((x * 400)/(fardist - neardist) + (400-((400)/(fardist - neardist)*neardist))), 
      ifelse((fardist <= x & x <= maxdist),((x * 400)/(maxdist - fardist) + (800-((400)/(maxdist - fardist)*fardist))), NA)))
    }

    # function to make new circles with any stepwise function set above
    custom_predictstep <- function(dataframe) {
      customcirclesdataframe <- dataframe # duplicate dataframe
      for (row in 1:nrow(dataframe)){
        customcirclesdataframe$r[row] <- a(dataframe$r[row])} # applying function a to every row in dataframe
      return(customcirclesdataframe)
    }
    customcircles <- custom_predictstep(circles) # this projects the circles

    # Replot coordinates on custom distance scale using piecewise function
    df2 <- df2 %>% mutate(
      customdistancex = (useful::pol2cart(a(distance),ctrPtMathbearing,degrees = TRUE)[[1]]),
      customdistancey = (useful::pol2cart(a(distance),ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    df2$customcoords <- cbind(df2$customdistancex,df2$customdistancey)
    df2 <- select(df2,-starts_with("customdistance"))
    
    # custom distance coordinates
    polygon_dataframe_polar <- polygon_dataframe_polar %>% mutate(
      customdistancex = (useful::pol2cart(a(distance),ctrPtMathbearing,degrees = TRUE)[[1]]),
      customdistancey = (useful::pol2cart(a(distance),ctrPtMathbearing,degrees = TRUE)[[2]])
    )
    polygon_dataframe_polar$customcoords <- cbind(polygon_dataframe_polar$customdistancex,polygon_dataframe_polar$customdistancey)
    polygon_dataframe_polar <- select(polygon_dataframe_polar,-starts_with("customdistance"))
    
    ###################################
    #            PLOT CALL            #
    ################################### 
    
    #initating variables
    circleColor = "red" #default value for circle color. If they are red, something is broken
    ctrPtColor = "red"  #default value for center point color. If it is red, something is broken
    themeText = "red"
    
    # PLOT STYLE 
    darkPlot <- list(
      scale_color_scico(palette = "tokyo", begin = 0.1, end = 0.95),
      #scale_color_viridis_c(option = "plasma"),
      theme(panel.background = element_rect(fill = "grey80", linetype = "blank"),
            plot.background = element_rect(fill= "grey80"),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid = element_blank(),
            legend.background = element_rect(fill = "grey80"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
      ),
      coord_fixed(),
      labs(color = paste0("Total ",tolower(LegendValName), " :: ", '\n',ctrPtName), x = NULL, y = NULL),
      geom_point(na.rm = TRUE, stroke = 1, alpha = 0.7, size = df2$valTrans),
      guides(colour = "colorbar",size = "legend")#,
    )
    scale_fill_scico()
    lightPlot <- list(
scale_color_scico(palette = "imola", begin = 0.2, end = 0.95),
      theme(panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()),
      coord_fixed(),
      geom_point(stroke = 1, alpha = 0.78, size = df2$valTrans),
      labs(color = paste0("Total ",tolower(LegendValName), " :: ", '\n',ctrPtName), x = NULL, y = NULL),
      guides(colour = "colorbar", size = "legend")#,
    )
    
    monoPlot <- list(
      scale_color_gradient(low = "gray80", high = "#000000"),
      theme(panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()),
      coord_fixed(),
      geom_point(stroke = 1,  alpha = 0.7, size = df2$valTrans),
      labs(color = paste0("Total ",tolower(LegendValName), " :: ", '\n',ctrPtName), x = NULL, y = NULL),
      guides(size = guide_legend())#,
    ) 
    
    #this is where all the themes go
    if(input$plotTheme == "Light Theme"){
      selectedPlotTheme <- lightPlot
      circleColor <- "ivory3"
      ctrPtColor <- "red"
      themeText = "black" #change for nacis
  #    themeText = "gray60"
    } else if (input$plotTheme == "Dark Theme"){
      selectedPlotTheme <-darkPlot
      circleColor <- "gray50"
      ctrPtColor <- "red"
      themeText = "black"
    } else if (input$plotTheme == "Mono Theme"){
      selectedPlotTheme <-monoPlot
      circleColor <- "gray75"
      ctrPtColor <- "red"
      themeText = "black"
    } 
    
    # Figure out which plot to show
    plot_circles <- circles # set default for great circle/logarithmic
    if(input$interpMeth == "Great Circle"){
      plot_coordinates <- df2$circcoords
      basemapcoords <- polygon_dataframe_polar$circcoords
    } else if(input$interpMeth == "Square Root"){
      plot_coordinates <- df2$sqrtcoords
      plot_circles <- sqrt(circles)
      basemapcoords <- polygon_dataframe_polar$sqrtcoords
    } else if(input$interpMeth == "Cube Root"){
      plot_coordinates <- df2$cuberootcoords
      basemapcoords <- polygon_dataframe_polar$cuberootcoords
    } else if(input$interpMeth == "Log"){
      plot_coordinates <- df2$logcoords
      basemapcoords <- polygon_dataframe_polar$logcoords
    } else if(input$interpMeth == "Decimal Log"){
      plot_coordinates <- df2$declogcoords
    } else if(input$interpMeth == "Custom"){
      plot_coordinates <- df2$customcoords
      basemapcoords <- polygon_dataframe_polar$customcoords
      plot_circles <- customcircles
    }
    
    polygon_dataframe_polar_list<-split(polygon_dataframe_polar, polygon_dataframe_polar$L1_Chr) # this splits into list of polygons, but some are still multipolygons (and would need to be split previously)
    
    # For all the circular plots, plug in variables
    if(input$interpMeth != "Lat & Long"){
      plot <- ggplot(df2 %>% arrange(desc(val)), aes(
        plot_coordinates[,1], 
        plot_coordinates[,2],
        color = df2$val,
        stroke = 'black')) +
        selectedPlotTheme
      
      if(input$basemap == TRUE){
      
      if(input$plotTheme == "Light Theme"){
        plot$layers <- c(geom_polygon(colour = 'black', data = polygon_dataframe_polar, 
                                      aes(x=basemapcoords[,1], y=basemapcoords[,2], group = L1_Chr), rule = 'winding', fill = 'gray', colour = "black", size = .2), plot$layers)
        #    themeText = "gray60"
      } else if (input$plotTheme == "Dark Theme"){
        plot$layers <- c(geom_polygon(colour = 'black', data = polygon_dataframe_polar, 
                                      aes(x=basemapcoords[,1], y=basemapcoords[,2], group = L1_Chr), rule = 'winding', fill = 'white', colour = "black", size = .2), plot$layers)
      } else if (input$plotTheme == "Mono Theme"){
        plot$layers <- c(geom_polygon(colour = 'black', data = polygon_dataframe_polar, 
                                      aes(x=basemapcoords[,1], y=basemapcoords[,2], group = L1_Chr), rule = 'winding', fill = 'gray95', colour = "black", size = .2), plot$layers)
      } 
      }

      if(input$interpMeth == "Log"){ # sneaky way to add circles below
        plot$layers <- c(geom_circle(aes(x0 = x0, y0 = y0, r = log(r)),
                                     colour = "black",
                                     data = plot_circles,
                                     show.legend = NA,
                                     inherit.aes = FALSE), plot$layers)
      }
      else if(input$interpMeth == "Cube Root"){ # sneaky way to add circles below
        plot$layers <- c(geom_circle(aes(x0 = x0, y0 = y0, r = pracma::nthroot(r, 3)),
                                     colour = circleColor,
                                     data = plot_circles,
                                     show.legend = NA,
                                     inherit.aes = FALSE), plot$layers)
      }
      else if(input$interpMeth == "Decimal Log"){ # sneaky way to add circles below
        plot$layers <- c(geom_circle(aes(x0 = x0, y0 = y0, r = log10(r)),
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

      if(input$labelsOn == TRUE){
        plot <- plot + geom_text(data = df2,
                                 aes(plot_coordinates[,1],
                                     plot_coordinates[,2],
                                     label = df2$labelName),
                                 size = 0,
                                 check_overlap = input$showAllLabels, # tried to do the text outline thing and failed. could we do a conditional or expression for text color, where depending where it is on the scale it gets a different text color?
                                 color = "white",  fontface = "bold") + geom_label_repel(data = df2,
                                                              aes(plot_coordinates[,1],
                                                                  plot_coordinates[,2],
                                                                  label = df2$labelName),
                                                              size = input$label_size,
                                                              min.segment.length = 0.75,
                                                              check_overlap = input$HideOverlappingLabels,
                                                              color = themeText,
                                                              segment.color = 'gray20',
                                                              segment.size = 0.25,
                                                              alpha = 0.75, seed = 1234) + geom_label_repel(data = df2,
                                                                                              aes(plot_coordinates[,1],
                                                                                                  plot_coordinates[,2],
                                                                                                  label = df2$labelName),
                                                                                              size = input$label_size,
                                                                                              min.segment.length = 0.75,
                                                                                              check_overlap = input$HideOverlappingLabels,
                                                                                              color = themeText,
                                                                                              segment.color = 'gray20',
                                                                                              segment.size = 0.25,
                                                                                              fill = NA,
                                                                                              alpha = 1, seed = 1234)
      }
      
      #if(input$centerOn == TRUE){
      #  plot <- plot + geom_point(data = (as.data.frame(ctrPt)), aes(0, 0), colour = ctrPtColor, shape = 10, size = 3)
      #}
      
      plot
      
    }
    # Use simpler plot for Lat-Long
    else{
      plot_latLon <- ggplot(df2, aes(df2$lon, df2$lat, color = df2$val)) +
        geom_point() +
        selectedPlotTheme +
        geom_point(data = (as.data.frame(ctrPt)), aes(ctrPt[2], ctrPt[1]), color = ctrPtColor, shape = 10, size = 3) +
        scale_color_viridis_c(option = "plasma")
      plot_latLon
    }
    
  }) 
}

shinyApp(ui = ui, server = server) # Run the application 

#shinylive::export(appdir = "app", destdir = "docs")

#httpuv::runStaticServer("docs")
#install.packages('rsconnect')

#rsconnect::deployApp("app",appName="rrp_beta")
