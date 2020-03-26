    
    #library(shiny)
    library(shinydashboard)
    library(shinyWidgets)
    library(ggplot2)
    library(lubridate)
    library(DT)
    library(jpeg)
    library(leaflet)
   # library(plyr)
   # library(dplyr)
    library(stringr)
    library(scales)
    library(tidyverse)
    library(stats)
    library(scales)
    
    # ------------------------------------------------------------------------------------- #
    # ------------------------------------------------------------------------------------- #
    
    # Read in the raw data
    Rawdata = read.csv("data.csv", header=TRUE, dec=',')
    unTouchedData <- Rawdata
    
    
    # ------------------------------------------------------------------------------------- #
    # Redefine latitude and longitude to make is easier to use; narrowed to 4 decimals
    Rawdata$lat <- as.numeric(as.character(Rawdata$lat)) # Convert to char
    Rawdata$lon <- as.numeric(as.character(Rawdata$lon))
    
    # ------------------------------------------------------------------------------------- #
    
    # Naming blank tags propely, as untagged
    Rawdata$tags <- as.character(Rawdata$tags) # Convert into characters
    Rawdata$tags[nchar(Rawdata$tags) == 0] <- "untagged" #nchar finds the number of characters in that vector, when its blank, name it untagged
    
    # ------------------------------------------------------------------------------------- #
    # Time Manipulations
    
    # Creating column for only hour of the day
    Rawdata$EuropeanHour <- format(as.POSIXct(Rawdata$litterTimestamp), format = "%H")
    
    # Add time of day column, based off of the hour of the day
    Rawdata$timeOfDay[Rawdata$hour >= 0 & Rawdata$hour <= 6] <- "night"
    Rawdata$timeOfDay[Rawdata$hour >= 7 & Rawdata$hour <= 12] <- "morning"
    Rawdata$timeOfDay[Rawdata$hour >= 13 & Rawdata$hour <= 18] <- "afternoon"
    Rawdata$timeOfDay[Rawdata$hour >= 19 & Rawdata$hour <= 24] <- "evening"
    
    # Create Temp Data Frame, and make changes to that one
    tempData <- Rawdata
    
    # ------------------------------------------------------------------------------------- #
    # Removing any data outside of the range of the study
    tempData<-tempData[tempData$lat > 41 & tempData$lat < 42,]
    tempData<-tempData[tempData$lon > -88 & tempData$lon < -86,]
    longitude <- subset(tempData, select ="lon")
    latitude <- subset(tempData, select ="lat")
    
    # ------------------------------------------------------------------------------------- #
    # Making a Date and Year Column / Data Frame
    justTheDates <- as.character(as.Date(tempData$litterTimestamp))
    tempData$Date <- justTheDates
    justTheYear <- year(tempData$Date)
    
    #Created Data frame with each year in the challenge, and calculated the frequency of each year
    FrequencyByYear <- as.data.frame(sort(table(justTheYear),decreasing=T))
    
    # ------------------------------------------------------------------------------------- #
    # Making a Day of Week column
    tempData$dotw <- wday(tempData$Date, label=TRUE, abbr=FALSE)

    # Data frame to have as back up incase issues occur 
    backUpTempData <- tempData # N
    
    # ------------------------------------------------------------------------------------- #
    
    # Split the strings in the tag column, making them into a vector and change into chars
    
    tempData$Newtags <- strsplit(tempData$tags,",")
    tempData$Newtags <- as.character(tempData$Newtags)
    tempData$username <- as.character(tempData$username)
    
    # ------------------------------------------------------------------------------------- #  
    CleanedData <- tempData # Data frame holds all data, cleaned and ready for sub data frames
    # ------------------------------------------------------------------------------------- #
   
    # Total Amount of liter 
    totalLitter <- nrow(CleanedData) #11,913

    # Calculating the top ten, most frquently logged tags
    justTheTags <- Rawdata$tags #untouched column with all of the data
    s <- strsplit(justTheTags, split = ",") # splits the scolumns that have multiple tags in them
    
    splitUpValues <- data.frame(V1 = unlist(s)) #seperates all of the splits, and gives each tag its own colu,
    litterCountByTag <- as.data.frame(sort(table(splitUpValues),decreasing=T)) # No tags are repeated, each tag is listed with its frequency
    litterCountByTag <- litterCountByTag[-c(22, 30, 102), ] # Deleting unessecary tags
    
    # df of top ten tags and their frequency
    topTenTags <- head(litterCountByTag,10)
    
    # Renaming columns
    names(topTenTags)[1] <- "Tag" 
    names(topTenTags)[2] <- "Count"
    names(litterCountByTag)[1] <- "Tag" 
    names(litterCountByTag)[2] <- "Count"
    # ------------------------------------------------------------------------------------- #
    
    # df of the top ten people who collected litter
    justThePickers <- Rawdata$username
    rankPickers <- as.data.frame(sort(table(justThePickers),decreasing=T))
    topTenPickers <- head(rankPickers, 10)
    
    # Renaming columns
    names(topTenPickers)[1] <- "Username" 
    names(topTenPickers)[2] <- "Count"
    names(rankPickers)[1] <- "Username" 
    names(rankPickers)[2] <- "Count"
    
    # ------------------------------------------------------------------------------------- #
    # Options for different map styles / themes
    mapRenderingsList <- c('Standard','Light', 'Dark')
    mapSourcesList <- c(providers$OpenStreetMap.Mapnik,  providers$CartoDB.Positron, providers$CartoDB.DarkMatter)
    mapRenderings <- hashmap(mapRenderingsList, mapSourcesList)
    
    # ------------------------------------------------------------------------------------- #
    # ------------------------------------------------------------------------------------- #
    
    # Start of ui dashboard page
    ui <- dashboardPage( skin = "black", 
            dashboardHeader(title = "No Time Left To Waste"),
            dashboardSidebar(disable = FALSE, collapsed = FALSE,
                       sidebarMenu(
                         
                         # side bar items, and selction input drop boxes are initialized here
                         menuItem("Home", tabName = "home", icon = icon("home", lib = "glyphicon"), selected=TRUE),
                          # menuItem("About", tabName = "about", icon = icon("info-sign", lib = "glyphicon")),
                        
                         menuItem("Project by Amber Little", tabName = "cheapBlankSpace", icon = NULL),
                         # Spaced out for ease of scrolling to view other graphs
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         
                         menuItem("Data Source: litterati.org", tabName = "cheapBlankSpace", icon = NULL),
                         
                         menuItem("Select a user &/ Tag Below", tabName = "cheapBlankSpace", icon = NULL),
                         
                         selectInput("Tags", "Top Ten Tags", topTenTags$Tag, selected = "plastic", multiple = TRUE),
                         selectInput("Pickers", "Top Ten Pickers", topTenPickers$Username, selected = "julieta", multiple = TRUE),
                         selectInput(inputId="mapRender",  #choose map style
                                     label="Map Style",
                                     choices=mapRenderingsList) 
                       ) # End side bar menu
        
      ), #End dashboardSidebar
      
      # ------------------------------------------------------------------------------------- #
      # Left Column
      dashboardBody(
        fluidRow (
          column(2,
                 
            fluidRow(
              box(title = "Collection By User", solidHeader = TRUE, status = "success", width = 12,
                  plotOutput("plotByContrb", height = 200)
              )
            ),    
            
            fluidRow(
              box(title = "Top 10 Pickers", solidHeader = TRUE, color = "black", width = 12,
                dataTableOutput("topTenPickerBox", )
              )
            )
            
        ), # End of left side column
        
        
        # ------------------------------------------------------------------------------------- #
        # Middle Column
        column(8,
          fluidRow(
            box(title = "Litter Picked Up By Hour", solidHeader = TRUE, status = "success", width = 12,
              plotOutput("pickByHour", height = 200)
            )
          ),
               
          fluidRow(
            box(title = "Map", width = 12, leafletOutput("MapofChicago", height = 400)
            )
          ),
               
          fluidRow(
              box(title = "Litter Picked By The Day Of The Week", solidHeader = TRUE, status = "success", width = 12,
                    plotOutput("box5", height = 300)
                 )
              ),
               
            fluidRow(
                box(title = "Amount of Litter Picked Up By Tag", solidHeader = TRUE, status = "success", width = 12,
                    plotOutput("barChartLitterByTag", height = 200)
                )
              )
            ),
        
        # ------------------------------------------------------------------------------------- #
        # Right side Column
        column(2,
            fluidRow(
              box(title = "Collection By Year", solidHeader = TRUE, status = "success", width = 12,
                plotOutput("plotByYear", height = 200)
              )
            ),

            fluidRow(
              box(title = "Top 10 Tags", solidHeader = TRUE, color = "black", width = 12,
                  dataTableOutput("topTenTagBox")
              )
            )
          ) 
        ) # End fluid row that holds everything
      ) # End dashboardBody
    ) # End dashboardPage
    
    
    # ------------------------------------------------------------------------------------- #
    
    # Start of server, input & output
    server <- function(input, output) { 
    
     # Reactive variables to avoid repeating later    
     userReactive <- reactive({subset(CleanedData, CleanedData$username == input$Pickers & CleanedData$Newtags == input$Tags)}) # Reactive variable for usernames, and tags
     justUsernameReactive <- reactive({subset(CleanedData, CleanedData$username == input$Pickers)}) # Reactive Variable for only the username
     
     # Pie Chart Displaying User Contributions
     output$plotByContrb <- renderPlot({
      
       ggplot(topTenPickers, aes(x = "", y = Count, fill = Username)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0)
     })
     
     # Top Ten Tags List - Ouputs list of top ten tags and their frequencies
      output$topTenTagBox<- DT::renderDataTable(
        DT::datatable( {
            litterCountByTag },
              options = list(searching=FALSE, pageLength = 10, lengthChange=FALSE,  rownames=FALSE)
        )
      )
        
      # Top Ten Username List - Ouputs list of top ten users (by username) and their frequencies
      output$topTenPickerBox<- DT::renderDataTable(
        DT::datatable ( {
            rankPickers },
             options = list(searching=FALSE, pageLength = 10, lengthChange=FALSE,  rownames=FALSE)
        )
      )
      
      # Litter by hour - Outputs litter count / hour
      output$pickByHour <- renderPlot({
        hourReactive <- userReactive()
          ggplot(hourReactive, aes(x = EuropeanHour)) + 
            labs(x = "Hour (24-Hour Clock)" , y= "Count") +
              geom_bar(fill = "#4D6466", color = '#333333')
      })
      
      # Map output
      output$MapofChicago <- renderLeaflet({ 
        mapReactive <- userReactive()
        leaflet(data = mapReactive, width = 100) %>% addTiles() %>%
          addProviderTiles(mapRenderings[[input$mapRender]]) %>%
          addMarkers(clusterOptions = markerClusterOptions(),
                     lng=mapReactive$lon,
                     lat=mapReactive$lat,
                        popup = paste (
                          "Username: ", mapReactive$username, "<br>",
                          "Tags: ", mapReactive$Newtags, "<br>",
                          "Date:", mapReactive$Date, "<br>",
                          "(Lat, Lon): (", mapReactive$lat, ", ", mapReactive$lon, ")")
                          ) %>%
                              setView(lng=-87.81, lat=41.87, zoom=11)
        })
    
      
      # Litter by day of the week 
      output$box5 <- renderPlot({
        userReactiveData <- userReactive() 
          ggplot(userReactiveData, aes(x = dotw)) +
            labs(x = "Day of The Week" , y= "Count") +
              geom_bar(fill = "#4D6466", color = '#333333')
      })

      
      # Every Year Pie Chart
      output$plotByYear <- renderPlot({
       ggplot(FrequencyByYear, aes(x = "", y = Freq, fill = justTheYear)) +
          geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0)
             
      })
      
      
      # Bar chart of top ten tags
      output$barChartLitterByTag <- renderPlot({
        ggplot(topTenTags, aes(x = topTenTags$Tag, y = topTenTags$Count)) +
          labs(x = "Tags" , y= "Count") +
            geom_bar(stat = "identity", fill = "#4D6466", color = '#333333')  
      })
      
      
      } # End Server
    
     shinyApp(ui, server)
    
    
