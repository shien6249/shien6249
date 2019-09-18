library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(ggplot2)
library(DT)
library(lme4) # for mix ffect model
install.packages("leafsync")
library(leafsync) # for sync the maps


# get shapefile data
#qof_shp <- readOGR(dsn = "data/link", "qof_shp")
full_shp <- readOGR(dsn = "data/fulllink", "full_shp_new_5")
names(full_shp@data)

View(full_shp)

#imd_shp <- readOGR(dsn = "data/link", "imd_shp1_new")
#air_shp <- readOGR(dsn = "data/link", "air_shp_new")
#air_shp <- readOGR(dsn = "data/linkedair", "air_shp_new_4")
#air_shp <- readOGR(dsn = "data/link", "air_shp")
View(full_shp@data)
air <- full_shp[1:17]

df <- read.csv("data/twodataccg.csv")
df<-na.omit(df)
df$Year <- as.character(df$Year)

# adjust the value
full_shp@data$Vallue <- round(full_shp@data$Vallue,2)
air@data$Vallue <- round(air@data$Vallue,2)
full_shp@data$Year <- as.factor(as.character(full_shp@data$Year))

# Find the edges of our map
bounds <- bbox(full_shp)

server<-
  function(input, output, session){
    
    
    getDataSetdisease <- reactive({
      
      #data_pg_df[!is.na(data_pg_df$Region) & data_pg_df$Region == input$regionInput, ]
      
      req(input$dataDisease)
      req(input$dataYear)
      datasubset1 <- full_shp[!is.na(full_shp$Year)&full_shp$Year==input$dataYear&
                                !is.na(full_shp$Disease)&full_shp$Disease==input$dataDisease&
                                !is.na(full_shp$PlltntT)&full_shp$PlltntT==input$dataPollu,]
      datasubset1
      
    })
    
    getDataSetair <- reactive({
      req(input$dataPollu)
      datasubset2 <- air[air$PlltntT==input$dataPollu,]
      datasubset2
      
    })
    

    #################################################################
    ## Map part
    #################################################################   

    # Due to use of leafletProxy below, this should only be called once
    output$synced_maps <- renderUI({
      
      theDatadisease <- getDataSetdisease()
      theDataair <- getDataSetair()
      
      # colour palette mapped to data
      pal <- colorQuantile("YlOrRd", theDatadisease$Prevlnc, n = 4)
      pal_air <- colorQuantile("Greys", theDataair$Vallue, n = 4) 
      
      
      # set text for the clickable popup labels
      qof_popup <- paste("<strong>Disease</strong>: ", theDatadisease$Disease, "<br>",
                         "<strong>Area</strong>: ", theDatadisease$lad18nm, "<br>",
                         "<strong>Prevalence</strong>： ", theDatadisease$Prevlnc, "<br>")
      
      air_popup <- paste("<strong>Pollutant</strong>: ", theDataair$PlltntT, "<br>",
                         "<strong>Area</strong>: ", theDataair$lad18nm, "<br>",
                         "<strong>Value</strong>: ", theDataair$Vallue, "<br>")
      
      m1 <- 
        leaflet(full_shp,options = leafletOptions(preferCanvas = TRUE)) %>% 
        addTiles() %>%
        addPolygons(fillColor = pal(theDatadisease$Prevlnc), 
                    fillOpacity = 0.8, 
                    color = "#BDBDC3", 
                    weight = 2,
                    popup = qof_popup,
                    data=theDatadisease)%>%
        
        setView(mean(bounds[1,]), 
                mean(bounds[2,]),
                zoom=5.5)
      
      m2 <- 
        leaflet(air,options = leafletOptions(preferCanvas = TRUE)) %>% 
        addTiles() %>%
        addPolygons(fillColor = pal_air(theDataair$Vallue), 
                    fillOpacity = 0.8, 
                    color = "#525266", 
                    weight = 2,
                    popup = air_popup,
                    data=theDataair)%>%
        
        setView(mean(bounds[1,]), 
                mean(bounds[2,]),
                zoom=5.5)
      
      sync(m1, m2)      
    
      })

    
    
    observe({

      # If the data changes, the polygons are cleared and redrawn, however, 
      # the map (above) is not redrawn
      leafletProxy("synced_maps")%>%
                   #,data = theDatadisease) %>%
        clearShapes()

      
    })
    

    
    # allow to zoom in when click
    observe({
      click <- input$EnglandMap_shape_click
      proxy <- leafletProxy("synced_maps")
      if(is.null(click))
        return()
      proxy %>% 
        setView(lng = click$lng, lat = click$lat, zoom = 8)
    })
    
    
    ###
    # table of results, rendered using data table
    output$results <- output$results <- renderDataTable(datatable({

      dataSet<-getDataSetdisease()
      dataSet<-dataSet@data[,c(1,12,13,14,18,19)] # Get the name and value columns
      names(dataSet)<-c("Area","Year","Pollutant","Value","Disease","Prevalence")
      dataSet},
      
      options = list(lengthMenu = c(5, 10, 33), pageLength = 5)))
    

    #################################################################
    ## Analysis part
    #################################################################
    
    df <- read.csv("data/twodataccg.csv")
    df<-na.omit(df)
    df$Year <- as.character(df$Year)


    # Create a subset of data filtering for selected title types
    disease_subset <- reactive({

      req(input$selected_type)
      req(input$selected_year)
      dplyr::filter(df, Disease %in% input$selected_type & 
                      Year %in% input$selected_year)
      
    })
    
    
    # Create scatterplot to see the correlation
    output$scatterplot <- renderPlot({
      ggplot(data = disease_subset(), 
             aes_string(x = input$x, y = input$y, color = input$z)) +
        geom_point()
      

    })

    
    # Create random effect model output
    output$summary <- renderPrint({
      
      df<-
        df %>%
        spread(key = Disease, value = Prevalence)
      
      x <- df %>% pull(input$x)

      if (input$selected_type == "COPD") {
        print(summary(lmer(COPD ~ x + (1|LocalAuthority), data = df)))
      } else {
        print(summary(lmer(Asthma ~ x + (1|LocalAuthority), data = df)))
      }

    })

  }
