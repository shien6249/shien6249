library(leaflet)
library(DT)
install.packages("shinyWidgets")
library(shinyWidgets)
library("shinydashboard")


header<-dashboardHeader(title="QOF Map from 2012/13 to 2017/18", titleWidth = 1000)
#pols <- c("None", sort(unique(as.vector(full_shp$PlltntT)), decreasing=FALSE))
#diseasesel <- c("None", levels(full_shp$Disease))
yearse <- unique(df$Year)
yer <- sort(unique(as.character(full_shp$Year),decreasing=TRUE))

body<-
  dashboardBody(
    navbarPage(theme = "cerulean",
               "Menu",
               tabPanel("Map",
                        fluidRow(
                          column(width = 9,
                                 box(width = NULL, solidHeader = TRUE,
                                     uiOutput("synced_maps")
                                     #leafletOutput("EnglandMap", height=400)
                                 ),
                                 box(width=NULL,
                                     dataTableOutput("results")
                                 )
                          ),
                          
                          column(width=3,
                                 box(width=NULL, 
                                     #uiOutput("yearSelect"),
                                     selectInput(inputId = "dataYear", 
                                                 label = "Select Year:",
                                                 choices = yer,
                                                 selected = yer[1]),
                                                 
                                     radioButtons("dataPollu", "Select Pollutant Type", 
                                                  choices = c("NO2" = "NO2", 
                                                              "NOx" = "NOx",
                                                              "PM10" = "PM10", 
                                                              "PM2.5" = "PM25")),
                                     
                                     radioButtons(inputId = "dataDisease",
                                                  label = "Select Disease type:",
                                                  choices = c("Asthma", "COPD"),
                                                  selected = "Asthma")
                                     
                                 )
                          )
                        )
               ),
               
               tabPanel("Analysis",
                        fluidRow(
                          column(width = 9,
                                 box(width = NULL, solidHeader = TRUE,
                                     plotOutput(outputId = "scatterplot",
                                                hover="plot_hover"),
                                     verbatimTextOutput("summary")
                                 )
                          ),
                          
                          column(width=3,
                                 box(width=NULL, 
                                     selectInput(inputId = "x", 
                                                 label = "X-axis:",
                                                 choices = c("NO2" = "NO2", 
                                                             "NOx" = "NOx",
                                                             "PM10" = "PM10", 
                                                             "PM2.5" = "PM25"), 
                                                 selected = "NO2"),
                                     
                                     
                                     # Select variable for y-axis
                                     selectInput(inputId = "y", 
                                                 label = "Y-axis:",
                                                 choices = c("Prevalence Rate" = "Prevalence"), 
                                                 selected = "Prevalence Rate"),
                                     
                                     
                                     # Select variable for color
                                     selectInput(inputId = "z", 
                                                 label = "Colour by:",
                                                 choices = c("NO2"="NO2",
                                                             "NOx"="NOx",
                                                             "PM10"="PM10",
                                                             "PM2.5"="PM25"),
                                                 selected = "Year"),
                                     
                                     # Select disease type
                                     radioButtons(inputId = "selected_type",
                                                  label = "Select Disease type:",
                                                  choices = c("Asthma", "COPD"),
                                                  selected = "Asthma"),
                                     
                                     # Select year
                                     selectInput(inputId = "selected_year", 
                                                 label = "Select Year:",
                                                 choices = yearse, 
                                                 selected = yearse[1])
                                     
                                 )
                          )
                        )
                        
               )
               
    )
  )


ui<-
  #fluidPage(theme = shinytheme("cosmo"),
  dashboardPage(#skin = "black",
    header,
    dashboardSidebar(disable = TRUE),
    body
  )

shinyApp(ui = ui, server = server)
