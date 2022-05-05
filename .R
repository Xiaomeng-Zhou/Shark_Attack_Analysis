## app.R ##
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(leaflet)
library(shinyWidgets)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(hrbrthemes)

## read data -------------------------------------------------------------------
df <- read_csv("data/sharks_geocoding_test.csv")
names(df)[13] <- 'Fatal'

## year for selectInput list 
year_span <-c("All year",1968:2018)

countries <- c("All place","USA","BRAZIL","AUSTRALIA","ENGLAND","COSTA RICA","MALDIVES","SOUTH AFRICA",
               "THAILAND","BAHAMAS","ECUADOR","MALAYSIA","CUBA","SPAIN","JAPAN","EGYPT","ST HELENA, British overseas territory",
               "MEXICO","REUNION","UNITED KINGDOM","UNITED ARAB?EMIRATES","PHILIPPINES",
               "INDONESIA","NEW CALEDONIA","CHINA","COLUMBIA","NEW ZEALAND","FRENCH POLYNESIA",
               "FIJI","MOZAMBIQUE","MAURITIUS","KIRIBATI","ISRAEL","FRANCE","JAMAICA","NIGERIA",
               "TONGA","SCOTLAND","TAIWAN","DOMINICAN REPUBLIC","KENYA","PAPUA NEW GUINEA",
               "RUSSIA","SEYCHELLES","TURKS & CAICOS","SOUTH KOREA","MALTA","VIETNAM","MADAGASCAR",
               "PANAMA","GUAM","CROATIA","ST.?MARTIN","GRAND CAYMAN","VANUATU","URUGUAY",
               "VENEZUELA","INDIA","CANADA","OKINAWA","MARSHALL ISLANDS","HONG KONG",
               "CHILE","SOMALIA","EL SALVADOR","ITALY","PORTUGAL","SOUTH CHINA SEA","WESTERN SAMOA",
               "BRITISH ISLES","TURKEY","MICRONESIA","PALAU","GRENADA")

shark_species <- c("All species","Angel shark","Blacktip shark","Blue shark",
                  "Bronze whaler shark","Bull shark","Caribbean reef shark","Carpet shark",
                  "Copper shark","Dogfish shark","Dusky shark","Galapagos shark","Grey nurse shark",
                  "Grey reef shark", "Hammerhead shark", "Juvenile shark","Lemon shark","Mako shark",
                  "Oceanic whitetip shark","Porbeagle shark", "Raggedtooth shark","Reef shark",
                  "Sand shark", "Sevengill shark", "Spinner shark", "Thresher shark","Tiger shark",
                  "White shark","Wobbegong shark","Zambesi shark","Unknown")

activities <- c("All activity","Paddle and Boogie boarding","Standing", "Surfing" ,"Swimming",
                "Fishing", "Walking","Diving","Snorkeling","Kayaking", "Floating", "Playing in the water")

ui <- dashboardPage(
  dashboardHeader(title = "Sharks Attacks"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Info", tabName = "Info", icon = icon("dashboard")),
      menuItem("Overview of Sharks Attacks", tabName = "map", icon = icon("th")),
      menuItem("Frequencies", tabName = "freq", icon = icon("th")),
      menuItem("Time", tabName = "time", icon = icon("th")),
      menuItem("Shark Attack Analysis", tabName = "species_attack", icon = icon("th")),
      menuItem("Map 2", tabName = "map_2", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # Info
      tabItem(tabName = "Info",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Shark",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Map of Shark Attacks - Overview
      tabItem(tabName = "map",
              h2("Overview of Attacks"),
              
              # Dynamic infoBoxes
              #infoBoxOutput("progressBox"),
              
              leafletOutput("map"),
              
              absolutePanel(top = 50, right = 10,
                            pickerInput("species", label = "Select a shark:",
                                        choices = list("All sharks", 
                                                       "Angel shark",
                                                       "Blacktip shark",
                                                       "Blue shark",
                                                       "Bronze whaler shark",
                                                       "Bull shark",
                                                       "Caribbean reef shark",
                                                       "Carpet shark",
                                                       "Copper shark",
                                                       "Dogfish shark",
                                                       "Dusky shark",
                                                       "Galapagos shark",
                                                       "Grey nurse shark",
                                                       "Grey reef shark",
                                                       "Hammerhead shark",
                                                       "Juvenile shark",
                                                       "Lemon shark",
                                                       "Mako shark",
                                                       "Oceanic whitetip shark",
                                                       "Porbeagle shark",
                                                       "Raggedtooth shark",
                                                       "Reef shark",
                                                       "Sand shark",
                                                       "Sevengill shark",
                                                       "Spinner shark",
                                                       "Thresher shark",
                                                       "Tiger shark",
                                                       "White shark",
                                                       "Wobbegong shark",
                                                       "Zambesi shark",
                                                       "Unknown"
                                        ),
                                        options = list(
                                          
                                          `live-search` = TRUE)
                            )),
              fluidRow(valueBoxOutput("Total"), valueBoxOutput("Survival"), valueBoxOutput("Death"))
      ),
      
      # Frequencies
      tabItem(tabName = "freq",
              fluidRow(
                box(
                  title = "Shark Attack Frequency by Type of Attack",width = 100,
                  plotOutput("actOccurrPlot", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Shark Attack Frequency Chart Settings",
                  sliderInput("num_attacks",
                              "Number of shark attacks:",
                              min = 1,
                              max = 700,
                              value = 1),
                  
                  sliderInput(inputId = "word_size", label = "Word size for frequency chart:",
                              min = 1, max = 30, value = 15)
                )
              )
      ),
      
      #Species Attacks
      tabItem(tabName = "species_attack",
              fluidRow(
                box(
                  title = "Shark Attack Frequency by Type of Attack",width = 100,
                  plotlyOutput("Attack_by_species", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Shark Attack Frequency by Type of Attack",width = 100,
                  plotlyOutput("Attack_Type", height = "600px")
                )
              ),
              fluidRow(
                box(
                  ## slider input for setting the max number of words in the word cloud
                  sliderInput(inputId = "MinNum", label = "Minimum count of Attack",
                              min = 1, max = 314, value = 10, step = 1),
                )
              )
      ),
      
      # Map2
      tabItem(tabName = "map_2",
              fluidRow(
                leafletOutput(outputId ="map2"),
              box(title = "Xiaomeng",
                  
                  ## select year 
                  selectInput(inputId = "year", label = "Year",
                              choices = year_span,
                              selected = "All year"),
                  # select country/place
                  pickerInput(inputId = "country", label = "Country/Place",
                              choices = countries,selected = "All place",
                              options = list(`live-search`=TRUE)),
                  ## select shark species 
                  pickerInput(inputId = "species2", label = "Select shark species",
                              choices =shark_species,selected = "All species",
                              options = list(`live-search`=TRUE)),
                  ## select activity
                  pickerInput(inputId = "activity", label = "Activity when attack happened",
                              choices = activities,selected = "All activity",
                              options = list(`live-search`=TRUE)),
                  #add action button to refresher the app
                  actionButton(inputId = "Refresh", 
                               label = "Refresh Map")),
                box(
                  #create a check box for whether or not show the table
                  checkboxInput(inputId = "showtable", label = "Show Data Table",
                                value = FALSE),
                  ## add button to download data 
                  actionButton(inputId = "download", 
                              label = "Download as csv")
              )),
              fluidRow(DT::dataTableOutput("table"))
      ),
      
      ####Time
      tabItem(tabName = "time",
              fluidRow(
                box(title = "Shark Attacks During The Time","This tab is giving you basic
                    information about shark attacks during a specific time period.", br(), br(),
                    "You can select a shark specie from more than 20 species.",br(), br(),
                    "The time period of shark attacks is 50 years. You can specify a desired range of years."),
                box(title = "Specify Shark Specie and Time Period:",
                    pickerInput("species3", label = "Select a shark:",
                                choices = list("All sharks", 
                                               "Angel shark",
                                               "Blacktip shark",
                                               "Blue shark",
                                               "Bronze whaler shark",
                                               "Bull shark",
                                               "Caribbean reef shark",
                                               "Carpet shark",
                                               "Copper shark",
                                               "Dogfish shark",
                                               "Dusky shark",
                                               "Galapagos shark",
                                               "Grey nurse shark",
                                               "Grey reef shark",
                                               "Hammerhead shark",
                                               "Juvenile shark",
                                               "Lemon shark",
                                               "Mako shark",
                                               "Oceanic whitetip shark",
                                               "Porbeagle shark",
                                               "Raggedtooth shark",
                                               "Reef shark",
                                               "Sand shark",
                                               "Sevengill shark",
                                               "Spinner shark",
                                               "Thresher shark",
                                               "Tiger shark",
                                               "White shark",
                                               "Wobbegong shark",
                                               "Zambesi shark",
                                               "Unknown"
                                ),
                                options = list(`live-search` = TRUE)),
                    
                    sliderInput("range", label = h5("Choose Range of Years:"), min = 1968, 
                                max = 2018, value = c(1968, 2018))
                    
                )),
              
              fluidRow(
                box(title = "Attacks in Specific Time Period", plotOutput(outputId = "year")),
                box(title = "Attacks by Months", plotOutput(outputId = "bar")))
      )
      
    )
  )
)  
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  ###### info
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  ###### Map Sharks
  filteredData <- reactive({
    if (input$species == "All sharks") {
      df
    } else {
      filter(df, Species == input$species)
    }
  })
  
  
  output$map <- renderLeaflet({
    PointUsePalette <- colorFactor(palette = c("blue", "red"),
                                   domain = c("N", "Y"))
  
    
    leaflet(filteredData()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(~longitude, ~latitude, 
                       label = ~Species,
                       color = ~PointUsePalette(Fatal),
                       popup = ~Activity,
                       radius = 5,
                       labelOptions = labelOptions(textsize = "12px"))
  })
  
  observe({
    PointUsePalette <- colorFactor(palette = c("blue", "red"),
                                   domain = c("N", "Y"))
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircleMarkers(~longitude, ~latitude, 
                       label = ~Species,
                       radius = 5,
                       color = ~PointUsePalette(Fatal),
                       popup = ~Activity,
                       labelOptions = labelOptions(textsize = "12px"))
  })
  
  output$year <- renderPlot({
    
    count_date <- count(filteredData(),Year)
    
    p <- ggplot(count_date, aes(x=Year, y=n)) + 
      geom_line()+
      labs(x = "Years", y = "Number of Attacks")+
      ggtitle("Attacks during the 50 Years")+
      theme_gray()
    
    p
    
  })
  
  output$bar <- renderPlot({
    count_date2 <- filteredData() %>%
      group_by(Month) %>%
      summarise(count2=n())
    
    q <- ggplot(count_date2, aes(x = reorder(Month, -count2), count2)) + 
      geom_bar(stat="identity")+
      labs(x = "Months", y = "Number of Attacks")+
      ggtitle("Attacks by Months during the 50 Years")+
      theme_gray()
    q
    
  })
  
  output$Total <- renderValueBox({
    valueBox(
      paste0(count(filteredData()[, 'Species'])), "# of Attacks", icon = icon("globe", lib = "glyphicon"),
      color = "aqua"
    )
  })
  
  
  output$Survival <- renderValueBox({
    
    b <- count(filteredData()[, c('Species', "Fatal")] %>%
                 filter(Fatal == "N") %>%
                 select(Species))
    
    valueBox(
      paste0(round((b/count(filteredData()[, 'Species']))*100,0), " %"), "Survival Rate", icon = icon("heart", lib = "glyphicon"),
      color = "lime"
    )
  })
  
  output$Death <- renderValueBox({
    a <- count(filteredData()[, c('Species', "Fatal")] %>%
                 filter(Fatal == "Y") %>%
                 select(Species))
    
    valueBox(
      paste0(a), "# of Fatal Attacks", icon = icon("user", lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  ####Frequencies
  output$actOccurrPlot <- renderPlot({
    
    df2 <- df %>% group_by(Activity) %>% mutate(count_act_occurr = n()) %>% filter(count_act_occurr > input$num_attacks)
    
    g2 <- ggplot(data=df2, aes(x=reorder(Activity,count_act_occurr))) +
      geom_bar(stat="count") +
      coord_flip() +
      theme(text = element_text(size = input$word_size),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()
      )
    g2
  })
  
  
  ######Xiaomeng
  
  # Tab"Shark Species"
  output$Attack_by_species <- renderPlotly({
    
    barchart <- df %>%
      filter(Species !="Unknown") %>%
      group_by(Species) %>%
      mutate(count = n()) %>%
      filter(count > input$MinNum) %>%
      ggplot(aes(fill = Fatal,x = reorder(Species,count)))+
      geom_bar(stat="count")+
      labs(x = "Shark Species",
           y = "count",
           title = "Frequency of Attacks by Species")+
      scale_fill_manual(values = c("blue","red" ))+
      coord_flip()
    
    ggplotly(barchart) 
  })
  
  output$Attack_Type <- renderPlotly({
    Type_Barchart <-  df %>%
      filter(Type %in% c("Boating","Unprovoked","Provoked","Sea Disaster")) %>%
      group_by(Type) %>%
      mutate(count = n()) %>%
      ggplot(aes(fill = Type,x = reorder(Type,count)))+
      geom_bar(stat="count")+
      labs(x = "Cause Type of Shark Bites",
           y = "Accident Count",
           title = "Most Attacks are Unprovoked")+
      coord_flip()
    ggplotly(Type_Barchart) 
  })
  
  
  
  #####xiaomeng's part
  
  conditional <- function(condition, success) {
    if (condition) success else TRUE
  }
  
  shark_subset <- reactive({
    df %>%
      filter(
        conditional(input$year != "All year", Year == input$year),
        conditional(input$country != "All place", Country == input$country),
        conditional(input$species2 != "All species", Species == input$species2),
        conditional(input$activity != "All activity",Activity == input$activity)
      )
  })
  
  DataForMap <- eventReactive(
    input$Refresh,
    {
      FullDataSign = (input$year == "All year")&
        (input$country == "All place")&
        (input$species2 == "All species")&
        (input$activity == "All activity")
      
      
      if (FullDataSign)
      {
        df
      }
      else
      {
        shark_subset()
      }
      
    }
  )
  
  
  # Tab"Shark attack map"
  output$map2 <- renderLeaflet({
    
    pal <- colorFactor(palette = c("blue", "red"),domain = c("N", "Y"))
    # Fatal = c("Y","N")
    leaflet(DataForMap()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        #radius = ~ifelse(Type == "Provoked", 10, 5),
        color = ~pal(Fatal),
        label = ~Species,
        popup = ~Activity,
        radius = c(6),
        #color = ifelse(Fatal =="Y","red","blue"),
        stroke = FALSE, fillOpacity = 0.5
      )
  })
  
  output$table <- DT::renderDataTable({
    
    if(input$showtable)
      
      DT::datatable(shark_subset(), rownames = FALSE)
    
  })
  
  
  ###Tab Time
  filteredData3 <- reactive({
    if (input$species3 == "All sharks") {
      df
    } else {
      filter(df, Species == input$species3)
    }
  })
  
  filteredData4 <- reactive({
    filter(filteredData3(), Year >= input$range[1] & Year <= input$range[2])
  })
  
  output$year <- renderPlot({
    
    count_date <- count(filteredData4(),Year)
    
    p <- ggplot(count_date, aes(x=Year, y=n)) + 
      geom_line(color="black")+
      geom_point(shape=21, color="black", fill="#69b3a2", size=3)+
      labs(x = "Years", y = "# of Attacks")+
      theme_ipsum()
    
    p
    
  })
  
  output$bar <- renderPlot({
    count_date2 <- filteredData4() %>%
      group_by(Month) %>%
      summarise(count2=n())
    
    count_date2 <- count_date2 %>%
      mutate(Month = recode(Month, Sep = 'September', Jan = "January",  Feb ="February", 
                            Mar = "March", Apr = "April", Jun = "June", Jul= "July", 
                            Aug = "August", Oct = "October", Nov = "November", Dec = "December"
      ))
    
    q <- ggplot(count_date2, aes(x = reorder(Month, count2), count2, fill = count2)) + 
      geom_bar(stat="identity")+
      labs(x = "", y = "# of Attacks")+
      scale_fill_distiller("# of attacks",palette = "Spectral")
    
    q + coord_flip()
    
  }) 
  
  
}

shinyApp(ui, server)