
# Project X CS 424 Spring 2021 UIC - Rabia Ibtasar
#FINAL VERSION
#To deploy 
#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')
#libraries to include



library(shiny)
library(shinydashboard,warn.conflicts = FALSE)
library(ggplot2)
# library(RColorBrewer)
# library(dplyr)
library(DT)
# library(gridExtra)
library(leaflet)
# library(sf)
 library(leaflet.extras)
library(tidyverse)
# library(scales)
# library(rgdal)
library(sp)
library(countrycode)


#set the correct wd
#setwd('C:/Users/ribta/OneDrive/Desktop/UIC/Courses/Spring 2021/CS424/project3/cs424p3')

set.seed(122)


#read the excel data into d
d1<-read.csv('Global_data.csv')
d1$continent <- countrycode(sourcevar = d1[, "country_long"],
                            origin = "country.name",
                            destination = "continent")

d1 <- within(d1, {
       f <- country_long == 'Antarctica'
       continent[f] <- 'Antarctica'
   }) 


#subset(d1,d1$country_long=="Antarctica")

#north<-subset(d1,d1$continent=="North America")

selections1<-c("Asia","Europe","Africa","Antarctica","Americas","Oceania" )

sources<-c("Hydro","Gas","Other","Oil",
               "Wind","Nuclear" ,"Coal","Solar",
               "Waste", "Biomass","Wave and Tidal",
               "Petcoke", "Geothermal","Cogeneration","Storage")
selections2<-c(sources, "All")

pal <- colorFactor(c("#a6cee3", "#1f78b4", "#b2df8a", 
                     "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", 
                     "#ff7f00", "#cab2d6", "#6a3d9a","#8DD3C7",
                     "#FDB462","#FB8072","#80B1D3","#FCCDE5"),sources)

#===========
#start of UI
#===========

#create the sidebar menu for dashboard

sidebar <-dashboardSidebar (disable = FALSE, collapsed = FALSE,
                            
                            sidebarMenu(
                              menuItem("Continents", tabName = "cont", icon =NULL),
                              menuItem("About", tabName = "about", icon = NULL)
                            )
)

body<-dashboardBody(
  tabItems(
    tabItem(tabName = "cont",
            #first row
            fluidRow(
              column(2, checkboxGroupInput("input2", "Select fuel type:", selections2, selected="All"),),
              column(width=8,
                     tabBox(title = "Continent View", id="tabset1", selected = "Map", width = NULL,               
                            tabPanel("Map", leafletOutput("map1", width = "100%", height = 400))
                            )
                   
                     
              ),
              column(width=2,
                     #        box(
                     #          title = "Map1", status = "primary", width = 12,
                     #          collapsible = TRUE, collapsed = TRUE, 
                     selectInput("input1", "Select option:", selections1, selected="Americas"),
                     
                     sliderInput("input3","Select capacity range of Power plants", 
                                 min=0, max=max(d1$capacity_mw), 
                                 value= c(0, max(d1$capacity_mw)),
                                 post="MWh")
                     #),
              )
            )
            
    ),
    
  #About TAB
    tabItem(tabName = "about",
            h2("About"),
            mainPanel(
              h4("Thanks for visiting the App."),
              h4("Data Source:The original data is available from  
                https://datasets.wri.org/dataset/globalpowerplantdatabase"),
              
              h5("App developer: Rabia Ibtasar"),
              
              h5("What is this application for?"),
              h5(" 
                      This interactive application has been designed to allow visitors to explore electrical energy production globally.
                      It allows users to make selections by Continents to view all the energy plants in that region. Users can filter
                      by fuel type to make comparisons across energy sources and also filter out plants by capacity. This is useful for anyone 
                      interested in understanding energy production on a global scale.
                      ")
              
            )
    )
  )
)

# Create the shiny dashboard
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "CS 424 Project X Comparing Energy Usage Globally"), sidebar, body
)

#========================
#START OF SERVER FUNCTION
#========================

server <- function(input, output,session) {
  
  
  
  #setup reactives for the first drop down 
  
  option1Reactive <- reactive({

    if(input$input1=="Antartica"){
      t<-subset(d1,d1$continent=="Antartica")
      t

    }
    else if(input$input1=="Europe"){
      t<-subset(d1,d1$continent=="Europe")
      t
    }
   else if(input$input1=="Africa"){
     t<-subset(d1,d1$continent=="Africa")
     t

   }
    else if(input$input1=="Asia"){

      t<-subset(d1,d1$continent=="Asia")
      t
    }
    else if(input$input1=="Oceania"){
      t<-subset(d1,d1$continent=="Oceania")
      t
    }
    else if(input$input1=="Americas"){
      t<-subset(d1,d1$continent=="Americas")
      t
    }
    else{
      t<-d1
      t
    }
    
  })
  
  
  sourceReactive <- reactive({
    
    s<-option1Reactive()
    s<-s[s$capacity_mw >= input$input3[1] & s$capacity_mw <= input$input3[2],]
    
    #return all the plants in the continent selected
    if("All" %in% input$input2){
      
     # 
      s
      
      }
    else{
      #filter for plants by making sure you select them if they match any of the
      #checkboxes for sources selected
      
      s %>% filter((s$primary_fuel %in% input$input2))
      }
    
  })
  
  #########################################################################
  #output for TAB1 
  #########################################################################
  
  
  
  output$map1 <- renderLeaflet({
    
    t=sourceReactive()
    
    # handles case where no selections are made and rows are empty and shows the default
    #world map
    
    if(is.null(input$input2) | nrow(t) == 0) {
      
        leaflet(data=t) %>% addProviderTiles(providers$CartoDB.Positron) %>%
        #setView(lng = 54.5260, lat = 105.2551, zoom = 7) %>%
        #creates a JS button on the leaflet map
        addEasyButton(easyButton(icon = "fas fa-compress", title = "Reset View",
                                 onClick = JS("function(btn, map){ map.setView([40,-89], 7);}")))
      
    }
    else{
  
      leaflet(data=t,width = "50%") %>%
      addTiles() %>% # Add default OpenStreetMap map tiles
      addResetMapButton()%>%
      addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # Layers control
      addLayersControl(
      baseGroups = c("OSM (default)", "Topo", "Toner Lite"),
      options = layersControlOptions(collapsed = FALSE)) %>%
      setView(lng = mean(t$longitude), lat = mean(t$latitude), zoom = 3) %>%
      addCircleMarkers(lng=~longitude, lat=~latitude, color=~pal(t$primary_fuel),
      radius =~ifelse(t$capacity_mw  < mean (t$capacity_mw),1,3),
      popup = paste("<b>","Country Name:", t$country_long,
                                     "<br>Plant Name",t$name,
                                     "<br>Plant Capacity:",t$capacity_mw,
                                     "<br>Plant fuel:",t$primary_fuel,"</b>"), 
              fillOpacity = 0.5) %>%
       addLegend(position = "bottomright",pal=pal, values=~sources,
       title = "Energy Sources")
    }
  })
  
}

shinyApp(ui = ui, server = server)
