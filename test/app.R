library(shiny)
# 'tigris' requires 'rgdal', which requires 'units' and 'sf'. Install this stuff first:
# sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev libudunits2-dev

library(leaflet)
library(tidyverse)
library(tigris)
library(RColorBrewer)
library(sf)

# options(tigris_use_cache = TRUE)
setwd("~/MSBA/shinyVTA/test")
# cache zip boundaries that are download via tigris package
options(tigris_use_cache = TRUE)
# Helper values and functions

# va_zipcodes <- c('20', '22', '23', '24')
# nvazips <- read.csv("nvazips.csv")
# smInv <- read.csv("VTA_Invoice_all 7.8.2021.csv")
# zipcodes <- tigris::zctas(starts_with = va_zipcodes)
# st_write(zipcodes, "zipcodes.shp")

zipcodes2=st_read("zipcodes.shp")
zipcode_totals <- read.csv("zipcodetotals.csv")

# 
# dput(zipcodes, file="out.txt")
# new <- source("out.txt")
# loaded=new[1]
# loadeddf <- as.data.frame(new)
# 
# # loaded = as.data.frame(loaded)
# loaded = setNames(loaded, c("ZCTA5CE10",  "GEOID10",    "CLASSFP10",  "MTFCC10",    "FUNCSTAT10", "ALAND10",    "AWATER10",   "INTPTLAT10", "INTPTLON10", "geometry" ))
# loadeddf2 <- loadeddf %>% select( ! "visible")
# loadeddf2= setNames(loaded, c("ZCTA5CE10",  "GEOID10",    "CLASSFP10",  "MTFCC10",    "FUNCSTAT10", "ALAND10",    "AWATER10",   "INTPTLAT10", "INTPTLON10", "geometry" ))

ui <- fluidPage(
    leafletOutput("mymap"),
    p()
    
)

server <- function(input, output, session) {
    
    
    output$mymap <- renderLeaflet({
        # zipcode_totals <- smInv %>% 
        #     mutate(Zip.Code = str_sub(Zip.Code, start=1, end=5)) %>% 
        #     group_by(Zip.Code) %>% 
        #     summarize(zipcode_totals = sum(Total.Line.Revenue),zipcode_profit = sum(Revenue.Less.Cost))
        
        # zipcodes <- new
        # Geojoin totals with zipcodes
        zipcodes_w_totals <- geo_join(zipcodes2, 
                                      zipcode_totals, 
                                      by_sp = "GEOID10", 
                                      by_df = "Zip.Code",
                                      how = "left")
        # Palette
        # bins <- c(0, 100, 200, 500, 1000, Inf)
        bins <- c(0, 1e3, 1e4, 1e5, 25e4, 50e4, 75e4, 1e6,2e6,Inf)
        pal <- colorBin("YlOrRd", domain = zipcode_totals$zipcode_totals, bins = bins)
        
        x=read.csv("../mapdata/vtalocations.csv")
        x
        # Leaflet map
        map <- zipcodes_w_totals %>% 
            leaflet %>% 
            # add base map
            addProviderTiles("CartoDB") %>% 
            # add zip codes
            addPolygons(fillColor = ~pal(zipcode_totals),
                        weight = 0.5,
                        opacity = 1,
                        color = "black",
                        dashArray = "1",
                        fillOpacity = 0.5,
                        popup = paste0("Zip: ",zipcodes_w_totals$GEOID10, "<br>", "Revenue: ",
                                       comma( round(zipcodes_w_totals$zipcode_totals,0),prefix = '$' )), 
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.4,
                                                     bringToFront = TRUE)) %>% 
            setView(lng = -77.8, lat = 38.7,  zoom = 8) %>% 
            # add legend
            addLegend(pal = pal, 
                      values = ~zipcode_totals, 
                      opacity = 0.7, 
                      title = "Sum Invoices by Zip",
                      position = "topright") %>% 
            addMarkers(lng = ~x$Long, lat = ~x$Lat, label = x$ï..Location)
        
        
        map
    })
}

shinyApp(ui, server)
