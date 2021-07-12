library(shiny)
# 'tigris' requires 'rgdal', which requires 'units' and 'sf'. Install this stuff first:
# sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev libudunits2-dev

library(leaflet)
# library(plyr)
library(tidyverse)
library(tigris)
library(RColorBrewer)
library(sf)
library(scales)
library(ggthemes)

# options(tigris_use_cache = TRUE)
setwd("~/MSBA/shinyVTA/test")
# cache zip boundaries that are download via tigris package
options(tigris_use_cache = TRUE)
# Helper values and functions
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
# va_zipcodes <- c('20', '22', '23', '24')
# nvazips <- read.csv("nvazips.csv")
# smInv <- read.csv("VTA_Invoice_all 7.8.2021.csv")
# zipcodes <- tigris::zctas(starts_with = va_zipcodes)
# Saves for reading in (in sshiny app)
# st_write(zipcodes, "zipcodes.shp")
input=tibble(age=2010,
             marketing=76,
             margin=64,
             discount=10,
             dates=c("2018-01-01", "2020-12-31")
)
# NEED
invoice<- read.csv("../data/VTA_Invoice_all 7.8.2021.csv")
vtaLocations=read.csv("../mapdata/vtalocations.csv")

zipcodes2=st_read("zipcodes.shp")
zipcode_totals <- read.csv("zipcodetotals.csv")
invDates=invoice %>% select(INVOICE_DATE) %>% summarise(earliestINV=min(INVOICE_DATE,na.rm=TRUE), 
                                                        latestINV=max(INVOICE_DATE,na.rm=TRUE))

invoice <- invoice %>%  mutate(Zip.Code = str_sub(Zip.Code, start=1, end=5))
# format(invoice$Zip.Code, digits = 0)

invoice$Zip.Code <- as.factor(invoice$Zip.Code)


# missing=invoice %>% filter(nchar(as.character(Zip.Code)) == 5) %>% nrow()
# total=invoice %>% nrow()

# per=label_percent()(missing/total)

ui <- fluidPage(
    textOutput("missing"),
    # p("Missing", paste0(as.character(per)), "Zipcodes!"),
    dateRangeInput("dates",'Invoice Date Range (CLV)', min=invDates$earliestINV, max=invDates$latestINV,
                   start=invDates$earliestINV, end=invDates$latestINV),
    leafletOutput("mymap"),
    br(),
    plotOutput("bar"),
    
    p()
    
)

server <- function(input, output, session) {

    output$missing <- renderText({
        totals <- invoice %>% filter(INVOICE_DATE>= input$dates[1] &
                                     INVOICE_DATE<= input$dates[2])
        total <- totals %>% nrow()
        missing <- totals %>%  filter(nchar(as.character(Zip.Code)) == 5) %>% nrow()
        per=label_percent()(missing/total)
        # paste(per, "of ", total, "invoices are missing zipcode data")
        paste(per, "of", label_comma()(total), "invoices are missing zipcode data")
        
    })
    output$mymap <- renderLeaflet({
        zipcode_totals <- invoice %>% filter(INVOICE_DATE>= input$dates[1] &
                                             INVOICE_DATE<= input$dates[2]) %>% 
            group_by(Zip.Code) %>% summarise(Revenue=sum(Total.Line.Revenue))
        
        # Geojoin totals with zipcodes
        zipcodes_w_totals <- geo_join(zipcodes2, 
                                      zipcode_totals, 
                                      by_sp = "GEOID10", 
                                      by_df = "Zip.Code",
                                      how = "left")

        themax=max(zipcodes_w_totals$Revenue, na.rm = TRUE)
        bins <- c(0, 
                  # round(themax*.20, 0),
                  # round(themax*.30, 0),
                  round_any(themax*.10, 100000, floor),
                  round_any(themax*.20, 100000),
                  round_any(themax*.30, 100000),
                  round_any(themax*.40, 100000),
                  round_any(themax*.60, 100000),
                  round_any(themax*.80, 100000),
                  round_any(themax+100000, 100000))
        
        bins <- bins %>% unique()
        
        pal <- colorBin("YlOrRd", domain = zipcode_totals$Revenue, bins = bins)
        
        
        # Leaflet map
        map <- zipcodes_w_totals %>% 
            leaflet %>%
            # add base map
            addProviderTiles("CartoDB") %>% 
            # add zip codes
            addPolygons(fillColor = ~pal(Revenue),
                        weight = 0.5,
                        opacity = 1,
                        color = "black",
                        dashArray = "1",
                        fillOpacity = 0.5,
                        popup = paste0("Zip: ",zipcodes_w_totals$GEOID10, "<br>", "Revenue: ",
                                       comma( round(zipcodes_w_totals$Revenue,0),prefix = '$' )), 
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.4,
                                                     bringToFront = TRUE)) %>% 
            setView(lng = -77.5, lat = 38.7,  zoom = 9) %>% 
            # add legend
            addLegend(pal = pal, 
                      values = ~Revenue, 
                      opacity = 0.7, 
                      title = "Invoices by Zip",
                      position = "topright") %>% 
            addMarkers(lng = ~vtaLocations$Long, lat = ~vtaLocations$Lat, label = vtaLocations$ï..Location)
        
        
        map
    })
    
    output$bar <- renderPlot({
        p <- invoice %>% 
            group_by(Store.ID) %>% 
            summarise(Revenue=sum(Total.Line.Revenue), Profit=sum(Revenue.Less.Cost)) %>% 
            ggplot() + geom_col(aes(x=reorder(Store.ID,Revenue), y=Revenue)) +
            coord_flip() +
            labs(x="", title=paste("Store Revenues from", input$dates[1], "to", input$dates[2])) +
            theme_minimal() +
            scale_y_continuous(labels = label_comma(prefix = '$'), breaks=pretty_breaks())
        
        p
    })
}

shinyApp(ui, server)
