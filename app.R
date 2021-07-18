library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(networkD3)
library(lubridate)
library(tidyverse)
library(scales)
library(ggthemes)
library(RColorBrewer)
library(sf)
library(leaflet)
library(fontawesome)
library(tigris)
palette="Dark2"
setwd("~/MSBA/shinyVTA")

####invoice<- read.csv("data/VTA_Invoice_all 7.8.2021.csv")
#### zipcodes2=st_read("mapdata/zipcodes.shp") Entire State

#Good data
invoice <- read.csv("data/VTA_Invoice_all 7.17.2021_wCityCountyMycar.csv")
vtaLocations=read.csv("mapdata/vtalocations.csv")
zipcodes2=st_read("mapdata/zipcodesSmall.shp")

# input=tibble(age=2010,
#              marketing=76,
#              margin=64,
#              discount=10,
#              dates=c("2018-01-01", "2018-12-31"),
#              datesMap=c("2018-01-01", "2018-12-31")
#              )

invoice$CUST_CREATE_DATE <- lubridate::ymd(invoice$CUST_CREATE_DATE)
dates=invoice %>% select(CUST_CREATE_DATE) %>% summarise(EarliestCust=min(CUST_CREATE_DATE,na.rm=TRUE), LatestCust=max(CUST_CREATE_DATE,na.rm=TRUE))
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

My_Theme = theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))


invDates=invoice %>% select(INVOICE_DATE) %>% summarise(earliestINV=min(INVOICE_DATE,na.rm=TRUE), 
                                                        latestINV=max(INVOICE_DATE,na.rm=TRUE))

invoice <- invoice %>%  mutate(Zip.Code = str_sub(Zip.Code, start=1, end=5))

invoice$Zip.Code <- as.factor(invoice$Zip.Code)

ui=navbarPage(title="Virginia Tire and Auto", theme = shinytheme("flatly"),
fluid = TRUE,
collapsible = TRUE,
navbarMenu("CLV",
tabPanel("Vehicle Age",
     sidebarLayout(sidebarPanel(icon("dashboard"),width = 3,
         sliderInput("age",
                     "Vehicle Age Segment:",
                     min = 1995,
                     max = 2020,
                     value = 2010,
                     sep = ""),
         h3("CLV Inputs"),
         numericInput('marketing', 'Marketing per Customer/year ($)', 76, min = 1, max = 9),
         numericInput('margin', 'Sales Margin - Parts&Labor (%)', 64, min = 1, max = 99),
         numericInput('discount', 'Discount Rate (%)', 10, min = 1, max = 50),
         dateRangeInput("dates",'Customer Creation Date Range (CLV)', min=dates$EarliestCust, max=dates$LatestCust,
                        start="2018-01-01", end="2018-12-31"),
                 hr()
         ),
        mainPanel(
            # tags$style(".small-box.bg-yellow {  border-radius: 10px; background-color: #33B44A !important; color: #000000 !important; }"),
        fluidRow(
            # header = tagList(
                useShinydashboard(),
            # ),
             valueBoxOutput("value1")
             ,valueBoxOutput("value2")
             ,valueBoxOutput("value3")
         ),
         fluidRow(
             box(
                 title = "Average Revenue per Visit"
                 ,status = "primary"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,plotOutput("two_L")
             )
             ,box(
                 title = "CLV per Customer Segment"
                 ,status = "primary"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,plotOutput("two_R")
             )
         ),
        hr(),
        br(),
         # fluidRow(
             column(12,
                    # box(width = 12,
                        title="Customer Return Rate by Segment",
                        sankeyNetworkOutput("three_L", width = "100%")))
         # )
        # )# End main panel
        )# End sidebar panel
),
tabPanel("MyCar Member",
         # fluidRow(
         h3("Coming Soon")
)   
,
tabPanel("City",
         # fluidRow(
         h3("Coming Soon")
)   
),# End Tab 1
#################### TAB 2 ###########################
navbarMenu(title="Data Viz",
    tabPanel("Customer Data",
             # h1("Hello"),
             
        fluidRow(
            fluidRow(
            # splitLayout(cellWidths = c("50%", "50%"),
            box(
                title="Number of Visits / Lifetime Revenue by NPS Score",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("custPlot1")
            ),
            box(
                title="Top 10 Lifetime Revenue Makes",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("custPlot2")
            )
            )
        ),
        # hr(),
        # br(),
        fluidRow(
            box(
                title="3-Year Visit History for 2018 new Customers",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("Retention")
            ),
            box(
                title="Number of Vehicles per Customer",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("Vehicles")
            )
            
        )
        
        # column(width = 8, offset=2,
        #     plotOutput("revenue")
        # )
    ),
    tabPanel("Historical Trends",
             # h1("Margin Data"),
             fluidRow(
             box(
                 title="2018-2020 Historical Margins",
                 status = "primary",
                 solidHeader = TRUE,
                 plotOutput("margins")
             ),
             box(
                 title="New Customers/Year",
                 status = "primary",
                 solidHeader = TRUE,
                 plotOutput("customers")
             )
             ),
             fluidRow(
                 box(
                     title="2018-2020 Service Margins",
                     status = "primary",
                     solidHeader = TRUE,
                     plotOutput("services")
                     
                 ),
                 box(
                     title="2018-2020 Tire Margins",
                     status = "primary",
                     solidHeader = TRUE,
                     plotOutput("tires")
                 )
             )
             )
),
#################### TAB 3 ###########################
tabPanel("Map",
         fluidRow(
             textOutput("missing"),
             # p("Missing", paste0(as.character(per)), "Zipcodes!"),
             column( width=2, offset = 1,
             dateRangeInput("datesMap",'Invoice Date Range', min=invDates$earliestINV, max=invDates$latestINV,
                            start="2018-12-01", end="2018-12-31"),
             ),
             column(width = 8,
                    leafletOutput("mymap"),
                    br()
             )
         ),
        fluidRow(
         box(#width=6,
             title="2018-2020 Service Margins",
             status = "primary",
             solidHeader = TRUE,
                plotOutput("bar") 
                
                )),
         # box(#width=6,
         #     title="2018-2020 Service CLV",
         #     status = "primary",
         #     solidHeader = TRUE,
         #     plotOutput("bar") 
         #     
         # )),
         fluidRow(
            p("Map is being drawn, please wait...")
         )
        
         )# End main panel
         # )# End sidebar panel
    
    )





#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################

total.revenue <- as.numeric(round(invoice %>% summarise(sum(Total.Line.Revenue)), 2))
total.customers <- as.numeric(round(invoice %>% group_by(CUSTOMER_NUMBER) %>% select(CUSTOMER_NUMBER) %>% unique() %>% nrow(), 2))
customer.2018 <- as.numeric(round(invoice %>% filter(createYear == 2018) %>% group_by(CUSTOMER_NUMBER) %>% select(CUSTOMER_NUMBER) %>% unique() %>% nrow(), 2))



# create the server functions for the dashboard  
server <- function(input, output) { 
    #some data manipulation to derive the values of KPI boxes
    print("1")
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
        valueBox(
            paste0('$',formatC(as.numeric(total.revenue/1000000)),"")
            ,'MM Total Reveneue'
            ,icon = icon("money-bill-wave",lib='font-awesome')
            ,color = "green")  
    })
    output$value2 <- renderValueBox({ 
        valueBox(
            formatC(as.numeric(total.customers), format="f", big.mark=',', drop0trailing = TRUE)
            ,'Total Unique Customers'
            ,icon = icon("oil-can",lib='font-awesome')
            ,color = "olive")  
    })
    output$value3 <- renderValueBox({
        valueBox(
            formatC(customer.2018, format="f", big.mark=',', drop0trailing = TRUE)
            ,'New Customers in 2018'
            ,icon = icon("smile",lib='font-awesome')
            ,color = "purple")   
    })
    #creating the plotOutput content
    print("2")
    output$two_L <- renderPlot({
        vehage <- invoice %>%
            mutate(factor=case_when(
                CAR_YEAR>=input$age ~ paste0(input$age,' or Newer'),
                TRUE ~ paste0('Older than ',input$age)
            )) %>% group_by(factor) %>%  summarise(AverageInvoice=mean(Total.Line.Revenue))
        
        vehage %>% ggplot(aes(x=factor, y=AverageInvoice, fill=factor)) +
            geom_col() +
            geom_text(nudge_y = 5,aes(x=factor, y=AverageInvoice, label=paste0(round(AverageInvoice,0),'$'))) +
            scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                                    prefix = "$", suffix = "",
                                                    big.mark = ",", decimal.mark = ".")) +
            labs(x="", title="Average Revenue Per Visit") +
            theme_tufte()+ scale_fill_brewer(palette = palette) + My_Theme
    })
    # scaleFUN <- function(x) sprintf("%.2f", x)
    output$two_R <- renderPlot({
        print("3")
        start=lubridate::ymd(input$dates[1])
        end=lubridate::ymd(input$dates[2])
        CLVInvoice=invoice %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end)
        # print(input$dates)
        # print(input$age)
        
        vehage <- CLVInvoice %>% 
            mutate(factor=case_when(
                CAR_YEAR>=input$age ~ paste0(input$age, ' or Newer'),
                TRUE ~ paste0('Older than ',input$age)
            )) %>% filter(visit<=6)
        
        marketingpp=as.numeric(input$marketing/2)
        discount=1+(as.numeric(input$discount)*.01)
        marginPercent=(100-as.numeric(input$margin))*.01
        
        print(marketingpp)
        print(discount)
        print(marginPercent)
        vehage %>% group_by(factor, visit) %>% summarise(AvgVisitRev=mean(Total.Line.Revenue), customers=n()) %>%  arrange(desc(visit))
        
        clvAge <- vehage %>% group_by(factor, visit) %>% summarise(AvgVisitRev=mean(Total.Line.Revenue), customers=n()) %>% 
            mutate(Attrition=customers/lag(customers)) %>% mutate(Attrition=case_when(is.na(Attrition)~1, TRUE ~ Attrition)) %>% 
            mutate(AttrMult=Attrition*lag(Attrition)) %>% 
            mutate(AttrMult=case_when(is.na(AttrMult)~1, TRUE ~ AttrMult)) %>% 
            rowwise() %>% mutate(num=AvgVisitRev-(AvgVisitRev*marginPercent)-marketingpp)%>% 
            rowwise() %>%  mutate(denominator=case_when(
                visit==1 ~ 1,
                visit==2 ~ discount,
                visit==3 ~ discount^2,
                visit==4 ~ discount^3,
                visit==5 ~ discount^4,
                visit==6 ~ discount^5)) %>% 
            rowwise() %>% mutate(line=AttrMult*(num/denominator))
        
        clvAge <-clvAge %>% group_by(factor) %>% mutate(CLV=sum(line))
        
        p <- clvAge %>% group_by(factor) %>% summarise(CLV=CLV) %>% unique() 
        
        ggplot(p, aes(x=factor, y=CLV, fill=factor)) + geom_col() +
            scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                                    prefix = "$", suffix = "",
                                                    big.mark = ",", decimal.mark = ".")) +
            geom_text(nudge_y = 5,aes(x=factor, y=CLV, label=paste0(round(CLV,0),'$'))) +
            labs(x="", title="", y="USD") + 
            theme_tufte() + scale_fill_brewer(palette = palette) + My_Theme
    })
    output$three_L <- renderSankeyNetwork({
        start=lubridate::ymd(input$dates[1])
        end=lubridate::ymd(input$dates[2])
        CLVInvoice=invoice %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end)
        factor1Sankey <- CLVInvoice %>%  filter(CAR_YEAR >= input$age) %>%
            group_by(visit) %>%
            summarise(visits=n()) %>%
            filter(visit<=6) %>% mutate( group="factor1Sankey") %>%
            arrange(desc(visits))  %>% mutate(per=paste0(round(visits/lag(visits)*100),'%'))
        # rowwise() %>% mutate(per=paste0(visits,"\n",per))
        
        factor2Sankey  <- CLVInvoice %>% filter(CAR_YEAR<input$age) %>%
            group_by(visit) %>%
            summarise(visits=n()) %>%
            filter(visit<=6) %>% mutate(group="factor2Sankey") %>%
            arrange(desc(visits))  %>% mutate(per=paste0(round(visits/lag(visits)*100),'%'))
        # rowwise() %>% mutate(per=paste0(visits,"\n",per))
        
        
        
        all <- rbind(factor1Sankey,factor2Sankey)
        
        all <-  all %>% rowwise() %>% mutate(source=case_when(
            visit== 1 ~ "2018Customers",
            visit== 2 & group=="factor1Sankey" ~ "Visit~1",
            visit== 2 & group=="factor2Sankey" ~ "Visit-1",
            visit== 3 & group=="factor1Sankey" ~ "Visit~2",
            visit== 3 & group=="factor2Sankey" ~ "Visit-2",
            visit== 4 & group=="factor1Sankey" ~ "Visit~3",
            visit== 4 & group=="factor2Sankey" ~ "Visit-3",
            visit== 5 & group=="factor1Sankey" ~ "Visit~4",
            visit== 5 & group=="factor2Sankey" ~ "Visit-4",
            visit== 6 & group=="factor1Sankey" ~ "Visit~5",
            visit== 6 & group=="factor2Sankey" ~ "Visit-5",
            TRUE ~ "GFY")) %>%
            mutate(target=case_when(
                source== "2018Customers" & group=="factor2Sankey" ~ "Visit-1",
                source== "2018Customers" & group=="factor1Sankey" ~ "Visit~1",
                source== "Visit-1" ~ "Visit-2",
                source== "Visit~1" ~ "Visit~2",
                source== "Visit-2" ~ "Visit-3",
                source== "Visit~2" ~ "Visit~3",
                source== "Visit-3" ~ "Visit-4",
                source== "Visit~3" ~ "Visit~4",
                source== "Visit-4" ~ "Visit-5",
                source== "Visit~4" ~ "Visit~5",
                source== "Visit-5" ~ "Visit-6",
                source== "Visit~5" ~ "Visit-6",
                TRUE ~ "GFY"))
        # .domain(['2018Customers','Visit-1','Visit-2','Visit-3','Visit-4','Visit-5','Visit-6','Visit~1','Visit~2','Visit~3','Visit~4','Visit~5'])
        # .range(['#1b9e77','#000','#000','#1b9e77','#1b9e77','#1b9e77','#d95f02','#d95f02','#d95f02','#d95f02','#d95f02','#000']);"

        color_scale <- 
            "d3.scaleOrdinal()
        .range(['#808080','#1b9e77','#d95f02']);"
        
        all$source= as.factor(all$source)
        all$target= as.factor(all$target)
        all
        
        
        
        nodes <- data.frame(
            name=c(as.character(all$source),
                   as.character(all$target)) %>% unique()
        )
        
        
        nodes
        nodes$group <- as.factor(c("a","b","b","b","b","b","c","c","c","c","c","d"))
        
        # library(networkD3)
        
        match(all$source, nodes$name)
        all$IDsource <- match(all$source, nodes$name)-1
        all$IDtarget <- match(all$target, nodes$name)-1
        fontSize <- 16
        nodeWidth <- 40
        fontFamily <- "sans-serif"
        # Make the Network
        # HTML(h3("This is my app!"),'<br/>')
        
        p <- sankeyNetwork(Links = all, Nodes = nodes,
                           Source = "IDsource", Target = "IDtarget",
                           Value = "visits", NodeID = "name", NodeGroup = 'group', width=1000,
                           sinksRight=FALSE, fontSize = fontSize, fontFamily = fontFamily, nodeWidth =nodeWidth, nodePadding = 40,
                           colourScale = color_scale)
    p
    })
    output$missing <- renderText({
        totals <- invoice %>% filter(INVOICE_DATE>= input$datesMap[1] &
                                         INVOICE_DATE<= input$datesMap[2])
        total <- totals %>% nrow()
        missing <- totals %>%  filter(nchar(as.character(Zip.Code)) == 5) %>% nrow()
        per=label_percent()(missing/total)
        # paste(per, "of ", total, "invoices are missing zipcode data")
        paste(per, "of", label_comma()(total), "invoices are missing zipcode data for this time period")
        
    })
    output$mymap <- renderLeaflet({
        zipcode_totals <- invoice %>% filter(INVOICE_DATE>= input$datesMap[1] &
                                                 INVOICE_DATE<= input$datesMap[2]) %>% 
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
                  round_any(themax*.10, 50000, floor),
                  round_any(themax*.20, 50000),
                  round_any(themax*.30, 50000),
                  round_any(themax*.40, 50000),
                  round_any(themax*.60, 50000),
                  round_any(themax*.80, 50000),
                  round_any(themax*1.1, 50000))
        
        bins <- unique(bins)
        
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
            addMarkers(lng = ~vtaLocations$Long, lat = ~vtaLocations$Lat, label = vtaLocations$Location)
        
        
        map
    })
    
    output$bar <- renderPlot({
        
        
        p <- invoice %>%
            filter(INVOICE_DATE>= input$datesMap[1] &
                        INVOICE_DATE<= input$datesMap[2]) %>% 
            group_by(Store.ID) %>% 
            summarise(Revenue=sum(Total.Line.Revenue), Profit=sum(Revenue.Less.Cost)) %>% 
            ggplot() + geom_col(aes(x=reorder(Store.ID,Revenue), y=Revenue)) +
            coord_flip() +
            labs(x="", title=paste("Store Revenues from", input$datesMap[1], "to", input$datesMap[2])) +
            theme_minimal() +
            scale_y_continuous(labels = label_comma(prefix = '$'), breaks=pretty_breaks())+ My_Theme
        
        p
    })
 



output$custPlot1 <- renderPlot({
    print("I'm here in 1")
    # df = invoice %>% filter(! is.na(NPS)) %>% group_by(NPS) %>% 
    #     summarise(Rev = mean(Total.Line.Revenue)) 
    
    df = invoice %>% filter(! is.na(NPS)) %>% group_by(CUSTOMER_NUMBER) %>% 
        summarise(visits = n(), custRevenue = sum(Total.Line.Revenue), NPS=max(NPS)) %>% 
        group_by(NPS) %>% summarise(meanVisists=mean(visits), meanLifetimeRev=mean(custRevenue))
    
    df$NPS <- as.factor(df$NPS)
    
    ggplot(df, aes(x=NPS, y=meanVisists, fill= meanLifetimeRev)) +
        geom_col() +
        scale_y_continuous(labels = label_comma()) +
        # scale_x_discrete(breaks=pretty_breaks(12)) +
        theme_tufte() +
        scale_fill_viridis_c() +
        labs(y="Average Visits per Customer", title="", fill="Mean Lifetime Rev ($)") +
        guides(title = "Mean Lifetime Rev per NPS") + My_Theme
    # p
})

output$custPlot2 <- renderPlot({
    print("I'm here in 2")
    df = invoice  %>% group_by(BrandSum) %>% mutate(countBrand=n()) %>% filter(countBrand>5000) %>% ungroup %>% 
        group_by(CUSTOMER_NUMBER, BrandSum) %>% 
        summarise(LifetimeRev = sum(Total.Line.Revenue), LifetimeVisits=n()) %>%
        group_by(BrandSum) %>% summarise(AvgLifetimeRev=mean(LifetimeRev), AvgVisits=mean(LifetimeVisits)) %>% 
        arrange(desc(AvgLifetimeRev)) %>% slice_max(AvgLifetimeRev, n=10) 
    ggplot(df, aes(reorder(x=BrandSum, -AvgLifetimeRev), y=AvgLifetimeRev, fill=AvgVisits)) +
        geom_col() +
        scale_y_continuous(labels = label_comma(prefix = "$")) +
        scale_fill_viridis_c() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        # scale_x_continuous(breaks=pretty_breaks(9)) +
        labs(y="Average Invoice Amount",x="", title="", x="", fill="Avg Visits",
             caption = "Only Makes with >5000 Customers")  + My_Theme
    # p
})

output$margins <- renderPlot({
    marginByYr=read.csv("data/marginByYear.csv")
    # z
    # marginByYr=invoice %>%  group_by(InvoiceYear) %>% 
    #     summarise(Rev=sum(Total.Line.Revenue), Profit = sum(Revenue.Less.Cost), Cost=sum(Line.Cost))
    
    # marginByYr$margin = marginByYr$Profit/marginByYr$Rev
    # marginByYr$margin = round((marginByYr$Rev - marginByYr$Cost)/marginByYr$Rev,3)
    marginByYr$InvoiceYear = as.factor(marginByYr$InvoiceYear)
    marginByYr
    ggplot(marginByYr, aes(x=InvoiceYear, y=Rev, fill=InvoiceYear)) + geom_col(alpha=.4) +
        geom_col(data=marginByYr, aes(x=InvoiceYear, y=Profit, fill=InvoiceYear)) +
        scale_fill_brewer(palette = "Dark2", labels = c("2018 Profit", "2019 Profit", "2020 Profit")) +
        scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                                prefix = "$", suffix = "mm",
                                                big.mark = ",")) +
        geom_text(data=marginByYr, 
                  aes(x=InvoiceYear, y=Rev, label=paste0(round(margin*100,1),'%')), nudge_y = 2000000) +
        labs(x="",y="Revenue",fill="", title="", caption = "label=margin%") + theme_minimal() + My_Theme
    # p
})


output$services <- renderPlot({
    # marginByClass=read.csv("data/marginbyclass.csv")
    # serviceMargins<- marginByClass %>%  filter( str_detect(CLASS_Desc,"Tire|tire", negate=TRUE))
    # serviceMargins <- serviceMargins %>% select(CLASS_Desc, Rev, Profit, margin)
    # write.csv(serviceMargins, 'serviceMargins.csv')
    serviceMargins <- read.csv("data/serviceMargins.csv")
    serviceMargins <- serviceMargins %>% pivot_longer(c("Rev", "Profit"), names_to="Metric")
    serviceMarginsRev <- serviceMargins %>% filter(Metric=="Rev")
    serviceMarginsRev
    serviceMarginsPro <- serviceMargins %>% filter(Metric=="Profit")
    serviceMarginsPro$Metric[serviceMarginsPro$value<0] = "Loss"

    serviceMarginsPro <- serviceMarginsPro %>% filter(value>0)
    serviceMarginsRev <- serviceMarginsRev %>% filter(value>0)
    
    
    ggplot(serviceMarginsRev, aes(x=reorder(CLASS_Desc, margin), y=value, fill=Metric)) + geom_col() +
        geom_col(data=serviceMarginsPro, aes(x=CLASS_Desc, y=value, fill=Metric), show.legend = FALSE) + coord_flip() +
        scale_fill_manual(values = c("green4","grey76")) +
        scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                                prefix = "$", suffix = "mm",
                                                big.mark = ",")) +
        geom_text(data=serviceMarginsRev,
                  aes(x=CLASS_Desc, y=value, label=paste0(round(margin*100,1),'%')), nudge_y = 1000000) +
        labs(x="",y="Revenue",fill="", title="",
             legend="test") + theme(legend.box = 'horizontal') + theme_minimal() + My_Theme
    
})

output$tires <- renderPlot({
    # marginByClass=read.csv("data/marginbyclass.csv")
    # tireMargins <- marginByClass %>%  filter(str_detect(CLASS_Desc,"Tire|tire"))
    # tireMargins <- tireMargins %>% select(CLASS_Desc, Rev, Profit, margin)
    # write.csv(tireMargins, 'tireMargins.csv')
    tireMargin <- read.csv("data/tireMargins.csv")
    tireMargin <- tireMargin %>% pivot_longer(c("Rev", "Profit"), names_to="Metric")
    tireMarginsRev <- tireMargin %>% filter(Metric=="Rev")
    tireMarginsPro <- tireMargin %>% filter(Metric=="Profit")
    tireMarginsPro$Metric[tireMarginsPro$value<0] = "Coupon"
    
    tireMarginsPro <- tireMarginsPro %>% filter(value>0)
    tireMarginsRev <- tireMarginsRev %>% filter(value>0)
    
    ggplot(tireMarginsRev, aes(x=reorder(CLASS_Desc, margin), y=value, fill=Metric)) + geom_col() +
        geom_col(data=tireMarginsPro, aes(x=CLASS_Desc, y=value, fill=Metric), show.legend = FALSE) + coord_flip() +
        scale_fill_manual(values = c("green4","grey76")) +
        scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                                prefix = "$", suffix = "mm",
                                                big.mark = ",")) +
        geom_text(data=tireMarginsRev,
                  aes(x=CLASS_Desc, y=value, label=paste0(round(margin*100,1),'%')), nudge_y = 500000) +
        labs(x="",y="Revenue",fill="", title="",
             legend="test") + theme(legend.box = 'horizontal')+ theme_minimal() + My_Theme
})


output$Retention <- renderPlot({
    invoice %>% filter(createYear==2018) %>% 
        group_by(visit) %>% summarise(count=n(), AvgProfit=mean(Revenue.Less.Cost)) %>% 
        filter(as.numeric(visit)<=10 ) %>% 
        mutate(dropoff = count/lag(count)) %>%
        mutate(dropoff = case_when(is.na(dropoff)~1, TRUE ~ dropoff)) %>%
        ggplot(aes(x=as.factor(visit), y=count, fill=AvgProfit)) +
        geom_text(nudge_y = 1000,aes(x=as.factor(visit),y=count,label=paste0(round(dropoff*100,0),"%"))) +
        # facet_wrap(~MyCarClub) +
        geom_col() + theme_minimal() + scale_y_continuous(labels = label_comma(), breaks = pretty_breaks()) +
        labs(title="", x="Visit Number", y="# of Customers") + 
        scale_fill_gradient() + My_Theme
})

output$Vehicles <- renderPlot({
    z=invoice %>%  group_by(numVehicles) %>% summarise(num=n()) %>% mutate(total=sum(num))
    z=invoice %>% group_by(CUSTOMER_NUMBER) %>% 
        summarise(numVehicles=n_distinct(VIN_NUMBER))
    
    z$numVehicles=as.factor(z$numVehicles)
    z=z %>% 
        mutate(numVehicles=case_when(
            as.numeric(numVehicles)>=5 ~ "5+",
            TRUE ~ as.character(numVehicles)
        ))
    
    z %>% group_by(numVehicles) %>% summarise(num=n()) %>% 
        mutate(total=sum(num), per=num/total) %>% 
        ggplot(aes(x=as.factor(numVehicles), y=num)) + geom_col() +
        geom_text(aes(label=paste0(round(per*100,0),"%")), nudge_y = 10000) +
        scale_y_continuous(labels = label_comma(), breaks=pretty_breaks(6)) +
        theme_minimal() + labs(x="Number of Vehicles Serviced", y="Customers")+ My_Theme
})

output$customers <- renderPlot({
    
inv=invoice
inv$VIN_NUMBER[inv$VIN_NUMBER=='NULL'] = NA
inv <- inv %>% drop_na(CUST_CREATE_DATE, VIN_NUMBER)
inv %>% group_by(createYear) %>% summarise(Reveneue=sum(Total.Line.Revenue)) %>% 
    ggplot(aes(x=createYear, y=Reveneue)) + geom_col()

inv %>% group_by(createYear) %>% select(CUSTOMER_NUMBER) %>% unique() %>%  summarise(newCustomers=n()) %>% 
    ggplot(aes(x=createYear, y=newCustomers)) + geom_col() +
    scale_y_continuous(labels = label_comma()) +
    labs(title="", y="", x="")+ theme_minimal() + My_Theme
})

}
# Run the application 
shinyApp(ui = ui, server = server)
