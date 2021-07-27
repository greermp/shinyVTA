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
# setwd("~/MSBA/shinyVTA")

#### zipcodes2=st_read("mapdata/zipcodes.shp") Entire State

#Good data
#####invoice <- read.csv("data/VTA_Invoice_all 7.18.2021_wCityCountyMycarNumVeh.csv")

invoice <- read.csv("data/VTA_Invoice_all_2  7.27.2021.csv")
vtaLocations=read.csv("mapdata/vtalocations.csv")
zipcodes2=st_read("mapdata/zipcodesSmall.shp")

# invoice %>% mutate(numVehiclesFactor=fct_relevel()
invoice$numVehiclesFactor <- factor(invoice$numVehiclesFactor, levels=c("1", "2", "3", "4", "5-10", "10+"))
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

# My_Theme = theme(
#     axis.title.x = element_text(size = 16),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     axis.title.y = element_text(size = 16))
My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 16),
  plot.title = element_text(color = "#3BB44A", size = 20, face = "bold"),
  plot.subtitle = element_text(color = "#3BB44A", size = 16, face = "bold"))

invDates=invoice %>% select(INVOICE_DATE) %>% summarise(earliestINV=min(INVOICE_DATE,na.rm=TRUE), 
                                                        latestINV=max(INVOICE_DATE,na.rm=TRUE))

createDates=invoice %>% select(CUST_CREATE_DATE) %>% summarise(earliestCreate=min(CUST_CREATE_DATE,na.rm=TRUE), 
                                                        latestCreate=max(CUST_CREATE_DATE,na.rm=TRUE))

# invoice <- invoice %>%  mutate(Zip.Code = str_sub(Zip.Code, start=1, end=5))

invoice$Zip.Code <- as.factor(invoice$Zip.Code)

ui=navbarPage(title="Virginia Tire and Auto", theme = shinytheme("flatly"),
fluid = TRUE,
collapsible = TRUE,
navbarMenu("CLV",
tabPanel("Vehicle Age",
     sidebarLayout(sidebarPanel(icon("dashboard"),width = 3,
            sliderInput("age",
                        "Vehicle Age Segment 1:",
                        min = 1995,
                        max = 2020,
                        value = c(2005,2010),
                        sep = ""),
            sliderInput("age2",
                        "Vehicle Age Segment 2:",
                        min = 1995,
                        max = 2020,
                        value = c(2015,2020),
                        sep = ""),
         # sliderInput("age",
         #             "Vehicle Age Segment:",
         #             min = 1995,
         #             max = 2020,
         #             value = 2010,
         #             sep = ""),
         h3("CLV Cohort:"),
         # numericInput('marketing', 'Marketing per Customer/year ($)', 76, min = 1, max = 9),
         # numericInput('margin', 'Sales Margin - Parts&Labor (%)', 64, min = 1, max = 99),
         # numericInput('discount', 'Discount Rate (%)', 10, min = 1, max = 50),
         dateRangeInput("dates",'Customer Creation Date Range', min=dates$EarliestCust, max=dates$LatestCust,
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
                title = "Vehicle Age Segment 1"
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,plotOutput("two_L")
            )
            ,box(
                title = "Vehicle Age Segment 2"
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,plotOutput("two_R")
            )
        ),
        fluidRow(
            box(
                title = "Customer Lifetime Value"
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,plotOutput("three_L")
            )
            ,box(
                title = "Average Invoice Amount and Time Between Visits"
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,plotOutput("three_R")
            )
        ),
        hr(),
        br(),
         # fluidRow(
             # column(12,
             #        # box(width = 12,
             #            title="Customer Return Rate by Segment",
             #            sankeyNetworkOutput("three_L", width = "100%"))
        )
         # )
        # )# End main panel
        )# End sidebar panel
),
tabPanel("MyCar Member",
         fluidRow(
             # column(width = 6, title="HI", renderText("test")),
             # column(width = 6, title="HI",renderText("test")),
             # 
             valueBoxOutput(width = 3, "mycarCust")
             ,valueBoxOutput(width = 3,"mycarAvgRev")
             ,valueBoxOutput(width = 3,"mycarAvgVisits")
             ,valueBoxOutput(width = 3,"mycartimeBetweenVisits")
         ),
         fluidRow(
             valueBoxOutput(width = 3, "Cust")
             ,valueBoxOutput(width = 3,"AvgRev")
             ,valueBoxOutput(width = 3,"AvgVisits")
             ,valueBoxOutput(width = 3,"timeBetweenVisits")
         ),
         fluidRow(
             title = "My Car Customers"
                 ,status = "primary"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,plotOutput("MyCarRetentionHistory" 
             )
             # ,box(
             #     title = "Non-My Car Members"
             #     ,status = "primary"
             #     ,solidHeader = TRUE
             #     ,collapsible = TRUE
             #     ,plotOutput("MyCarRetentionHistory")
             # )
         ),
         fluidRow(
             box(
                 title = "Customer Lifetime Value"
                 ,status = "primary"
                 ,solidHeader = TRUE
                 ,collapsible = TRUE
                 ,plotOutput("myCarCLV2018")
             )
         )
)   
,tabPanel("Zipcode",
          fluidRow(
              column(width = 8, offset = 2,
                     leafletOutput("clvMap"),
                     br()
                  )
              ),
              fluidRow(
                  column( width=2, offset = 5,
                          dateRangeInput("datesCLVMap",'Customer Creation Range', min=invDates$earliestCreate, max=invDates$latestCreate,
                                         start="2018-01-01", end="2018-12-31"),
                  )
              ),
          fluidRow(
              box(
                  title = "CLV by City"
                  ,status = "primary"
                  ,solidHeader = TRUE
                  ,collapsible = TRUE
                  ,plotOutput("City")
              )
              ,box(
                  title = "CLV by County"
                  ,status = "primary"
                  ,solidHeader = TRUE
                  ,collapsible = TRUE
                  ,plotOutput("County")
              )
          )
),tabPanel("Number of Vehicles",
           fluidRow(
               box(
                   title = "Distribution of Vehicles Serviced per Customer"
                   ,status = "primary"
                   ,solidHeader = TRUE
                   ,collapsible = TRUE
                   ,plotOutput("NumVehicles")
               )
               ,box(
                   title = "Total Revenue by Customer Segment (2018-2020)"
                   ,status = "primary"
                   ,solidHeader = TRUE
                   ,collapsible = TRUE
                   ,plotOutput("LifetimeRevByVehicle")
               )
           ),
           fluidRow(
               box(
                   title = "Average # of Visits by Number of Vehicles Serviced"
                   ,status = "primary"
                   ,solidHeader = TRUE
                   ,collapsible = TRUE
                   ,plotOutput("visitByVehicle")
               )
               ,box(
                   title = "Customer Lifetime Value by Number of Vehicles Serviced"
                   ,status = "primary"
                   ,solidHeader = TRUE
                   ,collapsible = TRUE
                   ,plotOutput("vehicleCLV")
               )
           )
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
             column( width=2, offset = 1,
                 textOutput("missing"),
             dateRangeInput("datesMap",'Invoice Date Range', min=invDates$earliestINV, max=invDates$latestINV,
                            start="2020-12-01", end="2020-12-31"),
             ),
             column(width = 8,
                    leafletOutput("mymap"),
                    br()
             )
         ),
        fluidRow(
         box(#width=6,
             title="Revenue By Store",
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


# myCarCLV2018=invoice %>% filter(createYear>=2018) %>% 
#     group_by(MyCarClub, visit) %>% summarise(count=n(), 
#                                              avgTimeBeteen=mean(TimeToNextVisit, na.rm = TRUE), 
#                                              AvgProfit=mean(Revenue.Less.Cost)) %>% 
#     group_by(MyCarClub) %>%
#       mutate(totalSegment=first(count), perActive=count/totalSegment) 
myCarCLV2018=invoice %>% filter(createYear>=2018) %>% 
  group_by(visit, EverMyCar) %>% summarise(count=n(), 
                                           avgTimeBeteen=mean(TimeToNextVisit, na.rm = TRUE), 
                                           AvgProfit=mean(Revenue.Less.Cost)) %>% 
  mutate(totalSegment=first(count), perActive=count/totalSegment) 


# create the server functions for the dashboard  
server <- function(input, output) { 
    #some data manipulation to derive the values of KPI boxes
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
      start=lubridate::ymd(input$dates[1])
      end=lubridate::ymd(input$dates[2])
      total.revenue <- as.numeric(round(invoice %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end) %>% summarise(sum(Total.Line.Revenue)), 2))
       # <- as.numeric(round(invoice %>% summarise(sum(Total.Line.Revenue)), 2))
      
        valueBox(
            paste0('$',formatC(as.numeric(total.revenue/1000000)),"")
            ,'MM Total Reveneue'
            ,icon = icon("money-bill-wave",lib='font-awesome')
            ,color = "green")  
    })
    output$value2 <- renderValueBox({ 
      start=lubridate::ymd(input$dates[1])
      end=lubridate::ymd(input$dates[2])
      total.customers <- as.numeric(round(invoice  %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end)%>% group_by(CUSTOMER_NUMBER) %>% select(CUSTOMER_NUMBER) %>% unique() %>% nrow(), 2))
      
        valueBox(
            formatC(as.numeric(total.customers), format="f", big.mark=',', drop0trailing = TRUE)
            ,'Total Unique Customers'
            ,icon = icon("oil-can",lib='font-awesome')
            ,color = "olive")  
    })
    output$value3 <- renderValueBox({
      start=lubridate::ymd(input$dates[1])
      end=lubridate::ymd(input$dates[2])
      # customers <- as.numeric(round(invoice%>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end)  %>% group_by(CUSTOMER_NUMBER) %>% select(CUSTOMER_NUMBER) %>% unique() %>% nrow(), 2))
      total.revenue <- as.numeric(round(invoice %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end) %>% summarise(sum(Revenue.Less.Cost)), 2))
      
        valueBox(
          paste0('$',formatC(as.numeric(total.revenue/1000000)),"")
          ,'MM Total Profit'
          ,icon = icon("money-bill-wave",lib='font-awesome')
          ,color = "green")    
    })
############################
    output$mycarCust <- renderValueBox({
        numCust=myCarCLV2018 %>% ungroup() %>%  filter(EverMyCar==TRUE & visit==1) %>% select(count)
            valueBox(width=3,
                formatC(as.numeric(numCust))
                ,'New My Car Customers (2018-2021)'
                ,icon = icon("smile",lib='font-awesome')
                ,color = "green")  
        })
    output$mycarAvgRev <- renderValueBox({ 
        avg=invoice %>% filter(EverMyCar==TRUE) %>% summarise(mean(Total.Line.Revenue))
        valueBox(width=3,
            paste0('$ ',formatC(as.numeric(avg)))
            ,'Average Invoice amount (My-Car Customers)'
            ,icon = icon("money-bill-wave",lib='font-awesome')
            ,color = "green")  
    })
    output$mycarAvgVisits <- renderValueBox({
        numCust=myCarCLV2018 %>% ungroup() %>%  filter(EverMyCar==FALSE & visit==1) %>% select(count)
        valueBox(width=3,
                 formatC(round(as.numeric(numCust),0),format="f", big.mark = ',', drop0trailing = TRUE)
                 ,'New Customers 2018-2021 (Not My Car Customers)'
                 ,icon = icon("smile",lib='font-awesome')
                 ,color = "orange") 
    })
    output$mycartimeBetweenVisits <- renderValueBox({
        
        avg=invoice %>% filter(EverMyCar==FALSE) %>% summarise(mean(Total.Line.Revenue))
        valueBox(width=3,
                 paste0('$ ',formatC(as.numeric(avg)))
                 ,'Average Invoice amount (Non My-Car Customers)'
                 ,icon = icon("money-bill-wave",lib='font-awesome')
                 ,color = "orange")  
    })
    ####################
    output$Cust <- renderValueBox({
        avg=invoice %>% filter(EverMyCar==TRUE) %>% summarise(mean(numVisits))
        
        valueBox(width=3,
                 formatC(round(as.numeric(avg),1), format="f", big.mark=',', drop0trailing = TRUE)
                 ,'Average Vists from 2018-2020 (My-Car Customers)'
                 ,icon = icon("door-open",lib='font-awesome')
                 ,color = "green")   
    })
    output$AvgRev <- renderValueBox({ 
        avg=invoice %>% filter(EverMyCar==TRUE) %>% summarise(mean(TimeToNextVisit, na.rm = TRUE))
        
        valueBox(width=3,
                 formatC(round(as.numeric(avg),1), format="f", big.mark=',', drop0trailing = TRUE)
                 ,'Average Time Between Visits (My-Car Customers)'
                 ,icon = icon("history",lib='font-awesome')
                 ,color = "green")  
        
    })
    output$AvgVisits <- renderValueBox({
        avg=invoice %>% filter(EverMyCar==FALSE) %>% summarise(mean(numVisits))
        
        valueBox(width=3,
                 formatC(round(as.numeric(as.numeric(avg),1)), format="f", big.mark=',', drop0trailing = TRUE)
                 ,'Average Vists from 2018-2020 (Non My-Car Customers)'
                 ,icon = icon("door-open",lib='font-awesome')
                 ,color = "orange")   
    })
    output$timeBetweenVisits <- renderValueBox({
        avg=invoice %>% filter(EverMyCar==FALSE) %>% summarise(mean(TimeToNextVisit, na.rm = TRUE))
        
        valueBox(width=3,
                 formatC(round(as.numeric(avg),1), format="f", big.mark=',', drop0trailing = TRUE)
                 ,'Average Days Between Visits (Non My-Car Customers)'
                 ,icon = icon("history",lib='font-awesome')
                 ,color = "orange")   
    })
    #creating the plotOutput content
    output$two_L <- renderPlot({
      # input=tibble(
      #              discount=10,
      #              dates=c("2018-01-01", "2018-12-31"),
      #              age=c("1995", "2000"),
      #              age2=c("2000", "2005")
      #              )
      
        group1=paste0(input$age[1], ' to ', input$age[2])
        group2=paste0(input$age2[1], ' to ', input$age2[2])
        start=lubridate::ymd(input$dates[1])
        end=lubridate::ymd(input$dates[2])
        
        CLVInvoice=invoice %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end)
        # print(input$dates)
        # print(input$age)
        
        vehage <- CLVInvoice %>% 
            mutate(factor=case_when(
                CAR_YEAR>=input$age[1] & CAR_YEAR<=input$age[2]  ~ paste0(input$age[1], ' to ', input$age[2]),
                CAR_YEAR>=input$age2[1] & CAR_YEAR<=input$age2[2]  ~ paste0(input$age2[1], ' to ', input$age2[2]),
                TRUE ~ paste0('remove'))) %>% 
            filter (factor != "remove")
        
        vehage %>% 
            group_by(factor, visit) %>% summarise(count=n(), AvgProfit=mean(Revenue.Less.Cost)) %>% 
            filter(as.numeric(visit)<=10) %>% 
            mutate(dropoff = count/lag(count)) %>%
            mutate(dropoff = case_when(is.na(dropoff)~1, TRUE ~ dropoff)) %>%
            filter(factor == group1) %>% 
            ggplot(aes(x=as.factor(visit), y=count, fill=AvgProfit)) +geom_col() +
            # facet_wrap(~factor) +
            geom_text(nudge_y = 400,aes(x=as.numeric(visit),y=count,label=paste0(round(dropoff*100,0),"%"))) +
            theme_minimal() + scale_y_continuous(labels = label_comma(), breaks = pretty_breaks()) +
            labs(title=group1, x= "Visit Number", y="Number of Customers", fill="Average Profit") + My_Theme
     
    })
    # scaleFUN <- function(x) sprintf("%.2f", x)
    output$two_R <- renderPlot({
        group1=paste0(input$age[1], ' to ', input$age[2])
        group2=paste0(input$age2[1], ' to ', input$age2[2])
        start=lubridate::ymd(input$dates[1])
        end=lubridate::ymd(input$dates[2])
        CLVInvoice=invoice %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end)
        # print(input$dates)
        # print(input$age)
        
        vehage <- CLVInvoice %>% 
            mutate(factor=case_when(
                CAR_YEAR>=input$age[1] & CAR_YEAR<=input$age[2]  ~ paste0(input$age[1], ' to ', input$age[2]),
                CAR_YEAR>=input$age2[1] & CAR_YEAR<=input$age2[2]  ~ paste0(input$age2[1], ' to ', input$age2[2]),
                TRUE ~ paste0('remove'))) %>% 
            filter (factor != "remove")
        
        vehage %>% 
            group_by(factor, visit) %>% summarise(count=n(), AvgProfit=mean(Revenue.Less.Cost)) %>% 
            filter(as.numeric(visit)<=10) %>% 
            mutate(dropoff = count/lag(count)) %>%
            mutate(dropoff = case_when(is.na(dropoff)~1, TRUE ~ dropoff)) %>%
            filter(factor == group2) %>% 
            ggplot(aes(x=as.factor(visit), y=count, fill=AvgProfit)) +geom_col() +
            # facet_wrap(~factor) +
            geom_text(nudge_y = 400,aes(x=as.numeric(visit),y=count,label=paste0(round(dropoff*100,0),"%"))) +
            theme_minimal() + scale_y_continuous(labels = label_comma(), breaks = pretty_breaks()) +
            labs(title=group2, x= "Visit Number", y="Number of Customers", fill="Average Profit") + My_Theme

      
    })
    output$three_L <- renderPlot({
        start=lubridate::ymd(input$dates[1])
        end=lubridate::ymd(input$dates[2])
        CLVInvoice=invoice %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end)
        # print(input$dates)
        # print(input$age)
        
        vehage <- CLVInvoice %>% 
            mutate(factor=case_when(
                CAR_YEAR>=input$age[1] & CAR_YEAR<=input$age[2]  ~ paste0(input$age[1], ' to ', input$age[2]),
                CAR_YEAR>=input$age2[1] & CAR_YEAR<=input$age2[2]  ~ paste0(input$age2[1], ' to ', input$age2[2]),
                TRUE ~ paste0('remove'))) %>% 
            filter (factor != "remove")
        
        
        vehage <- vehage %>% group_by(factor, visit) %>% summarise(count=n(),
                                                                   avgTimeBeteen=mean(TimeToNextVisit, na.rm = TRUE),
                                                                   AvgProfit=mean(Revenue.Less.Cost)) %>%
            group_by(factor) %>% mutate(totalSegment=first(count), perActive=count/totalSegment)
        
        
        vehageSum <- vehage %>% group_by(factor, visit) %>% mutate(Profit=AvgProfit*count) %>% 
            group_by(factor) %>% 
            summarise(TotalProfit=sum(Profit), perCust=TotalProfit/first(count), custAtGroupOne=first(count))
        
        ggplot(vehageSum, aes(x=reorder(factor, -perCust), y=perCust, fill=factor(comma(round(custAtGroupOne,0))))) +
            geom_col() + scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                                                 prefix = "$", suffix = "",
                                                                 big.mark = ",", decimal.mark = ".")) +
            theme_minimal() +
          scale_fill_manual(values=c("#3BB44A", "#000000"))+
          # scale_fill_brewer(palette = palette) +
            My_Theme + labs(x="", y="Customer Timetime Value", fill="# Customers") + theme(axis.text.x = element_text(angle=45, hjust = 1))
        
    })
    
    output$three_R <- renderPlot({
        start=lubridate::ymd(input$dates[1])
        end=lubridate::ymd(input$dates[2])
        CLVInvoice=invoice %>% filter(CUST_CREATE_DATE >= start & CUST_CREATE_DATE <= end)
        # print(input$dates)
        # print(input$age)
        
        vehage <- CLVInvoice %>% 
            mutate(factor=case_when(
                CAR_YEAR>=input$age[1] & CAR_YEAR<=input$age[2]  ~ paste0(input$age[1], ' to ', input$age[2]),
                CAR_YEAR>=input$age2[1] & CAR_YEAR<=input$age2[2]  ~ paste0(input$age2[1], ' to ', input$age2[2]),
                TRUE ~ paste0('remove'))) %>% 
            filter (factor != "remove")
        
        
        vehage <- vehage %>% group_by(factor) %>% summarise(count=n(),
                                                                   avgTimeBeteen=mean(TimeToNextVisit, na.rm = TRUE),
                                                                   AvgInvoiceProfit=mean(Revenue.Less.Cost),
                                                            avgVisits = mean(numVisits, na.rm=TRUE)) 

        
        ggplot(vehage, aes(x=reorder(factor, -AvgInvoiceProfit), y=AvgInvoiceProfit, fill=factor(comma(round(avgTimeBeteen,0))))) +
            geom_col() + scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                                                 prefix = "$", suffix = "",
                                                                 big.mark = ",", decimal.mark = ".")) +
            scale_fill_manual(values=c("#3BB44A", "#000000"))+
            # scale_fill_brewer(palette=palette) +
            theme_minimal() +
            My_Theme + labs(x="", y="Average Invoice Profit", fill="Time Between Visits") + 
            theme(axis.text.x = element_text(angle=45, hjust = 1)) 
            
        
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
            group_by(Zip.Code.Clean) %>% summarise(Revenue=sum(Total.Line.Revenue), count=n()) %>% 
          ungroup() %>% 
            filter(Zip.Code.Clean != "") %>% filter(count >=50)
        
        zipcode_totals$Zip.Code.Clean = as.character(zipcode_totals$Zip.Code.Clean)
        # Geojoin totals with zipcodes
        zipcodes_w_totals <- geo_join(zipcodes2, 
                                      zipcode_totals, 
                                      by_sp = "GEOID10", 
                                      by_df = "Zip.Code.Clean",
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
        filter(as.numeric(visit)<=7 ) %>% 
        mutate(dropoff = count/lag(count)) %>%
        mutate(dropoff = case_when(is.na(dropoff)~1, TRUE ~ dropoff)) %>%
        ggplot(aes(x=as.factor(visit), y=count, fill=AvgProfit)) +
        geom_text(nudge_y = 1000,aes(x=as.factor(visit),y=count,label=paste0(round(dropoff*100,0),"%"))) +
        # facet_wrap(~MyCarClub) +
        geom_col() + theme_minimal() + scale_y_continuous(labels = label_comma(), breaks = pretty_breaks()) +
        labs(title="", x="Visit Number", y="# of Customers", fill="Average Profit") + 
        scale_fill_gradient() + My_Theme
})

output$Vehicles <- renderPlot({
    # z=invoice %>%  group_by(numVehicles) %>% summarise(num=n()) %>% mutate(total=sum(num))
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
# inv$VIN_NUMBER[inv$VIN_NUMBER=='NULL'] = NA
inv <- inv %>% drop_na(CUST_CREATE_DATE)
# inv %>% group_by(createYear) %>% summarise(Reveneue=sum(Total.Line.Revenue)) %>% 
#     ggplot(aes(x=createYear, y=Reveneue)) + geom_col()

inv %>% group_by(createYear) %>% select(CUSTOMER_NUMBER) %>% unique() %>%  summarise(newCustomers=n()) %>% 
    ggplot(aes(x=createYear, y=newCustomers)) + geom_col() +
    scale_y_continuous(labels = label_comma(), breaks=pretty_breaks()) +
    labs(title="", y="", x="")+ theme_minimal() + My_Theme
})

output$MyCarAvgRevenue <- renderPlot({
  invoice %>% filter(EverMyCar==TRUE &createYear >= 2018) %>% 
    group_by(EverMyCar, visit) %>% summarise(count=n(), AvgProfit=mean(Revenue.Less.Cost)) %>% 
    filter(as.numeric(visit)<=7 & ! is.na(EverMyCar)) %>% 
    mutate(dropoff = count/lag(count)) %>%
    mutate(dropoff = case_when(is.na(dropoff)~1, TRUE ~ dropoff)) %>%
    ggplot(aes(x=as.factor(visit), y=count, fill=AvgProfit)) +
    geom_col() +
    geom_text(nudge_y = 20,aes(x=as.numeric(visit),y=count,label=paste0(round(dropoff*100,0),"%"))) +
    theme_minimal() + scale_y_continuous(labels = label_comma(), breaks = pretty_breaks()) +
    labs(title="", x= "Visit Number", y="Number of Customers", fill="Average Profit") + My_Theme
})

output$MyCarRetentionHistory <- renderPlot({
  
  invoice <- invoice %>% mutate(MyCarLabel=case_when(.$EverMyCar==TRUE ~ "My Car Member", .$EverMyCar==FALSE ~ "Non-Member"))
  invoice %>% filter(createYear >= 2018) %>% 
    group_by(MyCarLabel, visit) %>% summarise(count=n(), AvgProfit=mean(Revenue.Less.Cost)) %>% 
    filter(as.numeric(visit)<=7 & ! is.na(MyCarLabel)) %>% 
    mutate(dropoff = count/lag(count)) %>%
    mutate(dropoff = case_when(is.na(dropoff)~1, TRUE ~ dropoff)) %>%
    ggplot(aes(x=as.factor(visit), y=count, fill=AvgProfit)) +
    geom_col() +
    facet_wrap(~MyCarLabel) +
    geom_text(nudge_y = 2000,aes(x=as.numeric(visit),y=count,label=paste0(round(dropoff*100,0),"%"))) +
    theme_minimal() + scale_y_continuous(labels = label_comma(), breaks = pretty_breaks()) +
    labs(title="", x= "Visit Number", y="Number of Customers", fill="Average Profit") + My_Theme
  # invoice %>% filter(EverMyCar==FALSE & createYear >= 2018) %>% 
  #   group_by(EverMyCar, visit) %>% summarise(count=n(), AvgProfit=mean(Revenue.Less.Cost)) %>% 
  #   filter(as.numeric(visit)<=10 & ! is.na(EverMyCar)) %>% 
  #   mutate(dropoff = count/lag(count)) %>%
  #   mutate(dropoff = case_when(is.na(dropoff)~1, TRUE ~ dropoff)) %>%
  #   ggplot(aes(x=as.factor(visit), y=count, fill=AvgProfit)) +geom_col() +
  #   geom_text(nudge_y = 20,aes(x=as.numeric(visit),y=count,label=paste0(round(dropoff*100,0),"%"))) +
  #   theme_minimal() + scale_y_continuous(labels = label_comma(), breaks = pretty_breaks()) +
  #   labs(title="", x= "Visit Number", y="Number of Customers", fill="Average Profit") + My_Theme
    })

output$myCarCLV2018 <- renderPlot({
    myCarCLV2018 %>% group_by(visit,EverMyCar) %>% mutate(Profit=AvgProfit*count) %>% 
    group_by(EverMyCar) %>% 
    summarise(TotalProfit=sum(Profit), perCust=TotalProfit/first(count)) %>% ungroup() %>% 
    mutate(cat=case_when(
      EverMyCar==TRUE ~ "My Car Club Member",
      TRUE ~ "Non-member"
    )) %>% 
    ggplot(aes(x=cat, y=perCust, fill=cat)) +
    geom_col() + theme_minimal() +
    scale_fill_brewer(palette = "Dark2") +
    labs(x="", y="Customer Lifetime Value", fill="") +
    geom_text(nudge_y = 20,aes(x=cat, y=perCust, label=paste0('$ ',round(perCust,0)))) +
    My_Theme

})

output$NumVehicles <- renderPlot({
    invoice %>% filter(createYear >= 2018) %>% 
    filter(numVehiclesFactor != "10+") %>% 
        group_by(numVehiclesFactor) %>% summarise(num=n()) %>% 
        mutate(total=sum(num), per=num/total) %>% 
        ggplot(aes(x=as.factor(numVehiclesFactor), y=num)) + geom_col() +
        geom_text(aes(label=paste0(round(per*100,0),"%")), nudge_y = 1500) +
        scale_y_continuous(labels = label_comma(), breaks=pretty_breaks(6)) +
        theme_minimal() + labs(x="Vehicles Serviced", y="Customers", fill="# Vehicles Per Customer") + My_Theme
})

output$vehicleCLV <- renderPlot({
    numVeh=invoice %>% filter(createYear >= 2018) %>%
      filter(numVehiclesFactor != "10+") %>% 
        group_by(numVehiclesFactor, visit) %>% summarise(count=n(),
                                                 avgTimeBeteen=mean(TimeToNextVisit, na.rm = TRUE),
                                                 AvgProfit=mean(Revenue.Less.Cost)) %>%
        group_by(numVehiclesFactor) %>% mutate(totalSegment=first(count), perActive=count/totalSegment)
    
    numVeh %>% group_by(numVehiclesFactor) %>% mutate(Profit=AvgProfit*(count*perActive)) %>% 
        summarise(TotalProfit=sum(Profit), perCust=TotalProfit/first(count)) %>% ungroup() %>% 
        ggplot(aes(x=numVehiclesFactor, y=perCust, fill=numVehiclesFactor )) +
        geom_col() + theme_minimal() +
        scale_fill_brewer(palette = palette) +
        labs(x="Vehicles Serviced", y="Customer Lifetime Value", fill="Vehicles per Customer") + My_Theme +
        scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                                prefix = "$", suffix = "",
                                                big.mark = ",", decimal.mark = "."), breaks=pretty_breaks(8)) 
})


output$visitByVehicle <- renderPlot({
    invoice %>% group_by(numVehiclesFactor) %>% 
        filter(numVehiclesFactor != "10+") %>% 
        summarise(meanTimeBetween = mean(TimeToNextVisit, na.rm = TRUE), numVisits = mean(numVisits), averageInvoice=mean(Total.Line.Revenue)) %>% 
        ggplot(aes(x=numVehiclesFactor, y=numVisits, fill=averageInvoice)) +
        geom_col() + theme_minimal() +
        geom_text(aes(label=round(numVisits,1)), nudge_y = 1) +
        labs(x="Vehicles Serviced", y="Average Number of Visits", fill="Average Inovice $") + My_Theme +
        scale_y_continuous(breaks=pretty_breaks())
})


output$LifetimeRevByVehicle <- renderPlot({
    invoice %>% filter(createYear==2018) %>% 
        filter(numVehiclesFactor != "10+") %>% 
        group_by(numVehiclesFactor) %>% 
        summarise(avgRevenue = sum(Total.Line.Revenue, na.rm = TRUE)) %>% 
        ggplot(aes(x=as.factor(numVehiclesFactor), y=avgRevenue)) +
        geom_col() + theme_minimal() +
        labs(x="Vehicles Serviced", y="2018-2020 Revenue") +
        scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                                prefix = "$", suffix = "",
                                                big.mark = ",", decimal.mark = ".")) +
        My_Theme
})


output$clvMap <- renderLeaflet({
    zipcode_totals <- invoice %>% filter(CUST_CREATE_DATE>= input$datesCLVMap[1] &
                                             CUST_CREATE_DATE<= input$datesCLVMap[2]) %>% 
        group_by(Zip.Code.Clean, visit) %>% summarise(count=n(),
                                                avgTimeBeteen=mean(TimeToNextVisit, na.rm = TRUE),
                                                AvgProfit=mean(Revenue.Less.Cost)) %>%
        group_by(Zip.Code.Clean) %>% mutate(totalSegment=first(count), perActive=count/totalSegment) %>% 
        filter(Zip.Code.Clean != "") %>% 
        filter(totalSegment >=15)
    
    zipcode_totals$Zip.Code.Clean <-  as.character(zipcode_totals$Zip.Code.Clean)
    
    zipcode_totals <- zipcode_totals %>% group_by(Zip.Code.Clean, visit) %>% mutate(Profit=AvgProfit*count) %>% 
        group_by(Zip.Code.Clean) %>% 
        summarise(TotalProfit=sum(Profit), perCust=TotalProfit/first(count), count=first(count))
    
    # Geojoin totals with zipcodes
    zipcodes_w_totals <- geo_join(zipcodes2, 
                                  zipcode_totals, 
                                  by_sp = "GEOID10", 
                                  by_df = "Zip.Code.Clean",
                                  how = "left")
    
    themax=max(zipcodes_w_totals$perCust, na.rm = TRUE)
    # bins <- c(0, 
    #           # round(themax*.20, 0),
    #           # round(themax*.30, 0),
    #           round_any(themax*.10, 50, floor),
    #           round_any(themax*.20, 50),
    #           round_any(themax*.30, 50),
    #           round_any(themax*.40, 50),
    #           round_any(themax*.50, 50),
    #           round_any(themax*.60, 50),
    #           round_any(themax*.70, 50),
    #           round_any(themax*.80, 50),
    #           round_any(themax*.90, 50),
    #           round_any(themax*1.1, 50))
    
    bins <- c(0, 100, 200, 300, 400, 500, 600, 700,800,Inf)
    
    bins <- unique(bins)
    
    pal <- colorBin("YlOrRd", domain = zipcode_totals$perCust, bins = bins)
    
    
    # Leaflet map
    map <- zipcodes_w_totals %>% 
        leaflet %>%
        # add base map
        addProviderTiles("CartoDB") %>% 
        # add zip codes
        addPolygons(fillColor = ~pal(perCust),
                    weight = 0.5,
                    opacity = 1,
                    color = "black",
                    dashArray = "1",
                    fillOpacity = 0.5,
                    popup = paste0("Zip: ",zipcodes_w_totals$GEOID10, "<br>", "CLV: ",
                                   comma( round(zipcodes_w_totals$perCust,0),prefix = '$' ),
                                   "<br>","Customers: ",comma(zipcodes_w_totals$count),"<br>",
                                   "Total Profit: ",comma(zipcodes_w_totals$TotalProfit, prefix = '$') 
                                   
                                   ), 
                    highlight = highlightOptions(weight = 2,
                                                 color = "#666",
                                                 dashArray = "",
                                                 fillOpacity = 0.4,
                                                 bringToFront = TRUE)) %>% 
        setView(lng = -77.5, lat = 38.7,  zoom = 9) %>% 
        # add legend
        addLegend(pal = pal, 
                  values = ~perCust, 
                  opacity = 0.7, 
                  title = "CLV by Zip",
                  position = "topright") %>% 
        addMarkers(lng = ~vtaLocations$Long, lat = ~vtaLocations$Lat, label = vtaLocations$Location)
    
    
    map
})


output$City <- renderPlot({
    zipcode_totals <- invoice %>% filter(CUST_CREATE_DATE>= input$datesCLVMap[1] &
                                             CUST_CREATE_DATE<= input$datesCLVMap[2]) %>% 
        group_by(city, visit) %>% summarise(count=n(),
                                                      avgTimeBeteen=mean(TimeToNextVisit, na.rm = TRUE),
                                                      AvgProfit=mean(Revenue.Less.Cost)) %>%
        group_by(city) %>% mutate(totalSegment=first(count), perActive=count/totalSegment) %>% 
        filter(city != "") %>% 
        filter(totalSegment >=50)
    
    zipcode_totals$city <-  as.character(zipcode_totals$city)
    
    zipcode_totals <- zipcode_totals %>% group_by(city) %>% mutate(Profit=AvgProfit*count) %>% 
        summarise(TotalProfit=sum(Profit), perCust=TotalProfit/first(count), custAtGroupOne=first(count))
    
    ggplot(zipcode_totals, aes(x=reorder(city, -perCust), y=perCust, fill=custAtGroupOne)) +
        geom_col()+ scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                                             prefix = "$", suffix = "",
                                                             big.mark = ",", decimal.mark = ".")) +
        theme_minimal() +
        My_Theme + labs(x="", y="Customer Lifetime Value", fill="# Customers") + theme(axis.text.x = element_text(angle=45, hjust = 1))
})


output$County <- renderPlot({
    zipcode_totals <- invoice %>% filter(CUST_CREATE_DATE>= input$datesCLVMap[1] &
                                             CUST_CREATE_DATE<= input$datesCLVMap[2]) %>% 
        group_by(county, visit) %>% summarise(count=n(),
                                            avgTimeBeteen=mean(TimeToNextVisit, na.rm = TRUE),
                                            AvgProfit=mean(Revenue.Less.Cost)) %>%
        group_by(county) %>% mutate(totalSegment=first(count), perActive=count/totalSegment) %>% 
        filter(county != "") %>% 
        filter(totalSegment >=50)
    
    zipcode_totals$county <-  as.character(zipcode_totals$county)
    
    zipcode_totals <- zipcode_totals %>% group_by(county) %>% mutate(Profit=AvgProfit*count) %>% 
        summarise(TotalProfit=sum(Profit), perCust=TotalProfit/first(count), custAtGroupOne=first(count))
    
    ggplot(zipcode_totals, aes(x=reorder(county, -perCust), y=perCust, fill=custAtGroupOne)) +
        geom_col() + scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                            prefix = "$", suffix = "",
                                            big.mark = ",", decimal.mark = ".")) +
        theme_minimal() +
        My_Theme + labs(x="", y="Customer Timetime Value", fill="# Customers") + theme(axis.text.x = element_text(angle=45, hjust = 1))
})

}


# input=tibble(
#              discount=10,
#              dates=c("2018-01-01", "2018-12-31"),
#              datesMap=c("2018-01-01", "2018-12-31"),
#              datesCLVMap=c("2018-01-01", "2018-12-31")
#              )

# Run the application 
shinyApp(ui = ui, server = server)

