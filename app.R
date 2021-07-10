library(shinydashboard)
library(shiny)
library(shinythemes)
library(networkD3)
library(lubridate)
library(tidyverse)
library(scales)
library(ggthemes)
library(RColorBrewer)
library(leaflet)
library(fontawesome)
palette="Dark2"
My_Theme = theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16))

# setwd("~/MSBA/shinyVTA")
invoice <- read.csv("data/InvoiceSummary.csv")

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)

invoice$CUST_CREATE_DATE <- lubridate::ymd(invoice$CUST_CREATE_DATE)
dates=invoice %>% select(CUST_CREATE_DATE) %>% summarise(EarliestCust=min(CUST_CREATE_DATE,na.rm=TRUE), LatestCust=max(CUST_CREATE_DATE,na.rm=TRUE))
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Virginia Tire and Auto")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "CLV", icon = icon("dashboard")),
        menuItem("VTA Location Summary", icon = icon("send",lib='glyphicon'), 
                 href = "https://rpubs.com/greermp/Regard_by_location"),
        sliderInput("age",
                    "Vehicle Age Segment:",
                    min = 1995,
                    max = 2020,
                    value = 2010),
        h3("Stuff"),
        numericInput('marketing', 'Marketing per Customer/year ($)', 76, min = 1, max = 9),
        numericInput('margin', 'Sales Margin - Parts&Labor (%)', 64, min = 1, max = 99),
        numericInput('discount', 'Discount Rate (%)', 10, min = 1, max = 50),
        dateRangeInput("dates",'Customer Creation Date Range (CLV)', min=dates$EarliestCust, max=dates$LatestCust,
                       start="2018-01-01", end="2018-12-31"),
        hr()
        
    )# label = h3("Customer Cohort")
)


frow1 <- fluidRow(
    valueBoxOutput("value1")
    ,valueBoxOutput("value2")
    ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
    box(
        title = "Average Revenue per Visit"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("two_L", height = "300px")
    )
    ,box(
        title = "CLV per Customer Segment"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("two_R", height = "300px")
    ) 
)
frow3 <- fluidRow(
    column(12,
           box(width = 12,
               title="Customer Return Rate by Segment",
               sankeyNetworkOutput("three_L")))
)


# frow4 <- fluidRow(
#   title="Customer Return Rate by Segment",
#   # box(  
#   ("three_R")
#   # ,plotOutput("three_L", height = "300px")
#   # )
# )
# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'VTA Analysis', header, sidebar, body, skin='green')
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################

total.revenue <- as.numeric(round(invoice %>% summarise(sum(TOTAL_SALE_AMOUNT)), 2))
total.customers <- as.numeric(round(invoice %>% group_by(CUSTOMER_NUMBER) %>% select(CUSTOMER_NUMBER) %>% unique() %>% nrow(), 2))
customer.2018 <- as.numeric(round(invoice %>% filter(createYear == 2018) %>% group_by(CUSTOMER_NUMBER) %>% select(CUSTOMER_NUMBER) %>% unique() %>% nrow(), 2))

# input=tibble(age=2010,
#              marketing=76,
#              margin=64,
#              discount=10
# )

# create the server functions for the dashboard  
server <- function(input, output) { 
    #some data manipulation to derive the values of KPI boxes
    print("1")
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
        valueBox(
            paste0('$',formatC(as.numeric(total.revenue/1000000)))
            ,'Total Reveneue'
            ,icon = icon("money-bill-wave",lib='font-awesome')
            ,color = "blue")  
    })
    output$value2 <- renderValueBox({ 
        valueBox(
            formatC(as.numeric(total.customers), format="f", big.mark=',', drop0trailing = TRUE)
            ,'Total Unique Customers'
            ,icon = icon("oil-can",lib='font-awesome')
            ,color = "green")  
    })
    output$value3 <- renderValueBox({
        valueBox(
            formatC(customer.2018, format="f", big.mark=',', drop0trailing = TRUE)
            ,'New Customers in 2018'
            ,icon = icon("smile",lib='font-awesome')
            ,color = "yellow")   
    })
    #creating the plotOutput content
    print("2")
    output$two_L <- renderPlot({
        vehage <- invoice %>%
            mutate(factor=case_when(
                CAR_YEAR>=input$age ~ paste0(input$age,' or Newer'),
                TRUE ~ paste0('Older than ',input$age)
            )) %>% group_by(factor) %>%  summarise(AverageInvoice=mean(TOTAL_SALE_AMOUNT))
        
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
        
        
        print("4")
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
        vehage %>% group_by(factor, visit) %>% summarise(AvgVisitRev=mean(TOTAL_SALE_AMOUNT), customers=n()) %>%  arrange(desc(visit))
        
        clvAge <- vehage %>% group_by(factor, visit) %>% summarise(AvgVisitRev=mean(TOTAL_SALE_AMOUNT), customers=n()) %>% 
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
            theme_tufte() + scale_fill_brewer(palette = palette)
    })
    print("5")
    output$three_L <- renderSankeyNetwork({
        print("6")
        # h1("Hello")
        print("7")
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
            visit== 1 ~ "2018 Customers",
            visit== 2 & group=="factor1Sankey" ~ "Visit 1",
            visit== 2 & group=="factor2Sankey" ~ "Visit 1 ",
            visit== 3 & group=="factor1Sankey" ~ "Visit 2",
            visit== 3 & group=="factor2Sankey" ~ "Visit 2 ",
            visit== 4 & group=="factor1Sankey" ~ "Visit 3",
            visit== 4 & group=="factor2Sankey" ~ "Visit 3 ",
            visit== 5 & group=="factor1Sankey" ~ "Visit 4",
            visit== 5 & group=="factor2Sankey" ~ "Visit 4 ",
            visit== 6 & group=="factor1Sankey" ~ "Visit 5",
            visit== 6 & group=="factor2Sankey" ~ "Visit 5 ",
            TRUE ~ "GFY")) %>%
            mutate(target=case_when(
                source== "2018 Customers" & group=="factor2Sankey" ~ "Visit 1 ",
                source== "2018 Customers" & group=="factor1Sankey" ~ "Visit 1",
                source== "Visit 1 " ~ "Visit 2 ",
                source== "Visit 1" ~ "Visit 2",
                source== "Visit 2 " ~ "Visit 3 ",
                source== "Visit 2" ~ "Visit 3",
                source== "Visit 3 " ~ "Visit 4 ",
                source== "Visit 3" ~ "Visit 4",
                source== "Visit 4 " ~ "Visit 5 ",
                source== "Visit 4" ~ "Visit 5",
                source== "Visit 5 " ~ "Visit 6",
                source== "Visit 5" ~ "Visit 6",
                TRUE ~ "GFY"))
        
        
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
                           Value = "visits", NodeID = "name", NodeGroup = 'group',
                           sinksRight=FALSE, fontSize = fontSize, fontFamily = fontFamily, nodeWidth =nodeWidth, nodePadding = 40)
    })
    
    # output$three_R <- renderPlot({
    #   HTML(
    #     paste(
    #       h3("This is my app!"),'<br/>',
    #       h4("Download your data using the choose file button"),'<br/>',
    #       h4("Thank you for using the app!")
    #     )
    #   )
    # })
    
}




#run/call the shiny app
# shinyApp(ui, server)


# Run the application 
shinyApp(ui = ui, server = server)
