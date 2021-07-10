library(shinydashboard)
library(shiny)
library(shinythemes)
library(networkD3)
library(lubridate)
library(tidyverse)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
invoice <- read.csv("data/InvoiceSummary.csv")

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