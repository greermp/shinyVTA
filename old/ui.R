
# setwd("~/MSBA/shinyVTA")

library(shiny)
library(shinythemes)
# library(leaflet)
library(networkD3)
shinyUI(fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),

    # Application title
    # navbarPage(
    #     "Virginia Tire and Auto",   
    #     tabPanel("CLV - Vehicle Age", "one"),
    #     tabPanel("VTA Locations", "two"),
    #     tabPanel("Customers", "three"),
    #     tabPanel("Vehicles", "four"),
    #     navbarMenu("Map",
    #                tabPanel("Average Revenue", "four-a"),
    #                tabPanel("Number of customers", "four-b"),
    #                tabPanel("Number of VINs", "four-c")
    #     )
    # ),
    titlePanel("Vehicle Age CLV"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("age",
                        "Vehicle Age Segment:",
                        min = 1995,
                        max = 2020,
                        value = 2010),
            numericInput('marketing', 'Marketing per Customer/year ($)', 76, min = 1, max = 9),
            numericInput('margin', 'Sales Margin - Parts&Labor (%)', 64, min = 1, max = 99),
            numericInput('discount', 'Discount Rate (%)', 10, min = 1, max = 50)
            
        ),

        # Show a plot of the generated distribution
        
        mainPanel(
            tabsetPanel(
                tabPanel("Customer Lifetime Value/Rev per Visit",
                    fluidRow(
                        plotOutput("distPlot")
                    ),
                    fluidRow(
                        plotOutput("revPlot"),
                    ),
                ),
                tabPanel("Customer Return Rate",
                     fluidRow(
                     sankeyNetworkOutput("plot")
                    )
                )#,
                # tabPanel("map",
                #          fluidRow(
                #              leafletOutput("map"),
                #              p()
                #          )
                # )
            )
        )
    )
))