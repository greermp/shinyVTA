#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# setwd("~/MSBA/shinyVTA")

library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Vehicle Age CLV"),

    # Sidebar with a slider input for number of bins
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
            plotOutput("distPlot"),
            plotOutput("revPlot")
        )
    )
))
