
# setwd("~/MSBA/shinyVTA")

library(shiny)
library(shinythemes)
shinyUI(fluidPage(theme=shinytheme("cosmo"),

    # Application title
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
            plotOutput("distPlot"),
            plotOutput("revPlot")
        )
    )
))
