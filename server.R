#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(ggthemes)
library(tidyverse)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # setwd("~/MSBA/MOD5/shinyex/example") 
    invoice <- read.csv("InvoiceSummary.csv")
    
    output$distPlot <- renderPlot({
        print(input$age)
        
        invoice$VIN_NUMBER[invoice$VIN_NUMBER=='NULL'] = NA
        invoice <- invoice %>% drop_na(CUST_CREATE_DATE, VIN_NUMBER)
        invoice=invoice %>% filter(CUST_CREATE_DATE >= '2018-01-01' & CUST_CREATE_DATE <= '2018-12-31')
        # vehage <- invoice %>% 
        #     mutate(factor=case_when(
        #         CAR_YEAR>=input$age ~ paste0(input$age,' or Newer'),
        #         TRUE ~ paste0('Older than ',input$age)
        #     )) %>% group_by(factor) %>%  summarise(AverageInvoice=mean(TOTAL_SALE_AMOUNT))
        # 
        # vehage %>% ggplot(aes(x=factor, y=AverageInvoice)) +
        #     geom_col() + 
        #     geom_text(nudge_y = 5,aes(x=factor, y=AverageInvoice, label=paste0(round(AverageInvoice,0),'$'))) +
        #     scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1, 
        #                                             prefix = "$", suffix = "",
        #                                             big.mark = ",", decimal.mark = ".")) +
        #     labs(x="", title="Older Vehicles More Profitable on Average")
        
        
        vehage <- invoice %>% 
            mutate(factor=case_when(
                CAR_YEAR>=input$age ~ paste0(input$age, ' or Newer'),
                TRUE ~ paste0('Older than ',input$age)
            )) %>% filter(visit<=6)
        
        marketingpp=38
        discount=1.1
        vehage %>% group_by(factor, visit) %>% summarise(AvgVisitRev=mean(TOTAL_SALE_AMOUNT), customers=n()) %>%  arrange(desc(visit))
        
        clvAge <- vehage %>% group_by(factor, visit) %>% summarise(AvgVisitRev=mean(TOTAL_SALE_AMOUNT), customers=n()) %>% 
            mutate(Attrition=customers/lag(customers)) %>% mutate(Attrition=case_when(is.na(Attrition)~1, TRUE ~ Attrition)) %>% 
            mutate(AttrMult=Attrition*lag(Attrition)) %>% 
            mutate(AttrMult=case_when(is.na(AttrMult)~1, TRUE ~ AttrMult)) %>% 
            rowwise() %>% mutate(num=AvgVisitRev-(AvgVisitRev*.34)-marketingpp)%>% 
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
            
        ggplot(p, aes(x=factor, y=CLV)) + geom_col() +
            scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1,
                                                    prefix = "$", suffix = "",
                                                    big.mark = ",", decimal.mark = ".")) +
            labs(x="", title="Customer Lifetime Value Analysis", y="USD") + 
            theme_tufte()
        # generate bins based on input$age from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$age + 1)
        # # 
        # # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
