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
library(RColorBrewer)
library(tidyverse)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # setwd("~/MSBA/MOD5/shinyex/example") 
    invoice <- read.csv("data/InvoiceSummary.csv")
    # par(mar=c(3,4,2,2))
    # display.brewer.all()
    palette="Dark2"
    
    output$revPlot <- renderPlot({
        

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
            theme_tufte()+ scale_fill_brewer(palette = palette)
    })
    
    output$distPlot <- renderPlot({
        print(input$age)
      
        
        
        vehage <- invoice %>% 
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
            labs(x="", title="Customer Lifetime Value Analysis", y="USD") + 
            theme_tufte() + scale_fill_brewer(palette = palette)

    })

})
