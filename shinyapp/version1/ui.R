library(shiny)
library(formattable)
library(sparkline)
library(ggplot2)
library(dygraphs)
library(DT)
library(genalg)
library(animation)
library(caTools)
library(gganimate)
library(gapminder)
library(ggplot2)

allBonds <- c("GZ1Y", "GZ2Y","GZ3Y", "GZ5Y","GZ7Y","GZ10Y",
                "GK1Y", "GK2Y","GK3Y", "GK5Y","GK7Y","GK10Y",
                "FGK1Y", "FGK2Y","FGK3Y", "FGK5Y","FGK10Y",
                "AA1Y", "AA2Y","AA3Y", "AA5Y",
                "CAA1Y", "CAA2Y","CAA3Y", "CAA5Y",
                "AAA1Y", "AAA2Y","AAA3Y", "AAA5Y",
                "CAAA1Y", "CAAA2Y","CAAA3Y", "CAAA5Y")



ui <- shinyUI(fluidPage(
  tags$style(type='text/css',
             ".jqstooltip { position: absolute;left: 0px;top: 0px;visibility: hidden;background: rgb(0, 0, 0) transparent;background-color: rgba(0,0,0,0);filter:progid:DXImageTransform.Microsoft.gradient(startColorstr=#99000000, endColorstr=#99000000);-ms-filter: 'progid:DXImageTransform.Microsoft.gradient(startColorstr=#99000000, endColorstr=#99000000)';color: white;font: 10px arial, san serif;text-align: left;white-space: nowrap;padding: 5px;border: 0px solid white;z-index: 10000;}.jqsfield { color: red;font: 15px arial, san serif;text-align: left;}"
  ),
  titlePanel(h1("Pairs Trading and GA Optimization",align="center"),windowTitle ="Pairs Trading and GA Optimization" ),
    tabsetPanel(
        tabPanel('Cointegration Pairs Trading Model',
                 sidebarPanel(
                   width = 3,
                   dateRangeInput("date1_1","Date",min = "2003-06-01",max = "2015-06-01",start = "2003-06-01",end = "2015-06-01"),
                   
                   selectInput("bond1_1","Pair1",allBonds,selected = "GZ5Y"),
                   selectInput("bond1_2","Pair2",allBonds,selected = "GZ7Y"),
                   
                   sliderInput("num1_1","Estimate Period",min = 150,max = 250,step = 1,value = 200),
                   sliderInput("num1_2","Trading Window",min = 50,max = 150,step = 1, value = 100),
                   sliderInput("num1_3","Entry Zscore",min = 0.5,max = 3,step = 0.1, value = 0.5),
                   sliderInput("num1_4","Stoploss Zscore",min = 3.1,max = 5,step = 0.1, value = 3),
                   
                   actionButton("begin1_1", "Begin")
                 ),
                 
                 mainPanel(
                   conditionalPanel("output.type == 'input'",
                                    h2(textOutput('title1_1'),align = "center"),
                                    formattableOutput("table1_1",width = 800),
                                    h3("Historical Results",align = "center"),
                                    formattableOutput("table1_2",width = 800),
                                    dygraphOutput('graph1_1',width = 745),
                                    dygraphOutput('graph1_2',width = 800),
                                    dygraphOutput("graph1_3",width = 800),
                                    plotOutput("graph1_4",width = 745),
                                    plotOutput("graph1_5",width = 745)            
                   ),
                   conditionalPanel("output.type != 'input'",  
                     h2(textOutput('titleGA_1'),align = "center"),
                     formattableOutput("tableGA_1",width = 800),
                     h3("Historical Results",align = "center"),
                     formattableOutput("tableGA_2",width = 800),
                     dygraphOutput('graphGA_1'),
                     dygraphOutput('graphGA_2'),
                     dygraphOutput("graphGA_3"),
                     plotOutput("graphGA_4"),
                     plotOutput("graphGA_5")
                   )
                      
                 ),
                 h6(textOutput('type'),style="color:white")
                 
        ),
        
        tabPanel('Genetic Algorithm Model',
                 sidebarPanel(
                   width = 3,
                   dateRangeInput("date2_1","Date",min = "2003-06-01",max = "2015-06-01",start = "2013-06-01",end = "2015-06-01"),
                   
                   selectInput("bond2_1","Pair1",allBonds,selected = "GZ3Y"),
                   selectInput("bond2_2","Pair2",allBonds,selected = "GZ5Y"),
                   
                   sliderInput("num2_1","Estimate Period",min = 150,max = 250,step = 1,value = c(150,250)),
                   sliderInput("num2_3","Trading Window",min = 50,max = 150,step = 1, value = c(50,150)),
                   sliderInput("num2_5","Entry Zscore",min = 0.5,max = 3,step = 0.1,value = c(0.5,3)),
                   sliderInput("num2_7","Stoploss Zscore",min = 3.1,max = 5,step = 0.1, value = c(3.1,5)),
                   sliderInput("num2_9","Population Size",min = 5,max = 100,step = 1, value = 5),
                   sliderInput("num2_10","Iteration",min = 1,max = 100,step = 1, value = 5),
                   sliderInput("num2_11","Mutation Rate",min = 0,max = 1,step = 0.01, value = 0.2),
                   sliderInput("num2_12","Crossover Rate",min = 0,max = 1,step = 0.01, value = 0.8),
                   
                   actionButton("begin2_1", "Begin")
                 ),
                 mainPanel(
                   #imageOutput("graph2_1",width = "800px",height = "80%"),
                   #imageOutput("graph2_2",width = "800px",height = "80%"),
                   
                   fluidRow(column(5,imageOutput("graph2_1",width = "800px",height = "80%")), column(5,imageOutput("graph2_2",width = "800px",height = "80%"))),
                   dataTableOutput('table2_1'),
                   formattableOutput('table2_2')
                 )
        ),
        tabPanel('Genetic Algorithm Simulation',
                 sidebarPanel(
                   width = 3,
                   selectInput("bond3_1","Pair1",allBonds,selected = "GZ5Y"),
                   selectInput("bond3_2","Pair2",allBonds,selected = "GZ7Y"),
                   actionButton("begin3_1", "Show Training Summary"),
                   dateRangeInput("date3_1","Testing Set",min = "2006-06-01",max = "2015-06-01",start = "2006-06-01",end = "2015-06-01"),
                   sliderInput("num3_1","Testing Period",min = 2,max = 9,step = 1,value = 2),
                   actionButton("begin3_2", "Choose Training Set"),
                   actionButton("begin3_3", "Show Results")
                 ),
                 mainPanel(
                   h3(textOutput("title3_1"),align = "center"),
                   dataTableOutput('table3_1',width = 850),
                   h3(textOutput("title3_2"),align = "center"),
                   dataTableOutput('table3_2',width = 850),
                   h3(textOutput("title3_3"),align = "center"),
                   dataTableOutput('table3_3',width = 850),
                   h3(textOutput("title3_4"),align = "center"),
                   formattableOutput('table3_4',width = 850)
                   
                 )
        ),
        tabPanel('Test Page',
                 sidebarPanel(
                   width = 3,
                   actionButton("begin4_1", "Begin")
                 ),
                 mainPanel(
                   imageOutput("graph4_1")
                 )
        )
    
  )
  
))
