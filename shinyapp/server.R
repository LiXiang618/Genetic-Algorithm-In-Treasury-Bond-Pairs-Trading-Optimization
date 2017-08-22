setwd("C:/Users/admin/Desktop/一专毕设/Working/shinyapp")

load.library <- function(package_name) {
  package_name <- as.character(match.call()[[2]])
  if (!require(package_name, character.only = T)) {
    install.packages(package_name)
    require(package_name)
  }
}
####### FUNCTION DEFINITIONS #######

load.library("plyr")
load.library("foreach")
load.library('doParallel')
load.library("ggplot2")
load.library("tseries")
load.library("xts")
load.library("quantmod")
load.library("PerformanceAnalytics")
load.library("assertthat")
load.library("mongoquery")
load.library("data.table")
load.library("lubridate")
load.library("pipeR")
load.library("shinyIncubator")
load.library("genalg")
load.library("dygraphs")
load.library("mailR")
load.library("sparkline")
load.library("formattable")
load.library("lubridate")

source("./PairsTrading/func.R")
source("./PairsTrading/Cointegration.R")
source("./PairsTrading/PairsTrading.R")
source("./PairsTrading/Signal.R")

tags$script(src="shared/jquery.js", type="text/javascript")
tags$script(src="shared/shiny.js", type="text/javascript")
tags$link(rel="stylesheet", type="text/css", href="shared/shiny.css")
tags$link(rel="stylesheet", type="text/css", href="./bootstrap-3.3.5-dist/css/bootstrap-theme.css")
tags$link(rel="stylesheet", type="text/css", href="./bootstrap-3.3.5-dist/css/bootstrap-theme.min.css")
tags$link(rel="stylesheet", type="text/css", href="./bootstrap-3.3.5-dist/css/bootstrap.css")
tags$link(rel="stylesheet", type="text/css", href="./bootstrap-3.3.5-dist/css/bootstrap.min.css")
tags$link(rel="stylesheet", type="text/css", href="./bootstrap-3.3.5-dist/js/bootstrap.js")
tags$link(rel="stylesheet", type="text/css", href="./bootstrap-3.3.5-dist/js/bootstrap.min.js")
tags$link(rel="stylesheet", type="text/css", href="./bootstrap-3.3.5-dist/js/npm.js")

traceback()
self <- list()
self$day <- double()    #当前日期
self$sample <- NA       #测试集
self$signals <- NA      #保存开仓信号
self$weight <- NA       #beta
self$zscore <- NA       #zscore

self$entry <- NA
self$stoploss <- NA

self$zscore.pre <- NA

self$beta <- NA
self$premium <- NA
self$reward <- 0

OPENED <- 0L
CLOSED <- 1L
TERMINAL <- 2L

self$assets.id = array(dim = 2,dimnames = c("Left","Right"))

init <- function(assets,bExploratio,date1,date2) {
  self$assets.id <<- assets
  date.duration <- c(as.POSIXct(date1), as.POSIXct(date2))
  print(assets)
  print(date.duration)
  price.pair <- GetAssetsData('markets', self$assets.id, date.duration)
  self$sample <<- price.pair 
}

#初始化
s0 <- function(beginDay)
{
  self$zscore.pre <<- NA
  
  self$day <<- beginDay
  self$signals <<-
    xts(array(NA, dim = nrow(self$sample)), order.by = index(self$sample))
  self$zscore <<-
    xts(array(NA, dim = nrow((self$sample))), order.by = index(self$sample))
  num.price <- nrow(self$sample)
  self$weight <<-
    xts(data.frame(p1 = rep(0, num.price), p2 = rep(0, num.price)), order.by = index(self$sample))
  
  self$entry <<- xts(array(NA, dim = num.price), order.by = index(self$sample))
  self$stoploss <<- xts(array(NA, dim = num.price), order.by = index(self$sample))
  
  self$beta <<- xts(array(NA, dim = num.price), order.by = index(self$sample))
  self$premium <<- xts(array(NA, dim = num.price), order.by = index(self$sample))
  self$reward <<- 0
}

.step <- function(context, status, a)
{
  # if terminal state, exit gracefully
  if (status[1] == TERMINAL)
    return(list(context,status,0))
  
  new.s <- status
  num.days <- nrow(context$sample)
  
  if(webPageChozen == 1)
    setProgress(value=context$day,message=paste0("Please wait ... ",round(context$day/num.days*100),"%"))
  
  if (context$day >= num.days) {
    new.s[1] <- TERMINAL
    status <- TERMINAL
    return(list(context,status,0))
  }
  
  oldSignal <- 0
  zScore.pre <- context$zscore.pre
  #parameter setting
  params.period <- a[1]
  params.tradingwindow <- a[2]
  params.entryZscore <- a[3]
  params.stoplossZscore <- a[4]
  params.exitZscore <- 0
  
  if (params.period < params.tradingwindow || params.entryZscore > params.stoplossZscore) {
    new.s[1] <- TERMINAL
    status <- TERMINAL
    return(list(context,status,-10))
  }
  
  context$entry[index(context$sample[context$day])] <- params.entryZscore
  context$stoploss[index(context$sample[context$day])] <- params.stoplossZscore
  
  openDay <- 0
  
  while (context$day <= num.days) {
    rolldayPrice <-
      context$sample[(context$day - params.period + 1):context$day,]
    reg <- EstimateParameters(rolldayPrice, method = lm)
    
    meanSpread <- mean(na.omit(reg$spread))
    stdSpread <- sd(na.omit(reg$spread))
    
    zScore <- (reg$spread - meanSpread) / stdSpread
    zScore.last <- xts::last(zScore)
    context$zscore[index(zScore.last)] <- zScore.last
    context$beta[index(zScore.last)] <- xts::last(reg$hedge.ratio)
    context$premium[index(zScore.last)] <- xts::last(reg$premium)
    
    signal <- DaySignal(zScore.last, zScore.pre, params.entryZscore, params.stoplossZscore)
    zScore.pre <- zScore.last
    # exceed trading window, close position
    if (oldSignal != 0 &&
        (is.na(signal) ||
         signal == oldSignal) &&
        (context$day - openDay) >= params.tradingwindow) {
      signal <- 0
    }
    
    if (!is.na(signal)) {
      #browser()
      context$signals[index(zScore.last)] <- signal
      
    }
    # open position
    # 3-19 NOTE: 0 1 -1, will miss middle 1
    if (!is.na(signal) && signal != oldSignal && signal != 0) {
      #browser()
      status[1] <- OPENED
      openDay <-  context$day
      weight <- HedgeRatio2Weight(reg$hedge.ratio)
    }
    # close position
    # 3-19 NOTE: 1 1 -1, will miss close position of long
    if (!is.na(signal) && signal != oldSignal && signal == 0) {
      
      context$day <- context$day + 1
      status[1] <- CLOSED
      
      window.current <- context$day - openDay + 1
      
      if(nrow(context$sample)<context$day)
        return(list(context,new.s))
      
      effect.price <- context$sample[openDay:context$day, ]
      
      weights <- xts(matrix(rep(weight*c(-oldSignal, oldSignal), each = window.current), nrow = window.current), order.by = index(effect.price))
      
      context$weight[index(weights),] <- weights
      
      context$zscore.pre <- zScore.last
      
      return(list(context,new.s))
    }
    if (!is.na(signal))
      oldSignal <- signal
    
    context$day <- context$day + 1
  }
  
  # at last close all position
  if (oldSignal != 0)
    context$signals[context$day - 1] <- 0
  new.s[1] <- TERMINAL
  # whole reward
  
  return(list(context, new.s))
}

step <- function(status, a)
{
  ret <- .step(self,status,a)
  self <<- ret[[1]]
  return(c(ret[[2]][1]))
}

webPageChozen <<- 1 

history.results <<- data.frame()

ga.pairs <<-c() 

ga.dates <<-c() 

ga.totalProgress <<- 1 

ga.tempProgress <<- 0

ga.monitorProgress <<- 1 

ga.output <<- c()

ga.generation <<- list()

simu.pairs <<-c()

simu.GAsummary <<-data.frame()

simu.selection <<-data.frame()

button3_1Pushed <<- FALSE

globeRowSelectedCheck <<- c()

evalFunc <- function(string=c()) {
  
  assets.id <- ga.pairs
  init(assets.id, TRUE,ga.dates[1],ga.dates[2])
  s0(round(string[1]))
  status <- c(CLOSED)
  a <- c(round(string[1]),round(string[2]),round(string[3],1),round(string[4],1))
  print(a)
  
  while (status[1] != TERMINAL) {
    tmp <- step(status, a)
    status <- tmp[1]
  }
  price.return <- na.omit(Return.calculate(self$sample))
  portfolio.return <- Return.portfolio(price.return, weights = lag(self$weight), geometric = F)
  
  maxDrawdown <- maxDrawdown(portfolio.return)
  sortinoRatio <- SortinoRatio(portfolio.return)
  Return.annualized <- Return.annualized(portfolio.return)
  print(c(maxDrawdown,sortinoRatio ,Return.annualized ))
  
  if(webPageChozen == 2)
  {
    setProgress(value=ga.tempProgress+ga.monitorProgress,
                message=paste0("Please wait ............ ",round((ga.tempProgress+ga.monitorProgress)/ga.totalProgress*100),"% ............ "),
                detail=ga.output)
    ga.tempProgress <<- 1 + ga.tempProgress
  }
  
  return(-Return.annualized)
  
}

monitor <- function(obj) {
  if(obj$iter==1)
    ga.generation <<- list()
  ga.monitorProgress <<- obj$iter*obj$popSize
  ga.tempProgress <<- 0
  ga.output <<- paste0("Iter: ",obj$iter,",\nBest: ",round(max(-obj$best,na.rm = TRUE),4),",\nMean: ",round(mean(-obj$mean,na.rm = TRUE),4))
  ga.generation[[obj$iter]] <<- obj$population
  print(print(obj))
}

shinyServer(function(input, output, session) {

  observeEvent(input$begin1_1, {
    output$type <- renderText("input")
    webPageChozen <<- 1 
    
    date1 <- as.character(input$date1_1[1])
    date2 <- as.character(input$date1_1[2])
    
    bond1 <- input$bond1_1
    bond2 <- input$bond1_2
    
    num1 <- input$num1_1
    num2 <- input$num1_2
    num3 <- input$num1_3
    num4 <- input$num1_4
    
    assets.id <- c(bond1, bond2)
    

    init(assets.id, TRUE,date1,date2)
    s0(num1)
    status <- c(CLOSED)
    a <- c(num1,num2,num3,num4)
      
    withProgress(session, min = num1, max = nrow(self$sample), {
      setProgress(message = "Please wait ...")

      while (status[1] != TERMINAL) {
        tmp <- step(status, a)
        status <- tmp[1]
      } 
      
      setProgress(value=nrow(self$sample-1),message="Prepare results ... ")
      
      price.return <- na.omit(Return.calculate(self$sample))
      portfolio.return <- Return.portfolio(price.return, weights = lag(self$weight), geometric = F)
      
      output$title1_1 <- renderText(paste0(bond1,"-",bond2," From ",date1," To ",date2))
      
      df <- data.frame(Pairs = paste(bond1,bond2,sep = "-"),
                       From = as.character(date1),
                       To = as.character(date2),
                       EW = num1,
                       TW = num2,
                       EZ = num3,
                       SZ = num4,
                       MD = adRound(maxDrawdown(portfolio.return),4),
                       SR = adRound(as.numeric(SortinoRatio(portfolio.return)),4),
                       AR = adRound(as.numeric(Return.annualized(portfolio.return)),4))
      
      history.results <<- rbind(df,history.results)
      
      output$table1_1 <- renderFormattable(
        formattable(df, list(
          AR = formatter("span",style = x ~ style(color = ifelse(x<0.01, ifelse(x<0,"green","orange"), "red"),font.weight = "bold")),
          MD = color_tile("white", "lightgreen"),
          SR = color_tile("white", "lightpink")
        ),
        align="c"
        )
      )
      
      output$table1_2 <- renderFormattable(
        formattable(history.results, list(
          AR = formatter("span",style = x ~ style(color = ifelse(x<0.01, ifelse(x<0,"green","orange"), "red"),font.weight = "bold")),
          MD = color_tile("white", "lightgreen"),
          SR = color_tile("white", "lightpink")
        ),
        align="c"
        )
      )

      output$graph1_1 <- renderDygraph({
        PlotAssetsDy(self$sample, self$assets.id)
      })
      
      output$graph1_2 <- renderDygraph({
        PlotSpreadDy(self$sample, self$assets.id)
      })

      output$graph1_3 <- renderDygraph({
        PlotBetaAndPremiumDy(self$beta, self$premium)
      })
      
      signals <- na.omit(na.locf(self$signals))
      output$graph1_4 <- renderPlot({
        PlotWithSignalFloatEntry(self$zscore, signals, self$entry, 0, self$stoploss)
      })
      price.return <- Return.calculate(self$sample)
      portfolio.return <- Return.portfolio(na.omit(price.return), weights = lag(self$weight), geometric = F)
      output$graph1_5 <- renderPlot({
        charts.PerformanceSummary(portfolio.return)
      })
      setProgress(value=nrow(self$sample),message="Prepare results ... ")
      
    })
    
  })

  observeEvent(input$begin2_1, {
    output$type <- renderText("input")
    webPageChozen <<- 2 
    
    date1 <- input$date2_1[1]
    date2 <- input$date2_1[2]
    
    ga.dates <<- c(date1,date2)
    
    bond1 <- input$bond2_1
    bond2 <- input$bond2_2
    
    ga.pairs <<- c(bond1,bond2)
    
    num1 <- input$num2_1[1]
    num2 <- input$num2_1[2]
    num3 <- input$num2_3[1]
    num4 <- input$num2_3[2]
    num5 <- input$num2_5[1]
    num6 <- input$num2_5[2]
    num7 <- input$num2_7[1]
    num8 <- input$num2_7[2]
    num9 <- input$num2_9
    num10 <- input$num2_10
    num11 <- input$num2_11
    num12 <- input$num2_12
    
    ga.totalProgress <<- num9*num10 
    
    ga.tempProgress <<- 0
    
    ga.monitorProgress <<- 1
    
    ga.output <<- c()
    
    withProgress(session, min = 0, max = num9*num10, {
      setProgress(message = "Please wait ............ ")
      
      beginTime <- Sys.time()
      
      rbga.results <-  rbga(c(num1, num3, num5, num7), c(num2, num4, num6, num8),popSize = num9,iters = num10,
                            evalFunc=evalFunc,monitorFunc=monitor, verbose=TRUE, mutationChance=num11,elitism = adRound((1-num12)*num9))
      
      endTime <- Sys.time()
      
      df1 <- as.data.frame(rbga.results$population)
      names(df1) <- c("Estimate Period","Trading Window","Entry Zscore","Stoploss Zscore")
      df1[,1] <- adRound(df1[,1])
      df1[,2] <- adRound(df1[,2])
      df1[,3] <- adRound(df1[,3],1)
      df1[,4] <- adRound(df1[,4],1)
      df1 <- cbind(Individual = c(1:nrow(df1)),df1,Evaluations = -adRound(rbga.results$evaluations,4))
      tmpdf <- df1
      output$table2_1 <- renderDataTable(
        tmpdf,
        options = list(
          columnDefs = list(
            list(visible = FALSE, targets = c(0)),
            list(className = 'details-control', targets = 1)
          ),
          lengthMenu = c(10, 20, 50)
        ),
        callback = JS("
                      table.on('mouseover', 'td.details-control', function() {                      
                          $(this).css('cursor','pointer');
                      });
                      table.on('click', 'td.details-control', function() {
                          var td = $(this), row = table.row(td.closest('tr'));
                          tmpWindow = window.open('http://127.0.0.1:8080/?paras='+row.data()[2]+'_'+row.data()[3]+'_'+row.data()[4]+'_'+row.data()[5])
                      });"
          )
      )
      
      write.csv(df1,"df1.csv",row.names = FALSE)
      df1 <- df1[,-1]
      names(df1) <- c("EW","TW","EZ","SZ")
      write.csv(df1,paste0("./GAResults/",bond1,"_",bond2,"_",date1,"-",date2,".csv"),row.names = TRUE)
        
      df3 <- data.frame(best = -rbga.results$best, mean = -rbga.results$mean)
      df3[,1] <- adRound(df3[,1],4)
      df3[,2] <- adRound(df3[,2],4)
      df3 <- cbind(Iteration = 1:nrow(df3),df3)
      
      
      output$table2_2 <- renderFormattable(
        formattable(df3, list(
          best = color_tile("white", "lightpink"),
          mean = color_tile("white", "lightpink")
        ),
        align="c"
        )
      )
      
      write.csv(df3,"df3.csv",row.names = FALSE)
      
      animate_plot1 <- function(){
        for (i in seq(1, num10)) 
        {
          temp <- data.frame(Generation = c(seq(1, i), seq(1, i)), Variable = c(rep("mean", i), rep("best", i)), Survivalpoints = c(-rbga.results$mean[1:i], -rbga.results$best[1:i]))
          pl <- ggplot(temp,aes(x = Generation, y = Survivalpoints, group = Variable, colour = Variable)) + 
            geom_line() + 
            scale_x_continuous(limits = c(0, num10)) + 
            scale_y_continuous(limits = c(-0.05, 0.15)) + 
            geom_hline(yintercept = max(temp$Survivalpoints), lty = 2) + 
            annotate("text", x = 1, y = max(temp$Survivalpoints) + 2, hjust = 0, size = 3, color = "black", label = paste("Best solution:", max(temp$Survivalpoints))) + 
            scale_colour_brewer(palette = "Set1") + 
            ggtitle("Best and Mean performance during evolution") + theme(plot.title = element_text(lineheight=1,face="bold",hjust=0.2))
          print(pl)
        }
      }
      
      saveGIF(animate_plot1(),interval = 0.5, outdir = getwd(),movie.name = paste0(bond1,"_",bond2,"_",date1,"-",date2,"_1.gif"))
      
      output$graph2_1<- renderImage({
        list(src = paste0(bond1,"_",bond2,"_",date1,"-",date2,"_1.gif"),
             contentType = 'image/gif'
        )
      },deleteFile = FALSE)
      
      animate_plot2 <- function(){
        for (i in seq(1, num10)) 
        {
          matrix <- ga.generation[[i]]
          
          range <- c(num1,num2,num3,num4,num5,num6,num7,num8)
          for(j in 1:ncol(matrix))
          {
            matrix[,j] <- 1+(matrix[,j]-range[2*j-1])/(range[2*j]-range[2*j-1])*(9-1)
          }

          size <- sapply(1:length(matrix[,1]), 
                         function(j) { sum(matrix[,1]==matrix[,1][j] & matrix[,2]==matrix[,2][j] &  
                                             matrix[,3]==matrix[,3][j] &  matrix[,4]==matrix[,4][j]) })
          
          colnames(matrix) <- c("EW","TW","EZ","SZ")
          pl <- pairs(matrix,cex = size,main=paste0("Relationships among variables during evalution","(iter",i,")"),
                      xlim=c(0,10),ylim=c(0,10))
          print(pl)
        }
      }

      saveGIF(animate_plot2(),interval = 0.5, outdir = getwd(),movie.name = paste0(bond1,"_",bond2,"_",date1,"-",date2,"_2.gif"))

      output$graph2_2<- renderImage({
        list(src = paste0(bond1,"_",bond2,"_",date1,"-",date2,"_2.gif"),
             contentType = 'image/gif'
        )
      },deleteFile = FALSE)

      
      send.mail(from = "<lx_19930618@163.com>",
                to = "<lx_19930618@163.com>",
                subject = "GA Results",
                body = paste0("Pairs: ",bond1,"-",bond2," .\n",
                              "From ",date1," to ",date2," .\n",
                              "Time counsumed is ",adRound(as.numeric(difftime(endTime, beginTime,units = "mins")),2)," mins.\n",
                              "Attached are the results of GA."),
                attach.files = c(paste0(bond1,"_",bond2,"_",date1,"-",date2,"_1.gif"),
                                 paste0(bond1,"_",bond2,"_",date1,"-",date2,"_2.gif"),"df1.csv", "df3.csv"),
                smtp = list(host.name = "smtp.163.com", port = 465, user.name = "LX_19930618", passwd = "19930618_LX", ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
      
      
      setProgress(value=num9*num10,message="Prepare results ... ")
    
    })
    
    
  })
  
  observeEvent(input$begin3_1,{
    button3_1Pushed <<- TRUE

    bond1 <- input$bond3_1
    bond2 <- input$bond3_2
    
    simu.pairs <<- c(bond1,bond2)
    
    allFileNames <- list.files("./GAResults/")
    allFileNames <- allFileNames[which(substr(allFileNames,1,9) == paste(bond1,bond2,sep="_"))]
    
    df1 <- data.frame()
    for(i in 1:length(allFileNames))
    {
      tmp <- read.csv(paste0("./GAResults/",allFileNames[i]))
      tmp <- tmp[,-1]
      ew <- gsub(" ","",toString(adRound(tmp[,1],4)))
      tw <- gsub(" ","",toString(adRound(tmp[,2],4)))
      ez <- gsub(" ","",toString(adRound(tmp[,3],4)*100))
      sz <- gsub(" ","",toString(adRound(tmp[,4],4)*100))
      
      tmpdf <- data.frame(From=substr(allFileNames[i],11,20),To=substr(allFileNames[i],22,31),EW=ew,TW=tw,EZ=ez,SZ=sz)
      df1 <- rbind(df1,tmpdf)
    }
    names(df1)[5] <- paste0(names(df1)[5],"(1e-4)")
    names(df1)[6] <- paste0(names(df1)[6],"(1e-4)")
    
    cd <- list(list(targets = 2, render = JS("function(data, type, full){ return '<span class=class1>' + data + '</span>' }")),
               list(targets = 3, render = JS("function(data, type, full){ return '<span class=class2>' + data + '</span>' }")),
               list(targets = 4, render = JS("function(data, type, full){ return '<span class=class3>' + data + '</span>' }")),
               list(targets = 5, render = JS("function(data, type, full){ return '<span class=class4>' + data + '</span>' }")))
    box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', 		medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"

    cb = JS(paste0("function (oSettings, json) {\n  $('.class1:not(:has(canvas))').sparkline('html', { ",box_string,", chartRangeMin: ", 150, ", chartRangeMax: ", 250,  " });\n
                                                    $('.class2:not(:has(canvas))').sparkline('html', { ",box_string,", chartRangeMin: ", 50, ", chartRangeMax: ", 150,  " });\n
                                                    $('.class3:not(:has(canvas))').sparkline('html', { ",box_string,", chartRangeMin: ", 50, ", chartRangeMax: ", 300,  " });\n  
                                                    $('.class4:not(:has(canvas))').sparkline('html', { ",box_string,", chartRangeMin: ", 300, ", chartRangeMax: ", 500,  " });\n}"), collapse = "")
    
    dt <- datatable(data.table(df1), rownames = FALSE,filter = 'top', options = list(columnDefs = cd, fnDrawCallback = cb))
    dt$dependencies <- append(dt$dependencies, htmlwidgets:::getDependency("sparkline"))
    
    output$table3_1 <- renderDataTable(
      dt,
      filter='top',
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(0)),
          list(className = 'details-control', targets = 1)
        ),
        lengthMenu = c(10, 20, 50)
      )
    )
    
    output$title3_1 <- renderText("Available GA Summary")
    
    df2 <- read.table(paste0("./GAResults/TrainingResultsSummary",bond1,"_",bond2,".csv"),sep=",",header = TRUE)
    
    df2[,3] <- adRound(df2[,3])
    df2[,4] <- adRound(df2[,4])
    df2[,5] <- adRound(df2[,5],1)
    df2[,6] <- adRound(df2[,6],1)
    df2[,7] <- adRound(df2[,7],4)
    df2[,8] <- adRound(df2[,8],4)
    df2[,9] <- adRound(df2[,9],4)
    
    simu.GAsummary <<- df2
    
    output$table3_2 <- renderDataTable(
      df2,
      filter='top',
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(0)),
          list(className = 'details-control', targets = c(1,2))
        ),
        lengthMenu = c(10, 20, 50)
      ),
      callback = JS("
                    table.on('mouseover', 'td.details-control', function() {                      
                    $(this).css('cursor','pointer');
                    });
                    table.on('click', 'td.details-control', function() {
                    var td = $(this), row = table.row(td.closest('tr'));
                    tmpWindow = window.open('http://127.0.0.1:8080/?paras2='+row.data()[1]+'_'+row.data()[2]+'_'+row.data()[3]+'_'+row.data()[4]+'_'+row.data()[5]+'_'+row.data()[6])
                    });"
          )
    )
    output$title3_2 <- renderText("Available Training Set")
    
    output$title3_3 <- renderText("")
    output$title3_4 <- renderText("") 
    simu.selection <<- data.frame()
    output$table3_3 <- renderDataTable(
      simu.selection,
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(0))
        )
      )
    )
    
  })

  observeEvent(input$begin3_2,{
    if(button3_1Pushed)
    {
      date1 <- input$date3_1[1] 
      date2 <- input$date3_1[2] 
      
      stepYear <- input$num3_1
      datePoint <- seq.Date(date1,date2,"years")
      datePoint1 <- datePoint[1:(length(datePoint)-stepYear)]
      datePoint2 <- datePoint[(1+stepYear):length(datePoint)]
      testPeriod <- paste(as.character(datePoint1),as.character(datePoint2),sep = "~")
      
      output$title3_3 <- renderText("Chosen Training Set")
      simu.selection <<- data.frame(TestingPeriod = as.character(testPeriod))
      output$table3_3 <- renderDataTable(
        simu.selection,
        options = list(
          columnDefs = list(
            list(visible = FALSE, targets = c(0))
            #list(className = 'details-control', targets = c(1))
          )
        )
      )
      
      df <- simu.GAsummary[which(year(simu.GAsummary$To) <= year(datePoint1[1])+1),]
      output$table3_2 <- renderDataTable(
        df,
        filter='top',
        options = list(
          columnDefs = list(
            list(visible = FALSE, targets = c(0)),
            list(className = 'details-control', targets = c(1,2))
          ),
          lengthMenu = c(10, 20, 50)
        ),
        callback = JS("
                      table.on('mouseover', 'td.details-control', function() {                      
                      $(this).css('cursor','pointer');
                      });
                      table.on('click', 'td.details-control', function() {
                      var td = $(this), row = table.row(td.closest('tr'));
                      tmpWindow = window.open('http://127.0.0.1:8080/?paras2='+row.data()[1]+'_'+row.data()[2]+'_'+row.data()[3]+'_'+row.data()[4]+'_'+row.data()[5]+'_'+row.data()[6])
                      });"
          )
        )
      output$title3_2 <- renderText("Available Training Set")
      
      globeRowSelectedCheck<<-c()
    }
    
  })
  
  observeEvent(input$begin3_3,{

    if(nrow(simu.selection)==length(globeRowSelectedCheck))
    {
      webPageChozen <<- 3
      withProgress(session, min = 0, max = nrow(simu.selection)+1, {
        setProgress(message = "Please wait ...")
        summaryTable <- data.frame()
        for(i in 1:nrow(simu.selection))
        {
          date1 <- unlist(strsplit(simu.selection[i,1],split = "~"))[1]
          date2 <- unlist(strsplit(simu.selection[i,1],split = "~"))[2]
          
          bond1 <- simu.pairs[1]
          bond2 <- simu.pairs[2]
          
          num1 <- as.numeric(simu.selection[i,4])
          num2 <- as.numeric(simu.selection[i,5])
          num3 <- as.numeric(simu.selection[i,6])
          num4 <- as.numeric(simu.selection[i,7])
          
          assets.id <- c(bond1, bond2)
          init(assets.id, TRUE,date1,date2)
          s0(num1)
          status <- c(CLOSED)
          a <- c(num1,num2,num3,num4)
          
          while (status[1] != TERMINAL) {
            tmp <- step(status, a)
            status <- tmp[1]
          } 
          price.return <- na.omit(Return.calculate(self$sample))
          portfolio.return <- Return.portfolio(price.return, weights = lag(self$weight), geometric = F)
          
          df <- data.frame(Pairs = paste(bond1,bond2,sep = "-"),
                           From = as.character(date1),
                           To = as.character(date2),
                           EW = num1,
                           TW = num2,
                           EZ = num3,
                           SZ = num4,
                           MD = adRound(maxDrawdown(portfolio.return),4),
                           SR = adRound(as.numeric(SortinoRatio(portfolio.return)),4),
                           AR = adRound(as.numeric(Return.annualized(portfolio.return)),4))
          summaryTable <- rbind(summaryTable,df)
          setProgress(value=i,message=paste0("Please wait ... ",round(i/(nrow(simu.selection)+1)*100),"%"))
        }
          
        output$title3_4 <- renderText("Simulation Result Summary")
        output$table3_4 <- renderFormattable(
          formattable(summaryTable, list(
            AR = formatter("span",style = x ~ style(color = ifelse(x<0.01, ifelse(x<0,"green","orange"), "red"),font.weight = "bold")),
            MD = color_tile("white", "lightgreen"),
            SR = color_tile("white", "lightpink")
          ),
          align="c"
          )
        )
        
        output$table3_3 <- renderDataTable(
          simu.selection,
          options = list(
            columnDefs = list(
              list(visible = FALSE, targets = c(0)),
              list(className = 'details-control', targets = 1)
            )
          ),
          callback = JS("
                        table.on('mouseover', 'td.details-control', function() {                      
                          $(this).css('cursor','pointer');
                        });
                        table.on('click', 'td.details-control', function() {
                        var td = $(this), row = table.row(td.closest('tr'));
                        tmpWindow = window.open('http://127.0.0.1:8080/?paras3='+row.data()[1]+'_'+row.data()[4]+'_'+row.data()[5]+'_'+row.data()[6]+'_'+row.data()[7])
                        });"
          )
        )
        
        setProgress(value=nrow(simu.selection)+1,message="Prepare Results ... 100%")
        
      })
    }
  })
  
  observe({

    table3_2selectedRow <- c()
    
    if(!is.null(input$table3_2_rows_selected))
      table3_2selectedRow <- as.numeric(input$table3_2_rows_selected)
    
    datePoint1 <- 0

    if(length(table3_2selectedRow)!=0&length(globeRowSelectedCheck)<nrow(simu.selection))
    {
      
      globeRowSelectedCheck <<- c(globeRowSelectedCheck,table3_2selectedRow)
      
      datePoint1 <- as.numeric(substr(simu.selection[length(globeRowSelectedCheck)+1,1],1,4))
      
      if(is.na(datePoint1))
      {
        df <- simu.GAsummary
      }else{
        df <- simu.GAsummary[which(year(simu.GAsummary$To) <= datePoint1+1),]
      }
      
      output$table3_2 <- renderDataTable(
        df,
        filter='top',
        options = list(
          columnDefs = list(
            list(visible = FALSE, targets = c(0)),
            list(className = 'details-control', targets = c(1,2))
          ),
          lengthMenu = c(10, 20, 50)
        ),
        callback = JS("
                      table.on('mouseover', 'td.details-control', function() {                      
                      $(this).css('cursor','pointer');
                      });
                      table.on('click', 'td.details-control', function() {
                      var td = $(this), row = table.row(td.closest('tr'));
                      tmpWindow = window.open('http://127.0.0.1:8080/?paras2='+row.data()[1]+'_'+row.data()[2]+'_'+row.data()[3]+'_'+row.data()[4]+'_'+row.data()[5]+'_'+row.data()[6])
                      });"
          )
        )
      output$title3_2 <- renderText("Available Training Set")
      
      output$title3_3 <- renderText("Chosen Training Set")

      tmpdf <- simu.GAsummary[globeRowSelectedCheck,]
      tmpdf[,1] <- as.character(tmpdf[,1])
      tmpdf[,2] <- as.character(tmpdf[,2])
      
      while(nrow(tmpdf)<=nrow(simu.selection))
      {
        tmpdf <- rbind(tmpdf,c('None','None',0,0,0,0,0,0,0))
      }
      
      if(nrow(tmpdf)>nrow(simu.selection))
      {
        tmpdf <- tmpdf[-nrow(tmpdf),]
      }
      
      simu.selection <<- cbind(simu.selection[,1],tmpdf)
      
      simu.selection[,1] <<- as.character(simu.selection[,1])

      names(simu.selection)<<-c("Testing Period","Training From","Training To","EW","TW","EZ","SZ","MD","SR","AR")
      
      #output$title3_3 <- renderText("Chosen Training Set")
      
      output$table3_3 <- renderDataTable(
        simu.selection,
        options = list(
          columnDefs = list(
            list(visible = FALSE, targets = c(0))
          )
        )
      )
    }
    
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['paras']])){
      output$type <- renderText("graph")
      para1<-unlist(strsplit(query[['paras']],"_"))[1]
      para2<-unlist(strsplit(query[['paras']],"_"))[2]
      para3<-unlist(strsplit(query[['paras']],"_"))[3]
      para4<-unlist(strsplit(query[['paras']],"_"))[4]
      
      date1 <- ga.dates[1]
      date2 <- ga.dates[2]
      
      bond1 <- ga.pairs[1]
      bond2 <- ga.pairs[2]
      
      num1 <- as.numeric(para1)
      num2 <- as.numeric(para2)
      num3 <- as.numeric(para3)
      num4 <- as.numeric(para4)
      
      assets.id <- c(bond1, bond2)
      init(assets.id, TRUE,date1,date2)
      s0(num1)
      status <- c(CLOSED)
      a <- c(num1,num2,num3,num4)
      
      webPageChozen <<- 1 

      withProgress(session, min = num1, max = nrow(self$sample), {
        setProgress(message = "Please wait ...")
        
        while (status[1] != TERMINAL) {
          tmp <- step(status, a)
          status <- tmp[1]
        } 
        
        setProgress(value=nrow(self$sample-1),message="Prepare results ... ")
        
        price.return <- na.omit(Return.calculate(self$sample))
        portfolio.return <- Return.portfolio(price.return, weights = lag(self$weight), geometric = F)
        
        output$titleGA_1 <- renderText(paste0(bond1,"-",bond2," From ",date1," To ",date2))
        
        df <- data.frame(Pairs = paste(bond1,bond2,sep = "-"),
                         From = as.character(date1),
                         To = as.character(date2),
                         EW = num1,
                         TW = num2,
                         EZ = num3,
                         SZ = num4,
                         MD = adRound(maxDrawdown(portfolio.return),4),
                         SR = adRound(as.numeric(SortinoRatio(portfolio.return)),4),
                         AR = adRound(as.numeric(Return.annualized(portfolio.return)),4))
        
        history.results <<- rbind(df,history.results)
        
        output$tableGA_1 <- renderFormattable(
          formattable(df, list(
            AR = formatter("span",style = x ~ style(color = ifelse(x<0.01, ifelse(x<0,"green","orange"), "red"),font.weight = "bold")),
            MD = color_tile("white", "lightgreen"),
            SR = color_tile("white", "lightpink")
          ),
          align="c"
          )
        )
        
        output$tableGA_2 <- renderFormattable(
          formattable(history.results, list(
            AR = formatter("span",style = x ~ style(color = ifelse(x<0.01, ifelse(x<0,"green","orange"), "red"),font.weight = "bold")),
            MD = color_tile("white", "lightgreen"),
            SR = color_tile("white", "lightpink")
          ),
          align="c"
          )
        )
        
        output$graphGA_1 <- renderDygraph({
          PlotAssetsDy(self$sample, self$assets.id)
        })
        
        output$graphGA_2 <- renderDygraph({
          PlotSpreadDy(self$sample, self$assets.id)
        })
        
        output$graphGA_3 <- renderDygraph({
          PlotBetaAndPremiumDy(self$beta, self$premium)
        })
        
        signals <- na.omit(na.locf(self$signals))
        output$graphGA_4 <- renderPlot({
          PlotWithSignalFloatEntry(self$zscore, signals, self$entry, 0, self$stoploss)
        })
        
        price.return <- Return.calculate(self$sample)
        portfolio.return <- Return.portfolio(na.omit(price.return), weights = lag(self$weight), geometric = F)
        output$graphGA_5 <- renderPlot({
          charts.PerformanceSummary(portfolio.return)
        })

        setProgress(value=nrow(self$sample),message="Prepare results ... ")
        
      })

    }
    if(!is.null(query[['paras2']])|!is.null(query[['paras3']])){
      output$type <- renderText("graph")
      date1 <- c()
      date2 <- c()
      para1 <- c()
      para2 <- c()
      para3 <- c()
      para4 <- c()
      if(!is.null(query[['paras2']]))
      {
        date1 <- unlist(strsplit(query[['paras2']],"_"))[1]
        date2 <- unlist(strsplit(query[['paras2']],"_"))[2]
        
        para1<-unlist(strsplit(query[['paras2']],"_"))[3]
        para2<-unlist(strsplit(query[['paras2']],"_"))[4]
        para3<-unlist(strsplit(query[['paras2']],"_"))[5]
        para4<-unlist(strsplit(query[['paras2']],"_"))[6]
      }else{
        date <- unlist(strsplit(query[['paras3']],"_"))[1]
        date1 <- unlist(strsplit(date,split = "~"))[1]
        date2 <- unlist(strsplit(date,split = "~"))[2]
        
        para1<-unlist(strsplit(query[['paras3']],"_"))[2]
        para2<-unlist(strsplit(query[['paras3']],"_"))[3]
        para3<-unlist(strsplit(query[['paras3']],"_"))[4]
        para4<-unlist(strsplit(query[['paras3']],"_"))[5]
      }
      
      bond1 <- simu.pairs[1]
      bond2 <- simu.pairs[2]
      
      num1 <- as.numeric(para1)
      num2 <- as.numeric(para2)
      num3 <- as.numeric(para3)
      num4 <- as.numeric(para4)
      
      assets.id <- c(bond1, bond2)

      init(assets.id, TRUE,date1,date2)
      s0(num1)
      status <- c(CLOSED)
      a <- c(num1,num2,num3,num4)
      
      webPageChozen <<- 1 
      
      withProgress(session, min = num1, max = nrow(self$sample), {
        setProgress(message = "Please wait ...")
        
        while (status[1] != TERMINAL) {
          tmp <- step(status, a)
          status <- tmp[1]
        } 
        
        setProgress(value=nrow(self$sample-1),message="Prepare results ... ")
        
        price.return <- na.omit(Return.calculate(self$sample))
        portfolio.return <- Return.portfolio(price.return, weights = lag(self$weight), geometric = F)
        
        output$titleGA_1 <- renderText(paste0(bond1,"-",bond2," From ",date1," To ",date2))
        
        df <- data.frame(Pairs = paste(bond1,bond2,sep = "-"),
                         From = as.character(date1),
                         To = as.character(date2),
                         EW = num1,
                         TW = num2,
                         EZ = num3,
                         SZ = num4,
                         MD = adRound(maxDrawdown(portfolio.return),4),
                         SR = adRound(as.numeric(SortinoRatio(portfolio.return)),4),
                         AR = adRound(as.numeric(Return.annualized(portfolio.return)),4))
        
        history.results <<- rbind(df,history.results)
        
        output$tableGA_1 <- renderFormattable(
          formattable(df, list(
            AR = formatter("span",style = x ~ style(color = ifelse(x<0.01, ifelse(x<0,"green","orange"), "red"),font.weight = "bold")),
            MD = color_tile("white", "lightgreen"),
            SR = color_tile("white", "lightpink")
          ),
          align="c"
          )
        )
        
        output$tableGA_2 <- renderFormattable(
          formattable(history.results, list(
            AR = formatter("span",style = x ~ style(color = ifelse(x<0.01, ifelse(x<0,"green","orange"), "red"),font.weight = "bold")),
            MD = color_tile("white", "lightgreen"),
            SR = color_tile("white", "lightpink")
          ),
          align="c"
          )
        )
        
        output$graphGA_1 <- renderDygraph({
          PlotAssetsDy(self$sample, self$assets.id)
        })
        
        output$graphGA_2 <- renderDygraph({
          PlotSpreadDy(self$sample, self$assets.id)
        })
        
        output$graphGA_3 <- renderDygraph({
          PlotBetaAndPremiumDy(self$beta, self$premium)
        })
        
        signals <- na.omit(na.locf(self$signals))
        output$graphGA_4 <- renderPlot({
          PlotWithSignalFloatEntry(self$zscore, signals, self$entry, 0, self$stoploss)
        })
        
        price.return <- Return.calculate(self$sample)
        portfolio.return <- Return.portfolio(na.omit(price.return), weights = lag(self$weight), geometric = F)
        output$graphGA_5 <- renderPlot({
          charts.PerformanceSummary(portfolio.return)
        })
        
        setProgress(value=nrow(self$sample),message="Prepare results ... ")
        
      })
      
      
      
    }
  })
  
})
