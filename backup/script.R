setwd("C:/Users/admin/Desktop/一专毕设/Working/R")
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
source("func.R")
source("Cointegration.R")
source("PairTrading.R")
source("Signal.R")

traceback()
self <- list()
self$day <- double()    #当前日期
#self$price.pair <- NA  #配对价格
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

#self$in.sample <- NA   #测试集合
#self$out.sample <- NA  #验证集合

#status define
OPENED <- 0L
CLOSED <- 1L
TERMINAL <- 2L

self$assets.id = array(dim = 2,dimnames = c("Left","Right"))

init <- function(assets,bExploratio) {
  self$assets.id <<- assets
  date.duration <- c(as.POSIXct('2003-06-01'), as.POSIXct('2016-02-04'))
  print(assets)
  print(date.duration)
  
  #price.pair <- GetAssetsData('CFETS', self$assets.id, date.duration)
  price.pair <- GetAssetsData('markets', self$assets.id, date.duration)
  sample.num <- nrow(price.pair)
  in.sample <- price.pair[1:round(sample.num*2/3),]
  out.sample <- price.pair[(round(sample.num*2/3) -200):sample.num,]
  
  if (bExploratio) {
    self$sample <<- in.sample
  }
  else{
    self$sample <<- out.sample
  }
  self$sample <<- price.pair 
}

#初始化每次epeson
s0 <- function(beginDay)
{
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


# Inputs:
#   s: state, represented by an array of integers
#   s[1] 0
#   s[2] 0
#   s[3] == 1,end;
#   a: action
#    a[1] esimate period
#    a[2] entryZscore
#    a[3] stoplossZscore
#    a[4] trading window
# output:
# list of:
#   new state
#   reward

.step <- function(context, status, a)
{
  # if terminal state, exit gracefully
  if (status[1] == TERMINAL)
    return(list(context,status,0))
  
  new.s <- status
  num.days <- nrow(context$sample)
  if (context$day >= num.days) {
    new.s[1] <- TERMINAL
    return(list(context,status,0))
  }
  # both new state and reward will be returned

  reward <- 0L
  
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
      #browser()
      
      context$day <- context$day + 1
      status[1] <- CLOSED
      
      window.current <- context$day - openDay + 1
      effect.price <- context$sample[openDay:context$day, ]
      price.return <- na.omit(Return.calculate(effect.price))
      weights <- xts(matrix(rep(weight*c(-oldSignal, oldSignal), each = window.current), nrow = window.current), order.by = index(effect.price))
      portfolio.return <- Return.portfolio(price.return, weights = lag(weights), geometric = F)
      context$weight[index(weights),] <- weights
      reward <- SortinoRatio(portfolio.return) 
      if (reward[1] == Inf) {
        reward[1] = 0
      }
      reward <- reward[1] %>>% 
      atan  %>>%
      round(4)
      
      context$zscore.pre <- zScore.last
      
      return(list(context,new.s,reward))
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
  
  whole.price.return <- na.omit(Return.calculate(context$sample))
  whole.return <- Return.portfolio(whole.price.return, weights = lag(context$weight), geometric = F)
  tmp.table <- data.table(cbind(whole.return, year(index(whole.return)), month(index(whole.return))))
  names(tmp.table) <- c("portfolio.return", "year", "month")
  month.return <- tmp.table[,sum(portfolio.return), by=.(year, month)]
  month.timestamps <- as.Date(month.return[,paste(year, month, "01", sep="-")])
  whole.reward <-SortinoRatio(month.return[,V1])[1] %>>%
  atan %>>%
  round(4)
  #whole.reward <- round(SharpeRatio(xts(month.return[,V1], order.by = month.timestamps)), 4)
  cat("whole.reward:",whole.reward,"\r\n")
  return(list(context, new.s, whole.reward))
}


# Inputs:
#   s: state
#   s[0]:
#       0:OPENED
#       1:CLOSED
#       2:TERMINAL
#   a: action
#    a[1] esimate period
#    a[2] trading window
#    a[3] entryZscore
#    a[4] stoplossZscore
# output:
# list of:
#   new state
#   reward

step <- function(status, a)
{
  ret <- .step(self,status,a)
  #ret <- .steptest(self,status,a)
  # print(system.time(ret <- .step(self,status,a)))
  self <<- ret[[1]]
  return(c(ret[[2]][1],ret[[3]][1]))
}
.steptest <- function(context, status, a)
{
  # if terminal state, exit gracefully
  if (status[1] == TERMINAL)
    return(list(context,status,0))
  
  new.s <- status
  num.days <- nrow(context$sample)
  if (context$day >= num.days) {
    return(list(context,status,-1.0))
    
  }
  reward <- 0L
  
  oldSignal <- 0
  zScore.pre <- context$zscore.pre
  #parameter setting
  params.period <- a[1]
  params.tradingwindow <- a[2]
  params.entryZscore <- a[3]
  params.stoplossZscore <- a[4]
  params.exitZscore <- 0
  
  if (params.period < params.tradingwindow || params.entryZscore > params.stoplossZscore) {
    return(list(context,status,-1.0))
  }
  
  context$entry[index(context$sample[context$day])] <- params.entryZscore
  context$stoploss[index(context$sample[context$day])] <- params.stoplossZscore
  
  openDay <- 0
  while (context$day <= num.days) {
    if (!(context$day %% 300)) {
      context$day <- context$day + 1
      reward =log(params.period**2 + params.tradingwindow*params.entryZscore + sqrt(params.stoplossZscore))
      context$reward = reward + context$reward
      return(list(context, new.s, reward))
    
    }
    context$day <- context$day + 1
  }
  new.s[1] <- TERMINAL
  return(list(context, new.s, context$reward))
  
}
showDomain <- function(show = TRUE)
{
  if(show){
  price.return <- na.omit(Return.calculate(self$sample))
  portfolio.return <- Return.portfolio(price.return, weights = lag(self$weight), geometric = F)
  print(c(maxDrawdown=maxDrawdown(portfolio.return), SortinoRatio=SortinoRatio(portfolio.return), Return.annualized=Return.annualized(portfolio.return)))
  
  signals <- na.omit(na.locf(self$signals))
  PlotAssets(self$sample, self$assets.id)
  
  PlotWithSignalFloatEntry(self$zscore, signals, self$entry, 0, self$stoploss)
  # PlotWithSignal(self$zscore, signals, self$entry, 0, self$stoploss)
  
  price.return <- Return.calculate(self$sample)
  portfolio.return <- Return.portfolio(na.omit(price.return), weights = lag(self$weight), geometric = F)
  charts.PerformanceSummary(portfolio.return)
  
  PlotBetaAndPremium(self$beta, self$premium)
  }
  .save()
}
.save <- function()
{
  f_continue <- function(prompt = "Do you want to save the result? (y/n)") {
    return( TRUE )
    n <- readline(prompt=prompt)
    n <- as.character(n)
    if (n == 'y')
      return( TRUE )
    else
      return( FALSE )
  }
  if (f_continue()) {
    name <- paste(self$assets.id[1],self$assets.id[2],'_result.RData',sep = "")
    save(self, file=name)
  }
}
.load <- function(assets)
{
  name <- paste(assets[1],assets[2],'_result.RData',sep = "")
  load(name)
  return (self)
}

main <- function(){
  #browser()
  assets.id <- c('GZ3Y', 'GZ5Y')
  init(assets.id, TRUE)
  s0(200)
  status <- c(CLOSED)
  sample.entry <- rep(c(0.5, 0.6, 0.7, 1, 0.5, 0.6, 0.7, 0.5, 0.6, 0.7), 20)
  sample.stoploss <- rep(c(3.2, 2.9, 3, 3.5, 3.9, 4.9, 4.1, 3.1, 3.3, 3.4), 20)
  a <- c(200, 70, 0.5, 3.1)
  #trace(.step, exit = quote(if (reward[1] == Inf) browser()))
  Rprof("Rprof.log")
  while (status[1] != TERMINAL) {
    # performing step
    #a <- c(200, 150, sample.entry[randi(c(1,200), 1, 1)], sample.stoploss[randi(c(1,200), 1, 1)])
    
    tmp <- step(status, a)
    status <- tmp[1]
    r <- tmp[2]
    print(c(status[1],self$day - 1, self$signals[self$day - 1],self$zscore[self$day - 1],r))
  }
  Rprof(NULL)
  showDomain()
}
