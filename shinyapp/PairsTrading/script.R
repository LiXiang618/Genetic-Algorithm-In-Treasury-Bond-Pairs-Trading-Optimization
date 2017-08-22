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
self$sample <- NA       #测试集
self$signals <- NA      #保存开仓信号
self$weight <- NA       #beta
self$zscore <- NA       #zscore

self$entry <- NA
self$stoploss <- NA

self$zscore.pre <- NA

self$beta <- NA
self$premium <- NA

#status define
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
  self <<- ret[[1]]
  return(c(ret[[2]][1]))
}

showDomain <- function(show = TRUE)
{

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
library(genalg)
evalFunc <- function(string=c()) {
  assets.id <- c('GZ5Y', 'GZ7Y')
  init(assets.id, TRUE,'2005-06-01','2014-06-01')
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
  #-(20*Return.annualized+10*sortinoRatio-maxDrawdown)
  return(-Return.annualized)

}

rbga.results <-  rbga(c(150, 50, 0.5, 3.1), c(250, 150, 3, 5),popSize = 12,iters = 40, 
                    evalFunc=evalFunc, verbose=TRUE, mutationChance=0.2)

main <- function(){
  #browser()
  assets.id <- c('GZ5Y', 'GZ7Y')
  init(assets.id, TRUE,'2005-06-01','2012-06-01')
  s0(200)
  #s0(217)
  status <- c(CLOSED)
  a <- c(200,70,0.5,3.1)
  #a <- c(217,200,1.5,3.1)
  #trace(.step, exit = quote(if (reward[1] == Inf) browser()))
  
  while (status[1] != TERMINAL) {
    tmp <- step(status, a)
    status <- tmp[1]
    print(c(status[1],self$day - 1, self$signals[self$day - 1],self$zscore[self$day - 1]))
  }
  showDomain()
}
