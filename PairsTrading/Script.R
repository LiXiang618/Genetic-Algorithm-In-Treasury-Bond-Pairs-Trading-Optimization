setwd("C:/Users/admin/Desktop/一专毕设/Working/PairsTrading")
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
load.library('quantmod')
load.library("PerformanceAnalytics")
load.library("assertthat")
load.library('mongoquery')
source("func.R")
source("Cointegration.R")
source("PairTrading.R")
source("Signal.R")

self <- list()
self$day <- double()    #当前日期
#self$price.pair <- NA  #配对价格
self$sample <- NA       #测试集
self$signals <- NA      #保存开仓信号
self$weight <- NA       #beta
self$zscore <- NA       #zscore
#self$in.sample <- NA   #测试集合
#self$out.sample <- NA  #验证集合

#status define
OPENED <- 0
CLOSED <- 1
TERMINAL <- 2

self$assets.id = array(dim = 2,dimnames = c("Left","Right"))

init <- function(assets,bExploratio) {
  self$assets.id <<- assets
  #date.duration <- c(as.POSIXct('2003-01-04'), as.POSIXct('2016-02-04'))
  date.duration <- c(as.POSIXct('2003-01-04'), as.POSIXct('2016-02-04'))
  
  print(assets)
  print(date.duration)
  
  #price.pair <- GetAssetsData('CFETS', self$assets.id, date.duration)
  price.pair <- GetAssetsData('markets', self$assets.id, date.duration)
  sample.num <- nrow(price.pair)
  in.sample <- price.pair[1:round(sample.num*2/3),]
  out.sample <- price.pair[(round(sample.num*2/3) + 1):sample.num,]
  
  if (bExploratio) {
    self$sample <<- in.sample
  }
  else{
    self$sample <<- out.sample
  }
  #self$sample <<- price.pair
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
  if (status[1] == OPENED)
    return(list(status,0))
  
  # both new state and reward will be returned
  zScore.pre <- 0
  new.s <- status
  reward <- 0L
  
  oldSignal <- 0
  
  #parameter setting
  params.period <- a[1]
  params.tradingwindow <- a[2]
  params.entryZscore <- a[3]
  params.stoplossZscore <- a[4]
  params.exitZscore <- 0
  
  openDay <- 0
  
  #lastday <- s[0]
  #setpDone <- F
  num.days <- nrow(context$sample)

  while (context$day <= num.days) {
    rolldayPrice <-
      context$sample[(context$day - params.period + 1):context$day,]
    reg <- EstimateParameters(rolldayPrice, method = lm)
    #browser()
    meanSpread <- mean(na.omit(reg$spread))
    stdSpread <- sd(na.omit(reg$spread))
    
    zScore <- (reg$spread - meanSpread) / stdSpread
    zScore.last <- last(zScore)
    context$zscore[index(zScore.last)] <- zScore.last
    
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
    if (!is.na(signal) && signal != oldSignal && signal != 0) {
      #browser()
      status[1] <- OPENED
      openDay <-  context$day
      weight <- HedgeRatio2Weight(reg$hedge.ratio)
    }
    if (!is.na(signal) && signal != oldSignal && signal == 0) {
      #browser()
      context$day <- context$day + 1
      status[1] <- CLOSED
      # reward
      window.current <- context$day - openDay + 1
      effect.price <- context$sample[openDay:context$day, ]
      price.return <- na.omit(Return.calculate(effect.price))
      weights <- xts(matrix(rep(weight*c(-oldSignal, oldSignal), each = window.current), nrow = window.current), order.by = index(effect.price))
      portfolio.return <- Return.portfolio(price.return, weights = lag(weights), geometric = F)
      context$weight[index(weights),] <- weights
      reward <- round(SortinoRatio(portfolio.return),4)

      # print(reward)
      if (reward[1] == Inf) {
        reward[1] = 0
      }
      return(list(context,new.s,reward[1]))
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
  return(list(context,new.s,reward))
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
  ret = .step(self,status,a)
  self <<- ret[[1]]
  return(c(ret[[2]][1],ret[[3]][1]))
}

showDomain <- function()
{
  price.return <- na.omit(Return.calculate(self$sample))
  portfolio.return <- Return.portfolio(price.return, weights = lag(self$weight), geometric = F)
  print(c(maxDrawdown=maxDrawdown(portfolio.return), SortinoRatio=SortinoRatio(portfolio.return), Return.annualized=Return.annualized(portfolio.return)))
  
  signals <- na.omit(na.locf(self$signals))
  PlotWithSignal(self$zscore, signals, 1.6, 0, 10)
  
  price.return <- Return.calculate(self$sample)
  portfolio.return <- Return.portfolio(na.omit(price.return), weights = lag(self$weight), geometric = F)
  charts.PerformanceSummary(portfolio.return)
}
.save <- function()
{
  f_continue <- function(prompt = "Do you want to save the result? (y/n)") {
    
    n <- readline(prompt=prompt)
    n <- as.character(n)
    if (n == 'y')
      return( TRUE )
    else
      return( FALSE )
  }
  if (f_continue()) {
    save(self, file='result.RData')
  }
}
.load <- function()
{
 
  load('result.RData')
  return (self)
}

main <- function(){
  assets.id <- c('GZ3Y', 'GZ5Y')
  init(assets.id,TRUE)
  #s0(190)
  s0(200)
  status <- c(CLOSED)
  #a <- c(150,150,1.6,10)
  a <- c(200,70,0.5,10)
  #trace(.step, exit = quote(if (reward[1] == Inf) browser()))
  while (status[1] != TERMINAL) {
    # performing step
    
    tmp <- step(status, a)
    status <- tmp[1]
    r <- tmp[2]
    print(c(status[1],self$day - 1, self$signals[self$day - 1],self$zscore[self$day - 1],r))
  }
  print(c(status[1],self$day - 1, self$signals[self$day - 1],self$zscore[self$day - 1]))
  showDomain()
  .save()
}
