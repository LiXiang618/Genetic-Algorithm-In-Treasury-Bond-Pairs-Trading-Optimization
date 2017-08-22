#load library
#library(PairTrading)
library("tseries")
library("xts")
library(PerformanceAnalytics)
source("Cointegration.R")
source("PairTrading.R")
source("Signal.R")
#load sample stock price data
load("stock.price.rda")

#select 2 stocks
price.pair <- stock.price[,1:2]["2006-1-03::"]

params.period.init <- 150
params.entryZscore.init <- 1.5
params.exitZscore.init <- 0
params.stoplossZscore.init <- 5.0
day <<- params.period.init
s[0] <- day #s[0]表明lastday
#########################
# run codes below every time
a <- c(params.period.init, params.entryZscore.init, params.exitZscore.init, params.stoplossZscore.init)
pair.len <- nrow(price.pair) - params.period.init + 1
# trade signals
signals <<- xts(rep(NA, pair.len), order.by = index(price.pair[params.period.init:nrow(price.pair), ]))
# total zscore
pair.zscore <<- vector(mode = 'numeric', length = nrow(price.pair) - params.period.init + 1)
# Set Parameter Function
#s[0] <- s[0] + 1
Setp(price.pair, 0, a)
#########################

rollwindow <- function(max) return (150)

Applied <- function(price.pair){
  #如果开仓，选择新的rollwindow, 继续roll,return 0
  
  #如果不开不平，继续roll，return 0
  
  #如果平仓，return 上一个阶段的Sortino Ratio
  l <- ndays(price.pair)
  w <- rollwindow(params.maxperiod)
  day <<-  day +1
  print(paste("day" ,day, sep =    ""))
  reg <- EstimateParameters(price.pair[l - w:l], method = lm)
  #meanSpread <- mean(na.omit(reg$spread))
#  stdSpread <- sd(na.omit(reg$spread))
  
#  zScore = (params$spread - meanSpread) / stdSpread
  signal <- SignalWithZscore(zScore, params.entryZscore)
  c(spread = as.numeric(last(reg$spread)), hedge.ratio = reg$hedge.ratio, premium = reg$premium)
}
day <<- 0
params <- as.xts(rollapplyr(price.pair, params.maxperiod, Applied, by.column = FALSE))

# for (day in seq(100,ndays(price.pair))) {
# 	params.period <- ifelse(params.period < day, params.period, day - 1)
# 	reg <- EstimateParameters(price.pair[(day - params.period):day],method = lm)
# 	reg.df[end(reg$spread)] <- c(end(reg$spread),as.numeric(last(reg$spread)),reg$hedge.ratio,reg$premium)
# }

#estimate parameters for back test
#params <- EstimateParametersHistorically(price.pair, period = params.period)

#create & plot trading signals


