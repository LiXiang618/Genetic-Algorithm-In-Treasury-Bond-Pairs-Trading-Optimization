Simple <- function(spread, spread.entry)
{
  signal <- ifelse(spread >=   spread.entry, -1, NA)
  signal <- ifelse(spread <=  -spread.entry,  1, signal)
  return(na.locf(signal))
}
SimpleWithTakeProfit <- function(spread, spread.entry, spread.take.profit)
{
  signal <- ifelse(spread >=   abs(spread.entry), -1, 0)
  signal <- ifelse(spread <=  -abs(spread.entry),  1, signal)
  
  take.profit.upper <-  abs(spread.take.profit)
  take.profit.lower <- -take.profit.upper

  #Hit take.profit line : 0
  #other case : continue previous position
  for(i in 2:nrow(signal))
  {
    if(signal[i] == 0){
      if(signal[i - 1] == 1){
        if(spread[i] >= take.profit.lower){
          signal[i] <- 0
        }else{
          signal[i] <- signal[i - 1]
        }
      }else if(signal[i - 1] == -1){
        if(spread[i] <= take.profit.upper){
          signal[i] <- 0
        }else{
          signal[i] <- signal[i - 1]          
        }
      }
    }
  }
  return(signal)
}
SignalWithZscore <- function(spreadZscore, entryZscore,stoplossZscore = NA)
{
  signal <- ifelse(spreadZscore >=   entryZscore, -1, NA)
  signal <- ifelse(spreadZscore <=  -entryZscore,  1, signal)
  if (is.na(stoplossZscore))   return(na.locf(signal))

  stoplossZscore.upper <-  abs(stoplossZscore)
  stoplossZscore.lower <- -stoplossZscore

  #Hit take.profit line : 0
  #other case : continue previous position
  for(i in 2:nrow(signal))
  {
    if(signal[i] == 0){
      if(signal[i - 1] == 1){
        if(spreadZscore[i] >= stoplossZscore.lower){
          signal[i] <- 0
        }else{
          signal[i] <- signal[i - 1]
        }
      }else if(signal[i - 1] == -1){
        if(spreadZscore[i] <= stoplossZscore.upper){
          signal[i] <- 0
        }else{
          signal[i] <- signal[i - 1]          
        }
      }
    }
  }
  return(signal)

}

#return:
#  -1 Short Open
#  1 Long Open
#  0 Close
#  NA DoNothing
DaySignal <- function(zscore.current, zscore.pre, entryZScore, stopLossZScroe) {
    if (is.na(zscore.pre)) {
      return(NA)
    }
    if (zscore.pre < entryZScore && zscore.current > entryZScore) {
      return(-1)
    }
    if (zscore.pre > -entryZScore && zscore.current < -entryZScore) {
      return(1)
    }
    if (zscore.pre > 0 && zscore.current < 0) {
      return(0)
    }
    if (zscore.pre < 0 && zscore.current > 0) {
      return(0)
    }
    if (zscore.pre < stopLossZScroe &&
        zscore.current > stopLossZScroe) {
      return(0)
    }
    if (zscore.pre > -stopLossZScroe &&
        zscore.current < -stopLossZScroe) {
      return(0)
    }
    return(NA)
  }

TestDaySignal <- function(){
  #test DaySignal
  assert_that(is.na(DaySignal(1.5, 0, 2,3)))
  assert_that(DaySignal(2.1, 0, 2,3) == -1)
  assert_that(DaySignal(3.1, -1, 2,3) == 0)
  assert_that(DaySignal(2.5, 0, 2,3) == -1) #here is error
  assert_that(is.na(DaySignal(1.8, -1, 2,3)))
  assert_that(DaySignal(0, -1, 2,3) == 0)
  assert_that(DaySignal(-2.0, 0, 2,3) == 1)
  assert_that(DaySignal(-3.0, 1, 2,3) == 0)
  assert_that(DaySignal(-2.5, 0, 2,3) == 1)
  assert_that(DaySignal(0.1, 1, 2,3) == 0)
}

PlotWithSignal <- function(zscore, signal, zscore.entry=1.5, zscore.exit=0, zscore.stoploss=2.5){
  data.comb <- na.omit(cbind(zscore, signal))
  zscore <- data.comb[, 1]
  signal <- data.comb[, 2]
  timestamps <- index(zscore)
  zScoreDiagramString <- paste('Strategy (Zscore entry above/below ',zscore.entry, ' std, Zscore exit below/above ', zscore.exit, ' std)', sep="")
  plot(timestamps, zscore, xlab="Time", ylab="Zscore", main=zScoreDiagramString, type="l")
  lim <- par("usr")
  beginRect <- 1
  endRect <- 1
  signal.length <- length(signal)
  for(i in 2:(signal.length - 1))
  {
    # Begin long position
    if(signal[i]==1 && signal[i-1]==0){
      beginRect <- i
    }
    # Begin short position
    if(signal[i]==-1 && signal[i-1]==0){
      beginRect <- i
    }
    # End long position
    if(signal[i]==0 && signal[i-1]==1){
      rect(timestamps[beginRect], lim[3]-1, timestamps[i], -zscore.exit, col = "green")
    }
    # End short position
    if(signal[i]==0 && signal[i-1]==-1){
      rect(timestamps[beginRect], zscore.exit, timestamps[i], lim[4]+2, col = "red")
    }
  }
  
  if(signal[signal.length] == 1 && signal[signal.length-1] == 1){
    rect(timestamps[beginRect], lim[3]-1, timestamps[signal.length], -zscore.exit, col = "green")
  }else if(signal[signal.length] == -1 && signal[signal.length-1] == -1){
    rect(timestamps[beginRect], zscore.exit, timestamps[signal.length], lim[4]+2, col = "red")
  }
  lines(timestamps, zscore)
  abline(h=c(-zscore.entry,zscore.entry,-zscore.exit,zscore.exit,-zscore.stoploss,zscore.stoploss),col=c("blue","blue","red","red","green","green"))
}


PlotWithSignalFloatEntry <- function(zscore, signal, zscore.entry, zscore.exit=0, zscore.stoploss){
  zscore.entry <- na.locf(zscore.entry)
  zscore.stoploss <- na.locf(zscore.stoploss)
  data.comb <- na.omit(cbind(zscore, signal, zscore.entry, zscore.stoploss))

  zscore <- data.comb[, 1]
  signal <- data.comb[, 2]
  timestamps <- index(zscore)
  zScoreDiagramString <- 'Strategy (Zscore)'
  plot(timestamps, zscore, xlab="Time", ylab="Zscore", main=zScoreDiagramString, type="l")
  lim <- par("usr")
  beginRect <- 1
  endRect <- 1
  signal.length <- length(signal)
  for(i in 2:signal.length)
  {
    # End long position
    if(signal[i-1] == 1 && signal[i] != 1){
      rect(timestamps[beginRect], lim[3]-1, timestamps[i-1], -zscore.exit, col = "green")
    }
    # End short position
    if(signal[i-1] == -1 && signal[i] != -1){
      rect(timestamps[beginRect], zscore.exit, timestamps[i-1], lim[4]+2, col = "red")
    }
    # Begin long position
    if(signal[i] == 1 && signal[i-1] != 1){
      beginRect <- i
    }
    # Begin short position
    if(signal[i] == -1 && signal[i-1] != -1){
      beginRect <- i
    }
  }
  
  lines(timestamps, zscore)
  abline(h=c(-zscore.exit,zscore.exit),col=c("red","red"))
  lines(timestamps, data.comb[, 3], col='blue')
  lines(timestamps, -data.comb[, 3], col='blue')
  lines(timestamps, data.comb[, 4], col='purple')
  lines(timestamps, -data.comb[, 4], col='purple')
}
