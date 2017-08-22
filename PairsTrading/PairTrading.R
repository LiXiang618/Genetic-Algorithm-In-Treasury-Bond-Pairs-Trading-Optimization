Return <- function(price.pair, signal.lagged, hedge.ratio.lagged)
  # 从开仓到平仓的价格[]，开仓信号[]，开仓时的hedge.ratio
{
  #
  signal      <- as.xts(na.omit(cbind(signal.lagged, -1*(signal.lagged))))
  return.pair <- as.xts(na.omit(.return(price.pair, type = "discrete")))
  weight.pair <- as.xts(na.omit(HedgeRatio2Weight(hedge.ratio.lagged)))
  #
  #names(return.pair) <- names(price.pair)
  #names(signal)      <- names(price.pair)
  #names(weight.pair) <- names(price.pair) 
  #as.xts(apply(signal * weight.pair * return.pair, 1, sum) * leverage)
  x <-          as.xts(apply(merge(signal[, 1], weight.pair[, 1], return.pair[, 1], all = FALSE), 1, prod))
  x <- merge(x, as.xts(apply(merge(signal[, 2], weight.pair[, 2], return.pair[, 2], all = FALSE), 1, prod)))

  if(!length(dim(x))){
    xts(rep(NA, nrow(price.pair)), order.by = index(price.pair))
  }else{
    xts(rowSums(x), order.by = index(x))
  }
}

.return <- function(x, type = c("continuous", "discrete"), na.pad = TRUE) 
{
    type <- match.arg(type)
    if (type == "discrete") {
        result <- x/lag(x, na.pad = na.pad) - 1
    }else if (type == "continuous") {
        result <- diff(log(x), na.pad = na.pad)
    }
    return(result)
}

PlotAllGraph <- function(zscore){
  zscore <- na.omit(zscore)
  signal <- SignalWithZscore2(zscore, params.entryZscore, params.exitZscore, params.stoplossZscore)
}

SignalWithZscore2 <- function(zscore.spread, zscore.entry=1.5, zscore.exit=0, zscore.stoploss=2.5){
  zscore.spread <- na.omit(zscore.spread)
  timestamps <- index(zscore.spread)
  longs.entry <- (zscore.spread < -zscore.entry)
  longs.exit <- (zscore.spread > -zscore.exit)
  longStopLoss.exit <- (zscore.spread < -zscore.stoploss)
  shorts.entry <- (zscore.spread > zscore.entry)
  shorts.exit <- (zscore.spread < zscore.exit)
  shortStopLoss.exit <- (zscore.spread > zscore.stoploss)
  
  spread.number <- length(zscore.spread)
  # Create vector with 0 values
  units.long <- vector(mode="numeric", length=spread.number)
  # Set NA from 2:end
  # first of units.long has value within processing above
  # if no value, then its initial value = 0
  units.long[2:spread.number] <- NA
  units.short <- units.long # Copy by value
  
  # numUnitsLong represents an array with 1 entrys on positions where we are long the portfolio spread
  units.long[longs.entry] <- 1;
  units.long[longs.exit] <- 0
  units.long[longStopLoss.exit] <- 0
  units.long <- na.locf(units.long)
  
  # numUnitsShort represents an array with -1 entrys on positions where we are short the portfolio spread
  units.short[shorts.entry] <- -1
  units.short[shorts.exit] <- 0
  units.short[shortStopLoss.exit] <- 0
  units.short <- na.locf(units.short)
  
  # numUnits represents an array that indicates how many units of the portfolio spread we bought (1), or sold (-1)
  units.pos <- units.long + units.short
  
  # Plot standardizedSpread
  zScoreDiagramString <- paste('Strategy (Zscore entry above/below ',zscore.entry, ' std, Zscore exit below/above ', zscore.exit, ' std)', sep="")
  plot(timestamps, zscore.spread, xlab="Time", ylab="Standardized Spread", main=zScoreDiagramString, type="l")
  lim <- par("usr")
  beginRect <- 1
  endRect <- 1
  pos.length <- length(units.pos)
  signal <- rep(NA, pos.length)
  signal[1] <- 0
  # uptick strategy
  for(i in 2:(pos.length-1))
  {
    # Begin long position
    if(units.pos[i]==1 & units.pos[i-1]==0){
      beginRect <- i
      signal[i] <- 1
    }
    # Begin short position
    if(units.pos[i]==-1 & units.pos[i-1]==0){
      beginRect <- i
      signal[i] <- -1
    }
    # end long position when timeout
    if(units.pos[i] == 1 && as.numeric(timestamps[i] - timestamps[beginRect]-max.duration)*as.numeric(timestamps[i-1] - timestamps[beginRect]-max.duration) <= 0){
      rect(timestamps[beginRect], lim[3]-1, timestamps[i-1], -zscore.exit, col = "green")
      signal[i] <- 0

    }
    # end short position when timeout
    if(units.pos[i] == -1 && as.numeric(timestamps[i] - timestamps[beginRect]-max.duration)*as.numeric(timestamps[i-1] - timestamps[beginRect]-max.duration) <= 0){
      rect(timestamps[beginRect], zscore.exit, timestamps[i-1], lim[4]+2, col = "red")
      signal[i] <- 0
    }
    # End long position
    if(units.pos[i]==0 && units.pos[i-1]==1 && (timestamps[i] - timestamps[beginRect]) < max.duration){
      # browser()
      rect(timestamps[beginRect], lim[3]-1, timestamps[i-1], -zscore.exit, col = "green")
      signal[i] <- 0
    }
    # End short position
    if(units.pos[i]==0 && units.pos[i-1]==-1 && (timestamps[i] - timestamps[beginRect]) < max.duration){
      rect(timestamps[beginRect], zscore.exit, timestamps[i-1], lim[4]+2, col = "red")
      signal[i] <- 0
    }
  }
  
  if(units.pos[pos.length] == 1 && units.pos[pos.length-1] == 1 && (timestamps[pos.length] - timestamps[beginRect]) < max.duration){
    rect(timestamps[beginRect], lim[3]-1, timestamps[pos.length], -zscore.exit, col = "green")
  }else if(units.pos[pos.length] == -1 && units.pos[pos.length-1] == -1 && (timestamps[pos.length] - timestamps[beginRect]) < max.duration){
    rect(timestamps[beginRect], zscore.exit, timestamps[pos.length], lim[4]+2, col = "red")
  }
  lines(timestamps, zscore.spread)
  abline(h=c(-zscore.entry,zscore.entry,-zscore.exit,zscore.exit,0),col=c("blue","blue","red","red","green"))
  
  xts(na.locf(signal), order.by = timestamps)
}



