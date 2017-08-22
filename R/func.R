JohansenTest <- function(formula){
  prices <- model.frame(formula)
  # browser()
  varest <- VAR(prices,p=1,type="const",lag.max=24, ic="SC")
  # in the Johansen procedure for cointegration a lagged VAR (VECM) is used. Hence we need to subtract 1 from the optimal VAR lag length.
  lagLength <- max(2,varest$p-1)
  
  # Perform Johansen procedure for cointegration
  # Allow intercepts in the cointegrating vector: data without zero mean
  # Use trace statistic (null hypothesis: number of cointegrating vectors <= r)
  res <- ca.jo(prices,type="trace",ecdet="const",K=lagLength,spec="longrun")
  coef <- res@V[1:2,which.max(res@lambda)]
  coef <- coef/coef[1]
  return(list(coef=c(0, coef[2])))
}

library(pracma)
library(pipeR)

GetAssetsData <- function(market.name, assets.id, date.duration){

  asset1.price <- GetBar(market.name, assets.id[1], '1d', date.duration[1], date.duration[2],ip = "127.0.0.1") %>>% 
    Cl() %>>% 
    OmitOutlier()
  asset2.price <- GetBar(market.name, assets.id[2], '1d', date.duration[1], date.duration[2],ip = "127.0.0.1") %>>% 
    Cl() %>>% 
    OmitOutlier()
  
  asset1.price[(asset1.price-lag(asset1.price))/asset1.price > 0.4] <- NA
  asset2.price[(asset2.price-lag(asset2.price))/asset2.price > 0.4] <- NA
  
  price.pair <- na.omit(na.locf(cbind(Cl(asset1.price), Cl(asset2.price))))
}
OmitOutlier <- function(input.data){
  # omit outlier
  QL <- quantile(input.data, probs = 0.25)
  QU <- quantile(input.data, probs = 0.75)
  QU_QL <- QU-QL
  # tmp <- max(input.data[which(input.data <= QU + 1.5*QU_QL)])
  input.data[which(input.data > QU + 1.5*QU_QL)] <- QU + 1.5*QU_QL
  input.data
}

PlotAssets <- function(effect.price, assets.id){
  main.string <- paste("Prices:", assets.id[1], assets.id[2])
  plot(effect.price[,1], type = 'l', ylim = c(min(effect.price), max(effect.price)), main = main.string)
  lines(effect.price[,2], col = 'red')
  legend('topright', legend = assets.id, col = c('black', 'red'), lty = 1)
}

PerformTLS <- function(formula){
  prices <- model.frame(formula)
  # odregress(x, y)
  odr <- odregress(prices[,2], prices[,1])
  #   browser()
  coef <- list(coef=c(odr$coeff[2],odr$coeff[1]))
  return(coef)
}

PlotZscore <- function(zscore){}

PlotResult2 <- function(roll.res, is.beta.fixed){
  pair.spread <- na.omit(roll.res$spread)
  pair.hedgeratio <- na.omit(roll.res$hedge.ratio)
  pair.premium <- na.omit(roll.res$premium)
  price.timestamp <- index(pair.premium)
  price.zscore <- na.omit(roll.res$zscore)
  #   res <- na.omit(roll.res)
  # price.timestamp <- index(res)
  #   dygraph(res) %>% 
  #     dySeries('spread', label = 'spread', color='black') %>% 
  #     dySeries('hedge.ratio', label='Beta', color='red') %>% 
  #     dySeries('premium', label='Alpha', color='blue') %>% 
  #     dySeries('zscore', label='zscore', color='green') %>% 
  #     dyLimit(limit = -params.entryZscore, color = 'red', strokePattern = 'solid') %>% 
  #     dyLimit(limit = params.entryZscore, color = 'red', strokePattern = 'solid')
  #   
  rp <- na.omit(roll.res$rp)
  dev.new()
  par(mfrow=c(2,3))
  # Plot price pair
  #   plot(price.pair[,1], type='l', ylim=c(min(price.pair), max(price.pair)) )
  #   lines(price.pair[,2], col='red')
  #   lines(pair.spread, col='green')
  plot(pair.spread, type='l', main='Spread')
  # Plot Zscore
  # signal <- SignalWithZscore2(price.zscore, params.entryZscore, params.exitZscore, params.stoplossZscore)
  signal <- SignalWithoutPlot(price.zscore, params.entryZscore, params.exitZscore, params.stoplossZscore)
  # signal <- xts(signal, order.by = price.timestamp)
  
  # Plot return
  if(is.beta.fixed){
    return.pairtrading <- ReturnWithFiexdBeta(price.pair, lag(signal), lag(pair.hedgeratio))
  }else{
    return.pairtrading <- Return(price.pair, lag(signal), lag(pair.hedgeratio))
  }
  
  pair.return <- return.pairtrading$pair.return
  # browser()
  #   plot(roll.res$zscore[152:178], type='l')
  #   par(new=T)
  #   cum.return <- cumprod(1 + pair.return[2:27])
  #   plot(cum.return, type='l', axes = F, col='blue')
  #   axis(4,ylim=(c(min(cum.return), max(cum.return))))
  
  
  
  #   weight.pair <- return.pairtrading$weight
  #   plot(weight.pair[,1], type = 'l', main='weight', ylim=c(min(weight.pair), max(weight.pair)))
  #   lines(weight.pair[,2], col='blue')
  #   legend('topright', legend = c('price1', 'price2'), col = c('black', 'blue'), lty = c(1))
  
  #   info <- vector(mode = 'numeric', length = length(signal))
  #   info[signal-lag(signal)>0] <- 1
  #   info[signal-lag(signal)<0] <- -1
  #   
  #   for(i in c(1:length(signal))){
  #     if(info[i] == 1){
  #       print(paste('Long:', roll.res[index(signal[i])], price.pair[index(signal[i])], sep = ''))
  #     }else if(info[i] == -1){
  #       print(paste('Short:', roll.res[index(signal[i])], price.pair[index(signal[i])], sep = ''))
  #     }
  #   }
  # browser()
  #   price.weight.spread <- xts(rowSums(weight.pair*price.pair), order.by = index(weight.pair))
  #   plot(price.weight.spread, type='l', main='Weight Price')
  #   lim <- par("usr")
  #   beginRect=1
  #   endRect=1
  #   pos.length <- nrow(weight.pair)
  #   timestamps <- index(weight.pair)
  #   # browser()
  #   for(i in 2:(pos.length-1))
  #   {
  #     # Begin long position
  #     if(weight.pair[i,2]>0 && weight.pair[i-1,2]<=0)
  #       beginRect=i
  #     # Begin short position
  #     if(weight.pair[i,2]<0 && weight.pair[i-1,2]>=0)
  #       beginRect=i
  #     
  #     # End long position
  #     if(weight.pair[i,2]<=0 && weight.pair[i-1,2]>0)
  #       rect(timestamps[beginRect], lim[3]-1, timestamps[i-1], lim[4]+2, col = "green")
  #     # End short position
  #     if(weight.pair[i,2]>=0 && weight.pair[i-1,2]<0)
  #       rect(timestamps[beginRect], lim[3]-1, timestamps[i-1], lim[4]+2, col = "red")
  #   }
  #   
  #   if(weight.pair[pos.length,2] > 0 && weight.pair[pos.length-1,2] > 0){
  #     rect(timestamps[beginRect], lim[3]-1, timestamps[pos.length], lim[4]+2, col = "green")
  #   }else if(weight.pair[pos.length,2] < 0 && weight.pair[pos.length-1,2] < 0){
  #     rect(timestamps[beginRect], lim[3]-1, timestamps[pos.length], lim[4]+2, col = "red")
  #   }
  #   lines(price.weight.spread)
  
  # Plot beta & premium
  PlotBetaAndPremium(pair.hedgeratio, pair.premium)
  
  #   browser()
  #   pair.return.correct <- rowSums(50*cumprod(1+pair.return))
  #   plot(cumsum(pair.return), type='l')
  
}

PlotBetaAndPremium <- function(beta, premium){
  data.comb <- na.omit(cbind(beta, premium))
  beta <- data.comb[,1]
  premium <- data.comb[,2]
  premium.max <- max(premium)
  premium.min <- min(premium)
  hedgeratio.max <- max(beta)
  hedgeratio.min <- min(beta)
  
  plot(beta, type = "l", main = "beta & alpha", ylim = c(hedgeratio.min, hedgeratio.max))
  par(new=T)
  plot(premium, type = "l", axes = F, col='blue', main=NULL)
  axis(4, ylim=c(premium.min, premium.max))
  legend('topright', legend = c('beta(left)', 'premium(right)'), col = c('black', 'blue'), lty = 1)
}

# params: entry, exit, stoploss, period
ReturnFunction2 <- function(price.pair, params){
  ols.params <- EstimateParametersHistorically(price.pair, period = params[4], method = lm)
  pair.spread <- na.omit(ols.params$spread)
  pair.hedgeratio <- na.omit(ols.params$hedge.ratio)
  pair.premium <- na.omit(ols.params$premium)
  price.timestamp <- index(pair.premium)
  price.zscore <- na.omit(ols.params$zscore)
  signal <- SignalWithoutPlot(price.zscore, params[1], params[2], params[3])
  ret <- Return(price.pair, lag(signal), lag(pair.hedgeratio), price.timestamp)
  last(cumprod(1 + ret))
}

ParamOptimPlot <- function(price.pair){
  # entryZscore
  x <- seq(0.2, 1.5, 0.1)
  # exitZscore
  y <- seq(0, 0.5, 0.1)
  # period
  k <- seq(180, 300, 10)
  # stoplossZscore
  s <- seq(3, 5, 0.1)
  param.frame <- data.frame(entry=rep(x, each=length(k)), period=rep(k, length(x)))
  res <- matrix(nrow = length(x), ncol = length(k))
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)
  
  # 170 2.2 1.62
  X<-foreach(i=1:nrow(param.frame), .combine='c', .packages = c('tseries', 'xts', 'quantmod', 'PerformanceAnalytics'), .export = c('ReturnFunction', 'EstimateParametersHistorically', 'EstimateParameters', 'SignalWithZscore2', 'Return', '.return', 'HedgeRatio2Weight')) %dopar%{
    as.numeric(ReturnFunction(price.pair, c(param.frame[i, 1], 0, 3, param.frame[i, 2])))
  }
  
  stopCluster(cl)
  #   for(l in 1:length(k)){
  #     for(i in 1:length(x)){
  # #       for(j in 1:length(y)){
  #         z <- as.numeric(ReturnFunction(price.pair, c(x[i], 0, 3, k[l])))
  #         res[i, l] <- z
  #       # }
  #     }
  #   }
  #   res <- matrix(X, nrow = length(x))
  #   plot_ly(x=x, y=k, z=res, type='surface')
  list(x, k, X)
}

ParamOptimization <- function(price.pair){
  theta <- c(params.entryZscore, params.exitZscore, params.stoplossZscore)
  ReturnFunction <- function(price.pair, params){
    ols.params <- EstimateParametersHistorically(price.pair, period = params[4], method = lm)
    pair.spread <- na.omit(ols.params$spread)
    pair.hedgeratio <- na.omit(ols.params$hedge.ratio)
    pair.premium <- na.omit(ols.params$premium)
    price.timestamp <- index(pair.premium)
    ols.params <- cbind(pair.spread, pair.hedgeratio,pair.premium)
    
    meanSpread <- mean(ols.params$spread)
    stdSpread <- sd(ols.params$spread)
    
    zScore = (ols.params$spread - meanSpread) / stdSpread
    signal <- SignalWithZscore2(zScore, params[1], params[2], params[3])
    ret <- Return(price.pair, lag(signal), lag(pair.hedgeratio), price.timestamp)
    browser()
    return(last(cumprod(1 + ret)))
  }
  GradientFunction <- function(params){
    params-c(0.2, -0.1, 0)
  }
  #   constrOptim(theta = theta, f = ReturnFunction, grad = GradientFunction, ui = matrix(c(),), ci = c())
  #   optim(par = theta, fn = ReturnFunction, gr = GradientFunction, control = list(fnscale=-1), method = 'L-BFGS-B', upper = c(300, 2.5, 0.5, 4), lower = c(100, 1, 0, 3))
  optim(par = theta, fn = ReturnFunction, control = list(fnscale=-1))
}

# deprecated
# calculate zscore with whole price.pair
PlotResult <- function(params, price.pair, is.beta.fixed){
  # create & plot trading signals
  # params$spread <- na.locf(params$spread, na.rm=TRUE)
  pair.spread <- na.omit(params$spread)
  pair.hedgeratio <- na.omit(params$hedge.ratio)
  pair.premium <- na.omit(params$premium)
  price.timestamp <- index(pair.premium)
  params <- cbind(pair.spread, pair.hedgeratio, pair.premium)
  
  meanSpread <- mean(params$spread)
  stdSpread <- sd(params$spread)
  
  zScore = (params$spread - meanSpread) / stdSpread
  dev.new()
  par(mfrow=c(2,2))
  plot(price.pair[,1], type='l', ylim=c(min(price.pair), max(price.pair)) )
  lines(price.pair[,2], col='red')
  signal <- SignalWithZscore2(zScore, params.entryZscore, params.exitZscore, params.stoplossZscore)
  premium.max <- max(pair.premium)
  premium.min <- min(pair.premium)
  hedgeratio.max <- max(pair.hedgeratio)
  hedgeratio.min <- min(pair.hedgeratio)
  plot(params$hedge,type = "l", main = "hedge & alpha", ylim = c(min(premium.min, hedgeratio.min), max(premium.max, hedgeratio.max)))
  lines(params$premium,type = "l",col = "blue")
  if(is.beta.fixed){
    return.pairtrading <- ReturnWithFiexdBeta(price.pair, lag(signal), lag(params$hedge.ratio), price.timestamp)
  }else{
    return.pairtrading <- Return(price.pair, lag(signal), lag(params$hedge.ratio), price.timestamp)
  }
  plot(100 * cumprod(1 + return.pairtrading))
}
# deprecated
# params = entry, exit, stoploss, period
ReturnFunction <- function(price.pair, params){
  ols.params <- EstimateParametersHistorically(price.pair, period = params[4], method = lm)
  pair.spread <- na.omit(ols.params$spread)
  pair.hedgeratio <- na.omit(ols.params$hedge.ratio)
  pair.premium <- na.omit(ols.params$premium)
  price.timestamp <- index(pair.premium)
  ols.params <- cbind(pair.spread, pair.hedgeratio,pair.premium)
  
  meanSpread <- mean(ols.params$spread)
  stdSpread <- sd(ols.params$spread)
  
  zScore = (ols.params$spread - meanSpread) / stdSpread
  signal <- SignalWithZscore2(zScore, params[1], params[2], params[3])
  ret <- Return(price.pair, lag(signal), lag(pair.hedgeratio), price.timestamp)
  last(cumprod(1 + ret))
  # SharpeRatio(ret, FUN='StdDev')
}
