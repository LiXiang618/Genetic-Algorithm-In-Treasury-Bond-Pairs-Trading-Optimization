library(WindR)
w.start()

#data<-w.wsd("600004.SH","close","2014-03-16","2016-03-15","Fill=Previous")
w_data<-w.wsd("600004.SH","close","2013-03-16","2016-03-15","Fill=Previous")
mydata <<- w_data$Data

library(quantmod)

#data$MA50 <- SMA(close,50)
#data$MA70 <- SMA(close,70)

data.frame(SMA(mydata$CLOSE,10.4),SMA(mydata$CLOSE,10),SMA(mydata$CLOSE,11))
SMA(mydata$CLOSE,10.4)
SMA(mydata$CLOSE,10)

fun <- function(para)
{
  data <- mydata
  data$MA <- SMA(mydata$CLOSE,ceiling(para))
  
  init_position <- 100000
  cash_position <- init_position
  total_position <- init_position
  stock_num <- 0
  ratio <- 0.2
  if_can_buy <- TRUE
  
  data <- data[which(data$DATETIME>="2014-03-16"),]
  
  
  cash_position_log <- data.frame()
  
  for(i in 2:length(data[,1]))
  {
    tmp_pre <- data[i-1,]
    tmp <- data[i,]
    stock_price <- tmp_pre$CLOSE
    if(tmp_pre$CLOSE>tmp_pre$MA&if_can_buy==TRUE)
    {
      stock_num <- floor(cash_position*ratio/stock_price)
      cash_position <- cash_position-stock_num*stock_price
      if_can_buy <- FALSE
    }
    total_position <- cash_position+stock_num*stock_price
    cash_position_log <- rbind(cash_position_log,data.frame(tmp$DATETIME,total_position,cash_position,stock_num))
  }
  
  cash_position_log
  annual_rate <- cash_position_log[length(cash_position_log[,1]),]$total_position/cash_position_log[1,]$total_position
  annual_rate
  
  return(annual_rate)
}


library(genalg)

evaluate <- function(string=c()) {
  print(string)
  returnVal = NA;
  if (length(string) == 1) {
    returnVal = -fun(string[1])
  } else {
    stop("Expecting a chromosome of length 1!");
  }
  returnVal
}

obj_pop <<- list()

monitor <- function(obj) {
  # plot the population
}

rbga.results = rbga(c(5), c(100),popSize = 20,iters = 100, monitorFunc=monitor, 
                    evalFunc=evaluate, verbose=TRUE, mutationChance=0.01)

plot(rbga.results)
plot(rbga.results, type="hist")
plot(rbga.results, type="vars")

par(mfrow=c(3,3),mar=c(1,1,1,1))

for(i in 1:length(obj_pop))
{
  plot(obj_pop[[i]],xlim=c(1,100), ylim=c(1,100), xlab="x", ylab="y")
}


