series <- c(1:10,9:3,3:20,20:(-20),(-20):40)

CR <- series

maxDrawdown <- function(CR)
{
  drawdowns <- c()
  for(i in 1:length(series))
  {
    drawdown <- 1-series[i]/max(series[1:i])
    drawdowns <- c(drawdowns,drawdown)
  }
  return(max(drawdowns))
}



