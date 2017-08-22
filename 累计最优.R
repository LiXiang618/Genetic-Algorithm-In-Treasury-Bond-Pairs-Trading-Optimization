table <- read.csv("table.csv",sep = "\t")
table <- table[,-1]

CumulativeBestFirst <- function(table)
{
  score <- rep(0,nrow(table))
  scoreTable <- as.matrix(table)
  scoreTable[which(!is.na(scoreTable))] <- 0
  for(j in 2:(length(table)-1))
  {
    
    score[which.max(table[which(!is.na(table[j])),j][1:j])] <- score[which.max(table[which(!is.na(table[j])),j][1:j])] + 1
    scoreTable[1:j,j-1] <- (score/(j-1))[1:j]
    
  }
  
  path <- c()
  for(j in 1:(ncol(scoreTable)-2))
  {
    path <- c(path,max(which(scoreTable[,j]==max(na.omit(scoreTable[,j])))))
  }
  return(path)
}


    