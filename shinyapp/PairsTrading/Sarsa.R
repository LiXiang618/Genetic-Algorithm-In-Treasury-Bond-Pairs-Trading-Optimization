#####################################################
# File: q3-TD-learning.R
# Author: Antoine Sachet
# Github: github.com/antoine-sachet/easy21-RL-project
# Date: 04/2015
#####################################################

# This file contains the functions to perform Temporal Difference reinforcement learning.
# I use the SARSA algorithm for on-policy control,
# implemented using the backward-view (eligibility traces).
#
# The helper function epsgreedy is also defined.
#
# Note that the function sarsa() requires some functions from q1-step.R.
# Please run q1-step.R prior to running this file.

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

periodFeatures <- function(period) {
  return (c(
    (period>=100&period<=110),
    (period>110&period<=120),
    (period>120&period<=130),
    (period>130&period<=140),
    (period>140&period<=150),
    (period>150&period<=160),
    (period>160&period<=170),
    (period>170&period<=180),
    (period>180&period<=190),
    (period>190&period<=200)))
}


thesholdFeatures <- function(theshold) {
  return (c(
    (theshold>=2.0&theshold<=2.2),
    (theshold>2.2 & theshold<=2.4),
    (theshold>2.4&theshold<=2.6),
    (theshold>2.6 & theshold<=2.8),
    (theshold>2.8&theshold<=3.0)))
}

# INPUT
#   playerState: sum of the player, integer between 1 and 21
# OUTPUT
#   boolean vector coding the player card interval on 6 bits

stoplossFeatures <- function(stoploss) {
  return (c(
    (stoploss>=3.0&stoploss<=3.1),
    (stoploss>3.1&stoploss<=3.2),
    (stoploss>3.2 & stoploss<=3.3),
    (stoploss>3.3&stoploss<=3.4),
    (stoploss>3.4 & stoploss<=3.5)
    ))
}


# INPUTS
#   s: state (as defined in q1-step.R)
#   a: action, integer: HIT(1) or STICK(2)
# returns a binary vector of length 36 representing the features
phi <- function(s, a) {
  tmp <- array(0, dim=c(10,5,5)) #empty array of dim 10*5*5
  tmp[periodFeatures(a[1]),
      thesholdFeatures(a[2]),
      stoplossFeatures(a[3])] <- 1 #putting one where a feature is on
  return(as.vector(tmp)) #returning 'vectorized' (1-dim) array
}

# INPUTS
#   s: state (as defined in q1-step.R)
#   Q: action-value function, that is an array of dim: (10, 21, 2)
#   eps: numeric value for epsilon
# OUTPUT
# action to take following an epsilong-greedy policy
# 1 is HIT and 2 is STICK (as defined in q1-step.R)
# a[1] period a[2] theshold a[3] stoploss
epsgreedy <- function(s,a, Q, eps) {
  if (runif(1) < eps)
    return(c(sample(seq(100,200, by = 10),1),sample(seq(2, 3, by = 0.1),1),sample(seq(3, 3.5, by = 0.1),1))) # random action taken with eps probability
  else
    return(which.max(Q(s,a))) # else action maximizing Q
}


# INPUTS
#   lambda: lambda parameter for SARSA
#       numeric between 0 and 1
#       quantifies the weighting of the steps within an episode
#   gamma: dicounting factor
#       default to 1
#   Q: action-value "function",
#       that is an array of dim: (10, 21, 2)
#       will be an array of 0 if not provided
#   w: parameter vector of length 500
#     Q(s,a) is approximated by phi*w where phi is detailed above
#   nb.episode: number of episode to play
#   N0: offset for N.
#       At each episode, an epsilon-greedy action is taken with
#       eps = N0/(total+N0) where total is the total number of times
#       this state has been visited.
# OUTPUT
#   list of:
#     Q: updated action-value function
#     N: updated counter of state-action visits
sarsa <-
  function(lambda, gamma=1, w=NULL, nb.episode=1, eps=0.05, step.size=0.01) {
    
    if (is.null(w))
    w <- array(0,dim=250)

    Q <- function(s,a) as.vector(phi(s,a) %*% w)

    policy <- function(s) {
      epsgreedy(s, Q, eps)
    }

    for (i in 1:nb.episode) {
      s <- s.ini()
      # choosing initial action a
      a <- policy(s)
      r <- 0L
      # eligibility trace
       e <- array(0L, dim=250)

      # s[3] is the "terminal state" flag
      # s[3]==1 means the game is over
      while (s[3] == 0L) {
       
        # performing step
        tmp <- step(s, a)
        s2 <- tmp[[1]]
        r <- r + tmp[[2]]

        # if s2 is terminal
        if (s2[3] == 0) {
          # choosing new action
          a2 <- policy(s2)
          # sarsa backward view formula with estimated return r+gamma*Q2
          delta <- r + gamma *Q(s2,a2) - Q(s,a)
        } else {
          # sarsa backward view formula, with now known return r
          #计算全部的R
          delta <- r - Q(s,a)
          a2 <- 0L
        }
        ind <- which(e > 0)
        # updating Q
        w <- w + step.size*delta*e
        e <- gamma * lambda * e
        s <- s2
        a <- a2
      }
    }
    return(w)
  }


######## COMPUTING Q WITH SARSA #########

#start from 100 day
s.ini <- function () {
  return ( c(0L,0L, 0L))
}

MSE <- foreach(1:10, .combine = rbind) %do% {
  # computing Q for lambda from 0 to 1
  lambdas <- seq(0, 1, 0.1)
  Qsarsa <- llply(
    lambdas, .fun = function(lambda) {
      sarsa(lambda, nb.episode = 1000)[[1]]
    }
  )

  # computing MSE between sarsa's Q and montecarlo's Q
  MSE <- laply(
    Qsarsa, .fun = function(Q) {
      mean((Q - QMC) ** 2)
    }
  )
}







###################################################################################################################

# plotting the MSE vs lambda
load.library("ggplot2")
load.library("tseries")
load.library("xts")
load.library("PerformanceAnalytics")
load.library("assertthat")
source("Cointegration.R")
source("PairTrading.R")
source("Signal.R")
#source("Setp.R")
#load sample stock price data
load("stock.price.rda")
price.pair <- stock.price[, 1:2]["2006-1-03::"]


day <<- 190
signals <<- xts(array(NA, dim = nrow(price.pair)), order.by = index(price.pair))
pair.zscore <<- xts(array(NA, dim = nrow((price.pair))), order.by = index(price.pair))
num.price <- nrow(price.pair)
whole.weight <<- xts(data.frame(p1=rep(0, num.price), p2=rep(0, num.price)), order.by = index(price.pair))

#test setp
s <- s.ini()
a <- c(120, 2.0, 4.0, 130)
while (s[3] == 0L) {
  # performing step
  tmp <- Setp(price.pair, s, a)
  s <- tmp[2:4]
  print(c(tmp[1], s[3], day-1, signals[day-1], pair.zscore[day-1]))
}
print(c(s[3], day-1, signals[day-1], pair.zscore[day-1]))

signals <- na.omit(na.locf(signals))
PlotWithSignal(pair.zscore, signals, a[2], 0, a[3])

price.return <- Return.calculate(price.pair)
portfolio.return <- Return.portfolio(na.omit(price.return), weights = lag(whole.weight), geometric = F)
charts.PerformanceSummary(portfolio.return)

###################################################################################################################



















df <-
  data.frame(
    lambda = lambdas, mse = colMeans(MSE), se = aaply(MSE, .margins = 2, sd)
  )
ggplot(df) + geom_point(aes(x = lambda, y = mse)) +
  geom_errorbar(aes(
    x = lambda, y = mse, ymin = mse - se, ymax = mse + se
  ))

# computing MSE at every step for lambda=1
Q <- NULL
N <- NULL
mse <- times(1000) %do% {
  res <- sarsa(
    lambda = 1, Q = Q, N = N, nb.episode = 1
  )
  Q <- res$Q; N <- res$N
  mean((Q - QMC) ** 2)
  }
plot(mse)

# computing MSE at every step for lambda=0
Q <- NULL
N <- NULL
mse <- times(1000) %do% {
  res <- sarsa(
    lambda = 0, Q = Q, N = N, nb.episode = 1
  )
  Q <- res$Q; N <- res$N
  mean((Q - QMC) ** 2)
}
plot(mse)
