#GA算法作为生物计算的仿生算法产出应用在寻优的过程中
#GA首选计算种群适应度，复制概率等
#GA先将连续变量转换成连续的2进制变量
#根据（轮盘选择）随机数选择性赋值与累计复制概率的比较
#根据再一次随机数，选择交配染色体,其中交配规则可以使单断，或者是多段规则
#从所有的2进制中随机选择对应的编号进行基因突变，2进制的反转
#然后冲寻回到第二行开始寻混
#GA算法核心是适应度和遗传过程的匹配


#10进制转二进制
#x1<=12.1 x1>=-3
#x2<=5.8 x2>=4.1
#取出区间以精度.4
m1<-matrix(c(-3,4.1,12.1,5.8),2,2,1)#all.names()all.vars()
m2<-(m1[2,]-m1[1,])*10^4
z1<-m2[1];
z2<-m2[2];
for(i in 1:2000)
{
  z1<-z1%/%2;
  if(z1==0)
  {
    break;
  }
}
m11<-i;
for(i in 1:1000)
{
  z2<-z2%/%2;
  if(z2==0)
  {
    break;
  }
}
mm<-m11+i;
#随机生成50个染色体
ml<-50;
mrnm<-rnorm(ml*mm,0,2);
mrnm<-sign(mrnm);
mrnm[which(mrnm==-1)]<-0;
m10<-NULL;
mz<-1;
for(i in 1:ml)
{
  mr<-NULL;
  for(i in 1:mm)
  {
    mr<-paste(mr,as.character(mrnm[mz]),sep="")
    mz<-mz+1;
  }
  m10<-append(m10,mr);
}
print("m10");
print(m10);
#评价个体适应度
#解码
k1<-substr(m10,1,m11);
k2<-substr(m10,m11+1,mm);
#十进制代码
k11<-strtoi(k1,base=2L);
k21<-strtoi(k2,base=2L);
k111<-m1[1,1]+k11*(m1[2,1]-m1[1,1])/(2^m11-1);
k211<-m1[1,2]+k21*(m1[2,2]-m1[1,2])/(2^(mm-m11)-1);
#带入需要寻优的函数中
#max(21.5+x1*sin(4*pi*x1)+x2*sin(20*pi*x2))
fx<-21.5+k111*sin(4*pi*k111)+k211*sin(20*pi*k211);
#极大值可以直接将适应度为fx

#遗传的循环探索过程
for(bb in 1:50000)
{
  #计算种群适应度综合
  fsum<-sum(fx);
  #判断种群是否已经遗传到最顶端获取方差#不了解终止情况，考虑用方差
  fz<-sum((fx[-which.min(fx)]-(fsum-fx[which.min(fx)])/(ml-1))^2)/(ml-2)
  print("fz");
  print(fx);
  print(fz);
  if(fz<1.4 | bb==2000)
  {
    break;
  }
  #计算被复制概率
  fper<-fx/fsum;
  #计算每个染色体被复制累计概率
  fsumper<-cumsum(fper)
  #随机轮盘ml50次#修改为最大值一定会被复制
  kk<-NULL;
  ran10<-runif(ml-1,min=0+1e-14,max=1-1e-14)
  #print(kk);
  #print(ran10);
  kk<-append(kk,m10[which.max(m10)])
  for(i in 1:ml)
  {
    zn<-which(fsumper>=ran10[i]);
    if(length(zn)==0)
    {
      kk<-append(kk,m10[1]);
    }
    else
    {
      zn<-which(fsumper<=ran10[i])[1]
      kk<-append(kk,m10[i]);
    }
  }
  #print(kk);
  #kk为随机淘汰后选出的染色体
  #种群交配初始化交配概率#规定交配和突变种群中最大值不参与
  pc<-0.2;
  pc1<-floor(ml*pc);#判断是否为偶数
  ran11<-runif(ml,min=0+1e-14,max=1-1e-14);
  jiaopei<-NULL;
  ran12<-ran11;
  n1<-which.max(fx);
  kz<-0;
  #m12<-m10;
  #print(m12);
  if(length(which(ran11<=pc))>=2)#如果满足条件则进行交配
  {
    for(i1 in 1:(length(which(ran11<=pc))%/%2))
    {
      if(which.min(ran12)!=n1)
      {
        jiaopei<-append(jiaopei,which.min(ran12));
      }
      else
      {
        kz<-1;
        ran12[which.min(ran12)]<-1;
        jiaopei<-append(jiaopei,which.min(ran12));
      }
      if(which.min(ran12)!=n1 & kz!=1)
      {
        ran12[which.min(ran12)]<-1;
        jiaopei<-append(jiaopei,which.min(ran12));
      }
      else
      {
        ran12[which.min(ran12)]<-1;
        jiaopei<-append(jiaopei,which.min(ran12));
      }
      sam<-sample(2:32,4);#多段交配
      sam<-sort(sam);
      jiaopei1<-substr(kk[jiaopei],sam[1],sam[2]);
      guding1<-substr(kk[jiaopei],sam[2]+1,sam[3]);
      jiaopei2<-substr(kk[jiaopei],sam[3]+1,sam[4]);
      guding2<-substr(kk[jiaopei],sam[4]+1,nchar(kk[1]));
      #print(jiaopei1);
      #print("jiaopei")
      #print(jiaopei);
      #执行交配
      kk[jiaopei[1]]<-paste(substr(kk[jiaopei[1]],1,sam[1]-1),jiaopei1[2],guding1[1],jiaopei2[2],guding2[1],sep="");
      kk[jiaopei[2]]<-paste(substr(kk[jiaopei[2]],1,sam[1]-1),jiaopei1[1],guding1[2],jiaopei2[1],guding2[2],sep="");
    }
  }
  #print(kk);
  #基因突变
  yita=0.02;
  ran13<-runif(ml*10,min=0+1e-10,max=1-1e-10);
  z1<-which(ran13<=yita);
  if(length(z1)!=0)
  {
    z122<-z1%/%mm+1;#判断是哪个染色体
    z122[which(z122==11)]<-ml;
    z12<-z122[-which(z122==n1)];
    z133<-z1%%mm;#判断是染色体的哪个发生突变
    z133[which(z133==0)]<-mm;
    z13<-z133[-which(z122==n1)];
    #print(z1);
    #print(z12);
    #print(z13);
    #print(substr(kk[z12[i]],z13[i],z13[i]))
    if(length(z12)!=0)
    {
      for(i in 1:length(z12))
      {
        #print(i);
        if(substr(kk[z12[i]],z13[i],z13[i])==0)
        {
          kk[z12[i]]<-paste(substr(kk[z12[i]],1,z13[i]-1),1,substr(kk[z12[i]],z13[i]+1,33),sep="")
        }
        else
        {
          kk[z12[i]]<-paste(substr(kk[z12[i]],1,z13[i]-1),0,substr(kk[z12[i]],z13[i]+1,33),sep="")
        }
      }
    }
  }
  #print(kk);
  m10<-kk;
  k1<-substr(m10,1,m11);
  k2<-substr(m10,m11+1,mm);
  #十进制代码
  k11<-strtoi(k1,base=2L);
  k21<-strtoi(k2,base=2L);
  k111<-m1[1,1]+k11*(m1[2,1]-m1[1,1])/(2^m11-1);
  k211<-m1[1,2]+k21*(m1[2,2]-m1[1,2])/(2^(mm-m11)-1);
  #带入需要寻优的函数中
  #max(21.5+x1*sin(4*pi*x1)+x2*sin(20*pi*x2))
  fx<-21.5+k111*sin(4*pi*k111)+k211*sin(20*pi*k211);
  #极大值可以直接将适应度为fx
}
print("x对应值");
print(paste("x1:",k111[which.max(fx)]," x2:",k211[which.max(fx)],sep=""));
print("fx极值点:");
print(fx[which.max(fx)]);

###########################################################
library(genalg)
?rbga
?rbga.bin

# a very simplistic optimization
evaluate <- function(string=c()) {
  returnVal = 1 / sum(string);
  returnVal
  print(string)
}

rbga.results = rbga.bin(size=20,iters = 20, mutationChance=0.1, zeroToOneRatio=2,
                        evalFunc=evaluate)
par(mfrow=c(1,1), mar=c(5,5,5,5))
plot(rbga.results)
###########################################################
data(iris)
library(MASS)
X <- cbind(scale(iris[,1:4]), matrix(rnorm(36*150), 150, 36))
Y <- iris[,5]

iris.evaluate <- function(indices) {
  result = 1
  if (sum(indices) > 2) {
    huhn <- lda(X[,indices==1], Y, CV=TRUE)$posterior
    result = sum(Y != dimnames(huhn)[[2]][apply(huhn, 1,
                                                function(x)
                                                  which(x == max(x)))]) / length(Y)
  }
  result
}

monitor <- function(obj) {
  minEval = min(obj$evaluations);
  plot(obj, type="hist");
}

woppa <- rbga.bin(size=40, mutationChance=0.05, zeroToOneRatio=10,
                  evalFunc=iris.evaluate, verbose=TRUE, monitorFunc=monitor)
###########################################################

evaluate <- function(string=c()) {
  print(string)
  returnVal = NA;
  if (length(string) == 2) {
    returnVal = abs(string[1]-pi) + abs(string[2]-sqrt(50));
  } else {
    stop("Expecting a chromosome of length 2!");
  }
  returnVal
}

obj_pop <<- list()

monitor <- function(obj) {
  # plot the population
  xlim = c(obj$stringMin[1], obj$stringMax[1]);
  ylim = c(obj$stringMin[2], obj$stringMax[2]);
  par(mfrow=c(5,5),new=TRUE)
  #plot(obj$population, xlim=xlim, ylim=ylim, 
  #     xlab="pi", ylab="sqrt(50)");
  obj_pop[[length(obj_pop)+1]] <<- obj$population
}

rbga.results = rbga(c(1, 1), c(5, 10),popSize = 100,iters = 9, monitorFunc=monitor, 
                    evalFunc=evaluate, verbose=TRUE, mutationChance=0.01)

plot(rbga.results)
plot(rbga.results, type="hist")
plot(rbga.results, type="vars")

par(mfrow=c(3,3),mar=c(1,1,1,1))

for(i in 1:length(obj_pop))
{
  plot(obj_pop[[i]],xlim=c(1,5), ylim=c(1,10), xlab="pi", ylab="sqrt(50)")
}
############################################################
library(NMOF)
size <- 20L  ### the length of the string
OF <- function(x, y) sum(x != y)
y <- runif(size) > 0.5
x <- runif(size) > 0.5
OF(y, y)     ### the optimum value is zero
OF(x, y)
algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
             printBar = TRUE)
sol <- GAopt(OF, algo = algo, y = y)

## show differences (if any: marked by a '^')
cat(as.integer(y), "\n", as.integer(sol$xbest), "\n",
    ifelse(y == sol$xbest , " ", "^"), "\n", sep = "")

algo$nP <- 3L  ### that shouldn't work so well
sol2 <- GAopt(OF, algo = algo, y = y)

## show differences (if any: marked by a '^')
cat(as.integer(y), "\n", as.integer(sol2$xbest), "\n",
    ifelse(y == sol2$xbest , " ", "^"), "\n", sep = "")

############################################################
field <- rnorm(10000,100,100)
matrix_field <- matrix(field,nrow=100)

library(genalg)

evaluate <- function(string=c()) {
  #print(string)
  returnVal = NA;
  if (length(string) == 2) {
    returnVal = matrix_field[string[1],string[2]]
  } else {
    stop("Expecting a chromosome of length 2!");
  }
  returnVal
}

obj_pop <<- list()

monitor <- function(obj) {
  # plot the population
  xlim = c(obj$stringMin[1], obj$stringMax[1]);
  ylim = c(obj$stringMin[2], obj$stringMax[2]);
  obj_pop[[length(obj_pop)+1]] <<- obj$population
}

rbga.results = rbga(c(1, 1), c(100, 100),popSize = 1000,iters = 27, monitorFunc=monitor, 
                    evalFunc=evaluate, verbose=TRUE, mutationChance=0.01)

plot(rbga.results)
plot(rbga.results, type="hist")
plot(rbga.results, type="vars")

par(mfrow=c(3,3),mar=c(1,1,1,1))

for(i in 1:length(obj_pop))
{
  plot(obj_pop[[i]],xlim=c(1,100), ylim=c(1,100), xlab="x", ylab="y")
}
############################################################
library(NMOF)

matrix_field

size <- 16L  ### the length of the string
OF <- function(x) {
  x <- as.integer(x)
  x <- as.character(x)
  x <- paste(x,collapse = "")
  x <- strtoi(x,2L)
  x <- 1+((x-0)/(2^16-1))*(10000-1)
  matrix_field[x]
}

algo <- list(nB = size, nP = 20L, nG = 100L, prob = 0.002,
             printBar = TRUE)
sol <- GAopt(OF, algo = algo)

## show differences (if any: marked by a '^')
cat(as.integer(y), "\n", as.integer(sol$xbest), "\n",
    ifelse(y == sol$xbest , " ", "^"), "\n", sep = "")

algo$nP <- 3L  ### that shouldn't work so well
sol2 <- GAopt(OF, algo = algo, y = y)

## show differences (if any: marked by a '^')
cat(as.integer(y), "\n", as.integer(sol2$xbest), "\n",
    ifelse(y == sol2$xbest , " ", "^"), "\n", sep = "")
############################################################
library(mcga)
?mcga
