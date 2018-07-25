n = 3798
sample_strategies = t(rmultinom(n,100,rep(1/10,10)))
summary_pl = data.frame(profit=1:dim(data1)[1], loss = 1:dim(data1)[1])
for(i in 1:n){
  sample_strategies[i,] = sample(sample_strategies[i,], 10)
}
data2 = rbind(as.matrix(data2),sample_strategies)

for(i in 1:dim(data1)[1]){
  y = data1[i,1:10]
  profit_loss = apply(sample_strategies, 1, function(x) {sum((x < y)*c(1:10))})
  summary_pl[i,] = performance(profit_loss)[1,]
}

proft_loss = rep(0, dim(sample_strategies)[1])
for(i in 1:length(proft_loss)){
  proft_loss[i] = sum((data1[1,2:11] > sample_strategies[i,1:10] )*c(1:10))
}
summary_pl2 = data.frame(profit=1:dim(data2)[1], loss = 1:dim(data2)[1])

for(i in 1:dim(data2)[1]){
  y = data2[i,1:10]
  profit_loss = apply(data2, 1, function(x) {sum((x < y)*c(1:10))})
  summary_pl2[i,] = performance(profit_loss)[1,]
}

data1 = read.csv("castle-solutions.csv")
data1 = data1[,1:10]
data2 = dplyr::distinct(data1)
dim(data2)

performance = function(profit_loss){
  n = length(profit_loss)
  profit = sum(profit_loss >= 28)
  loss = sum(profit_loss <= 27)
  df = data.frame(profit = profit/n*100, loss = loss/n*100)
  df
}

square_strategy = function(s, resource = 90){
  (s^2/(sum(s^2))*resource)
}
strategy_perf = function(s){
  y = s
  profit_loss = apply(data2, 1, function(x) {sum((x < y)*c(1:10))})
  performance(profit_loss)[1,]
}
s1 = c(0,3,4,7,16,24,4,34,4,4)
s2 = c(0,3,5,9,15,21,5,34,4,4)

square_strategy(c(2,3,4,5,6,8),90)
s4 = c(0,3,5,9,15,21,4,37,3,3)
s5 = c(1,3,5,9,15,21,3,37,3,3)
square_strategy(1:7)
s6 = c(1,3, 6 ,10, 16, 23, 31,3,4,4)
s7 = c(1,3,5,9,15,21,3,27,3,3)
s8 = c(2,3,5,9,14,5,6,36,9,11)
strategy_perf(s1+rep(1,10))
strategy_perf(s2)
strategy_perf(s3)
strategy_perf(s4)
strategy_perf(s5 + 0*rep(1,10))
strategy_perf(s6 + -1*rep(1,10))
strategy_perf(s8)
