# 06_selfishmodelanalysis
rm(list=ls())
library(coopSelfHerd)

n.gener.plot = 2000
split = "1.5"
mutate = "0.05"
par(mfrow=c(3,1))
par(mar = c(4,4,4,4))
sq = seq(1,n.gener.plot,1)


fold = file.path (PROJHOME , "Output" , "selfishherdmodel")
folds = list.files(fold)
spl = stringr::str_split_fixed(folds,"_",10)
fold2 = folds[which( spl[,2] == split & spl[,6] == mutate)]
files = list.files(file.path(fold, fold2))
spl = stringr::str_split_fixed(files,"_",15)
spl[,6] = stringr::str_split_fixed(spl[,6],"\\.",2)[,1]

### B self
Bcoop = "0.5"
n.i = "10"

wh = which( spl[,2] == Bcoop & spl[,6] == n.i)
files2 = files[wh]
files2 = files2[order(as.numeric(spl[wh,4]))]
load(file.path(fold,fold2, files2[1]))
dat = lis.[[2]]
d2 = dat[,1,sq]
mean = colMeans(d2)
plot(mean~sq,type="l",ylim = c(0,1))
abline(h=0.5,lty=2)
for ( i in 2:length(files2)){
    #i=2
    load(file.path(fold,fold2, files2[i]))
    tb=lis.[[1]]
    dat = lis.[[2]]
    d2 = dat[,1,sq]
    mean = colMeans(d2)
    lines(mean~sq,col=i)
}

### B coop
Bself = "5"
n.i = "10"

wh = which( spl[,4] == Bself & spl[,6] == n.i)
files2 = files[wh]
files2 = files2[order(as.numeric(spl[wh,2]))]
load(file.path(fold,fold2, files2[1]))
dat = lis.[[2]]
d2 = dat[,1,sq]
mean = colMeans(d2)
plot(mean~sq,type="l",ylim = c(0,1))
abline(h=0.5,lty=2)
for ( i in 2:length(files2)){
  #i=2
  load(file.path(fold,fold2, files2[i]))
  tb=lis.[[1]]
  dat = lis.[[2]]
  d2 = dat[,1,sq]
  mean = colMeans(d2)
  lines(mean~sq,col=i)
}

### N i
Bcoop = "0.5"
Bself = "5"

wh = which( spl[,2] == Bcoop & spl[,4] == Bself)
files2 = files[wh]
files2 = files2[order(as.numeric(spl[wh,6]))]
load(file.path(fold,fold2, files2[1]))
dat = lis.[[2]]
d2 = dat[,1,sq]
mean = colMeans(d2)
plot(mean~sq,type="l",ylim = c(0,1))
abline(h=0.5,lty=2)
for ( i in 2:length(files2)){
  #i=2
  load(file.path(fold,fold2, files2[i]))
  tb=lis.[[1]]
  dat = lis.[[2]]
  d2 = dat[,1,sq]
  mean = colMeans(d2)
  lines(mean~sq,col=i)
}
load(file.path(PROJHOME, "Output", "ni=50","Bcoop_0.5_Bself_5_n.i_50.rda"))
dat = lis.[[2]]
d2 = dat[,1,sq]
mean = colMeans(d2)
lines(mean~sq,col=4)
df = data.frame(var = rnorm(90), x = 1:90,
                Bself = as.factor(rep(c(2,5,15     ),30)),
                Bcoop = as.factor(rep(c(0.2,0.5,0.8),30)),
                Ni    = as.factor(rep(c(5,10,25    ),30)))



load(file.path(PROJHOME, "Output", "ni=75","Bcoop_0.5_Bself_5_n.i_75.rda"))
dat = lis.[[2]]
d2 = dat[,1,sq]
mean = colMeans(d2)
lines(mean~sq,col=5)

library(ggplot2)
df = data.frame(var = rnorm(90), x = 1:90,
                Bself = as.factor(rep(c(2,5,15     ),30)),
                Bcoop = as.factor(rep(c(0.2,0.5,0.8),30)),
                Ni    = as.factor(rep(c(5,10,25    ),30)))# ggplot(df, aes(var, x, col = Bself ))+
#   geom_line()+
#   theme_classic()
#
# ggplot(df, aes(var, x, col = Bcoop ))+
#   geom_line()+
#   theme_classic()
#
# ggplot(df, aes(var, x, col = Ni ))+
#   geom_line()+
#   theme_classic()
