rm(list = ls())
library(plyr)
library(ggplot2)
folderDS(c("Output", "Plotdata"))
if( !file.exists(file.path(PROJHOME , "Output", "Plotdata","P01_grpsGettingSelfish.rda"))){
  #i=1
  s = selfishherdmodel (
    Bcoop               = 0.5,
    n.grp               = 1000,
    n.i                 = 10,
    split.n.i.multip    = 1.5,
    Bself               = 15,
    n.generations       = 500,
    mutate.sd           = 0.05)
  ad =adply(s,3)

  save(ad, file=file.path(PROJHOME , "Output", "Plotdata","P01_grpsGettingSelfish.rda"))
} else {
  load(file.path(PROJHOME , "Output", "Plotdata","P01_grpsGettingSelfish.rda"))
}

names(ad) = c("generation", "score", "group.id")
ad$generation = as.numeric(ad$generation)

tb=table(ad$group.id,ad$generation)
{
  par(mar = c(5,5,25,5))
  lenout = 65
  plot( 1:max(ad$generation),type = "n", ylim = c(0,1),xlab = "Generation", ylab = "")

  cols = rainbow(lenout)
  sq =round(seq(2,nrow(tb),length.out = lenout))
  for ( j in 1:length(sq)){
    #nrow(tb)){
    #j=2
    ad2 = ad[ad$group.id == sq[j],]
    lines(tapply( ad2$score,c(ad2$generation),mean)~unique(ad2$generation),col = cols[j] ,lwd=2)
    print(j)
  }
  lines(tapply(ad$score,ad$generation,mean),lwd =4)
  abline(h=0.5,lty=2)
}

