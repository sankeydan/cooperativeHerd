# 06_selfishmodelanalysis
rm(list=ls())
library(coopSelfHerd)
library(ggplot2)
library(gtable)
library(grid)


fold = file.path (PROJHOME , "Output" , "newselfishherdmodel")
folds = list.files(fold)
files = list.files(file.path(fold, folds2[1]))
for( i in 1:length(folds)){
  matVars = matrix ( stringr::str_split_fixed(folds[i],"_",10),2)
  nam = matVars[1,]
  matVars = as.numeric(matVars[2,])
  names(matVars)= nam
  for ( j in 1:length(files)){
    # i=1
    # j=1
    load(file.path(fold,folds[i],files[j]))
    comb. = c ( matVars, unlist (lis.[[1]]), delta = mean( lis.[[2]][(matVars["n.generations"]-100):matVars["n.generations"]]))
    if( i == 1 & j == 1){
      comb.dat = comb.
    }else {
      comb.dat = rbind( comb.dat, comb.)
    }
  }
}
comb.dat = as.data.frame(comb.dat)
row.names(comb.dat) = NULL

##### MUTATION SD###
dat = comb.dat [comb.dat$startingD == 0.5,]
dat = dat[dat$split.n.i.multip == 1.5,]
dat$n.i = as.factor ( dat$n.i)
p = ggplot(dat , aes ( x = mutate.sd , y = delta,col = n.i, by = Bself))
p = p + facet_grid( dat$Bself ~
                    dat$Bcoop, scales = "free")
p = p +  geom_hline(data = dat,  linetype = "dashed",aes( yintercept =   0.5))
p = p +  geom_vline(data = dat,  linetype = "dotted",aes( xintercept =  0.05))
p = p + ylim(c(0,1))
p = p + xlab ( "Mutate SD")
p = p+ geom_line(lwd = 1.2)
p + theme(plot.margin = unit(c(1.5,1.5,0.2,0.2), "cm"))
grid::grid.text(unit(0.98,"npc"),0.5,label = 'Bself', rot = 270) # right
grid::grid.text(unit(0.5,"npc"),unit(.98,'npc'),label = 'Bcoop', rot = 0)   # top

##### G MAX ###
dat = comb.dat [comb.dat$startingD == 0.5,]
dat = dat[dat$mutate.sd == 0.05,]
dat$n.i = as.factor ( dat$n.i)
p = ggplot(dat ,  aes ( x = split.n.i.multip , y = delta,col = n.i, by = Bself))
p = p +  geom_hline(data = dat,  linetype = "dashed",aes( yintercept =  0.5))
p = p +  geom_vline(data = dat,  linetype = "dotted",aes( xintercept =  1.5))
p = p + facet_grid( dat$Bself ~
                    dat$Bcoop, scales = "free")
p = p + cex.a
p = p + ylim(c(0,1))
p = p + xlab ( "Gmax", cex = 1.5)
p = p+ geom_line(lwd = 1.2)
p + theme(plot.margin = unit(c(1.5,1.5,0.2,0.2), "cm"))
grid::grid.text(unit(0.98,"npc"),0.5,label = 'Bself', rot = 270) # right
grid::grid.text(unit(0.5,"npc"),unit(.98,'npc'),label = 'Bcoop', rot = 0)   # top

