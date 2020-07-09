# P05_startingDelta = 0
rm(list= ls())
library(ggplot2)
fold = file.path ( PROJHOME , "Output", "newselfishherdmodel","split.n.i.multip_1.5_n.generations_5000_mutate.sd_0.05_n.grps_1000_startingD_0.5" )
files = list.files(fold)
spl = stringr::str_split_fixed(files,"_",15)
spl[,6] = stringr::str_split_fixed(spl[,6],"\\.",2)[,1]
for ( i in 1:length(files)){
  #i=1
  load (file.path ( fold, files[i]) )
  vars = lis.[[1]]
  df = data.frame ( Bcoop = vars$Bcoop,
                    Bself = vars$Bself,
                    Ni    = vars$n.i,
                    delta = lis.[[2]])
  df$generation = row.names(df)
  if ( i == 1){
    dfs = df
  } else {
    dfs = rbind ( dfs , df)
  }
}

dfs$Ni = as.factor(dfs$Ni)
dfs$generation = as.numeric(dfs$generation)
p = ggplot(dfs , aes ( x = generation , y = delta,col = Ni))
p = p + facet_grid( dfs$Bself ~
                      dfs$Bcoop, scales = "free")
p = p +  geom_hline(data = dfs,  linetype = "dashed",aes( yintercept =   0.5))
p = p + ylim(c(0,1))
p = p+ geom_line(lwd = 1.2)
p + theme(plot.margin = unit(c(1.5,1.5,0.2,0.2), "cm"))
grid::grid.text(unit(0.98,"npc"),0.5,label = 'Bself', rot = 270) # right
grid::grid.text(unit(0.5,"npc"),unit(.98,'npc'),label = 'Bcoop', rot = 0)   # top

