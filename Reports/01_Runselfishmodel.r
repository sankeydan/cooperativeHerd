rm(list = ls())
##### EDIT HERE

plot.mod = T

split.n.i.multip  = c(1.5 , 1.2, 1.8, 1.5,1.5)
n.generations     = 5000
mutate.sd         = c(0.05,0.05,0.05,0.01,0.1)
n.grp             = 1000
starting.delta    = 0.5
Bcoop =(c(0.2,0.5,0.8))
Bself =(c(2,5,15))
n.i   =(c(5,10,25))

for ( j in 2:length(split.n.i.multip)){

  #j=1

  ##### SCRIPT
  table = expand.grid(
    Bcoop =Bcoop ,
    Bself =Bself ,
    n.i   =n.i   )
  nam = dimnames(table)[[2]]
  fold.path = file.path( PROJHOME , "output" , "newselfishherdmodel",paste0(
    "split.n.i.multip_" , split.n.i.multip[j] ,
    "_n.generations_"    , n.generations    ,
    "_mutate.sd_"        , mutate.sd[j]       ,
    "_n.grps_"          , n.grp,
    "_startingD_" , starting.delta))
  if( !dir.exists( fold.path)){
    dir.create (  fold.path)
  }

  library(coopSelfHerd)

  for (  i in 1:nrow(table)){
    #i=1
    t = Sys.time()
    shm = selfishherdmodel(
      Bcoop = table$Bcoop [i],
      Bself = table$Bself [i],
      n.i   = table$n.i   [i],

      split.n.i.multip    = split.n.i.multip   [j]         ,
      n.generations       = n.generations               ,
      mutate.sd           = mutate.sd            [j]       ,
      n.grp               = n.grp,
      starting.delta = starting.delta
    )

    dat = shm
    d2 = dat[seq(1,dim(dat)[1],length.out = 300),1,]
    mean = colMeans(d2)
    if( plot.mod){
      plot(mean,type="l",ylim = c(0,1))
    }
    lis. = list( table[i,], mean)
    file = paste( t(cbind(nam,t(table[i,]))), collapse = "_")
    file.rda = paste0(file, ".rda")
    save( lis. , file = file.path (fold.path, file.rda))
    print(paste( i, "/", nrow(table)))
    print( Sys.time()-t)
  }

}
