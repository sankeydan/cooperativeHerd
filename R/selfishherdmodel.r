selfishherdmodel = function(
  Bcoop               = NULL,
  Bself               = NULL,
  n.grp               = NULL,
  n.i                 = NULL,
  split.n.i.multip    = NULL,
  n.generations       = NULL,
  mutate.sd           = NULL,
  starting.delta      = 0.5


){

  # vars for changing

  # Bcoop               = 0.5
  # n.grp               = 300
  # n.i                 = 10
  # split.n.i.multip    = 1.5
  # Bself               = 20
  # n.generations       = 3000
  # mutate.sd           = 0.1
  {
    # vars not changing
    max.ni = n.i * split.n.i.multip
    max.ni = round(max.ni)
    pop = lapply( seq_len(n.grp),function(X) rep(starting.delta,n.i))
    grp.ids = 1:n.grp
    new.grp.id = n.grp+1
    total.pop = length(unlist(pop))


    # save to array
    pop.save = array(NA, c(total.pop,2,n.generations))


    # Play / evolve
    for ( i in 1:n.generations){


      # population level stuff
      choice = lapply( pop, function(x) rbinom (length(x),1,  x))
      prop.ali = unlist( lapply(choice,function(x)sum(x)/length(x)))
      chance.pred.miss = prop.ali * Bcoop
      pred.any = rbinom( length(pop), 1, (1-chance.pred.miss) )
      extinct = rep(NA, length(pop))

      # group level stuff
      for( j in 1:length(pop)){
        #j=1
        po = pop[[j]]
        ch = choice[[j]]

        # Predate
        if ( pred.any[j] == 1){
          len = length(po)
          c.o.p = ifelse ( ch, 1/len, (1/len)/Bself ) # chance of predation
          c.o.p = c.o.p * 1 / sum(c.o.p)
          pred = sample(1:len,1,prob =  c.o.p)
          po = po[-pred]
        }


        # Replicate
        if( length(po)>0){
          newby= po[sample(1:length(po),1)]
          new2 = rnorm(1,newby,mutate.sd)
          new2 = ifelse( new2 > 1, 1, ifelse (new2 < 0,0,new2))
          po = c(po,new2)
        }

        # split
        if ( length(po) >= max.ni){
          sam = sample(1:length(po))
          uni = round(runif(1,2,length(po)))
          po2 = po[sam[1:uni]]
          po  = po[sam[(uni+1):length(po)]]

          pop[[length(pop)+1]] = po2
          grp.ids = c(grp.ids, new.grp.id)
          new.grp.id = new.grp.id+1

        } else{
          pop[[j]] = po
        }

        # Extinction?
        if( length(po)<2){
          extinct[j] = T
        } else {
          extinct[j] = F
        }

      }

      # Extinction?
      wh.ex = which(extinct)
      while( length(wh.ex)>0){
        pop[[wh.ex[1]]] = NULL
        grp.ids = grp.ids[-wh.ex[1]]
        wh.ex=wh.ex[-1]-1
      }

      # Reduce or increase population back to starting amount
      new.pop.total = length(unlist(pop))
      over.by = new.pop.total- total.pop

      if(over.by<0){
        cbadd = cbind(unlist(pop), rep( 1:length(pop), unlist(lapply(pop,length))))
        cbadd = rbind( cbadd, cbadd[ sample(1:nrow(cbadd),abs(over.by)),])
        pop = split(cbadd[,1],cbadd[,2])
        names(pop) = NULL
      }
      if(over.by>0){
        len = length(pop)
        cbrem = cbind(unlist(pop), rep( 1:length(pop), unlist(lapply(pop,length))))
        cbrem = cbrem[ - sample(1:nrow(cbrem),over.by),]
        pop = split(cbrem[,1],cbrem[,2])
        ex2 = which(! as.character(1:len)%in% names(pop) ) # more extinction afer removal
        if( length(ex2)>0){
        grp.ids = grp.ids[-ex2]
        }
        names(pop) = NULL
      }


      # save to array
     pop.save[,,i] = cbind(unlist(pop), rep( grp.ids, unlist(lapply(pop,length))))
    }
  }

  return(pop.save)
}

