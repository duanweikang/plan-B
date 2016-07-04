### multinomial extension ###

# getpartition data is used to get the initial partition
# the function will return the number of elements in each subinterval

getpartition = function(m,k,n){
  #m is the min size of each set in the partition
  #k is the number of partitons
  #n is the total number of points to be partitioned
  dum = 0
  while(dum<100){
    foo = c(0,sort(sample(m:(n-m+1),k-1)),n)
    foo1 = foo[2:(k+1)]-foo[1:k]
    if(min(foo1) >= m){ 
      ans = foo1
      dum = 105
    }
    else dum<-dum+1
  }
  if(dum>101) {
    return(ans)
  }
  else {
    return(NULL)
  }
}


# getszinfo will take the partition result and data as input
# the function will return two elements
# the number of 1s in each subinterval for each category
# the loglikelhood for each subinterval

getszinfo_multinom = function(sz,y,ct){
  k = length(sz)
  dd = c(0,cumsum(sz))
  tysz = matrix(rep(0,ct*k),nrow=ct,ncol=k)
  lgprb = NULL
  
  for(i in 1:k){
    for(j in 1:ct){
      tysz[j,i] = sum(y[j,(dd[i]+1):dd[i+1]])
    }
  }
  p = matrix(rep(0,ct*k),nrow=ct,ncol=k)
  for(i in 1:k){
    for(j in 1:ct){
      p[j,i] = tysz[j,i]/sz[i]
    }
  }
  
  for(i in 1:k){
    lgprb = c(lgprb,dmultinom(tysz[,i],sz[i],p[,i],log=TRUE))
  }
  return(list(tysz,lgprb))
}

# onestep function will propose a change on partition
# calculate the log likelihood of old and new partition choices
# and decided to whether to move new parition or stay in old position

onestep_multinom = function(sz,tysz,lgprb,y,m,k,ct){
  newsz = sz
  #      newtysz<-tysz
  #      newlgprb<-lgprb
  
  dum = (1:k)[sz>m]
  lndum = length(dum)
  if(lndum==1){
    di = dum
  }
  else {
    di = sample(dum,1)
  } #we will remove guys from sz[di]
  # determine the direction of move, 1 for right, -1 for left
  dn = sample(1:(sz[di]-m),1) #this is the number to be moved
  if(di==1){
    dd = 1
  } 
  else if(di==k){
    dd = -1
  }
  else{
    dd = sample(c(-1,1),1) #this is the direction we move them; 1=to right
  }
  if(dd==1){ 
    newsz[c(di,di+1)] = newsz[c(di,di+1)] +c(-dn,dn) 
  }
  else{  
    newsz[c(di-1,di)] = newsz[c(di-1,di)] + c(dn,-dn) 
  }
  
  if(min(newsz)<m){
    write(c(sz,di,dn,dd,newsz),file="error.txt")
  }
  foo = getszinfo_multinom(newsz,y,ct)
  # we will get the log likelihood of new partition
  newtysz = foo[[1]]
  newlgprb = foo[[2]]
  newdum = (1:k)[newsz>m]
  lnnewdum = length(newdum)
  # in the five cases i am finding log(p(o|n)/p(n|o))=lgrt
  # and decide wether to move or not
  if(di==1){
    lgrt = (log((1/lnnewdum)*(1/(newsz[2]-m))*(1/2)) -
              log((1/lndum)*(1/(sz[1]-m))*1))
  }
  else if(di==2 & dd==-1) {
    lgrt = (log((1/lnnewdum)*(1/(newsz[1]-m))*1) -
              log((1/lndum)*(1/(sz[2]-m))*(1/2)))
  }
  else if(di==k){
    lgrt = (log((1/lnnewdum)*(1/(newsz[k-1]-m))*(1/2)) -
              log((1/lndum)*(1/(sz[k]-m))*1))
  }
  else if (di==k-1 & dd==1) {
    lgrt = (log((1/lnnewdum)*(1/(newsz[k]-m))*1) -
              log((1/lndum)*(1/(sz[k-1]-m))*(1/2)))
  }
  else if (di >= 2 & di<= k-2 & dd==1) {
    lgrt = (log((1/lnnewdum)*(1/(newsz[di+1]-m))*(1/2)) -
              log((1/lndum)*(1/(sz[di]-m))*(1/2)))
  }
  else if (di>=3 & di<=k-1 & dd==-1) {
    lgrt = (log((1/lnnewdum)*(1/(newsz[di-1]-m))*(1/2)) -
              log((1/lndum)*(1/(sz[di]-m))*(1/2)))
  }
  lgrtall = sum(newlgprb-lgprb) + sum(lgrt)
  #        return(lgrtall)
  if(runif(1) <= exp(lgrtall)){
    return(list(newsz,newtysz,newlgprb))
  }
  
  else return(0)
}

# manysteps will allow us to construct the markov chain 
# the markov chain will approximate the posterior distribution of partitions
# we will get the average of esimate for each observation in each partition as the final estimate
# the function will return the estimate and the acceptance rate of MCMC


# y is the response variable data, notes that it should be ordered based on predictor value
# m is the minimum number of observations in each subinterval
# k is the number of partitions in each partition
# R is the length of MCMC
# ct is the number of categories

manysteps_multinom = function(y,m,k,R,ct){
  n = dim(y)[2]
  sz = getpartition(m,k,n)
  if(length(sz)<1) return(0)
  #        cat("sz", sz,"\n")
  foo = getszinfo_multinom(sz,y,ct)
  tysz = foo[[1]]
  lgprb = foo[[2]]
  est = matrix(rep(0,k*ct),nrow=ct,ncol=k)
  for(i in 1:k){
    for(j in 1:ct){
      est[j,i]=tysz[j,i]/sz[i]
    }
  }
  e = NULL
  for(i in 1:k){
    step = matrix(rep(0,ct*sz[i]),nrow=ct,ncol=sz[i])
    for(j in 1:ct){
      step[j,] = rep(est[j,i],sz[i])
    }
    e = cbind(e,step)
  }
  
  mv = 0
  ansest = matrix(rep(0,ct*n),nrow=ct,ncol=n)
  dans = NULL
  # here we construct the Markov Chain
  for(i in 1:R){
    dum = onestep_multinom(sz,tysz,lgprb,y,m,k,ct)
    dans = c(dans,NULL)
    if(length(dum)==1) {
      ansest = ansest + e
    }
    else{
      mv = mv + 1
      sz = dum[[1]]
      tysz = dum[[2]]
      lgprb = dum[[3]]
      est = matrix(rep(0,k*ct),nrow=ct,ncol=k)
      for(i in 1:k){
        for(j in 1:ct){
          est[j,i]=tysz[j,i]/sz[i]
        }
      }
      e = NULL
      for(i in 1:k){
        step = matrix(rep(0,ct*sz[i]),nrow=ct,ncol=sz[i])
        for(j in 1:ct){
          step[j,] = rep(est[j,i],sz[i])
        }
        e = cbind(e,step)
      }
      ansest = ansest + e
    }
  }
  #        cat("% times moved", mv/R,"\n")
  return(list(mv/R,ansest/R))
}
