############# test on real data ##################


##################################################
############### election data ####################
##################################################

setwd("/home/weikangduan/Desktop/plan B")
nes = read.table('nes.txt',header=T)
library(nnet)
# head(nes)
# str(nes)

########## nonparametric estimation ##############

# sort data by income
attach(nes)
new_nes = nes[order(income),]
n = dim(new_nes)[1] 
ct = dim(new_nes)[2]
dat = matrix(0,nrow=ct,ncol=n)
for(i in 1:n){
  for(j in 1:ct){
    if(new_nes$pid[i] == 'republican'){
      dat[1,i] = 1
    }
    else if(new_nes$pid[i] == 'democrat'){
      dat[2,i] = 1
    }
    else{
      dat[3,i] = 1
    }
  }
}

# estimate
p_np = manysteps_multinom(y=dat,m=5,k=37,R=1e3,ct=3)

############ logistic regression ###############
mod = multinom(pid~income,data=nes)
summary(mod)
p_multi = predict(mod,newdata=data.frame(nes[,3]),type='probs')


### merge the probability together for comparison ###
republican_np = cbind(seq(1,944,1),rep('republican',944),p_np[[2]][1,])
democrat_np = cbind(seq(1,944,1),rep('democrat',944),p_np[[2]][2,])
independent_np = cbind(seq(1,944,1),rep('independent',944),p_np[[2]][3,])

# data for nonparametic method
df_np = rbind(democrat_np,republican_np,independent_np)
colnames(df_np) = c('income_order','pid','probability')
df_np = as.data.frame(df_np)
df_np$income_order = as.numeric(as.character(df_np$income_order))
df_np$pid = as.factor(df_np$pid)
df_np$probability = as.numeric(as.character(df_np$probability))
#str(df_np)

# data for logistic regression
republican_multi = cbind(seq(1,944,1),rep('republican',944),p_multi[,3])
democrat_multi = cbind(seq(1,944,1),rep('democrat',944),p_multi[,1])
independent_multi = cbind(seq(1,944,1),rep('independent',944),p_multi[,2])
df_multi = rbind(democrat_multi,republican_multi,independent_multi)
colnames(df_multi) = c('income_order','pid','probability')
df_multi = as.data.frame(df_multi)
df_multi$income_order = as.numeric(as.character(df_multi$income_order))
df_multi$pid = as.factor(df_multi$pid)
df_multi$probability = as.numeric(as.character(df_multi$probability))
#str(df_multi)

###plot for np method
plt_np = ggplot(data=df_np,aes(x=income_order,y=probability))
plt_np = plt_np+geom_line(aes(color=pid),alpha=1/2,size=2)+scale_color_manual(values=c('#3366ff','#00cc66','#ff3366'))+facet_grid(pid~.)
plt_np = plt_np+labs(title='Estimation of Probability on Political ID by Nonparametric Model')
plt_np = plt_np+labs(x='Income_order')+labs(y='Probability')
plt_np

###plot for nonparametric method
plt_multi = ggplot(data=df_multi,aes(x=income_order,y=probability))
plt_multi = plt_multi+geom_line(aes(color=pid),alpha=1/2,size=2)+scale_color_manual(values=c('#3366ff','#00cc66','#ff3366'))+facet_grid(pid~.)
plt_multi = plt_multi+labs(title='Estimation of Probability on Political ID by Multi logit model')
plt_multi = plt_multi+labs(x='Income_order')+labs(y='Probability')
plt_multi

### compare prediction error ###
pred_y_multi = rep(NA,944)
for(i in 1:944){
  if(which.is.max(p_multi[i,])==1){
    pred_y_multi[i] = 'democrat'
  } 
  else if(which.is.max(p_multi[i,])==2){
    pred_y_multi[i] = 'independent'
  }
  else{
    pred_y_multi[i] = 'republican'
  }
}

pred_y_np = rep(NA,944)
for(i in 1:944){
  if(which.is.max(p_np[[2]][,i])==1){
    pred_y_np[i] = 'republican'
  } 
  else if(which.is.max(p_np[[2]][,i])==2){
    pred_y_np[i] = 'democrat'
  }
  else{
    pred_y_np[i] = 'independent'
  }
}

# check error
error_multi = 0
error_np = 0
for(i in 1:944){
  if(new_nes$pid[i] != pred_y_multi[i]){
    error_multi = error_multi+1
  }
  if(new_nes$pid[i] != pred_y_np[i]){
    error_np = error_np+1
  }
}

##### bat #####
bat = read.table("bat.txt",header=T)

#str(bat)
new_bat = bat[order(bat$temp),]
failed = new_bat$failed
p_bat = manysteps2(y=failed,m=3,k=5,R=1e3)[[2]]
dat = as.data.frame(cbind(p_bat,new_bat$temp))
colnames(dat) = c('Probability','Temp')
###plot for np method
plt_bat = ggplot(data=dat,aes(x=Temp,y=Probability))
plt_bat = plt_bat+geom_line(alpha=1/2,size=4)
plt_bat = plt_bat+labs(x='Temperature')+labs(y='Probability

