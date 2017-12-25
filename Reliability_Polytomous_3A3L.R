setwd("C:\\Users\\LiuWei\\Documents")

library(CDM)
# library(polycor)
##############  define parameters ##################
L = 3   ############# polytomous level for attribute
temp <- read.table("3A-3L.txt")
temp = temp[-1,]
##############         50 items
# q.matrix = t(temp)
############            25 items
temp1 = t(temp)
q.matrix = temp1[1:25,]

# slip = rep(0.1,nrow(q.matrix))
# guess = slip
NumofItem = nrow(q.matrix)

####################  high quality
slip <- runif(NumofItem,0.05,0.15)
guess <- runif(NumofItem,0.05,0.15)


temp = q.matrix
for(i in 1:nrow(temp))
{
  for (j in 1:ncol(temp))
  {
    if(temp[i,j] != 0) temp[i,j]=1
  }
}
q_collapsed = temp

##############  generate examinee ##################
N = 2000
m=diag(ncol(q.matrix))
m[m==0] = 0.5               ##### ???????????????????????????######
normsim <- rmvnorm( N, rep(0,ncol(q.matrix)), m)

# ##############  when L=3 #############
z1 = qnorm(0.25)
z2 = qnorm(0.5)
z3 = qnorm(0.75)
for (i in 1:N)
{
  for (j in 1:ncol(q.matrix))
  {
    if(normsim[i,j] < z1) normsim[i,j]=0
    else if(z1 < normsim[i,j] && normsim[i,j] <= z2) normsim[i,j] = 1
    else if(z2 < normsim[i,j] && normsim[i,j] <= z3) normsim[i,j] = 2
    else normsim[i,j] = 3
  }
}
KS_True = normsim





write.table(slip,file="slip.txt")
write.table(guess,file="guess.txt")
write.table(KS_True,file="KS_True.txt")
write.table(q.matrix,file="q.matrix.txt")
write.table(q_collapsed,file="q_collapsed.txt")




##################################################################################################################################
##################################################################################################################################
##                           Consistency
##################################################################################################################################
##################################################################################################################################
####################################################################################
####################################################################################
I = 2           ############# 产生重复作答数据的次数
K = 1
P = ncol(q.matrix)+1             ############# 概率之间计算相关的列数
P2 =  (L+1)*ncol(q.matrix)           ############# 计算PCCR时属性的列数（个数）
judge=matrix(NA,1,((I*(I-1))/2))      ######本质：模式判准率的计算
skill=matrix(NA,((I*(I-1))/2),ncol(q.matrix))    ######本质：属性判准率的计算
####################################################################################
####################################################################################

for(zzz in 1:I)
{
  xx=w=array(NA,c(N,nrow(q.matrix),ncol(q.matrix)))
  eta = matrix(0,ncol = nrow(q.matrix) ,nrow = N)
  prob = matrix(0,ncol = nrow(q.matrix) , nrow = N)
  response = matrix(0,ncol = nrow(q.matrix) , nrow = N)
  for(i in 1:N){
    for(j in 1:nrow(q.matrix)){
      for(k in 1:ncol(q.matrix)){w[i,j,k]=ifelse(KS_True[i,k] >= q.matrix[j,k],1,0)}
      xx[i,j,]=w[i,j,]^q_collapsed[j,]
      eta[i,j]=prod(xx[i,j,1:ncol(q.matrix)])
      prob[i,j]=guess[j]+(1-guess[j]-slip[j])*eta[i,j]
      r=runif(1)
      if(prob[i,j] >= r)  response[i,j]=1  else response[i,j]=0
    }
  }
  ################  Using pgDINA model to estimate the response ####################
  DINA <- gdina(response , q.matrix=q.matrix , rule="DINA", calc.se = F, progress = F)
  Attribute_DINA <- IRT.factor.scores(DINA, type='MAP')  ### read estimate profile
  postrior_distribution = DINA$posterior ##每个被试的后验概率分布
  
  
  
  
  #####------------------ Calculate the posterior of each L level --------------------
  mapFun <- function(Attribute_DINA, postrior_distribution){
    L <- ncol(Attribute_DINA)
    temp <- apply(postrior_distribution, 1, which.max)
    cDINA <- cbind(temp, Attribute_DINA)
    mappRelation <- unique(cDINA)
    return(mappRelation[order(mappRelation[,1]),])
  }
  L <- 4
  l <- 3
  mapRelation <- mapFun(Attribute_DINA, postrior_distribution)
  clsProbFun <- function(mapRelation){
    res <- array(dim=c(nrow(postrior_distribution), L, l))
    for(i in 1:L){ #i表示等于0,1,2
      for(j in 1:l){ #j 表示等于某种技能
        res[,i,j] <- apply(postrior_distribution, 1, function(x) sum(x[mapRelation[,j+1]==i-1]))
      }
    }
    return(res)
  }
  finalRes <- clsProbFun(mapRelation)
  
  
  postrior_KS = DINA$pattern[,5]
  postrior_A1_distr = finalRes[,,1]   ##### 只是属性1的保存结果。共3列，分别表示alpha=0，alpha=1，alpha=2的边际概率
  postrior_A2_distr = finalRes[,,2]   ##### 只是属性2的保存结果。共3列，分别表示alpha=0，alpha=1，alpha=2的边际概率
  postrior_A3_distr = finalRes[,,3]   ##### 只是属性3的保存结果。共3列，分别表示alpha=0，alpha=1，alpha=2的边际概率
  
  
  temp = cbind(Attribute_DINA,postrior_KS,postrior_A1_distr,postrior_A2_distr,postrior_A3_distr)
  assign(paste("Loop", zzz, sep = ""), temp)
}




######################## 开始计算100次作答两两配对之间的PCCR,ACCP #########################  
Loop=mget(paste("Loop",1:I,sep=""))
for(i in 1:I)
{
  x1 = matrix(unlist(Loop[i]),N,P+P2)
  j = i+1
  while(j<=I)
  {
    x2 = matrix(unlist(Loop[j]),N,P+P2)
    judge[1,K]=sum(apply((x1[,1:ncol(q.matrix)]==x2[,1:ncol(q.matrix)]),1,all))/N   ##### 模式判准率
    skill[K,]=t(apply(matrix(as.numeric(x1[,1:ncol(q.matrix)]==x2[,1:ncol(q.matrix)]),nrow=N),2,sum)/N)   #### 属性判准率
    K=K+1
    j=j+1
  }
}

PCCR_MEAN=mean(judge)
skill_MEAN=apply(skill,2,mean)














slip = read.table("slip.txt")
slip = as.matrix(slip)
guess = read.table("guess.txt")
guess = as.matrix(guess)
KS_True = read.table("KS_True.txt")
KS_True = as.matrix(KS_True)
q.matrix = read.table("q.matrix.txt")
q.matrix = as.matrix(q.matrix)
q_collapsed = read.table("q_collapsed.txt")
q_collapsed = as.matrix(q_collapsed)
K = ncol(q.matrix)
NumofItem = nrow(q.matrix)


############ Wang's method consistency
gama_1_final = matrix(0,ncol = 1,nrow = 1)
gama_2_final = matrix(0,ncol = 1,nrow = 1)
gama_3_final = matrix(0,ncol = 1,nrow = 1)
gama_pattern_final = matrix(0,ncol = 1,nrow = 1)


############ Wang's method accuracy
tao_A1_final = matrix(0,ncol = 1,nrow = 1)
tao_A2_final = matrix(0,ncol = 1,nrow = 1)
tao_A3_final = matrix(0,ncol = 1,nrow = 1)
tao_pattern_final = matrix(0,ncol = 1,nrow = 1)


for (v in 1:2)
{
  ################ using pgDINA model to generate response ###############
  xx=w=array(NA,c(N,nrow(q.matrix),ncol(q.matrix))) 
  eta = matrix(0,ncol = nrow(q.matrix) ,nrow = N)
  prob = matrix(0,ncol = nrow(q.matrix) , nrow = N)
  response = matrix(0,ncol = nrow(q.matrix) , nrow = N)
  for(i in 1:N){
    for(j in 1:nrow(q.matrix)){
      for(k in 1:ncol(q.matrix)){w[i,j,k]=ifelse(KS_True[i,k] >= q.matrix[j,k],1,0)}
      xx[i,j,]=w[i,j,]^q_collapsed[j,]
      eta[i,j]=prod(xx[i,j,1:ncol(q.matrix)])
      prob[i,j]=guess[j]+(1-guess[j]-slip[j])*eta[i,j]
      r=runif(1)
      if(prob[i,j] >= r)  response[i,j]=1  else response[i,j]=0
    }
  }
  
  ################  Using pgDINA model to estimate the response ####################
  DINA <- gdina(response , q.matrix=q.matrix , rule="DINA", calc.se = F, progress = F)
  Attribute_DINA <- IRT.factor.scores(DINA, type='MAP')  ### read estimate profile
  postrior_distribution = DINA$posterior ##每个被试的后验概率分布
  
  #####------------------ Calculate the posterior of each L level --------------------
  mapFun <- function(Attribute_DINA, postrior_distribution){
    L <- ncol(Attribute_DINA)
    temp <- apply(postrior_distribution, 1, which.max)
    cDINA <- cbind(temp, Attribute_DINA)
    mappRelation <- unique(cDINA)
    return(mappRelation[order(mappRelation[,1]),])
  }
  mapRelation <- mapFun(Attribute_DINA, postrior_distribution)
  clsProbFun <- function(mapRelation, L=3){
    res <- array(dim=c(nrow(postrior_distribution), L, L))
    for(i in 1:L){ #i表示等于0,1,2
      for(j in 1:L){ #j 表示等于某种技能
        res[,i,j] <- apply(postrior_distribution, 1, function(x) sum(x[mapRelation[,j+1]==i-1]))
      }
    }
    return(res)
  }
  finalRes <- clsProbFun(mapRelation)
  
  postrior_KS = DINA$pattern[,5]
  postrior_A1_distr = finalRes[,,1]   ##### 只是属性1的保存结果。共3列，分别表示alpha=0，alpha=1，alpha=2的边际概率
  postrior_A2_distr = finalRes[,,2]   ##### 只是属性2的保存结果。共3列，分别表示alpha=0，alpha=1，alpha=2的边际概率
  postrior_A3_distr = finalRes[,,3]   ##### 只是属性3的保存结果。共3列，分别表示alpha=0，alpha=1，alpha=2的边际概率
  
  
  
  ################ Wang Wenyi  ##############################
  
  ###################  A1  ######################
  temp  = 0
  for (i in 1:N)
  {
    temp = temp + sum(postrior_A1_distr[i,]^2)
  }
  gama_1 = temp / N
  gama_1_final = rbind(gama_1_final,gama_1)
  
  
  ###################  A2  ######################
  temp  = 0
  for (i in 1:N)
  {
    temp = temp + sum(postrior_A2_distr[i,]^2)
  }
  gama_2 = temp / N
  gama_2_final = rbind(gama_2_final,gama_2)
  
  
  ###################  A3  ######################
  temp  = 0
  for (i in 1:N)
  {
    temp = temp + sum(postrior_A3_distr[i,]^2)
  }
  gama_3 = temp / N
  gama_3_final = rbind(gama_3_final,gama_3)
  
  
  ################ pattern reliability using wang's method  ######################
  temp = postrior_distribution * postrior_distribution
  temp1 = colSums(temp)
  temp2 = as.matrix(temp1)
  gama_pattern = colSums(temp2)/N
  gama_pattern_final = rbind(gama_pattern_final,gama_pattern)
  
  
  ################################################################################
  ################################################################################
  ################################################################################
  ################################################################################
  ###########  Calculate accuracy ################### 
  
  ######## A1 ##########
  temp = 0
  for (n in 1:N)
  {
    temp = temp + max(postrior_A1_distr[n,])
  }
  tao_A1 = temp / N
  tao_A1_final = rbind(tao_A1_final,tao_A1)
  
  
  
  ######## A2 ##########
  temp = 0
  for (n in 1:N)
  {
    temp = temp + max(postrior_A2_distr[n,])
  }
  tao_A2 = temp / N
  tao_A2_final = rbind(tao_A2_final,tao_A2)
  
  ######## A3 ##########
  temp = 0
  for (n in 1:N)
  {
    temp = temp + max(postrior_A3_distr[n,])
  }
  tao_A3 = temp / N
  tao_A3_final = rbind(tao_A3_final,tao_A3)
  
  ############## PATTERN ACCURACY
  tao_pattern = mean(postrior_KS)
  tao_pattern_final = rbind(tao_pattern_final,tao_pattern)
  
  
}  ###### 循环30次的结束


##########  Consistency ###########
########## wang A1
temp = gama_1_final[-1,]
temp = as.matrix(temp)    #####保存30次结果的矩阵
temp1 = apply(temp,2,mean)
ABS_gama_1 = apply(abs(temp - skill_MEAN[1]),2,sum)/v
RMSE_gama_1 = sqrt(apply((temp - skill_MEAN[1])^2,2,sum)/v)

########### wang A2
temp = gama_2_final[-1,]
temp = as.matrix(temp)    #####保存30次结果的矩阵
temp1 = apply(temp,2,mean)
ABS_gama_2 = apply(abs(temp - skill_MEAN[2]),2,sum)/v
RMSE_gama_2 = sqrt(apply((temp - skill_MEAN[2])^2,2,sum)/v)

########### wang A3
temp = gama_3_final[-1,]
temp = as.matrix(temp)    #####保存30次结果的矩阵
temp1 = apply(temp,2,mean)
ABS_gama_3 = apply(abs(temp - skill_MEAN[3]),2,sum)/v
RMSE_gama_3 = sqrt(apply((temp - skill_MEAN[3])^2,2,sum)/v)

########### wang PATTERN
temp = gama_pattern_final[-1,]
temp = as.matrix(temp)    #####保存30次结果的矩阵
temp1 = apply(temp,2,mean)
ABS_gama_pattern = apply(abs(temp - PCCR_MEAN),2,sum)/v
RMSE_gama_pattern = sqrt(apply((temp - PCCR_MEAN)^2,2,sum)/v)


##########  Accuracy ###########
##########  A1
temp = tao_A1_final[-1,]
temp = as.matrix(temp)    #####保存30次结果的矩阵
temp1 = apply(temp,2,mean)
ABS_tao_A1 = apply(abs(temp - skill_MEAN[1]),2,sum)/v
RMSE_tao_A1 = sqrt(apply((temp - skill_MEAN[1])^2,2,sum)/v)


##########  A2
temp = tao_A2_final[-1,]
temp = as.matrix(temp)    #####保存30次结果的矩阵
temp1 = apply(temp,2,mean)
ABS_tao_A2 = apply(abs(temp - skill_MEAN[2]),2,sum)/v
RMSE_tao_A2 = sqrt(apply((temp - skill_MEAN[2])^2,2,sum)/v)


##########  A3
temp = tao_A3_final[-1,]
temp = as.matrix(temp)    #####保存30次结果的矩阵
temp1 = apply(temp,2,mean)
ABS_tao_A3 = apply(abs(temp - skill_MEAN[3]),2,sum)/v
RMSE_tao_A3 = sqrt(apply((temp - skill_MEAN[3])^2,2,sum)/v)


###########  PATTERN
temp = tao_pattern_final[-1,]
temp = as.matrix(temp)    #####保存30次结果的矩阵
temp1 = apply(temp,2,mean)
ABS_tao_pattern = apply(abs(temp - PCCR_MEAN),2,sum)/v
RMSE_tao_pattern = sqrt(apply((temp - PCCR_MEAN)^2,2,sum)/v)




########### Average the attribute resluts
ABS_Wang_Attribute = apply(as.matrix(c(ABS_gama_1,ABS_gama_2,ABS_gama_3)),2,mean)
RMSE_Wang_Attribute = apply(as.matrix(c(RMSE_gama_1,RMSE_gama_2,RMSE_gama_3)),2,mean)
ABS_Accuracy_Attribute = apply(as.matrix(c(ABS_tao_A1,ABS_tao_A2,ABS_tao_A3)),2,mean)
RMSE_Accuracy_Attribute = apply(as.matrix(c(RMSE_tao_A1,RMSE_tao_A2,RMSE_tao_A3)),2,mean)



############### Output results

Output = rbind(ABS_Wang_Attribute,RMSE_Wang_Attribute,ABS_gama_pattern,RMSE_gama_pattern,
               ABS_Accuracy_Attribute,RMSE_Accuracy_Attribute,ABS_tao_pattern,RMSE_tao_pattern)




##################################################################################################################################
##################################################################################################################################
##                           Accuracy
##################################################################################################################################
##################################################################################################################################
####################################################################################
####################################################################################



















