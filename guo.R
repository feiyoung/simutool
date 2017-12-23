setwd("C:\\Users\\LiuWei\\Documents")
rm(list=ls())
library(CDM)

##############  define parameters ##################
L = 2   ############# polytomous level for attribute
temp <- read.table("3-2L.txt")
##############  50 items
# q.matrix = t(temp)
############   25 items
temp1 = t(temp)
q.matrix = temp1[1:25,]

# slip = rep(0.1,nrow(q.matrix))
# guess = slip
NumofItem = nrow(q.matrix) #--
slip <- runif(NumofItem,0.05,0.15)
guess <- runif(NumofItem,0.05,0.15)

temp = q.matrix
for(i in 1:nrow(temp))
{
  for (j in 1:ncol(temp))
  {
    if(temp[i,j]==L) temp[i,j]=1
  }
}
q_collapsed = temp

##############  generate examinee ##################
N = 2000
m=diag(ncol(q.matrix))
m[m==0] = 0.5               ##### ???????????????????????????######
normsim <- rmvnorm( N, rep(0,ncol(q.matrix)), m)

##############  when L=2 #############
z1 = -0.44
z2 = 0.44
for (i in 1:N)
{
  for (j in 1:ncol(q.matrix))
  {
    if(normsim[i,j] < z1) normsim[i,j]=0 
    else if(normsim[i,j] > z2) normsim[i,j]=2 
    else normsim[i,j]=1
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
I = 100           ############# 产生重复作答数据的次数
K = 1
P = 4             ############# 概率之间计算相关的列数
P2 = 3            ############# 计算PCCR时属性的列数（个数）
R=matrix(NA,((I*(I-1))/2),P)          ######本质：每个属性后验概率的积差相关 
judge=matrix(NA,1,((I*(I-1))/2))      ######本质：模式判准率的计算
skill=matrix(NA,((I*(I-1))/2),P2)     ######本质：属性判准率的计算
####################################################################################
####################################################################################

for(zzz in 1:I)
{
  xx=w=array(NA,c(N,nrow(q.matrix),ncol(q.matrix)))   #`错误出现在这里，w是一个三维矩阵
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
  postrior_KS = DINA$pattern[,5]   
  postrior_A1 = DINA$pattern[,6]   
  postrior_A2 = DINA$pattern[,7]  
  postrior_A3 = DINA$pattern[,8]
  postrior_infor = cbind(Attribute_DINA,postrior_KS,postrior_A1,postrior_A2,postrior_A3)
  temp = cbind(Attribute_DINA,postrior_KS,postrior_A1,postrior_A2,postrior_A3)
  assign(paste("Loop", zzz, sep = ""), temp)
}

######################## 开始计算100次作答两两配对之间的PCCR,ACCP #########################  
Loop=mget(paste("Loop",1:I,sep=""))
for(i in 1:I)
{
  x1=matrix(unlist(Loop[i]),N,P+P2)
  j=i+1
  while(j<=I)
  {
    x2=matrix(unlist(Loop[j]),N,P+P2)
    judge[1,K]=sum(apply((x1[,1:P2]==x2[,1:P2]),1,all))/N
    skill[K,]=t(apply(matrix(as.numeric(x1[,1:P2]==x2[,1:P2]),nrow=N),2,sum)/N)
    for(k in 1:P)
    {
      R[K,k]=cor(x1[,k+P2],x2[,k+P2])
    }
    K=K+1
    j=j+1
  }
}
Post_prob_MEAN=apply(R,2,mean)
PCCR_MEAN=mean(judge)
skill_MEAN=apply(skill,2,mean)



############ Wang
gama_1_final = matrix(0,ncol = 1,nrow = 1)
gama_2_final = matrix(0,ncol = 1,nrow = 1)
gama_3_final = matrix(0,ncol = 1,nrow = 1)
gama_pattern_final = matrix(0,ncol = 1,nrow = 1)


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




for (v in 1:20)
{
  ################ using pgDINA model to generate response ###############
  xx=w=array(NA,c(N,nrow(q.matrix),ncol(q.matrix)))   #`错误出现在这里，w是一个三维矩阵
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
  postrior_KS = DINA$pattern[,5]   
  postrior_A1 = DINA$pattern[,6]   
  postrior_A2 = DINA$pattern[,7]  
  postrior_A3 = DINA$pattern[,8]
  postrior_infor = cbind(Attribute_DINA,postrior_KS,postrior_A1,postrior_A2,postrior_A3)
  #postrior_CDM = cbind(postrior_KS,postrior_A1,postrior_A2,postrior_A3) #第一个为后验概率分布，2-4为3个属性上的边际概率
  
  
  
  ################ Wang Wenyi  ##############################
  
  ###################  A1  ######################
  
  temp  = 0
  for (i in 1:N)
  {
    temp = temp + postrior_A1[i]^2 + (1-postrior_A1[i])^2
  }
  gama_1 = temp / N
  gama_1_final = rbind(gama_1_final,gama_1)
  
  
  ###################  A2  ######################
  temp  = 0
  for (i in 1:N)
  {
    temp = temp + postrior_A2[i]^2 + (1-postrior_A2[i])^2
  }
  gama_2 = temp / N
  gama_2_final = rbind(gama_2_final,gama_2)
  
  
  ###################  A3  ######################
  temp  = 0
  for (i in 1:N)
  {
    temp = temp + postrior_A3[i]^2 + (1-postrior_A3[i])^2
  }
  gama_3 = temp / N
  gama_3_final = rbind(gama_3_final,gama_3)
  
  
  ################ pattern using wang's method  ######################
  temp = postrior_distribution * postrior_distribution
  temp1 = colSums(temp)
  temp2 = as.matrix(temp1)
  gama_pattern = colSums(temp2)/N
  gama_pattern_final = rbind(gama_pattern_final,gama_pattern)
  
  
}  ###### 循环30次的结束




PCCR_MEAN
skill_MEAN



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



########### Average the attribute resluts
ABS_Wang_Attribute = apply(as.matrix(c(ABS_gama_1,ABS_gama_2,ABS_gama_3)),2,mean)
RMSE_Wang_Attribute = apply(as.matrix(c(RMSE_gama_1,RMSE_gama_2,RMSE_gama_3)),2,mean)


############### Output results

Output_consistency = rbind(ABS_Wang_Attribute,RMSE_Wang_Attribute,ABS_gama_pattern,RMSE_gama_pattern)




##################################################################################################################################
##################################################################################################################################
##                           Accuracy
##################################################################################################################################
##################################################################################################################################
####################################################################################
####################################################################################

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








#--------------------------------------

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









save.image(file='data.Rdata')





