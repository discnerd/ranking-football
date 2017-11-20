

pFG<-0.139377 #Need to add NoPAT and 2PT for D3 because the game is different
#pTD<-1-pFG
pTDnoPA<-0.11967
pTDPA<-0.69855
pTD2P<-0.03296
pSaf<-1-(pFG+pTDnoPA+pTDPA+pTD2P)


MaxMOV<-39
MOVChain<-matrix(rep(0,(2*MaxMOV-1)^2),nrow=2*MaxMOV-1,byrow=TRUE)
#rhs <- c(rep(0,2*MaxMOV-1-7),rep(pTD/2,4),rep(1/2,3))
rhs <- c(rep(0,2*MaxMOV-1-8),(pTD2P)/2,(pTDPA+pTD2P)/2,(pTDnoPA+pTDPA+pTD2P)/2,rep((1-pSaf-pFG)/2,2),(1-pSaf)/2,rep(1/2,2))

for(i in seq(1,(2*MaxMOV-1))){
  MOVChain[i,i]<-1
  if(i+2<=(2*MaxMOV-1)){
    MOVChain[i,i+2] <- -pSaf/2
  }
  if(i-2>0){
    MOVChain[i,i-2] <- -pSaf/2
  }
  
  if(i+3<=(2*MaxMOV-1)){
    MOVChain[i,i+3] <- -pFG/2
  }
  if(i-3>0){
    MOVChain[i,i-3] <- -pFG/2
  }
  
  if(i+6<=(2*MaxMOV-1)){
    MOVChain[i,i+6] <- -pTDnoPA/2
  }
  if(i-6>0){
    MOVChain[i,i-6] <- -pTDnoPA/2
  }
  
  if(i+7<=(2*MaxMOV-1)){
    MOVChain[i,i+7] <- -pTDPA/2
  }
  if(i-7>0){
    MOVChain[i,i-7] <- -pTDPA/2
  }
  
  if(i+8<=(2*MaxMOV-1)){
    MOVChain[i,i+8] <- -pTD2P/2
  }
  if(i-8>0){
    MOVChain[i,i-8] <- -pTD2P/2
  }
  
}
Expecteds<-solve(MOVChain,rhs)
#pulling data from Massey Site
scores <- read.csv("https://www.masseyratings.com/scores.php?s=295489&sub=11620&all=1&mode=2&format=1", header=FALSE)
teams <- read.csv("https://www.masseyratings.com/scores.php?s=295489&sub=11620&all=1&mode=2&format=2", header=FALSE)
names(scores)<-c("Time","Date","Team1","Home1","Score1","Team2","Home2","Score2")
names(teams)<-c("Label","Team")

A=matrix(rep(0,length(teams[,2])^2),nrow=length(teams[,2]))
b=rep(1,length(teams[,2]))
#diag(A)=rep(2,length(diag(A)))

#max_points=max(c(max( scores$Score1 ),max( scores$Score2 )))
#max_points=100

for(i in 1:length(scores$Team1) ){
  
  if(abs(scores$Score1[i]-scores$Score2[i])<39){
    Share1=Expecteds[scores$Score1[i]-scores$Score2[i]+MaxMOV]
  }  else{
    if(scores$Score1[i]>scores$Score2[i]){
      Share1=1
    } else{
      Share1=0
    }
  }
  
  
  Share2=1-Share1
  
  A[ scores$Team1[i] ,scores$Team2[i]  ]=A[ scores$Team1[i] ,scores$Team2[i]  ]+Share2;
  A[ scores$Team2[i] ,scores$Team1[i]  ]=A[ scores$Team2[i] ,scores$Team1[i]  ]+Share1;
  A[ scores$Team1[i] ,scores$Team1[i]  ]=A[ scores$Team1[i] ,scores$Team1[i]  ]+Share1;
  A[ scores$Team2[i] ,scores$Team2[i]  ]=A[ scores$Team2[i] ,scores$Team2[i]  ]+Share2;
  if( i%%10 ==0 ){
    print(c(i,Share1,Share2))
  }
}
image(A)
for(i in 1:length(teams[,2])){
  if(sum(A[i,])!=0){ 
    A[i,]=A[i,]/sum(A[i,])
  } else{
    print(paste(c("Row", i, "has a problem") ))
  }
}

#Put in to deal with the fact that we don't 
#neccessarily have one communication class early in the season

library(expm)
Rating<-t(b)%*% (A)
for( n in 1:100000 ){
  Rating <- Rating %*% A
}


library(tidyverse)
#Rating<-rowSums( eigen(t(A))$vectors[,eigen(t(A))$values==1])*64/sum(eigen(t(A))$values==1)
rankedteams<-mutate(teams,Rating = as.numeric(Rating)) %>% arrange(desc(Rating)) %>% 
  mutate(Ranking =min_rank(desc(Rating))) %>% select(Ranking, Rating, Team)

library(readr)
write_csv(rankedteams, paste("D3Current",".csv",sep=""))

