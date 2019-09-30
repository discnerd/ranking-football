
library(readxl)
library(tidyverse)

FootballD3Tournament <- read_excel("D3Tournament.xlsx")
tourneyTeams<-FootballD3Tournament %>% separate(Bracket, c("Region", "Spot"),1)  %>% select(Team, Region, Spot) %>% full_join(rankedteams %>% filter(Index %in% indices)) %>% filter(!is.na(Region))  %>% mutate(Spot=as.integer(Spot)) %>% arrange(Region, Spot)

tourneyTeams <- tourneyTeams %>% mutate( Round1=rep(1,nrow(tourneyTeams))) 

for(i in 1:length(tourneyTeams$Team)){
  
  teams = tourneyTeams %>%
    filter(Region == Region[i],
           (Spot-1) %/%2 == (Spot[i]-1) %/%2 ,
           Team != Team[i]) %>%
    .$Team
  
  tourneyTeams$Round2[i] = 1
  for( team in teams){
    tourneyTeams$Round2[i] = tourneyTeams$Round2[i]*
      shares[length(shares)/2+1+
               MOV[which(rownames(MOV)==team),
                   which(rownames(MOV)==tourneyTeams$Team[i])]]
  }
}




for(i in 1:length(tourneyTeams$Team)){
  
  teams = tourneyTeams %>% 
    filter(Region == Region[i], 
           (Spot-1) %/%4 == (Spot[i]-1) %/%4 , 
           (Spot-1) %/%2 != (Spot[i]-1) %/%2,
           Team != Team[i]) %>%
    .$Team
  
  tourneyTeams$Quarters[i] = 0
  for( team in teams){
    tourneyTeams$Quarters[i] = tourneyTeams$Quarters[i]+
      shares[length(shares)/2+1+
               MOV[which(rownames(MOV)==team),
                   which(rownames(MOV)==tourneyTeams$Team[i])]]*
      tourneyTeams$Round2[which(tourneyTeams$Team==team)]
  }
  tourneyTeams$Quarters[i] = tourneyTeams$Quarters[i]*tourneyTeams$Round2[i]
}

for(i in 1:length(tourneyTeams$Team)){
  
  teams = tourneyTeams %>% 
    filter(Region == Region[i], 
           (Spot-1) %/%4 != (Spot[i]-1) %/%4 , 
           Team != Team[i]) %>%
    .$Team
  
  tourneyTeams$Semis[i] = 0
  for( team in teams){
    tourneyTeams$Semis[i] = tourneyTeams$Semis[i]+
      shares[length(shares)/2+1+
               MOV[which(rownames(MOV)==team),
                   which(rownames(MOV)==tourneyTeams$Team[i])]]*
      tourneyTeams$Quarters[which(tourneyTeams$Team==team)]
  }
  tourneyTeams$Semis[i] = tourneyTeams$Semis[i]*tourneyTeams$Quarters[i]
}

for(i in 1:length(tourneyTeams$Team)){
  
  teams = tourneyTeams %>% 
    filter(Region == if_else(Region[i]=="A","B",
                             if_else(Region[i]=="B","A",
                                     if_else(Region[i]=="C", "D", "C")
                             ) 
    )
    )%>%
    .$Team
  
  tourneyTeams$Finals[i] = 0
  for( team in teams){
    tourneyTeams$Finals[i] = tourneyTeams$Finals[i]+
      shares[length(shares)/2+1+
               MOV[which(rownames(MOV)==team),
                   which(rownames(MOV)==tourneyTeams$Team[i])]]*
      tourneyTeams$Semis[which(tourneyTeams$Team==team)]
  }
  tourneyTeams$Finals[i] = tourneyTeams$Finals[i]*tourneyTeams$Semis[i]
}

for(i in 1:length(tourneyTeams$Team)){
  
  teams = tourneyTeams %>% 
    filter(Region %in% c(if_else(Region[i] %in% c("A","B"), "C","A"),
                         if_else(Region[i] %in% c("A","B"), "D","B"))
           
    )%>%
    .$Team
  
  tourneyTeams$Champs[i] = 0
  for( team in teams){
    tourneyTeams$Champs[i] = tourneyTeams$Champs[i]+
      shares[length(shares)/2+1+
               MOV[which(rownames(MOV)==team),
                   which(rownames(MOV)==tourneyTeams$Team[i])]]*
      tourneyTeams$Finals[which(tourneyTeams$Team==team)]
  }
  tourneyTeams$Champs[i] = tourneyTeams$Champs[i]*tourneyTeams$Finals[i]
}

tourneyTeams %>% select(-Region, -Spot, -Index, -Round1) %>% 
  arrange(desc(Champs)) %>% knitr::kable()