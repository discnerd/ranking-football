# R Notebook







```r
n<-network.initialize(nrow(rankedteams), directed = TRUE, multiple = FALSE, loops = TRUE)
rankedteams <- rankedteams %>% mutate(ranking = min_rank(desc(Rating)))
network.vertex.names(n) <- as.character(teams$Team)
n %v% "rank" <- left_join(teams, rankedteams, c("Team"))$ranking
n %v% "rating" <- left_join(teams, rankedteams, c("Team"))$Rating
#n %v% "conference" <- as.character(rankedteams$Conference)

network.adjacency( A_unnormed, n, ignore.eval = FALSE, names.eval = c("WinStrength"))
```

#D1 Top 25 


```r
rankedteams %>% filter(Ranking < 26) %>% select(-ranking) %>%knitr::kable()
```



 Ranking      Rating  Team           
--------  ----------  ---------------
       1   23.567063  UCF            
       2    7.817540  TCU            
       3    7.125005  Georgia        
       4    6.907949  Oklahoma       
       5    6.891897  Maryland       
       6    6.759410  Clemson        
       7    6.438280  Notre_Dame     
       8    5.455221  Virginia_Tech  
       9    5.199645  USC            
      10    4.972427  Washington     
      11    4.778911  Oklahoma_St    
      12    4.265673  Duke           
      13    4.208746  Ohio_St        
      14    4.207983  Michigan       
      15    4.019064  Alabama        
      16    3.934279  Texas          
      17    3.536333  Auburn         
      18    3.525398  West_Virginia  
      19    2.976214  Wake_Forest    
      20    2.866823  California     
      21    2.740326  SMU            
      22    2.527735  Baylor         
      23    2.462005  Wisconsin      
      24    2.417722  UT_San_Antonio 
      25    2.395519  Louisville     


```r
filter(rankedteams, Team %in% FBSteams$X2) %>% filter( min_rank( desc(Rating)) <26) %>% select(-ranking) %>% knitr::kable()
```



 Ranking      Rating  Team           
--------  ----------  ---------------
       1   23.567063  UCF            
       2    7.817540  TCU            
       3    7.125005  Georgia        
       4    6.907949  Oklahoma       
       5    6.891897  Maryland       
       6    6.759410  Clemson        
       7    6.438280  Notre_Dame     
       8    5.455221  Virginia_Tech  
       9    5.199645  USC            
      10    4.972427  Washington     
      11    4.778911  Oklahoma_St    
      12    4.265673  Duke           
      13    4.208746  Ohio_St        
      14    4.207983  Michigan       
      15    4.019064  Alabama        
      16    3.934279  Texas          
      17    3.536333  Auburn         
      18    3.525398  West_Virginia  
      19    2.976214  Wake_Forest    
      20    2.866823  California     
      21    2.740326  SMU            
      22    2.527735  Baylor         
      23    2.462005  Wisconsin      
      24    2.417722  UT_San_Antonio 
      25    2.395519  Louisville     


```r
#net<-ggnetwork(n %s% which( n %v% "rank" < 26), layout="fruchtermanreingold")
net<-ggnetwork(n , layout="fruchtermanreingold")
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(alpha=0.1)+
  geom_nodes( aes(color=rating), alpha=0.5 ) +theme_blank()+
  scale_color_gradient(low="purple", high="gold")#+
```

![](RankAndNetwork_files/figure-html/plotNetwork-1.png)<!-- -->

```r
#  geom_nodelabel_repel(aes(label=vertex.names))
```



```r
net<-ggnetwork(n %s% which( n %v% "rank" < 26), layout="fruchtermanreingold")
#net<-ggnetwork(n , layout="fruchtermanreingold")
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(aes(alpha=WinStrength), curvature = 0.2)+
  geom_nodes(  ) +theme_blank()+
  geom_nodelabel_repel(aes(label=vertex.names, fill=rank))+
  scale_color_gradient(low="purple", high="gold")+
  scale_fill_gradient(low="gold", high="purple")
```

```
## Warning: Ignoring unknown parameters: segment.color
```

![](RankAndNetwork_files/figure-html/plottop25-1.png)<!-- -->






```r
n<-network.initialize(nrow(rankedteams), directed = TRUE, multiple = FALSE, loops = TRUE)
rankedteams <- rankedteams %>% mutate(ranking = min_rank(desc(Rating)))
network.vertex.names(n) <- as.character(teams$Team)
n %v% "rank" <- left_join(teams, rankedteams, c("Team"))$ranking
n %v% "rating" <- left_join(teams, rankedteams, c("Team"))$Rating
#n %v% "conference" <- as.character(rankedteams$Conference)

network.adjacency( A_unnormed, n, ignore.eval = FALSE, names.eval = c("WinStrength"))
```

#D3 Top 25 


```r
rankedteams %>% filter(Ranking < 26) %>% select(-ranking) %>%knitr::kable()
```



 Ranking       Rating  Team           
--------  -----------  ---------------
       1   92.9040591  Mt_Union       
       2   57.7425569  Whitworth      
       3   31.2858674  Hardin-Simmons 
       4    5.5962055  Trinity_CT     
       5    3.6030707  Wheaton_IL     
       6    1.7398285  Brockport_St   
       7    1.4292304  Springfield    
       8    1.3475474  Carthage       
       9    1.1374014  Framingham_St  
      10    1.0184140  Hobart_&_Smith 
      11    0.9988303  Frostburg_St   
      12    0.9826222  Middlebury     
      13    0.9573602  Heidelberg     
      14    0.8850010  Frank_&_Marsh  
      15    0.8588962  Chris_Newport  
      16    0.8475407  Delaware_Val   
      17    0.7962765  Rensselaer     
      18    0.7822374  Williams       
      19    0.7758414  St_Thomas_MN   
      20    0.7706861  Johns_Hopkins  
      21    0.7300150  Loras          
      22    0.7263670  Wesleyan_CT    
      23    0.6949447  WI_LaCrosse    
      24    0.6684943  McDaniel_Col   
      25    0.6571169  Buffalo_St     




```r
#net<-ggnetwork(n %s% which( n %v% "rank" < 26), layout="fruchtermanreingold")
net<-ggnetwork(n , layout="fruchtermanreingold")
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(alpha=0.1)+
  geom_nodes( aes(color=rating), alpha=0.5 ) +theme_blank()+
  scale_color_gradient(low="purple", high="gold")#+
```

![](RankAndNetwork_files/figure-html/plotNetworkD3-1.png)<!-- -->

```r
#  geom_nodelabel_repel(aes(label=vertex.names))
```



```r
net<-ggnetwork(n %s% which( n %v% "rank" < 26), layout="fruchtermanreingold")
#net<-ggnetwork(n , layout="fruchtermanreingold")
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(aes(alpha=WinStrength), curvature = 0.2)+
  geom_nodes(  ) +theme_blank()+
  geom_nodelabel_repel(aes(label=vertex.names, fill=rank))+
  scale_color_gradient(low="purple", high="gold")+
  scale_fill_gradient(low="gold", high="purple")
```

```
## Warning: Ignoring unknown parameters: segment.color
```

![](RankAndNetwork_files/figure-html/plottop25D3-1.png)<!-- -->
