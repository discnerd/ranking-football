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



 Ranking     Rating  Team          
--------  ---------  --------------
       1   9.322323  Clemson       
       2   8.318624  Alabama       
       3   7.937061  UCF           
       4   7.796288  Georgia       
       5   7.589662  Notre_Dame    
       6   6.929612  TCU           
       7   6.077243  Auburn        
       8   6.059799  Oklahoma      
       9   5.292479  Ohio_St       
      10   5.173721  Oklahoma_St   
      11   5.146311  Washington    
      12   4.928995  Virginia_Tech 
      13   4.041966  Penn_St       
      14   3.409981  Miami_FL      
      15   3.262266  USC           
      16   3.012273  SMU           
      17   2.958641  Washington_St 
      18   2.917916  Oregon        
      19   2.838090  Wake_Forest   
      20   2.638103  West_Virginia 
      21   2.628894  Maryland      
      22   2.627164  Michigan      
      23   2.479852  Iowa          
      24   2.411139  Duke          
      25   2.405900  Stanford      


```r
filter(rankedteams, Team %in% FBSteams$X2) %>% filter( min_rank( desc(Rating)) <26) %>% select(-ranking) %>% knitr::kable()
```



 Ranking     Rating  Team          
--------  ---------  --------------
       1   9.322323  Clemson       
       2   8.318624  Alabama       
       3   7.937061  UCF           
       4   7.796288  Georgia       
       5   7.589662  Notre_Dame    
       6   6.929612  TCU           
       7   6.077243  Auburn        
       8   6.059799  Oklahoma      
       9   5.292479  Ohio_St       
      10   5.173721  Oklahoma_St   
      11   5.146311  Washington    
      12   4.928995  Virginia_Tech 
      13   4.041966  Penn_St       
      14   3.409981  Miami_FL      
      15   3.262266  USC           
      16   3.012273  SMU           
      17   2.958641  Washington_St 
      18   2.917916  Oregon        
      19   2.838090  Wake_Forest   
      20   2.638103  West_Virginia 
      21   2.628894  Maryland      
      22   2.627164  Michigan      
      23   2.479852  Iowa          
      24   2.411139  Duke          
      25   2.405900  Stanford      


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



 Ranking      Rating  Team            
--------  ----------  ----------------
       1   65.260136  Hardin-Simmons  
       2   13.930607  Mt_Union        
       3    6.494255  M_Hardin-Baylor 
       4    3.932512  Brockport_St    
       5    3.782316  E_Texas_Bap     
       6    3.763408  Springfield     
       7    3.110414  Wheaton_IL      
       8    2.749232  Delaware_Val    
       9    2.691736  Heidelberg      
      10    2.595981  Trinity_CT      
      11    2.356083  Wittenberg      
      12    2.125871  Chris_Newport   
      13    2.114714  Wesley_DE       
      14    2.064423  IL_Wesleyan     
      15    2.062068  Frank_&_Marsh   
      16    2.034996  Hobart_&_Smith  
      17    2.027928  WI_LaCrosse     
      18    1.992684  Framingham_St   
      19    1.969948  N_Central_IL    
      20    1.947630  Trine           
      21    1.938311  WI_Platteville  
      22    1.919808  Frostburg_St    
      23    1.854748  Linfield        
      24    1.800261  Carthage        
      25    1.781959  Alfred          

```r
rankedteams %>% filter(Team == " Loras") %>% select(-ranking)
```

```
##   Ranking   Rating   Team
## 1      64 0.965675  Loras
```




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
