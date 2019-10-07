---
title: "Football Rankings"
output: 
  html_document: 
    keep_md: yes
---







```r
n<-network.initialize(nrow(rankedteams), directed = TRUE, multiple = FALSE, loops = TRUE)
rankedteams <- rankedteams %>% mutate(ranking = min_rank(desc(Rating)))
network.vertex.names(n) <- as.character(teams$Team)
n %v% "rank" <- left_join(teams, rankedteams, c("Team"))$ranking
n %v% "rating" <- left_join(teams, rankedteams, c("Team"))$Rating
#n %v% "conference" <- as.character(rankedteams$Conference)

network.adjacency( A_unnormed, n, ignore.eval = FALSE, names.eval = c("WinStrength"))
```

# D1 Top 25 


```r
rankedteams %>% filter(Ranking < 26) %>% select(-ranking) %>%knitr::kable()
```



 Ranking      Rating  Team           
--------  ----------  ---------------
       1   24.141256  Ohio_St        
       2   13.635379  Alabama        
       3    7.080649  Oklahoma       
       4    6.632247  LSU            
       5    5.241785  Wisconsin      
       6    5.187188  Georgia        
       7    5.030091  Auburn         
       8    4.758245  Penn_St        
       9    4.684774  Michigan_St    
      10    4.482197  Florida        
      11    4.173261  UCF            
      12    4.143951  Clemson        
      13    4.049841  Notre_Dame     
      14    4.031868  Missouri       
      15    4.003343  Oregon         
      16    3.800367  Texas          
      17    3.499540  Washington     
      18    2.887472  Oklahoma_St    
      19    2.808919  Iowa_St        
      20    2.681486  Baylor         
      21    2.592911  California     
      22    2.592758  North_Carolina 
      23    2.519399  South_Carolina 
      24    2.462527  Mississippi    
      25    2.453368  Memphis        


```r
filter(rankedteams, Team %in% FBSteams$X2) %>% filter( min_rank( desc(Rating)) <26) %>% select(-ranking) %>% knitr::kable()
```



 Ranking      Rating  Team           
--------  ----------  ---------------
       1   24.141256  Ohio_St        
       2   13.635379  Alabama        
       3    7.080649  Oklahoma       
       4    6.632247  LSU            
       5    5.241785  Wisconsin      
       6    5.187188  Georgia        
       7    5.030091  Auburn         
       8    4.758245  Penn_St        
       9    4.684774  Michigan_St    
      10    4.482197  Florida        
      11    4.173261  UCF            
      12    4.143951  Clemson        
      13    4.049841  Notre_Dame     
      14    4.031868  Missouri       
      15    4.003343  Oregon         
      16    3.800367  Texas          
      17    3.499540  Washington     
      18    2.887472  Oklahoma_St    
      19    2.808919  Iowa_St        
      20    2.681486  Baylor         
      21    2.592911  California     
      22    2.592758  North_Carolina 
      23    2.519399  South_Carolina 
      24    2.462527  Mississippi    
      25    2.453368  Memphis        


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
net<-ggnetwork(n %s% which( (n %v% "vertex.names") %in% FBSteams$X2), layout="fruchtermanreingold")
#net<-ggnetwork(n , layout="fruchtermanreingold")
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(alpha=0.1)+
  geom_nodes( aes(color=rating), alpha=0.5 ) +theme_blank()+
  scale_color_gradient(low="purple", high="gold")#+
```

![](RankAndNetwork_files/figure-html/plotNetworkFBS-1.png)<!-- -->

```r
#  geom_nodelabel_repel(aes(label=vertex.names))
```



```r
net<-ggnetwork(n %s% which( n %v% "rank" < 26), layout="fruchtermanreingold")
#net<-ggnetwork(n , layout="fruchtermanreingold")
ggplot(net, aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(aes(alpha=WinStrength), curvature = 0.2, arrow = arrow(length=unit(2, "points")))+
  geom_nodes(  ) +theme_blank()+
  geom_nodelabel_repel(aes(label=vertex.names, fill=rating))+
  scale_color_gradient(low="purple", high="gold")+
  scale_fill_gradient(low="gold", high="purple")
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

# D3 Top 25 


```r
rankedteams %>% filter(Ranking < 26) %>% select(-ranking) %>%knitr::kable()
```



 Ranking      Rating  Team           
--------  ----------  ---------------
       1   15.088058  Mt_Union       
       2    6.866752  Wheaton_IL     
       3    6.415227  Cortland_St    
       4    6.199746  Redlands       
       5    5.652665  Bridgewater_VA 
       6    5.436215  Muhlenberg     
       7    4.402946  Delaware_Val   
       8    3.726166  Stevenson      
       9    3.643196  Ithaca         
      10    3.636253  N_Central_IL   
      11    3.474337  Union_NY       
      12    3.347932  Linfield       
      13    3.080867  John_Carroll   
      14    3.003997  Hobart_&_Smith 
      15    2.855547  Hardin-Simmons 
      16    2.844747  TX_Lutheran    
      17    2.827346  Susquehanna    
      18    2.694632  Berry          
      19    2.653960  Johns_Hopkins  
      20    2.541692  St_John's_MN   
      21    2.436286  IL_Wesleyan    
      22    2.434465  Endicott       
      23    2.418873  Shenandoah     
      24    2.363668  Alfred         
      25    2.323211  Worcester_Tech 

```r
rankedteams %>% filter(Team == " Loras") %>% select(-ranking)
```

```
##   Ranking    Rating   Team
## 1     180 0.2219363  Loras
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
  geom_edges(aes(alpha=WinStrength), curvature = 0.2, arrow = arrow(length=unit(2, "points")))+
  geom_nodes(  ) +theme_blank()+
  geom_nodelabel_repel(aes(label=vertex.names, fill=rating))+
  scale_color_gradient(low="purple", high="gold")+
  scale_fill_gradient(low="gold", high="purple")
```

![](RankAndNetwork_files/figure-html/plottop25D3-1.png)<!-- -->
