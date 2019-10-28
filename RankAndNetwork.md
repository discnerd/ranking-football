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
--------  ----------  ------------
       1   24.996465  Ohio_St     
       2   10.706391  Alabama     
       3    6.826014  Clemson     
       4    6.649974  LSU         
       5    5.427782  Wisconsin   
       6    5.236187  Auburn      
       7    5.092623  Oklahoma    
       8    5.048368  Penn_St     
       9    4.452010  Florida     
      10    4.343951  Utah        
      11    3.740043  Oregon      
      12    3.711261  Georgia     
      13    3.505267  UCF         
      14    3.332768  Michigan    
      15    3.206129  Texas_A&M   
      16    3.170862  Baylor      
      17    2.980399  Iowa_St     
      18    2.954257  Iowa        
      19    2.883718  Texas       
      20    2.870187  Washington  
      21    2.709765  Michigan_St 
      22    2.518222  USC         
      23    2.506525  Oklahoma_St 
      24    2.437307  Memphis     
      25    2.404594  Kansas_St   


```r
filter(rankedteams, Team %in% FBSteams$X2) %>% filter( min_rank( desc(Rating)) <26) %>% select(-ranking) %>% knitr::kable()
```



 Ranking      Rating  Team        
--------  ----------  ------------
       1   24.996465  Ohio_St     
       2   10.706391  Alabama     
       3    6.826014  Clemson     
       4    6.649974  LSU         
       5    5.427782  Wisconsin   
       6    5.236187  Auburn      
       7    5.092623  Oklahoma    
       8    5.048368  Penn_St     
       9    4.452010  Florida     
      10    4.343951  Utah        
      11    3.740043  Oregon      
      12    3.711261  Georgia     
      13    3.505267  UCF         
      14    3.332768  Michigan    
      15    3.206129  Texas_A&M   
      16    3.170862  Baylor      
      17    2.980399  Iowa_St     
      18    2.954257  Iowa        
      19    2.883718  Texas       
      20    2.870187  Washington  
      21    2.709765  Michigan_St 
      22    2.518222  USC         
      23    2.506525  Oklahoma_St 
      24    2.437307  Memphis     
      25    2.404594  Kansas_St   


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
--------  ----------  ----------------
       1   16.299170  Wheaton_IL      
       2   10.604124  Mt_Union        
       3    8.783914  N_Central_IL    
       4    5.521117  Ithaca          
       5    5.043196  M_Hardin-Baylor 
       6    5.019865  Redlands        
       7    4.959888  Muhlenberg      
       8    4.766297  Bridgewater_VA  
       9    4.688269  St_John's_MN    
      10    4.316374  Hardin-Simmons  
      11    3.768542  Linfield        
      12    3.484552  Chapman         
      13    3.318683  Cortland_St     
      14    3.310361  Delaware_Val    
      15    3.026023  IL_Wesleyan     
      16    3.023818  Berry           
      17    2.988237  TX_Lutheran     
      18    2.981541  WI_Whitewater   
      19    2.922449  WI_Platteville  
      20    2.815014  Susquehanna     
      21    2.558505  Bethel_MN       
      22    2.540266  Salisbury       
      23    2.477460  WI_LaCrosse     
      24    2.390712  Stevenson       
      25    2.319513  Johns_Hopkins   

```r
rankedteams %>% filter(Team == " Loras") %>% select(-ranking)
```

```
##   Ranking    Rating   Team
## 1     173 0.2454292  Loras
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
