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
       1   20.265092  Ohio_St     
       2    9.325561  Clemson     
       3    8.898255  Alabama     
       4    7.424678  LSU         
       5    5.319474  Georgia     
       6    5.047390  Florida     
       7    4.841621  Auburn      
       8    4.475098  Oklahoma    
       9    4.465822  Utah        
      10    4.437586  Wisconsin   
      11    4.276323  Oregon      
      12    3.922012  Penn_St     
      13    3.811352  Michigan    
      14    3.388783  Texas_A&M   
      15    3.265061  Baylor      
      16    3.186149  Iowa_St     
      17    3.086127  Iowa        
      18    2.956447  Notre_Dame  
      19    2.922934  Texas       
      20    2.864254  Washington  
      21    2.768382  Minnesota   
      22    2.597936  UCF         
      23    2.556736  Oklahoma_St 
      24    2.431575  Kansas_St   
      25    2.396190  USC         


```r
filter(rankedteams, Team %in% FBSteams$X2) %>% filter( min_rank( desc(Rating)) <26) %>% select(-ranking) %>% knitr::kable()
```



 Ranking      Rating  Team        
--------  ----------  ------------
       1   20.265092  Ohio_St     
       2    9.325561  Clemson     
       3    8.898255  Alabama     
       4    7.424678  LSU         
       5    5.319474  Georgia     
       6    5.047390  Florida     
       7    4.841621  Auburn      
       8    4.475098  Oklahoma    
       9    4.465822  Utah        
      10    4.437586  Wisconsin   
      11    4.276323  Oregon      
      12    3.922012  Penn_St     
      13    3.811352  Michigan    
      14    3.388783  Texas_A&M   
      15    3.265061  Baylor      
      16    3.186149  Iowa_St     
      17    3.086127  Iowa        
      18    2.956447  Notre_Dame  
      19    2.922934  Texas       
      20    2.864254  Washington  
      21    2.768382  Minnesota   
      22    2.597936  UCF         
      23    2.556736  Oklahoma_St 
      24    2.431575  Kansas_St   
      25    2.396190  USC         


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
       1   17.982289  Wheaton_IL      
       2   14.561438  Mt_Union        
       3   10.040843  N_Central_IL    
       4    6.402376  M_Hardin-Baylor 
       5    5.341310  Muhlenberg      
       6    5.163040  Hardin-Simmons  
       7    4.946521  Bridgewater_VA  
       8    3.762622  Redlands        
       9    3.650262  Salisbury       
      10    3.257168  St_John's_MN    
      11    3.210129  Susquehanna     
      12    3.051612  Union_NY        
      13    3.047628  Delaware_Val    
      14    2.964942  Ithaca          
      15    2.760444  Linfield        
      16    2.642450  WI_Whitewater   
      17    2.618613  Stevenson       
      18    2.499955  St_Thomas_MN    
      19    2.484331  WI_LaCrosse     
      20    2.438796  Johns_Hopkins   
      21    2.436386  Berry           
      22    2.281931  IL_Wesleyan     
      23    2.276214  Cortland_St     
      24    2.268011  Trinity_TX      
      25    2.253474  WI_Oshkosh      

```r
rankedteams %>% filter(Team == " Loras") %>% select(-ranking)
```

```
##   Ranking   Rating   Team
## 1     162 0.273235  Loras
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
