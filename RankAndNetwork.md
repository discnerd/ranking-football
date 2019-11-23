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
       1   22.605791  Ohio_St     
       2    8.520053  Alabama     
       3    7.611428  Clemson     
       4    7.511369  LSU         
       5    5.013381  Florida     
       6    4.985334  Auburn      
       7    4.785989  Oklahoma    
       8    4.767309  Georgia     
       9    4.619639  Wisconsin   
      10    4.153600  Oregon      
      11    3.996312  Penn_St     
      12    3.929391  Utah        
      13    3.269867  Baylor      
      14    3.229743  Iowa_St     
      15    3.057931  Texas       
      16    3.043166  Washington  
      17    3.022571  Michigan    
      18    2.988191  UCF         
      19    2.955579  Texas_A&M   
      20    2.912002  Iowa        
      21    2.793424  Kansas_St   
      22    2.692096  Oklahoma_St 
      23    2.561509  Minnesota   
      24    2.546869  Memphis     
      25    2.495000  Navy        


```r
filter(rankedteams, Team %in% FBSteams$X2) %>% filter( min_rank( desc(Rating)) <26) %>% select(-ranking) %>% knitr::kable()
```



 Ranking      Rating  Team        
--------  ----------  ------------
       1   22.605791  Ohio_St     
       2    8.520053  Alabama     
       3    7.611428  Clemson     
       4    7.511369  LSU         
       5    5.013381  Florida     
       6    4.985334  Auburn      
       7    4.785989  Oklahoma    
       8    4.767309  Georgia     
       9    4.619639  Wisconsin   
      10    4.153600  Oregon      
      11    3.996312  Penn_St     
      12    3.929391  Utah        
      13    3.269867  Baylor      
      14    3.229743  Iowa_St     
      15    3.057931  Texas       
      16    3.043166  Washington  
      17    3.022571  Michigan    
      18    2.988191  UCF         
      19    2.955579  Texas_A&M   
      20    2.912002  Iowa        
      21    2.793424  Kansas_St   
      22    2.692096  Oklahoma_St 
      23    2.561509  Minnesota   
      24    2.546869  Memphis     
      25    2.495000  Navy        


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
       1   18.323011  Wheaton_IL      
       2   13.888949  Mt_Union        
       3   11.119770  N_Central_IL    
       4    6.937599  M_Hardin-Baylor 
       5    5.466000  Hardin-Simmons  
       6    4.850508  Muhlenberg      
       7    4.848642  Bridgewater_VA  
       8    3.822238  Redlands        
       9    3.698741  Salisbury       
      10    3.394874  St_John's_MN    
      11    3.357433  Delaware_Val    
      12    3.040246  WI_Whitewater   
      13    3.023374  Linfield        
      14    3.004490  Susquehanna     
      15    2.637930  Berry           
      16    2.576001  WI_LaCrosse     
      17    2.568373  WI_Platteville  
      18    2.539493  Ithaca          
      19    2.467817  St_Thomas_MN    
      20    2.436026  Chapman         
      21    2.426602  Stevenson       
      22    2.422443  Trinity_TX      
      23    2.361808  John_Carroll    
      24    2.335543  Union_NY        
      25    2.329057  IL_Wesleyan     

```r
rankedteams %>% filter(Team == " Loras") %>% select(-ranking)
```

```
##   Ranking    Rating   Team
## 1     149 0.3302913  Loras
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
