library(rvest)
library(dplyr)
library(readr)
library(ggplot2)

Coleman <- read_html("http://www.unf.edu/~jcoleman/mperform.htm")
Coleman <- Coleman %>% html_nodes("pre span") %>% html_text() %>% read_table( skip=2)

Coleman <- Coleman %>% tidyr::separate(Retrodictive, c("Retro","Retro_wt")) %>% tidyr::separate(Predictive, c("Pre", "Pre_wt"))
Coleman <- Coleman %>% mutate(Pre=as.numeric(Pre), Pre_wt=as.numeric(Pre_wt), Retro = as.numeric(Retro), Retro_wt=as.numeric(Retro_wt))
Coleman<-Coleman %>% mutate(Retro_wt = ifelse(Retro_wt>1,Retro_wt/1000,Retro_wt), Pre_wt = ifelse(Pre_wt>1,Pre_wt/1000,Pre_wt))
Coleman <- Coleman %>% tidyr::separate(Efficiency, c("Efficiency","Eff_dec")) %>% 
  mutate(Efficiency=as.numeric(Efficiency)+as.numeric(Eff_dec)/100) %>%
  select(-Eff_dec)

Coleman <- Coleman %>% tidyr::separate(Retrodictive_1, c("Retro_1","Retro_wt_1")) %>% 
  tidyr::separate(Predictive_1, c("Pre_1", "Pre_wt_1"))
Coleman <- Coleman %>% mutate(Pre_1=as.numeric(Pre_1), Pre_wt_1=as.numeric(Pre_wt_1), 
                              Retro_1 = as.numeric(Retro_1), Retro_wt_1=as.numeric(Retro_wt_1))
Coleman<-Coleman %>% mutate(Retro_wt_1 = ifelse(Retro_wt_1>1,Retro_wt_1/1000,Retro_wt_1), 
                            Pre_wt_1 = ifelse(Pre_wt_1>1,Pre_wt_1/1000,Pre_wt_1))
Coleman <- Coleman %>% tidyr::separate(Efficiency_1, c("Efficiency_1","Eff_dec_1")) %>% 
  mutate(Efficiency_1=as.numeric(Efficiency_1)+as.numeric(Eff_dec_1)/100) %>%
  select(-Eff_dec_1)