#-------------------------------------------------------------------
#smoothing examples 
#
#
#
#Klemens Rosin, 2021-11-23
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#packages, functions, paths
#-------------------------------------------------------------------

#packages
    library(tidyverse)
    library(mgcv) #gam
    library(zoo) # moving average (no confusion with filter function in base R)
    library(RColorBrewer)


#themes: neutral design for ggplot
    neutral <- theme_bw() + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "grey85"),
        panel.border = element_rect(colour = "grey85"))


#path to open data on naturalization 
    nat_od <- "https://data.stadt-zuerich.ch/dataset/bev_brw_jahr_alter_geschlecht_herkunft_quartier_od5707/download/BEV570OD5707.csv"

    
    
#-------------------------------------------------------------------
#import and data preparation
#-------------------------------------------------------------------

#naturalization, Albisrieden district   
    nat <- read_csv(nat_od) %>% 
        rename(age = AlterVCd) %>% 
        filter((HerkunftBisherCd == 2) & (HerkunftCd == 1) 
               & (QuarCd == "091") & (EreignisDatJahr == max(EreignisDatJahr))) %>% 
        group_by(age) %>% 
            summarize(nat = sum(AnzEinbWir)) %>% 
        ungroup()
    
#all values (since missing values for certain ages in the data)
    nat_all <- tibble(age = 0:110) %>% 
        left_join(nat, by = "age") %>% 
        replace_na(list(nat = 0))

    
#-------------------------------------------------------------------
#smoothing
#-------------------------------------------------------------------
    
#different smoothing approaches
    nat_smooth <- nat_all %>% 
        mutate(pred_ma05 = rollmean(nat, k = 5, fill = NA),
               pred_ma11 = rollmean(nat, k = 11, fill = NA),
               pred_gam = gam(nat ~ s(age, bs = "tp"))$fitted.values,
               pred_loess10 = predict(loess(nat ~ age, span = 0.1, degree = 1)),
               pred_loess30 = predict(loess(nat ~ age, span = 0.3, degree = 1)))
    
#plot preparation
    nat_prep <- nat_smooth %>% 
        pivot_longer(cols = starts_with("pred_"), names_prefix = "pred_", 
            names_to = "method", values_to = "smoothed") %>% 
        rename(initial = nat) %>% 
        pivot_longer(cols = c("initial", "smoothed"), 
            names_to = "category", values_to = "values")
    
#plot
    ggplot(data = nat_prep) +
    # ggplot(filter(nat_prep, method %in% c("loess10", "loess30"))) +        
        geom_line(aes(x = age, y = values, color = category), size = 1) +
        facet_wrap(~method, ncol = 2) +
        scale_color_manual(values = c("grey75", "orange")) +
        labs(x = "age", y = "naturalizations per year", color = "") +
        neutral      
    
 
    