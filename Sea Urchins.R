#TITLE: TidyModels Tutorial: Using Parsnip on sea urchins data
#Goal : Understand how to use Parnips tools and develop modelling skills
#Derived from https://www.tidymodels.org/start/models/

#Load Libs
library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results
library(rstanarm)


#Load Data
urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
#> 
#> ── Column specification ──────────────────────────────────────────────
#> cols(
#>   TREAT = col_character(),
#>   IV = col_double(),
#>   SUTW = col_double()
#> )

#Exploratory gg
urchins.gg <-ggplot(urchins, aes(x = initial_volume,y = width,group = food_regime, col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = TRUE) +
  scale_color_viridis_d(option = "plasma", end = .7)
#> `geom_smooth()` using formula 'y ~ x'

#Print gg
urchins.gg
######LINEAR APPRAOCH ###########################################
#Fit a basic linear model
lm_mod <- linear_reg() %>% set_engine("lm")

#Create the linear fit model using our LM mod object
lm_fit <- lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)

#Print lm 
tidy(lm_fit)

#Output tidy fit as a dotplot
tidy(lm_fit) %>% 
  dwplot(dots_args = list(size = 2, color = "black"),
    whisker_args = list(colour = "black"),
    vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

new_points <- expand.grid(initial_volume = 20, 
                          food_regime = c("Initial", "Low", "High"))
#Predicting dbl
mean_pred <- predict(lm_fit, new_data = new_points)

mean_pred

#Predict confidence interval based off training data
conf_int_pred <- predict(lm_fit, new_data = new_points, type = "conf_int")

#Now Combine:
plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "urchin size")
#> 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg


  #Session info - I LOADED TOO MUCH SHIT!
#R version 4.0.0 (2020-04-24)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 18363)

#Matrix products: default

#locale:
#  [1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252   
#[3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
#[5] LC_TIME=English_Australia.1252    

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] Rcpp_1.0.4.6      dotwhisker_0.6.0  broom.mixed_0.2.6 readr_1.4.0       yardstick_0.0.7  
#[6] workflows_0.2.1   tune_0.1.2        tidyr_1.1.2       tibble_3.0.6      rsample_0.0.9    
#[11] recipes_0.1.15    purrr_0.3.4       parsnip_0.1.5     modeldata_0.1.0   infer_0.5.4      
#[16] ggplot2_3.3.3     dplyr_1.0.4       dials_0.0.9       scales_1.1.1      broom_0.7.5      
#[21] tidymodels_0.1.2 

#loaded via a namespace (and not attached):
#  [1] backports_1.1.6      broomExtra_4.2.1     plyr_1.8.6           igraph_1.2.6        
#[5] TMB_1.7.19           splines_4.0.0        crosstalk_1.1.0.1    listenv_0.8.0       
#[9] rstantools_2.1.1     inline_0.3.17        digest_0.6.25        foreach_1.5.1       
#[13] htmltools_0.5.1.1    rsconnect_0.8.16     fansi_0.4.1          magrittr_2.0.1      
#[17] globals_0.14.0       gower_0.2.2          matrixStats_0.58.0   RcppParallel_5.0.2  
#[21] xts_0.12.1           prettyunits_1.1.1    colorspace_1.4-1     blob_1.2.1          
#[25] jsonlite_1.6.1       callr_3.4.3          crayon_1.3.4         lme4_1.1-26         
#[29] survival_3.1-12      zoo_1.8-8            iterators_1.0.13     glue_1.4.1          
#[33] gtable_0.3.0         ipred_0.9-9          V8_3.4.0             pkgbuild_1.0.8      
#[37] DBI_1.1.0            miniUI_0.1.1.1       viridisLite_0.3.0    xtable_1.8-4        
#[41] performance_0.7.0    ggstance_0.3.5       GPfit_1.0-8          StanHeaders_2.21.0-7
#[45] stats4_4.0.0         lava_1.6.8.1         prodlim_2019.11.13   DT_0.15             
#[49] htmlwidgets_1.5.1    threejs_0.3.3        ellipsis_0.3.1       loo_2.4.1           
#[53] pkgconfig_2.0.3      farver_2.0.3         nnet_7.3-13          utf8_1.1.4          
#[57] tidyselect_1.1.0     labeling_0.3         rlang_0.4.10         DiceDesign_1.9      
#[61] reshape2_1.4.4       later_1.1.0.1        munsell_0.5.0        tools_4.0.0         
#[65] cli_2.3.1            generics_0.1.0       ggridges_0.5.3       stringr_1.4.0       
#[69] fastmap_1.1.0        processx_3.4.2       packrat_0.5.0        future_1.21.0       
#[73] nlme_3.1-147         whisker_0.4          mime_0.9             compiler_4.0.0      
#[77] bayesplot_1.8.0      shinythemes_1.2.0    rstudioapi_0.13      curl_4.3            
#[81] statmod_1.4.35       lhs_1.1.1            stringi_1.4.6        ps_1.3.3            
#[85] parameters_0.12.0    lattice_0.20-41      Matrix_1.2-18        nloptr_1.2.2.2      
#[89] markdown_1.1         shinyjs_2.0.0        vctrs_0.3.6          pillar_1.4.4        
#[93] lifecycle_0.2.0      furrr_0.2.2          insight_0.13.1       httpuv_1.5.5        
#[97] R6_2.4.1             promises_1.1.1       gridExtra_2.3        parallelly_1.23.0   
#[101] codetools_0.2-16     boot_1.3-24          colourpicker_1.1.0   MASS_7.3-51.5       
#[105] gtools_3.8.2         assertthat_0.2.1     withr_2.2.0          shinystan_2.5.0     
#[109] mgcv_1.8-31          bayestestR_0.8.2     parallel_4.0.0       hms_0.5.3           
#[113] grid_4.0.0           rpart_4.1-15         timeDate_3043.102    minqa_1.2.4         
#[117] coda_0.19-4          class_7.3-16         pROC_1.17.0.1        shiny_1.6.0         
#[121] lubridate_1.7.8      base64enc_0.1-3      dygraphs_1.1.1.6    