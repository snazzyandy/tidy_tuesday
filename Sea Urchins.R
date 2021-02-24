#TITLE: TidyModels Tutorial: Using Parsnip on sea urchins data
#Goal : Understand how to use Parnips tools and develop modelling skills
#Derived from https://www.tidymodels.org/start/models/

#Load Libs
library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results

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



