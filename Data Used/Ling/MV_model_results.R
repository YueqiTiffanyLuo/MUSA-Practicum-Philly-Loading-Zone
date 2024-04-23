library(tidyverse)
library(tidycensus)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot) # plot correlation plot
library(corrr)      # another way to plot correlation plot
library(kableExtra)
library(broom)
library(tufte)
library(rmarkdown)
library(hexbin)
library(viridis)
library(cbsodataR)
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots
library(ggpubr)    # plotting R^2 value on ggplot point scatter
library(broom.mixed) # needed for effects plots
library(stargazer)
library(jsonlite)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(raster)
library(RColorBrewer)
library(mapview)
library(leaflet)
library(plotly)
library(ggspatial)
library(openxlsx)
library(lubridate)
library(dplyr)
library(tidyr)
library(reshape2)
library(riem)

#library(rjson)

# functions and data directory

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette2 <- c('#3E4A89','#1F9E89')
palette4 <- c('#3E4A89','#1F9E89','#35B779','#B4DE2C')
palette5 <- c('#440154','#3E4A89','#1F9E89','#35B779','#B4DE2C')
palette6 <- c('#440154','#3E4A89','#1F9E89','#35B779','#B4DE2C','#FDE725')
palette10 <- c('#440154','#482777','#3E4A89','#31688E','#26828E','#1F9E89','#35B779','#6DCD59','#B4DE2C','#FDE725')

first_panel <- st_read("C:/Users/14145/Documents/GitHub/MUSA_550/MUSA-Practicum-Philly-Loading-Zone/finalpanel.geojson")

road_types <- read_csv("C:/Users/14145/Box/Practicum_Philly_2024/Michael/all_curbs_with_road_class.csv") %>%
  select(-"CLASS") %>%
  select(c("CLASS_2","OID_"))

curb_vars <- st_read("C:/Users/14145/Box/Practicum_Philly_2024/Data/zones_with_OMF_vars.geojson")

curb_vars <- merge(curb_vars, road_types, by.x="nearest_indices", by.y="OID_")

first_panel <- curb_vars %>%
  st_drop_geometry(.) %>%
  select(c("SmartZoneName","categry","rgltn_t","CLASS_2")) %>%
  merge(., first_panel, by="SmartZoneName")


filtered_panel <- first_panel %>%
  select(-c("week","car","freight","truck","Sun","Mon","Tue","Wed","Thu","Fri","Sat",
          "Mid_Day","overnight","PM_Rush","Total_Dwell_Time","Average_Dwell_Time","curb_zone_id","van")) %>%
  mutate(log_events = log10(Total_Events))

library(leaps)

#Set random seed
set.seed(1999)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(filtered_panel), replace=TRUE, prob=c(0.7,0.3))
train  <- filtered_panel[sample, ]
test   <- filtered_panel[!sample, ]

exact_collin_fit = train %>%
  select(-c("log_events","SmartZoneName","geometry")) %>%
  lm(Total_Events ~ ., data = .)
summary(exact_collin_fit) #adj R^2 of 0.83

log_exact_collin_fit = train %>%
  select(-c("Total_Events","SmartZoneName","geometry")) %>%
  lm(log_events ~ ., data = .)
summary(log_exact_collin_fit) #adj R^2 of 0.87

#Data skews
hist(filtered_panel$Total_Events)
hist(log10(filtered_panel$Total_Events))

#filtered_panel %>% dplyr::select(where(is.numeric)) %>%
#  cor() %>% view()

#Run test data through it
test$prediction <- predict(exact_collin_fit, newdata = test)
test <- mutate(test, prediction_error = Total_Events-prediction)
test <- test %>%
  mutate(log_prediction = 10^(predict(log_exact_collin_fit, newdata = test)))
test <- mutate(test, log_prediction_error = Total_Events-log_prediction)
test <- mutate(test,
               perc_error = 100*abs(prediction_error)/Total_Events,
               perc_log_error = 100*abs(log_prediction_error/Total_Events))

#Regular prediction charts
ggplot()+
  geom_point(data=test, aes(x=Total_Events, y=perc_error, color=prediction_error), size=3, alpha=0.5)+
  scale_color_viridis_b()

plot2<-test %>%
  st_as_sf(sf_column_name="geometry") %>%
  ggplot()+
  geom_sf(aes(size=perc_error, color=perc_error), alpha=0.2)+
  scale_color_binned(n.breaks=5, type = "viridis", direction=-1)+
  theme_bw()
ggplotly(plot2)

ggplot()+
  geom_histogram(data=test, aes(x=prediction_error), color="black", fill="lightblue")+
  labs(title="Histogram of Prediction Error (non-transformed data)")+
  theme_bw()
  
ggplot()+
  geom_histogram(data=test, aes(x=perc_error), color="black", fill="lightblue")+
  labs(title="Histogram of Percent Error (non-transformed data)")+
  theme_bw()
  
#Log-transformed prediction charts
plot3<-ggplot()+
  geom_point(data=test, aes(x=Total_Events, y=log_prediction, color=perc_log_error), size=3, alpha=0.5)+
  scale_color_viridis_b()+
  xlim(0,60)+
  ylim(0,60)+
  geom_abline(mapping=NULL, 1)
ggplotly(plot3)

plot4<-test %>%
  st_as_sf(sf_column_name="geometry") %>%
  ggplot()+
  geom_sf(aes(size=perc_error, color=perc_log_error), alpha=0.2)+
  scale_color_binned(n.breaks=5, type = "viridis", direction=-1)+
  theme_bw()
ggplotly(plot4)

ggplot()+
  geom_histogram(data=test, aes(x=log_prediction_error), color="black", fill="lightblue")+
  labs(title="Histogram of Prediction Error (transformed data)")+
theme_bw()

ggplot()+
  geom_histogram(data=test, aes(x=perc_log_error), color="black", fill="lightblue")+
  labs(title="Histogram of Percent Error (transformed data)")+
theme_bw()

#TIDYMODEL
library(tidymodels)  # for the parsnip package, along with the rest of tidymodels
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker) 
library(skimr)

set.seed(222)
# FIRST XGBOOST
data_split <- filtered_panel %>%
  mutate(categry = as.factor(categry),
         rgltn_t = as.factor(rgltn_t),
         CLASS_2 = as.factor(CLASS_2)) %>%
  select(-c("geometry","Total_Events")) %>%
  initial_split(prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

ldg_rec <- 
  recipe(log_events ~ ., data = train_data) %>% 
  update_role(SmartZoneName, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

view(summary(ldg_rec))

lr_mod <- 
  boost_tree() %>% 
  set_engine("xgboost") %>%
  set_mode("regression")

ldg_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(ldg_rec)

ldg_fit <- 
  ldg_wflow %>% 
  fit(data = train_data)

ldg_fit %>% 
  extract_fit_parsnip()

predict(ldg_fit, test_data)

ldg_aug <- 
  augment(ldg_fit, test_data)

ldg_aug %>%
  select(c(".pred","SmartZoneName","log_events")) %>%
  mutate(Total_Events = 10^(log_events),
         prediction = 10^(.pred),
         abs_error = abs(prediction-Total_Events),
         abs_perc_error = abs_error/Total_Events*100) %>%
  ggplot()+
  geom_histogram(aes(x=abs_perc_error), binwidth=10, color="white")+
  xlab("Abs. Percent Error (%)")+
  theme_bw()

# SECOND XGBOOST
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_data),
  learn_rate(),
  size = 30
)

xgb_wf <- workflow() %>%
  add_formula(log_events ~ .) %>%
  add_model(xgb_spec)

vb_folds <- vfold_cv(train_data, strata = CLASS_2)

doParallel::registerDoParallel()
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

show_best(xgb_res, metric="rmse")
best_rmse<-select_best(xgb_res, metric="rmse")

final_xgb <- finalize_workflow(
  xgb_wf,
  best_rmse
)

final_xgb

ldg_fit2 <-
  final_xgb %>%
  fit(data = train_data)

ldg_fit2 %>%
  extract_fit_parsnip()

predict(ldg_fit2, test_data)

ldg_aug2 <-
  augment(ldg_fit2, test_data)

ldg_aug2 %>%
  select(c(".pred","SmartZoneName","log_events")) %>%
  mutate(Total_Events = 10^(log_events),
         prediction = 10^(.pred),
         abs_error = abs(prediction-Total_Events),
         abs_perc_error = abs_error/Total_Events*100) %>%
  ggplot()+
  geom_histogram(aes(x=abs_perc_error), binwidth=10, color="white")+
  xlab("Abs. Percent Error (%)")+
  theme_bw()


