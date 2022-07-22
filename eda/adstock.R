library(tictoc)
library(future)
library(doFuture)
library(car)

model_spec_prophet_boost_tune <- prophet_boost(

  mode = "regression",
  changepoint_num = tune(),
  logistic_floor = min(model_data2$transacciones),
  logistic_cap = max(model_data2$transacciones),
  seasonality_yearly = TRUE,
  seasonality_weekly = TRUE,
  seasonality_daily = FALSE,
  growth = 'logistic',
  trees = 5000,
  mtry = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("prophet_xgboost")

wflw_spec_prophet_boost_tune <- workflow() %>%
  add_model(model_spec_prophet_boost_tune) %>%
  add_recipe(recipe_base)

set.seed(123)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_prophet_boost_tune) %>%
    update(
      mtry = mtry(range = c(1, 15)),
      learn_rate = learn_rate(range = c(-6.0, 1.0), trans = log10_trans()),
      min_n = min_n(range = c(1, 15)),
      tree_depth = tree_depth(range = c(1, 10))
           ),
  size = 100
)

model_tbl <- grid_spec_1 %>%
  create_model_grid(
    f_model_spec = prophet_boost,
    engine_name  = "prophet_xgboost",
    mode         = "regression",
    engine_params = list(
    logistic_floor = min(model_data2$transacciones),
    logistic_cap = max(model_data2$transacciones),
    growth = 'logistic',
    trees = 10000
    )
  )

model_list <- model_tbl$.models
model_list


model_wfset <- workflow_set(
  preproc = list(
    base = recipe_base
  ),
  models = model_list,
  cross = TRUE
)

model_wfset

tic()
model_sequential_tbl <- model_wfset %>%
  modeltime_fit_workflowset(
    data    = training_data,
    control = control_fit_workflowset(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )
toc()

model_parallel_tbl %>%
  modeltime_calibrate(testing_data) %>%
  modeltime_accuracy() %>%
  arrange(mape) %>%
  table_modeltime_accuracy(.interactive = FALSE)

###############################################################################
model_spec_random_forest_tune <- rand_forest(

  mode = "regression",
  trees = 10000,
  mtry = tune(),
  min_n = tune()
  ) %>%
  set_engine("ranger")

wflw_spec_random_forest_tune <- workflow() %>%
  add_model(model_spec_random_forest_tune) %>%
  add_recipe(recipe_base)

set.seed(123)
grid_spec_1 <- model_spec_random_forest_tune %>%
  extract_parameter_set_dials() %>%
    update(
      mtry = mtry(range = c(1, 25)),
      min_n = min_n(range = c(1, 25))
    ) %>%
  grid_latin_hypercube(size = 100)

model_tbl <- grid_spec_1 %>%
  create_model_grid(
    f_model_spec = rand_forest,
    engine_name  = "ranger",
    mode         = "regression",
    engine_params = list(
    trees = 10000
    )
  )

model_list <- model_tbl$.models
model_list[28]


model_wfset <- workflow_set(
  preproc = list(
    base = recipe_base
  ),
  models = model_list,
  cross = TRUE
)

model_wfset

tic()
model_sequential_tbl <- model_wfset %>%
  modeltime_fit_workflowset(
    data    = training_data,
    control = control_fit_workflowset(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )
toc()

model_sequential_tbl %>%
  modeltime_calibrate(testing_data) %>%
  modeltime_accuracy() %>%
  arrange(mape) %>%
  table_modeltime_accuracy(.interactive = TRUE)

###############################################################################
model_glmnet <- linear_reg(
  mode = "regression",
  penalty = tune(),
  mixture = tune()
  ) %>%
  set_engine("glmnet")

wflw_spec_random_forest_tune <- workflow() %>%
  add_model(model_glmnet) %>%
  add_recipe(recipe_wod)

set.seed(123)
grid_spec_1 <- model_glmnet %>%
  extract_parameter_set_dials() %>%
    update(
      penalty = penalty(range = c(-10, 1), trans = log10_trans()),
      mixture = mixture(range = c(0, 1))
    ) %>%
  grid_latin_hypercube(size = 100)

model_tbl <- grid_spec_1 %>%
  create_model_grid(
    f_model_spec = linear_reg,
    engine_name  = "glmnet",
    mode         = "regression"
  )

model_list <- model_tbl$.models
model_list[94]


model_wfset <- workflow_set(
  preproc = list(
    base = recipe_wod
  ),
  models = model_list,
  cross = TRUE
)

model_wfset

tic()
model_sequential_tbl <- model_wfset %>%
  modeltime_fit_workflowset(
    data    = training_data,
    control = control_fit_workflowset(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )
toc()

model_sequential_tbl %>%
  modeltime_calibrate(testing_data) %>%
  modeltime_accuracy() %>%
  arrange(mape) %>%
  table_modeltime_accuracy(.interactive = TRUE)

###############################################################################
model_spec_random_forest_tune <- nnetar_reg(

  mode = "regression",
  seasonal_period = "auto",
  non_seasonal_ar = tune(),
  seasonal_ar = tune(),
  hidden_units = tune(),
  num_networks = tune(),
  penalty = tune(),
  epochs = tune()
  ) %>%
  set_engine("nnetar")

wflw_spec_random_forest_tune <- workflow() %>%
  add_model(model_spec_random_forest_tune) %>%
  add_recipe(recipe_base)

set.seed(123)
grid_spec_1 <- model_spec_random_forest_tune %>%
  extract_parameter_set_dials() %>%
    update(
      non_seasonal_ar = non_seasonal_ar(),
      seasonal_ar = seasonal_ar(),
      hidden_units = hidden_units(),
      num_networks = num_networks(),
      penalty = penalty(),
      epochs = epochs()
    ) %>%
  grid_latin_hypercube(size = 20)

model_tbl <- grid_spec_1 %>%
  create_model_grid(
    f_model_spec = nnetar_reg,
    engine_name  = "nnetar",
    mode         = "regression"
  )

model_list <- model_tbl$.models
model_list


model_wfset <- workflow_set(
  preproc = list(
    base = recipe_base
  ),
  models = model_list,
  cross = TRUE
)

model_wfset

tic()
model_sequential_tbl <- model_wfset %>%
  modeltime_fit_workflowset(
    data    = training_data,
    control = control_fit_workflowset(
      verbose   = TRUE,
      allow_par = FALSE
    )
  )
toc()

model_sequential_tbl %>%
  modeltime_calibrate(testing_data) %>%
  modeltime_accuracy() %>%
  arrange(mape) %>%
  table_modeltime_accuracy(.interactive = FALSE)

model_sequential_tbl %>%
  autoplot() +
  geom_smooth(se = FALSE)

##############################################################################
optimizeAdStock <- function(grp,sales){

	rmax <- 0
	corrmax <- -1
	rates <- seq(0.00, 2.00, by = 0.001)
	corrs <- vector()

	for(r in rates){
		adstock <- stats::filter(grp, filter=r, method = "recursive")
		corr <- cor(adstock,sales)
		if(corr>corrmax){
			corrmax <- corr
			rmax <- r
		}
		corrs <- append(corrs, corr)
	}

	plot(rates,corrs)
	sprintf(
	  "When the forgetting rate is% f, the correlation coefficient is the maximum value .%f",
	  rmax, corrmax)
}

fb_costo_link_clicks
gads_costo_buscar
fb_costo_reach
gads_costo_display
gads_costo_vídeo

cor(model_data2$fb_costo_link_clicks, log(model_data2$transacciones))
optimizeAdStock(model_data2$fb_costo_link_clicks, log(model_data2$transacciones))

cor(model_data2$gads_costo_buscar, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(gads_costo_buscar)) %>%
with(
  optimizeAdStock(gads_costo_buscar, log(transacciones))
)

cor(model_data2$fb_costo_reach, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(fb_costo_reach)) %>%
with(
  optimizeAdStock(fb_costo_reach, log(transacciones))
)

cor(model_data2$gads_costo_display, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(gads_costo_display)) %>%
with(
  optimizeAdStock(gads_costo_display, log(transacciones))
)

cor(model_data2$gads_costo_vídeo, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(gads_costo_vídeo)) %>%
with(
  optimizeAdStock(gads_costo_vídeo, log(transacciones))
)

###############################################
cor(model_data2$gads_costo_shopping, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(gads_costo_shopping)) %>%
with(
  optimizeAdStock(gads_costo_shopping, log(transacciones))
)

cor(model_data2$gads_costo_rendimiento_máximo, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(gads_costo_rendimiento_máximo)) %>%
with(
  optimizeAdStock(gads_costo_rendimiento_máximo, log(transacciones))
) # ok

cor(model_data2$fb_costo_video_views, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(fb_costo_video_views)) %>%
with(
  optimizeAdStock(fb_costo_video_views, log(transacciones))
)

cor(model_data2$fb_costo_post_engagement, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(fb_costo_post_engagement)) %>%
with(
  optimizeAdStock(fb_costo_post_engagement, log(transacciones))
) # Ok

cor(model_data2$fb_costo_product_catalog_sales, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(fb_costo_product_catalog_sales)) %>%
with(
  optimizeAdStock(fb_costo_product_catalog_sales, log(transacciones))
) #ok

cor(model_data2$fb_costo_messages, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(fb_costo_messages)) %>%
with(
  optimizeAdStock(fb_costo_messages, log(transacciones))
) # Ok

cor(model_data2$fb_costo_outcome_awareness, log(model_data2$transacciones), use = "pairwise.complete.obs")
model_data2 %>%
  filter(!is.na(fb_costo_outcome_awareness)) %>%
with(
  optimizeAdStock(fb_costo_outcome_awareness, log(transacciones))
)



model_data2 %>% select_at(vars(contains("costo"))) %>% names()


model_data2 %>%
  select(fecha, transacciones, gads_costo_buscar) %>%
  filter(!is.na(gads_costo_buscar)) %>%
  mutate(adstock_gads_costo_buscar = stats::filter(gads_costo_buscar, filter = 0.482, method = "recursive"))





adstock_rate <- 0.19
max_memory   <- 10

# Create Data
advertising <- c(117.913, 120.112, 125.828, 115.354, 177.090, 141.647, 137.892,   0.000,   0.000,   0.000,   0.000,
                   0.000,   0.000,   0.000,   0.000,   0.000,   0.000, 158.511, 109.385,  91.084,  79.253, 102.706,
                  78.494, 135.114, 114.549,  87.337, 107.829, 125.020,  82.956,  60.813,  83.149,   0.000,   0.000,
                   0.000,   0.000,   0.000,   0.000, 129.515, 105.486, 111.494, 107.099,   0.000,   0.000,   0.000,
                   0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000)
advertising <- model_data2$fb_costo_link_clicks
# Calculate Advertising Adstock

learn_rates <- rep(adstock_rate, max_memory+1) ^ c(0:max_memory)
adstocked_advertising <- stats::filter(c(rep(0, max_memory), advertising), learn_rates, method="convolution")
adstocked_advertising <- adstocked_advertising[!is.na(adstocked_advertising)]
cor(adstocked_advertising, log(model_data2$transacciones))

# Graph Data
plot(seq(1,length(advertising)), advertising, type="h",
     xlab="Time (Usually in Weeks)", ylab="Advertising",
     ylim=c(0, max(c(advertising, adstocked_advertising))),
     frame.plot=FALSE)
lines(adstocked_advertising)


boxCox(model_data2$transacciones, family="yjPower", plotit = TRUE)

lambda1 <- forecast::BoxCox.lambda(model_data2$transacciones, method = "guerrero")
lambda2 <- forecast::BoxCox.lambda(model_data2$transacciones, method = "loglik", lower = -3)
forecast::BoxCox(model_data2$transacciones, lambda = lambda1)
step_box_cox()





