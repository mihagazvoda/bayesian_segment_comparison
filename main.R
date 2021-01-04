source("R/packages.R")
source("R/functions.R")

CHANGE_DATE <- as.Date("2020-03-01")
SEGMENT_LENGTH <- 10

df <- tibble(
  date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
  y = rnorm(length(date), mean = 0, sd = 10),
  x1 = y + rnorm(length(y), mean = 0, sd = 1),
  x2 = rnorm(length(y), mean = 0, 5)
)

# ---- split and prepare data ----
train_data_raw <- filter(df, date < CHANGE_DATE)
test_data_raw <- filter(df, date >= CHANGE_DATE)

rec <- recipe(y ~ ., data = train_data_raw) %>%
  step_rm(date) %>%
  step_normalize(all_numeric()) %>%
  prep()

train_data <- bake(rec, new_data = train_data_raw)
test_data <- bake(rec, new_data = test_data_raw)

# ---- CV ----
cv <- sliding_window(
  train_data,
  lookback = SEGMENT_LENGTH,
  assess_stop = SEGMENT_LENGTH,
  step = SEGMENT_LENGTH
) %>%
  mutate(
    model = map(splits, ~ build_model(analysis(.))),
    predictions_train = map2(
      model,
      splits,
      ~ create_prediction_table(.x, analysis(.y), n = 100)
    ),
    predictions_test = map2(
      model,
      splits,
      ~ create_prediction_table(.x, assessment(.y), n = 100)
    )
  )

cv %>% 
  select(-c(splits, model, predictions_train)) %>% 
  rename(fold_id = id) %>% 
  unnest(cols = predictions_test) %>% 
  ggplot() +
  geom_boxplot(aes(id, prediction, group = id)) + 
  geom_point(aes(id, y), color = "red") +
  facet_grid(~fold_id)

# ---- build model ----
m <- build_model(train_data)

predictions <- create_prediction_table(m, test_data, n = 100)

predictions %>% 
  ggplot() +
  geom_histogram(aes((z_score)))

m_avg <- quap(
  alist(
    norm_diff ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 0.25),
    sigma ~ dexp(0.5)
  ),
  data = list(norm_diff = unique(predictions$z_score))
)

tidybayes::tidy_draws(m_avg) %>% 
  ggplot(aes(mu)) + 
  geom_histogram()

# # ---- tiydmodels ----
# bayes_mod <-   
#   linear_reg() %>% 
#   set_engine("stan", 
#              prior_intercept = rstanarm::normal(0, 0.1), 
#              prior = rstanarm::normal(0, 0.5)
#            ) 
# 
# bayes_fit <- bayes_mod %>% 
#   fit(y ~ ., data = train_data)
# 
# tidy(bayes_fit, conf.int = TRUE)
# 
# broom::tidy(bayes_fit, conf.int = TRUE)
# 
# rstanarm::normal(location = 0)
