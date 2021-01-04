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
    ),
    metrics = map(
      predictions_test, 
      ~yardstick::metrics(.x, truth = y, estimate = .prediction))
  )

cv %>% 
  select(-c(splits, model, predictions_train)) %>% 
  unnest(cols = predictions_test) %>% 
  ggplot() +
  geom_point(aes(.row, .prediction, group = .row)) + 
  geom_errorbar(aes(x = .row , ymin = .prediction + sd, ymax = .prediction - sd)) + 
  geom_point(aes(.row, y), color = "red") +
  facet_grid(~id)

# ---- build model ----
m <- build_model(train_data)



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