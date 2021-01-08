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
  step_normalize(all_numeric())

# ---- CV ----
cv <- sliding_window(
  train_data_raw,
  lookback = SEGMENT_LENGTH,
  assess_stop = SEGMENT_LENGTH,
  step = SEGMENT_LENGTH
) %>% 
  mutate(
    recipes = map(splits, prepper, recipe = rec),
    model = map(recipes, fit_lm, y ~ .),
    predictions = pmap(
      lst(split_obj = splits, rec_obj = recipes,model_obj = model),
      pred_lm
    ),
    metrics <- map(predictions, metrics, truth = truth, estimate = estimate)
  )

# cv$recipes <- map(cv$splits, prepper, recipe = rec)
# 
# cv$model <- map(cv$recipes, fit_lm, y ~ .)
# 
# cv$pred <- pmap(
#   lst(
#     split_obj = cv$splits,
#     rec_obj = cv$recipes,
#     model_obj = cv$model
#   ),
#   pred_lm
# )

# cv$metrics <- map(cv$predictions, yardstick::metrics, truth = truth, estimate = estimate)
