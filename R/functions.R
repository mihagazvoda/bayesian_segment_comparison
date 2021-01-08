fit_lm <- function(rec_obj, ...) {
  linear_reg() %>%
    set_engine("lm") %>%
    fit(..., data = juice(rec_obj, everything()))
}

pred_lm <- function(split_obj, rec_obj, model_obj, ...) {
  mod_data <- bake(
    rec_obj,
    new_data = assessment(split_obj)
  )

  # out <- select(mod_data, y)
  predict(
    model_obj,
    new_data = mod_data,
    type = "pred_int",
    level = 0.95
  ) %>%
    mutate(
      estimate = (.pred_lower + .pred_upper) / 2,
      truth = mod_data$y
    )
}
