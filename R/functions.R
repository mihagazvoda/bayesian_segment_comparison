create_prediction_table <- function(model, data, n = 100) {
  browser()
  sim(model, n = n, data = data) %>% 
    as_tibble() %>% 
    mutate(sample = row_number()) %>% 
    pivot_longer(
      !sample, 
      names_to = "point", 
      values_to = "prediction"
    ) %>% 
    mutate(id = as.integer(stringr::str_remove(point, "V"))) %>% 
    inner_join(mutate(data, id = row_number()), by = "id") %>% 
    select(-point) %>% 
    mutate(
      diff = prediction - y,
      rel_diff = diff / y,
      z_score = (mean(prediction) - y) / sd(prediction)
    )
}

build_model <- function(data) {
  quap(
    alist(
      y ~ dnorm(mu, sigma),
      mu <- a * x1 + b * x2,
      a ~ dnorm(0, 1),
      b ~ dnorm(0, 1),
      sigma ~ exp(0.25)
    ),
    data = data
  )
}
