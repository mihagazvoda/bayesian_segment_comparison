create_prediction_table <- function(model, data, n = 100) {
  predicted_draws(model = model, newdata = data, n = n) %>% 
    mutate(
      diff = .prediction - y,
      rel_diff = diff / y,
      z_score = (mean(.prediction) - y) / sd(.prediction)
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
