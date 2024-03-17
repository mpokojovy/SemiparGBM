# (C) Andrews T. Anum (2024)

MSE.score <- function(pred, obs) mean((pred - obs)^2)

MAPE.score <- function(pred, obs) 100*mean(abs(pred - obs)/obs)
