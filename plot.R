library(ggplot2)

plot.mars <- function(f) {
  df <- data.frame(
    x = f[["fitted.values"]],
    y = f[["residuals"]]
  )
  ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth()
}
