norm.test <- function(data)
{
  result <- shapiro.test(data)
  return(result$p.value)
}

norm.test2 <- function(data, alpha=0.05)
{
  result <- shapiro.test(data)
  return(ifelse(result$p.value>alpha, T, F))
}

center <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}
