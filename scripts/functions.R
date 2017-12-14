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

center <- function(x) 
  {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}

## next two functions from: 
## https://stackoverflow.com/questions/19012529/correlation-corrplot-configuration

# This panel adds significance starts, or NS for not significant
panel.signif <-  function (x, y, corr = NULL, col.regions, digits = 2, cex.cor, 
                           ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  results <- cor.test(x, y, alternative = "two.sided")
  est <- results$p.value
  stars <- ifelse(est < 5e-4, "***", 
                  ifelse(est < 5e-3, "**", 
                         ifelse(est < 5e-2, "*", "NS")))
  cex.cor <- 0.4/strwidth(stars)
  text(0.5, 0.5, stars, cex = cex.cor)
}


panel.shadeNtext <- function (x, y, corr = NULL, col.regions, ...) 
{
  if (is.null(corr)) 
    corr <- cor(x, y, use = "pair")
  ncol <- 14
  pal <- col.regions(ncol)
  col.ind <- as.numeric(cut(corr, breaks = seq(from = -1, to = 1, 
                                               length = ncol + 1), include.lowest = TRUE))
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = pal[col.ind], 
       border = NA)
  box(col = "lightgray")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- formatC(corr, digits = 2, format = "f")
  cex.cor <- .8/strwidth("-X.xx")
  text(0.5, 0.5, r, cex = cex.cor)
}