perm.t.test.default <- function(x, y, B = 1000L, var.equal = FALSE, paired = FALSE,
                                conf.level = 0.95) {
  tst <- t.test(x, y, var.equal = var.equal, paired = paired)$statistic
  dat <- if (paired)
    x - y else c(x, y)
  idx <- rep(1:2, c(length(x), length(y)))

  res <- replicate(B, {
    if (paired) {
      t.test(dat * sign(runif(length(dat), -1, 1)))$statistic
    } else {
      ii <- sample(idx)
      t.test(dat[ii == 1L], dat[ii == 2L], var.equal = var.equal)$statistic
    }
  })

  pv <- mean(abs(res) >= abs(tst))
  ci <- qnorm(conf.level) * c(-1, 1) * sqrt(pv * (1 - pv) / B)

  pv + c(0, ci)
}
