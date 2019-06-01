##############################
# Create code for testthat.R #
##############################
ds <- substr(grep("rds", unname(unlist(data(package="replicateBE"))),
                  value=TRUE), 1, 5)
for (j in seq_along(ds)) {
  do <- get(ds[j])
  start.time <- proc.time()[[3]]
  x <- ABE(details=TRUE, print=FALSE, data=do)
  run.time <- signif(proc.time()[[3]] - start.time, 7)
  txt <- ""
  txt <- paste(paste0("\ntest_that(\"ABE (90% CI, PE) of ", ds[j], "\", {"),
               "\n  #", nrow(do), "observations, runtime", run.time, "seconds")
  if (run.time > 3) txt <- paste(txt, "\n  skip_on_cran()")
  txt <- paste(txt,
           paste0("\n  x <- ABE(details=TRUE, print=FALSE, data=", ds[j], ")"),
           "\n  expect_equivalent(x[15:17],",
           paste0("\n                    expected=c(",
           paste(signif(as.numeric(x[15:17]), 7), collapse=", "), "),"),
           "\n                    tolerance=0.001, scale=1)",
           "\n})\n")
  cat(txt)
  }

for (j in seq_along(ds)) {
  do <- get(ds[j])
  start.time <- proc.time()[[3]]
  x <- method.A(details=TRUE, print=FALSE, data=do)
  run.time <- signif(proc.time()[[3]] - start.time, 7)
  txt <- ""
  txt <- paste(paste0("\ntest_that(\"method.A (CVwR, EL, 90% CI, PE) of ", ds[j], "\", {"),
               "\n  #", nrow(do), "observations, runtime", run.time, "seconds")
  if (run.time > 3) txt <- paste(txt, "\n  skip_on_cran()")
  y <- x[, c(12, 15:19)]
  values <- as.numeric(signif(y, 7))
  comp <- paste0("c(", paste(values, collapse=", "), "),")
  txt <- paste(txt,
           paste0("\n  x <- method.A(details=TRUE, print=FALSE, data=", ds[j], ")"),
           "\n  expect_equivalent(x[, c(12, 15:19)],",
           paste0("\n                    expected=", comp,
           "\n                    tolerance=0.001, scale=1)",
           "\n})\n"))
  cat(txt)
  }

for (j in seq_along(ds)) {
  do <- get(ds[j])
  start.time <- proc.time()[[3]]
  x <- method.B(details=TRUE, print=FALSE, data=do)
  run.time <- signif(proc.time()[[3]] - start.time, 7)
  txt <- ""
  txt <- paste(paste0("\ntest_that(\"method.B (CVwR, EL, 90% CI, PE) of ", ds[j], "\", {"),
               "\n  #", nrow(do), "observations, runtime", run.time, "seconds")
  if (run.time > 3) txt <- paste(txt, "\n  skip_on_cran()")
  y <- x[, c(12, 15:19)]
  values <- as.numeric(signif(y, 7))
  comp <- paste0("c(", paste(values, collapse=", "), "),")
  txt <- paste(txt,
           paste0("\n  x <- method.B(details=TRUE, print=FALSE, data=", ds[j], ")"),
           "\n  expect_equivalent(x[, c(12, 15:19)],",
           paste0("\n                    expected=", comp,
           "\n                    tolerance=0.001, scale=1)",
           "\n})\n"))
  cat(txt)
  }

