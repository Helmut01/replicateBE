#######################################
# Scripts to check internal reference #
# data sets (RData).                  #
#######################################
if (packageDescription("replicateBE")$Version < "1.0.5") {
  message("replicateBE 1.0.5 or higher required.")
  } else {
  sets <- 22
  ds <- sprintf("rds%02i", 1:sets)
  for (j in seq_along(ds)) {
    ABE(data=eval(parse(text=ds[j])))
  }
  for (j in seq_along(ds)) {
    method.A(ola=TRUE, plot.bxp=TRUE, data=eval(parse(text=ds[j])))
  }
  for (j in seq_along(ds)) {
    method.B(ola=TRUE, data=eval(parse(text=ds[j])))
  }
  for (j in seq_along(ds)) {
    method.B(ola=TRUE, option=1, data=eval(parse(text=ds[j])))
  }
}
