#######################################
# Scripts to check internal reference #
# data sets (RData).                  #
#######################################
if (packageDescription("replicateBE")$Version < "1.0.5") {
  message("replicateBE 1.0.5 or higher required.")
  } else {
  sets <- 24
  ds <- sprintf("rds%02i", 1:sets)
  adjust <- FALSE # set to TRUE if you have some spare time
  ### ABE ###
  for (j in seq_along(ds)) {
    ABE(data=eval(parse(text=ds[j])))
    if (j == 2) {
      ABE(data=eval(parse(text=ds[j])), theta1=0.9)
    }
  }
  ### method.A ###
  for (j in seq_along(ds)) {
    method.A(ola=TRUE, plot.bxp=TRUE, adjust=adjust, data=eval(parse(text=ds[j])))
  }
  ### method.B (option=2) ###
  for (j in seq_along(ds)) {
    method.B(ola=TRUE, data=eval(parse(text=ds[j])))
  }
  ### method.B (option=1) ###
  for (j in seq_along(ds)) {
    method.B(ola=TRUE, option=1, data=eval(parse(text=ds[j])))
  }
}
