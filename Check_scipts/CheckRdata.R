########################################
# Scripts to check internal reference  #
# data sets (RData).                   #
########################################
#####################################################
# something is rotten (no output generated). Example:
# data("RRT.RTR.TRR")
# ABE(data=rds02) # works
# ds <- sprintf("%02i", c(2, 4, 7))
# j=1
# ABE(data=eval(parse(text=ds[j]))) # works in the console
# but not in the loop (lines 71-73)
# if I copypaste the entire loop to the console it works as well.
#####################################################
if (packageDescription("replicateBE")$Version < "1.0.5") {
  message("replicateBE 1.0.5 or higher required.")
  } else {
  #################
  data("RTRT.TRTR")
  ds <- sprintf("rds%02i", c(1, 6, 8:9, 12:15, 18:21))
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
  #################
  data("RTTR.TRRT")
  ds <- sprintf("rds%02i", c(5, 11, 16))
  for (j in seq_along(ds)) {
    method.A(ola=TRUE, plot.bxp=TRUE, data=eval(parse(text=ds[j])))
  }
  for (j in seq_along(ds)) {
    method.B(ola=TRUE, data=eval(parse(text=ds[j])))
  }
  for (j in seq_along(ds)) {
    method.B(ola=TRUE, option=1, data=eval(parse(text=ds[j])))
  }

  ###############
  data("RTR.TRT")
  ds <- sprintf("rds%02i", c(3, 17))
  # rds17: ABE() # No results without an error message! Why?
  for (j in seq_along(ds)) {
    method.A(ola=TRUE, plot.bxp=TRUE, data=eval(parse(text=ds[j])))
  }
  for (j in seq_along(ds)) {
    method.B(ola=TRUE, data=eval(parse(text=ds[j])))
  }
  for (j in seq_along(ds)) {
    method.B(ola=TRUE, option=1, data=eval(parse(text=ds[j])))
  }

  ###############
  data("TRR.RRT")
  ABE(data=rds10)
  method.A(data=rds10, ola=TRUE, plot.bxp=TRUE)
  method.B(data=rds10, ola=TRUE)
  method.B(data=rds10, ola=TRUE, option=1)

  ###################
  data("RRT.RTR.TRR")
  ds <- sprintf("rds%02i", c(2, 4, 7))
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
