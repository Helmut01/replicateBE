#######################################
# Scripts to check internal reference #
# data sets imported from CSV-files.  #
#######################################
sets    <- 1:24
set     <- sprintf("%02i", sets)
path.in <- paste0(find.package("replicateBE"), "/extdata/") # example files
adjust  <- FALSE # set to TRUE if you have some spare time
### ABE ###
for (j in seq_along(sets)) {
  ABE(path.in=path.in, file="DS", set=set[j], ext="csv")
  if (j == 2) {
    ABE(path.in=path.in, file="DS", set=set[j], ext="csv", theta1=0.9)
  }
}
### method.A ###
for (j in seq_along(sets)) {
  method.A(path.in=path.in, file="DS", set=set[j], ext="csv",
           ola=TRUE, plot.bxp=TRUE, adjust=adjust)
}
### method.B (option=2) ###
for (j in seq_along(sets)) {
  method.B(path.in=path.in, file="DS", set=set[j], ext="csv",
           ola=TRUE)
}
### method.B (option=1) ###
for (j in seq_along(sets)) {
  method.B(path.in=path.in, file="DS", set=set[j], ext="csv",
           ola=TRUE, option=1)
}
