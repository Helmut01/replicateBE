#######################################
# Scripts to check internal reference #
# data sets imported from CSV-files.  #
#######################################
path.in <- paste0(find.package("replicateBE"), "/extdata/") # example files
setwd(path.in)
sets    <- 1:length(list.files(pattern="DS\\d{2}\\.csv"))
set     <- sprintf("%02i", sets)
adjust  <- FALSE # set to TRUE if you have some spare time
### ABE ###
for (j in seq_along(sets)) {
  ABE(path.in=path.in, file="DS", set=set[j], ext="csv")
  if (j %in% c(2, 5, 10)) {
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
