#######################################
# Scripts to check internal reference #
# data sets imported from CSV-files.  #
#######################################
sets    <- 1:22
set     <- sprintf("%02i", sets)
path.in <- paste0(find.package("replicateBE"), "/extdata/") # example files
### ABE ###
for (j in seq_along(sets)) {
  ABE(path.in=path.in, file="DS", set=set[j], ext="csv")
}
### method.A ###
for (j in seq_along(sets)) {
  method.A(path.in=path.in, file="DS", set=set[j], ext="csv", ola=TRUE, plot.bxp=TRUE)
}
### method.B (option=2) ###
for (j in seq_along(sets)) {
  method.B(path.in=path.in, file="DS", set=set[j], ext="csv", ola=TRUE)
}
### method.B (option=1) ###
for (j in seq_along(sets)) {
  method.B(path.in=path.in, file="DS", set=set[j], ext="csv", ola=TRUE, option=1)
}
