#####################################
# S3 method for printing results of #
# ABE(), method.A(), and method.B() #
#####################################
print.repBE <- function(x, ...) {
  # easier to assign results to new variables
  # and get rid of the factors
  #browser()
  design   <- as.character(x$Design)
  method   <- as.character(x$Method)
  n        <- as.numeric(x$n)
  nTT      <- as.numeric(x$nTT)
  nRR      <- as.numeric(x$nRR)
  Nsub.seq <- as.character(x[["Sub/seq"]])
  Miss.seq <- as.character(x[["Miss/seq"]])
  Miss.per <- as.character(x[["Miss/per"]])
  alpha    <- x$alpha
  DF       <- as.numeric(as.character(x$DF))
  CVwT     <- x[["CVwT(%)"]]
  CVwR     <- x[["CVwR(%)"]]
  sw.ratio <- as.numeric(x$sw.ratio)
  EL       <- c(x[["EL.lo(%)"]], x[["EL.hi(%)"]])
  CI       <- c(x[["CI.lo(%)"]], x[["CI.hi(%)"]])
  PE       <- x[["PE(%)"]]
  CI.ass   <- x$CI
  GMR.ass  <- x$GMR
  BE.ass   <- x$BE
  if (method == "ABE") {
    BE.lim  <- c(x[["BE.lo(%)"]], x[["BE.hi(%)"]])
  } else {
    outlier <- x$outlier
    ifelse (is.null(x[["CVwR.new(%)"]]),
            CVwR.new <- NA,  CVwR.new<- x[["CVwR.new(%)"]])
    ifelse (is.null(x$sw.ratio.new),
            sw.ratio.new <- NA, sw.ratio.new<- x$sw.ratio.new)
    ifelse (is.null(x[["EL.new.lo(%)"]]),
            EL.new <- NA, EL.new <- c(x[["EL.new.lo(%)"]], x[["EL.new.hi(%)"]]))
    ifelse (is.null(x$CI.new),
            CI.new <- NA, CI.new <- x$CI.new)
    ifelse (is.null(x$GMR.new),
            GMR.new <- NA, GMR.new <- x$GMR.new)
    ifelse (is.null(x$BE.new),
            BE.new <- NA, BE.new <- x$BE.new)
  }
  cat("\ndesign =", design, " method =", method, " n =", n, " nTT =", nTT, " nRR =", nRR,
      "\nSub/seq =", Nsub.seq, " Miss/seq =", Miss.seq, " Miss/per =", Miss.per,
      "\nalpha =", alpha, " DF =", DF, " CVwT(%) =", signif(CVwT, 7), " CVwR(%) =", signif(CVwR, 7),
      " sw.ratio =", signif(sw.ratio, 7))
  if (method == "ABE") {
    cat("\nBE.lim =", sprintf("%6.2f%%", BE.lim))
  } else {
    cat("\nEL =", sprintf("%6.2f%%", EL))
  }
  cat("\nCI =", sprintf("%6.2f%%", CI))
  if (method == "ABE") {
    cat(": BE =", BE.ass, "\n",
        draw.line(called.from="ABE", L=BE.lim[1]/100, U=BE.lim[2]/100,
                  lo=CI[1]/100, hi=CI[2]/100, PE=PE/100))
  } else {
    cat(": CI =", CI.ass, " GMR =", GMR.ass, " BE =", BE.ass, "\n",
        draw.line(called.from="ABEL", L=EL[1]/100, U=EL[2]/100,
                  lo=CI[1]/100, hi=CI[2]/100, PE=PE/100))
  }
  cat("\n\n")
}
