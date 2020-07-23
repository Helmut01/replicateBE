####################################################
# Calculate CV according to the EMA's Q&A-document #
####################################################
CV.calc <- function(alpha = 0.05, path.in, path.out, file, set = "",
                    ext, na, sep = ",", dec = ".", logtrans = TRUE,
                    ola = FALSE, details = FALSE, adjust = FALSE,
                    print, verbose = FALSE, ask = FALSE,
                    theta1 = theta1, theta2 = theta2, plot.bxp = FALSE,
                    fence = 2, data) {
  if (missing(path.in)) path.in <- NULL
  if (missing(data)) data <- NULL
  called.from <- as.character(sys.call(-1))[1]
  ret   <- get.data(path.in=path.in, path.out=path.out, file=file,
                    set=set, ext=ext, na=na, sep=sep, dec=dec,
                    logtrans=logtrans, print=print,
                    plot.bxp=plot.bxp, data=data)
  logtrans <- ret$logtrans
  ow <- options("digits")
  options(digits=12) # more digits for anova
  on.exit(ow)        # ensure that options are reset if an error occurs
  if (logtrans) {    # use raw data and log-transform internally
    modCVR <- lm(log(PK) ~ sequence + subject%in%sequence + period,
                           data=ret$ref)
  } else {           # use already log-transformed data
    modCVR <- lm(logPK ~ sequence + subject%in%sequence + period,
                         data=ret$ref)
  }
  aovCVR <- anova(modCVR)
  msewR  <- aovCVR["Residuals", "Mean Sq"]
  DfCVR  <- aovCVR["Residuals", "Df"]
  CVwR   <- mse2CV(msewR)
  if (ret$design == "full") { # not for the EMA but the WHO
    if (logtrans) {
      modCVT <- lm(log(PK) ~ sequence + subject%in%sequence + period,
                             data=ret$test)
    } else {
      modCVT <- lm(logPK ~ sequence + subject%in%sequence + period,
                           data=ret$test)
    }
    aovCVT <- anova(modCVT)
    msewT  <- aovCVT["Residuals", "Mean Sq"]
    DfCVT  <- aovCVT["Residuals", "Df"]
    CVwT  <- mse2CV(msewT)
    sw.ratio <- sqrt(msewT)/sqrt(msewR)
    sw.ratio.CI <- c(sw.ratio/sqrt(qf(0.1/2, df1=DfCVT, df2=DfCVR, lower.tail=FALSE)),
                     sw.ratio/sqrt(qf(1-0.1/2, df1=DfCVT, df2=DfCVR, lower.tail=FALSE)))
    names(sw.ratio.CI) <- c("lower", "upper")
  }
  outlier <- FALSE
  BE.rec  <- rep(NA, 2)
  if (ola & !called.from == "ABE") { # check for outliers
    stud.res  <- rstudent(modCVR)  # studentized (SAS)
    stud.res  <- stud.res[!is.na(stud.res)] # get rid of NAs and zeros
    stud.res  <- stud.res[which(stud.res != 0)]
    stud.res  <- stud.res[c(TRUE, FALSE)] # need only the 1st occasions
    bp1       <- boxplot(stud.res, range=fence, plot=FALSE)
    names.ol1 <- names(bp1$out)
    stand.res <- rstandard(modCVR) # standardized (SAS, PHX/WNL)
    stand.res <- stand.res[!is.na(stand.res)]
    stand.res <- stand.res[which(stand.res != 0)]
    stand.res <- stand.res[c(TRUE, FALSE)] # need only the 1st occasions
    bp2       <- boxplot(stand.res, range=fence, plot=FALSE)
    names.ol2 <- names(bp2$out)
    if (length(names.ol1) > 0) { # >=1 detected
      outlier   <- TRUE
      ol.value1 <- as.numeric(bp1$out)
      ol.seq1   <- as.character(ret$ref[names(bp1$out), "sequence"])
      ol.subj1  <- as.character(ret$ref[names(bp1$out), "subject"])
      ol.value2 <- as.numeric(bp2$out)
      ol.seq2   <- as.character(ret$ref[names(bp2$out), "sequence"])
      ol.subj2  <- as.character(ret$ref[names(bp2$out), "subject"])
      pars <- list(boxwex=0.5, boxfill="lightblue", medcol="blue",
                   outpch=21, outcex=1.35, outcol="red", outbg="#FFCCCC")
      overwrite <- TRUE
      if (as.logical(capabilities("png"))) {
        if (plot.bxp) {   # save in PNG format to path.out
          if (ask & file.exists(ret$png.path)) {
            answer <- tolower(readline("Boxplot already exists. Overwrite the PNG  [y|n]? "))
            if(answer != "y") overwrite <- FALSE
          }
          if (overwrite) { # either the file does not exist or should be overwritten
            png(ret$png.path, width=720, height=720, pointsize=18)
          }
        }
      } else {
        message("png-device is not available; changed to plot.bxp = FALSE")
      }
      bxp(bp1, xlim=c(0, 3), ylim=c(-1, 1)*max(abs(c(ol.value1, ol.value2))),
          las=1, ylab="residual", pars=pars, main="")
      title(main=expression(paste("EMA\u2019s model for ", CV[wR], ":")),
            line=3, cex.main=1.1)
      title(main=expression(paste("log(response) ~ sequence + subject(sequence) + period; data = R")),
            line=2, cex.main=1.05)
      title(main=bquote(paste("Outlier fence ", .(fence), "\u00D7IQR")),
            line=1, cex.main=1.05)
      if (length(names.ol1) == 0) {
        lab.txt <- "no outlier"
      } else {
        ifelse (length(ol.value1) == 1,
          lab.txt <- "1 outlier",
          lab.txt <- paste(length(ol.value1), "outliers"))
      } # Note: not /exactly/ equal. SAS uses 'type=2'
      mtext(paste0("studentized\n(R, ~SAS)\n\n", lab.txt), 1, line=4, at=1)
      text(rep(1.1, 2), bp1$stats[c(1, 5)], adj=c(0, 0.25), cex=0.8,
           sprintf("%+.3f", bp1$stats[c(1, 5)]))
      if (!identical(ol.value1, numeric(0))) { # only if stud. outlier
        text(rep(0.9, length(ol.value1)), ol.value1, adj=c(1, 0.25), cex=0.8,
             paste0("# ", ol.subj1, " (", ol.seq1, ")"))
        text(rep(1.1, length(ol.value1)), ol.value1, adj=c(0, 0.25), cex=0.8,
             sprintf("%+.3f", ol.value1))
      }
      bxp(bp2, axes=FALSE, at=2, add=TRUE, pars=pars)
      if (length(names.ol2) == 0) {
        lab.txt <- "no outlier"
      } else {
        ifelse (length(ol.value2) == 1,
          lab.txt <- "1 outlier",
          lab.txt <- paste(length(ol.value2), "outliers"))
      } # Note: not /exactly/ equal. SAS uses 'type=2' and PHX/WNL 'type=6'
      mtext(paste0("standardized\n(R, ~SAS,\n~Phoenix WinNonlin)\n", lab.txt), 1, line=4, at=2)
      text(rep(2.1, 2), bp2$stats[c(1, 5)], adj=c(0, 0.25), cex=0.8,
           sprintf("%+.3f", bp2$stats[c(1, 5)]))
      if (!identical(ol.value2, numeric(0))) { # only if stand. outlier
        text(rep(1.9, length(ol.value2)), ol.value2, adj=c(1, 0.25), cex=0.8,
             paste0("# ", ol.subj2, " (", ol.seq2, ")"))
        text(rep(2.1, length(ol.value2)), ol.value2, adj=c(0, 0.25), cex=0.8,
             sprintf("%+.3f", ol.value2))
      }
      abline(h=0, lty="dotted")
      if (plot.bxp && file.exists(ret$png.path)) {
        if (!is.null(dev.list()["png"])) {
          invisible(dev.off(dev.list()["png"]))
        }
      }
      # recalculate CVwR for data without outlier(s)
      ol   <- ret$ref[names(bp1$out), "subject"]
      excl <- ret$ref[!ret$ref$subject %in% ol, ]
      if (logtrans) { # use the raw data and log-transform internally
        modCVR.rec <- lm(log(PK) ~ sequence + subject%in%sequence + period,
                                   data=excl)
      } else {
        modCVR.rec <- lm(logPK ~ sequence + subject%in%sequence + period,
                                 data=excl)
      }
      aovCVR.rec <- anova(modCVR.rec)
      msewR.rec  <- aovCVR.rec["Residuals", "Mean Sq"]
      DfCVR.rec  <- aovCVR.rec["Residuals", "Df"]
      CVwR.rec   <- mse2CV(msewR.rec)
      if (ret$design == "full") {
        sw.ratio.rec <- sqrt(msewT)/sqrt(msewR.rec)
        sw.ratio.rec.CI <- c(sw.ratio.rec/sqrt(qf(0.1/2, df1=DfCVT, df2=DfCVR.rec, lower.tail=FALSE)),
                             sw.ratio.rec/sqrt(qf(1-0.1/2, df1=DfCVT, df2=DfCVR.rec, lower.tail=FALSE)))
        names(sw.ratio.rec.CI) <- c("lower", "upper")
      }
      if (verbose) {
        stud.res.whiskers <- signif(range(bp1$stats[, 1]), 7)
        stud.res.outliers <- data.frame(ol.subj1, ol.seq1, signif(ol.value1, 7))#
        names(stud.res.outliers) <- c("subject", "sequence", "stud.res")
        stand.res.whiskers <- signif(range(bp2$stats[, 1]), 7)
        stand.res.outliers <- data.frame(ol.subj2, ol.seq2, signif(ol.value2, 7))#
        names(stand.res.outliers) <- c("subject", "sequence", "stand.res")
        cat(paste0("\nOutlier analysis\n (externally) studentized residuals",
                   "\n Limits (", fence, "\u00D7IQR whiskers): ",
                   stud.res.whiskers[1], ", ", stud.res.whiskers[2],
                   "\n Outliers:\n")); print(stud.res.outliers, row.names=FALSE)
        cat(paste0("\n standarized (internally studentized) residuals\n Limits (", fence, "\u00D7IQR whiskers): ",
                   stand.res.whiskers[1], ", ", stand.res.whiskers[2],
                   "\n Outliers:\n"))
        # since standardized residuals are more liberal,
        # we have to deal with such a special case
        if (nrow(stand.res.outliers) == 0) {
          cat(" none detected\n")
        } else {
          print(stand.res.outliers, row.names=FALSE)
        }
      } # EO verbose
    } # EO >= 1 outlier
  } # EO outlier analysis (only if called from method.A()/method.B() & ola=TRUE)
  if (!called.from == "ABE") {
    reg_set <- reg_const("EMA")
    BE <- as.numeric(scABEL(CV=CVwR, regulator="EMA"))
  } else {
    BE <- c(theta1, theta2)
  }
  txt <- ret$txt
  if (called.from != "ABE") { # only for scaling
    txt <- paste0(ret$txt, "\nSwitching CV       : ",
                  sprintf("%6.2f%%", 100*reg_set$CVswitch),
                  "\nScaling cap        : ",
                  sprintf("%6.2f%%", 100*reg_set$CVcap),
                  "\nRegulat. const (k) :   ",
                  sprintf("%.3f", reg_set$r_const),
                  "\nGMR restriction    :  80.00% ... 125.00%")
  }
  if (ret$design == "full") {
    # sw.ratio <- CV2se(CVwT)/CV2se(CVwR) # we should already have it, right?
    txt <- paste0(txt, "\nCVwT               : ",
                  sprintf("%6.2f%%", 100*CVwT))
    if (called.from != "ABE") { # not needed for ABE
      txt <- paste0(txt, "\nswT                :   ",
                    sprintf("%.5f", CV2se(CVwT)))
    }
  }
  txt <- paste0(txt, "\nCVwR               : ", sprintf("%6.2f%%", 100*CVwR))
  if (called.from != "ABE") { # only for scaling
    txt <- paste0(txt, " (reference-scaling ")
    if (CVwR <= 0.3) txt <- paste0(txt, "not ")
    txt <- paste0(txt, "applicable)")
    if (CVwR <= 0.3) {
      txt <- paste0(txt, "\nUnscaled BE-limits :  80.00% ... 125.00%")
    } else {
      txt <- paste0(txt, "\nswR                :   ",
                    sprintf("%.5f", CV2se(CVwR)))
      txt <- paste0(txt, "\nExpanded limits    : ",
                    sprintf("%6.2f%% ... %.2f%%",
                            100*BE[1], 100*BE[2]), " [100exp(\u00B1",
                    sprintf("%.3f", reg_set$r_const), "\u00B7swR)]")
    }
    if (ret$design == "full") {
      txt <- paste0(txt, "\nswT / swR          :   ",
                    sprintf("%.4f", sw.ratio))
      if (sw.ratio >= 2/3 & sw.ratio <= 3/2) { # like in PBE/IBE
        txt <- paste0(txt, " (similar variabilities of T and R)")
      } else {
        ifelse (sw.ratio < 2/3,
          txt <- paste0(txt, " (T lower variability than R)"),
          txt <- paste0(txt, " (T higher variability than R)"))
      }
      txt <- paste0(txt, "\nsw-ratio (upper CL):   ",
                    sprintf("%.4f", sw.ratio.CI[["upper"]]))
      if (sw.ratio.CI[["upper"]] <= 2.5) { # like in the FDA's warfarin guidance
        txt <- paste0(txt, " (comparable variabilities of T and R)")
      } else {
        txt <- paste0(txt, " (T higher variability than R)")
      }
    }
    if (ola) {
      if (outlier) {
        if (ret$design == "full") sw.ratio.rec <- sqrt(msewT)/sqrt(msewR.rec)
        BE.rec <- as.numeric(scABEL(CV=CVwR.rec, regulator="EMA"))
        txt <- paste0(txt, "\n\nOutlier fence      :  ", fence,
                      "\u00D7IQR of studentized residuals.")
        txt1 <- paste0("\nRecalculation due to presence of ",
                       length(ol))
        ifelse (length(ol) == 1,
          txt1 <- paste0(txt1, " outlier (subj. "),
          txt1 <- paste0(txt1, " outliers (subj. "))
        txt1 <- paste0(txt1, paste0(ol, collapse="|"), ")")
        txt <- paste0(txt, txt1, "\n",
                      paste0(rep("\u2500", nchar(txt1)-1), collapse=""))
        txt <- paste0(txt,
                      "\nCVwR (outl. excl.) : ", sprintf("%6.2f%%", 100*CVwR.rec),
                      " (reference-scaling ")
        if (CVwR.rec > 0.3) {
          txt <- paste0(txt, "applicable)")
          txt <- paste0(txt, "\nswR (recalculated) :   ",
                        sprintf("%.5f", CV2se(CVwR.rec)))
          txt <- paste0(txt, "\nExpanded limits    : ",
                        sprintf("%6.2f%% ... %.2f%%",
                                100*BE.rec[1], 100*BE.rec[2]), " [100exp(\u00B1",
                        sprintf("%.3f", reg_set$r_const), "\u00B7swR)]")
        } else {
          txt <- paste0(txt, "not applicable)")
          txt <- paste0(txt, "\nUnscaled BE-limits :  80.00% ... 125.00%")
        }
        if (ret$design == "full") {
          txt <- paste0(txt, "\nswT / swR (recalc.):   ",
                        sprintf("%.4f", sw.ratio.rec))
          if (sw.ratio.rec >= 2/3 & sw.ratio.rec <= 3/2) { # like in PBE/IBE
            txt <- paste0(txt, " (similar variabilities of T and R)")
          } else {
            ifelse (sw.ratio.rec < 2/3,
              txt <- paste0(txt, " (T lower variability than R)"),
              txt <- paste0(txt, " (T higher variability than R)"))
          }
          txt <- paste0(txt, "\nsw-ratio (upper CL):   ",
                        sprintf("%.4f", sw.ratio.rec.CI[["upper"]]))
          if (sw.ratio.rec.CI[["upper"]] <= 2.5) { # like in the FDA's warfarin guidance
            txt <- paste0(txt, " (comparable variabilities of T and R)")
          } else {
            txt <- paste0(txt, " (T higher variability than R)")
          }
        }
      } else {
        txt <- paste0(txt, "\n\nOutlier fence      :  ", fence,
                      "\u00D7IQR of studentized residuals.")
        txt <- paste0(txt, "\nNo outlier detected.",
                      "\n", paste0(rep("\u2500", 49), collapse=""))
      }
    } # EO ola
  } else { # called from ABE()
    txt <- paste0(txt, "\nBE-limits          : ",
                  sprintf("%6.2f%% ... %.2f%%", 100*BE[1], 100*BE[2]))
  }
  ret$txt <- txt
  ret <- c(ret, CVswitch=ifelse(called.from != "ABE", reg_set$CVswitch, NA),
           CVcap=ifelse(called.from != "ABE", reg_set$CVcap, NA),
           r_const=ifelse(called.from != "ABE", reg_set$r_const, NA), BE=BE,
           CVwT=ifelse(ret$design == "full", CVwT, NA), CVwR=CVwR,
           sw.ratio=ifelse(called.from != "ABE" & ret$design == "full",
                           sw.ratio, NA),
           sw.ratio.upper=ifelse(called.from != "ABE" & ret$design == "full",
                           sw.ratio.CI[["upper"]], NA),
           ol=ifelse(called.from != "ABE" & outlier, list(ol.subj1), NA),
           CVwR.rec=ifelse(called.from != "ABE" & outlier, CVwR.rec, NA),
           sw.ratio.rec=ifelse(called.from != "ABE" & outlier &
                               ret$design == "full", sw.ratio.rec, NA),
           sw.ratio.rec.upper=ifelse(called.from != "ABE" & outlier &
                                     ret$design == "full",
                                     sw.ratio.rec.CI[["upper"]], NA),
           BE.rec=BE.rec)
  return(ret)
} # end of function CV.calc()
