####################################################
# Calculate CV according to the EMA's Q&A-document #
####################################################
CV.calc <- function(alpha = 0.05, path.in, path.out, file, set, ext,
                    header, na, sep = ",", dec =".", logtrans = TRUE,
                    ola = FALSE, details = FALSE, adjust = FALSE,
                    print, verbose = FALSE, ask = FALSE,
                    plot.bxp = FALSE, data) {
  if (missing(path.in)) path.in <- NULL
  if (missing(data)) data <- NULL
  called.from <- as.character(sys.call(-1))[1]
  if (is.na(called.from)) called.from <- "internally"
  ret   <- get.data(path.in=path.in, path.out=path.out, file=file,
                    set=set, ext=ext, header=header, na=na, sep=sep,
                    dec=dec, logtrans=logtrans, print=print,
                    plot.bxp=plot.bxp, data=data)
  ow    <- options() # save options
  options(digits=12) # dealing with anova(): increase digits!
  if (logtrans) {    # use the raw data and log-transform internally
    modCVR <- lm(log(PK) ~ sequence + subject%in%sequence + period,
                           data=ret$ref)
  } else {           # use the already log-transformed data
    modCVR <- lm(logPK ~ sequence + subject%in%sequence + period,
                         data=ret$ref)
  }
  msewR <- anova(modCVR)["Residuals", "Mean Sq"]
  CVwR  <- mse2CV(msewR)
  outlier <- FALSE
  BE.new  <- rep(NA, 2)
  if (ola) { # check for outliers
    stud.res  <- rstudent(modCVR)  # studentized (SAS)
    stud.res  <- stud.res[!is.na(stud.res)] # get rid of NAs and zeros
    stud.res  <- stud.res[which(stud.res != 0)]
    stud.res  <- stud.res[c(TRUE, FALSE)] # need only the 1st occasions
    stand.res <- rstandard(modCVR) # standardized (SAS, PHX/WNL)
    stand.res <- stand.res[!is.na(stand.res)]
    stand.res <- stand.res[which(stand.res != 0)]
    stand.res <- stand.res[c(TRUE, FALSE)] # need only the 1st occasions
    bp1       <- boxplot(stud.res, range=3, plot=FALSE)
    bp2       <- boxplot(stand.res, range=3, plot=FALSE)
    names.ol1 <- names(bp1$out)
    names.ol2 <- names(bp2$out)
    if (length(names.ol1 > 0)) { # at least one found
      outlier <- TRUE
      # boxplot
      ol.value1 <- as.numeric(bp1$out)
      ol.seq1   <- as.character(ret$ref[names(bp1$out), "sequence"])
      ol.subj1  <- as.character(ret$ref[names(bp1$out), "subject"])
      ol.value2 <- as.numeric(bp2$out)
      ol.seq2   <- as.character(ret$ref[names(bp2$out), "sequence"])
      ol.subj2  <- as.character(ret$ref[names(bp2$out), "subject"])
      pars <- list(boxwex=0.5, boxfill="lightblue", medcol="blue",
                   outpch=21, outcex=1.35, outcol="red", outbg="#FFCCCC")
      overwrite <- TRUE # default
      if (plot.bxp) {   # save in PNG format to path.out
        if (ask & file.exists(ret$png.path)) {
          answer <- tolower(readline("Boxplot already exists. Overwrite the PNG  [y|n]? "))
          if(answer != "y") overwrite <- FALSE
        }
        if (overwrite) { # either the file does not exist or should be overwritten
          png(ret$png.path, width=720, height=720, pointsize=18)
        }
      }
      bxp(bp1, xlim=c(0, 3), ylim=c(-1, 1)*max(abs(ol.value1)), las=1, cex.main=1,
          main=paste0("EMA\u2019s model for CVwR:",
                      "\nlog(response) ~ sequence + subject(sequence) + period;",
                      " data = R"), ylab="residual", pars=pars)
      mtext("studentized\n(R, SAS)", 1, line=2.25, at=1)
      text(rep(1.1, 2), bp1$stats[c(1, 5)], adj=c(0, 0.25), cex=0.8,
           sprintf("%+.3f", bp1$stats[c(1, 5)]))
      text(rep(0.9, length(ol.value1)), ol.value1, adj=c(1, 0.25), cex=0.8,
           paste0("# ", ol.subj1, " (", ol.seq1, ")"))
      text(rep(1.1, length(ol.value1)), ol.value1, adj=c(0, 0.25), cex=0.8,
           sprintf("%+.3f", ol.value1))
      bxp(bp2, axes=FALSE, at=2, add=TRUE, pars=pars)
      mtext("standardized\n(R, SAS, Phoenix WinNonlin)", 1, line=2.25, at=2)
      text(rep(2.1, 2), bp2$stats[c(1, 5)], adj=c(0, 0.25), cex=0.8,
           sprintf("%+.3f", bp2$stats[c(1, 5)]))
      text(rep(1.9, length(ol.value2)), ol.value2, adj=c(1, 0.25), cex=0.8,
           paste0("# ", ol.subj2, " (", ol.seq2, ")"))
      text(rep(2.1, length(ol.value2)), ol.value2, adj=c(0, 0.25), cex=0.8,
           sprintf("%+.3f", ol.value2))
      abline(h=0, lty="dotted")
      if (plot.bxp & overwrite) dev.off() # close PNG
      # prep data and calculate new CVwR
      ol   <- ret$ref[names(bp1$out), "subject"]
      excl <- ret$ref[ret$ref$subject != ol, ]
      if (logtrans) { # use the raw data and log-transform internally
        modCVR1 <- lm(log(PK) ~ sequence + subject%in%sequence + period,
                                data=excl)
      } else {        # use the already log-transformed data
        modCVR1 <- lm(logPK ~ sequence + subject%in%sequence + period,
                              data=excl)
      }
      msewR1 <- anova(modCVR1)["Residuals", "Mean Sq"]
      CVwR.new  <- mse2CV(msewR1)
      if (verbose) {
        stud.res.whiskers <- signif(range(bp1$stats[, 1]), 7)
        stud.res.outliers <- data.frame(ol.subj1, ol.seq1, signif(ol.value1, 7))#
        names(stud.res.outliers) <- c("subject", "sequence", "stud.res")
        stand.res.whiskers <- signif(range(bp2$stats[, 1]), 7)
        stand.res.outliers <- data.frame(ol.subj2, ol.seq2, signif(ol.value2, 7))#
        names(stand.res.outliers) <- c("subject", "sequence", "stand.res")
        cat(paste0("\nOutlier analysis\n Studentized residuals",
                   "\n Limits (3IQR whiskers): ",
                   stud.res.whiskers[1], ", ", stud.res.whiskers[2],
                   "\n Outliers:\n")); print(stud.res.outliers, row.names=FALSE)
        cat(paste0("\n Standarized residuals\n Limits (3IQR whiskers): ",
                   stand.res.whiskers[1], ", ", stand.res.whiskers[2],
                   "\n Outliers:\n")); print(stand.res.outliers, row.names=FALSE)
      }
    } else { # none found
      CVwR.new <- ol.subj1 <- ol <- NA
      BE.new   <- rep(NA, 2)
    }
  } # end of outlier analysis (if requested, i.e., ola=TRUE)
  CVwT  <- NA
  if (!ret$type %in% c("RRT|RTR|TRR", "RTR|TRR")) { # only for full replicates
    if (logtrans) { # use the raw data and log-transform internally
      modCVT <- lm(log(PK) ~ subject + period, data=ret$test)
    } else {        # use the already log-transformed data
      modCVT <- lm(logPK ~ subject + period, data=ret$test)
    }
    msewT <- anova(modCVT)["Residuals", "Mean Sq"]
    CVwT  <- mse2CV(msewT) # nice to know
  }
  options(ow) # restore options
  reg_set <- reg_const("EMA")
  BE      <- as.numeric(scABEL(CV=CVwR, regulator="EMA"))
  txt <- ret$txt
  if (called.from != "ABE") { # only for scaling
    txt <- paste0(ret$txt, "\nSwitching CV       : ",
                  sprintf("%6.2f%%", 100*reg_set$CVswitch),
                  "\nScaling cap        : ",
                  sprintf("%6.2f%%", 100*reg_set$CVcap),
                  "\nRegulatory constant: ",
                  sprintf("%.3f", reg_set$r_const),
                  "\nGMR restriction    :  80.00% ... 125.00%")
  }
  if (!is.na(CVwT)) txt <- paste0(txt, "\nCVwT               : ",
                                  sprintf("%6.2f%%", 100*CVwT))
  txt <- paste0(txt,
                "\nCVwR               : ", sprintf("%6.2f%%", 100*CVwR))
  if (called.from != "ABE") { # only for scaling
    txt <- paste0(txt, " (reference-scaling ")
    if (CVwR <= 0.3) txt <- paste0(txt, "not ")
    txt <- paste0(txt, "applicable)")
    if (CVwR <= 0.3) {
      txt <- paste0(txt, "\nUnscaled BE-limits : ")
    } else {
      txt <- paste0(txt, "\nExpanded limits    : ")
    }
    txt <- paste0(txt, sprintf("%6.2f%% ... %.2f%%", 100*BE[1], 100*BE[2]))
    if (ola) {
      if (outlier) {
        BE.new <- as.numeric(scABEL(CV=CVwR.new, regulator="EMA"))
        txt1 <- paste0("\nRecalculation due to presence of ",
                      length(ol))
        if (length(ol) == 1) {
          txt1 <- paste0(txt1, " outlier (subj. ")
        } else {
          txt1 <- paste0(txt1, " outliers (subj. ")
        }
        txt1 <- paste0(txt1, paste0(ol, collapse="|"), ")")
        txt <- paste0(txt, txt1, "\n", paste0(rep("\u2500", nchar(txt1)-1), collapse=""))
        txt <- paste0(txt,
                    "\nCVwR (outl. excl.) : ", sprintf("%6.2f%%", 100*CVwR.new),
                    " (reference-scaling ")
        if (CVwR.new > 0.3) {
          txt <- paste0(txt, "applicable)")
        } else {
          txt <- paste0(txt, "not applicable)")
        }
        if (CVwR.new <= 0.3) {
          txt <- paste0(txt, "\nUnscaled BE-limits : ")
        } else {
          txt <- paste0(txt, "\nExpanded limits    : ")
        }
        txt <- paste0(txt, sprintf("%6.2f%% ... %.2f%%", 100*BE.new[1],
                      100*BE.new[2]))
      } else {
        txt <- paste0(txt, "\nNo outlier in studentized residuals of R detected",
                      "\n", paste0(rep("\u2500", 49), collapse=""))
      }
    } # end of ola
  } else { # specific for ABE
    txt <- paste0(txt, "\nBE-limits          : ")
    txt <- paste0(txt, sprintf("%6.2f%% ... %.2f%%", 80, 125))
  }
  ret$txt <- txt
  ret <- c(ret, CVswitch=reg_set$CVswitch, CVcap=reg_set$CVcap,
           r_const=reg_set$r_const, BE=BE, CVwT=CVwT, CVwR=CVwR,
           ol=ifelse(outlier, list(ol.subj1), NA),
           CVwR.new=ifelse(outlier, CVwR.new, NA), BE.new=BE.new)
  return(ret)
} # end of function CV.calc()
