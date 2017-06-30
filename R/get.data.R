#################################
# Get the data from the file or #
# internal data and generate    #
# output common to all methods. #
#################################
get.data <- function(path.in = NULL, path.out = NULL, file, set, ext,
                     header = 0, na = ".", sep = ",", dec=".",
                     logtrans=TRUE, print, plot.bxp, data) {
  graphics.off()
  if (is.null(data)) { # check only external data
    if (is.null(path.in) | missing(path.in)) {
      setwd("~/")
      cat("No path for input given; home folder", getwd(), "used.\n")
      path.in <- getwd()
    }
    if (!dir.exists(path.in)) {
      setwd("~/")
      cat("Path for input does not exist; home folder", getwd(), "used.\n")
      path.in <- getwd()
    } # Adds trailing '/' to path if missing
    path.in <- ifelse(regmatches(path.in, regexpr(".$", path.in)) == "/",
                      path.in, paste0(path.in, "/"))
  }
  if (print | plot.bxp) { # check only if necessary
    if (missing(path.out)) {
      setwd("~/")
      cat("No path for output given; output to", getwd(), "\n")
      path.out <- getwd()
    }
    if (is.null(path.out)) {
      setwd("~/")
      cat("No path for output given; output to", getwd(), "\n")
      path.out <- getwd()
    }
    if (!dir.exists(path.out)) {
      setwd("~/")
      cat("Path for output does not exist; output to", getwd(), "\n")
      path.out <- getwd()
    } # Adds trailing '/' to path if missing
    path.out <- ifelse(regmatches(path.out, regexpr(".$", path.out)) == "/",
                       path.out, paste0(path.out, "/"))
  }
  if (is.null(data)) { # read external data
    if (missing(file)) stop("Name of file must be given.")
    num.file <- paste0("File must be a string",
                       "\n(i.e., the name enclosed in single or double quotes)")
    if (is.numeric(file)) stop(num.file)
    num.set <- paste0("Set must be a string",
                      "\n(i.e., the set enclosed in single or double quotes)")
    if (is.numeric(set)) stop(num.set)
    if (missing(ext)) stop("Extension of file must be given.")
    if (!ext %in% c("CSV", "csv", "XLS", "xls", "XLXS", "xlsx"))
      stop("Data format not supported.")
    if (ext %in% c("CSV", "csv")) {
      if (!sep %in% c(";", ",", "\t"))
        stop(paste0("Reading CSV-file",
            "\nVariable separator must be any of",
            "\n\';\' (semicolon), \',\' (comma), or \'\\t\' (tab)."))
      if (!dec %in% c(".", ","))
        stop(paste0("Reading CSV-file",
             "\nDecimal separator must be \'.\' (period) or \',\' (comma)."))
      if (sep == dec)
        stop(paste0("Reading CSV-file",
             "\nVariable and decimal separators must be different."))
    }
    if (ext %in% c("XLS", "xls", "XLXS", "xlsx") & missing(set))
      stop("Name of sheet to read must be given.")
    if (ext %in% c("XLS", "xls", "XLSX", "xlsx")) { # read from Excel
      full.name <- paste0(path.in, file, ".", ext)
      if (!file.exists(full.name)) {
        setwd(dirname(file.choose()))
        path.in <- getwd()
        full.name <- paste0(path.in, "/", file, set, ".", ext)
      }
      # The entire content
      descr <- read_excel(path=full.name, sheet=set, skip=0)
      # Only the header
      descr <- names(head(descr, 1)[1])
      # Only the data (skip the header)
      data  <- read_excel(path=full.name, sheet=set,
                          na=c("NA", "ND", ".", "", "Missing"), skip=header)
      data  <- as.data.frame(data) # convert the tibble
      # Convert eventual mixed or upper case variable names to lower case
      facs  <- which(!names(data) %in% c("PK", "logPK")) # will be factors later
      names(data)[facs] <- tolower(names(data)[facs])
      if (sum(names(data)[facs] %in% c("subject", "period", "sequence", "treatment")) !=4)
        stop("Variables must be given as \'subject\', \'period\', \'sequence\', \'treatment\'.")
    } else { # read from CSV
      full.name <- paste0(path.in, file, set, ".", ext)
      if (!file.exists(full.name)) {
        setwd(dirname(file.choose()))
        path.in <- getwd()
        full.name <- paste0(path.in, "/", file, set, ".", ext)
      }
      # The entire content
      descr <- scan(file=full.name, quiet=TRUE, what="character",
                    sep="", strip.white=TRUE,
                    na.strings=c("NA", "ND", ".", "", "Missing"))
      # Only the header
      descr <- descr[1:(which(tolower(substr(descr, 1, 7)) == "subject") - 1)]
      descr <- descr[!descr %in% "#"]
      descr <- paste0(descr, collapse=" ")
      # Only the data (skip the header)
      data  <- read.csv(file=full.name, sep=sep, dec=dec, quote="",
                        strip.white=TRUE, comment.char="#",
                        na.strings=c("NA", "ND", ".", "", "Missing"))
      # Convert eventual mixed or upper case variable names to lower case
      facs <- which(!names(data) %in% c("PK", "logPK")) # will be factors later
      names(data)[facs] <- tolower(names(data)[facs])
      if (sum(names(data)[facs] %in% c("subject", "period", "sequence", "treatment")) !=4)
        stop("Variables must be given as \'subject\', \'period\', \'sequence\', \'treatment\'.")
    } # end of reading file
    if (print) res.file <- paste0(path.out, file, set, "_ABEL")
    if (plot.bxp) png.path <- paste0(path.out, file, set, "_boxplot.png")
    # If the user erroneously asks for analysis of logPK - which does not
    # exist in the data set - change to internal log-transformation.
    if (logtrans == FALSE & !"logPK" %in% colnames(data)) {
      warn.txt <- paste0("Column \'logPK\' does not exist in the data set.",
                         "\nLog-transformed column \'PK'\ internally.")
      warning(warn.txt)
      logtrans <- TRUE
    } # factorize variables except response(s)
    cols       <- c("subject", "period", "sequence", "treatment")
    data[cols] <- lapply(data[cols], factor)
    if (sum(!unique(data$treatment) %in% c("R", "T")) !=0)
      stop("treatments must be given as \'R\' and \'T\'.")
  } # end of reading external data
  # generate variables based on the attribute
  # 2nd condition: Otherwise, the header from a CSV file will be overwritten
  if (!is.null(data) & missing(ext)) {
    info  <- info.data(data)
    file  <- info$file
    set   <- info$set
    ref   <- info$ref
    descr <- info$descr
    if (print) res.file <- paste0(path.out, file, set, "_ABEL")
    if (plot.bxp) png.path <- paste0(path.out, "/", file, set, "_boxplot.png")
  }
  subjs <- unique(data$subject)
  seqs  <- levels(unique(data$sequence))   # Sequences
  Nseqs <- length(seqs)                    # Number of sequences
  # recode sequences if necessary
  if (Nseqs == 2) {
    if (sum(seqs %in% c("RTRT", "TRTR")) == Nseqs)
      seqs <- c("TRTR", "RTRT")
    if (sum(seqs %in% c("RTTR", "TRRT")) == Nseqs)
      seqs <- c("TRRT", "TRRT")
    if (sum(seqs %in% c("RTR", "TRT")) == Nseqs)
      seqs <- c("TRT", "RTR")
    if (sums(seqs %in% c("RTT", "TRR")) == Nseqs)
      seqs <- c("TRR", "RTT")
    if (sum(seqs %in% c("RTR", "TRR")) == Nseqs)
      seqs <- c("TRR", "RTR")
  }
  if (Nseqs == 3) {
    if (sume(seqs == c("RRT", "RTR", "TRR")) == Nseqs)
      seqs <- c("TRR", "RTR", "RRT")
  }
  if (Nseqs == 4) {
    if (sum(seqs %in% c("RTRT", "RTTR", "TRRT", "TRTR")) == Nseqs)
      seqs <- c("TRTR", "RTRT", "TRRT", "RTTR")
  }
  type  <- paste0(seqs, collapse="|")
  pers  <- unique(as.integer(data$period)) # Periods
  Npers <- length(pers)                    # Number of periods
  if (nchar(type) == 19) {  # 4-period 4-sequence full replicate designs
    if (Npers != 4) stop("4 periods required in this full replicate design.")
    if (Nseqs != 4) stop("4 sequences required in this full replicate design.")
  }
  if (nchar(type) == 9) {  # 4-period full replicate designs
    if (Npers != 4) stop("4 periods required in this full replicate design.")
    if (Nseqs != 2) stop("2 sequences required in this full replicate design.")
  }
  if (nchar(type) == 7) {  # 3-period replicates
    if (type %in% c("TRT|RTR", "TRR|RTT")) {
      if (Npers != 3) stop("3 periods required in this full replicate design.")
      if (Nseqs != 2) stop("2 sequences required in this full replicate design.")
    }
    if (type == "TRR|RTR") {
      if (Npers != 3) stop("3 periods required in the extra-reference design.")
      if (Nseqs != 2) stop("2 sequences required in the extra-reference design.")
    }
  }
  if (nchar(type) == 11) { # 3-sequence partial replicate
    if (Npers != 3) stop("3 periods required in this partial replicate design.")
    if (Nseqs != 3) stop("3 sequences required in this partial replicate design.")
  }
  Nsub.seq <- table(data$sequence[!duplicated(data$subject)])
  ref   <- data[data$treatment == "R", ]
  test  <- data[data$treatment == "T", ]
  NTR   <- sum(levels(test$subject) %in% levels(ref$subject)) # >= 1 T & >= 1 R
  n     <- length(unique(data$subject))
  # Get the wide data frame subject+sequence\period
  compl <- reshape(data[c("subject", "sequence", "period", "PK")],
                   idvar=c("subject", "sequence"), timevar=c("period"),
                   direction="wide")
  uncompletedata <- compl[!complete.cases(compl), ] # exclude complete
  if (nrow(uncompletedata)) {
    # Select NAs in period columns
    Miss.per <- data.frame(sapply(compl, function(y) sum(is.na(y))))
    Miss.per <- t(Miss.per[!rownames(Miss.per) %in% c("subject", "sequence"), ])
    colnames(Miss.per) <- paste0("PK.", 1:ncol(Miss.per))
    # Reformat uncomplete
    uncompletedeshaped <- reshape(uncompletedata, direction="long")
    # Exclude periods with data
    deshapeduncomplete <- uncompletedeshaped[!complete.cases(uncompletedeshaped), ]
    Miss.seq <- table(deshapeduncomplete$sequence)
  } else {
    Miss.seq <- rep(0, Nseqs)
    Miss.per <- rep(0, Npers)
  }
  # Data of subjects with two R treatments
  RR   <- ref[duplicated(ref$subject, fromLast=TRUE)|
              duplicated(ref$subject, fromLast=FALSE), ]
  RR   <- RR[!is.na(RR$PK), ]        # exclude NAs
  nRR  <- length(unique(RR$subject)) # number of subjects
  nTT  <- NA
  if (type %in% c("TRR|RTR|RRT", "TRR|RTR", "TRTR|RTRT|TRRT|RTTR")) { # only full replicates
    # Data of subjects with two T treatments
    TT  <- test[duplicated(test$subject, fromLast=TRUE)|
                duplicated(test$subject, fromLast=FALSE), ]
    TT  <- TT[!is.na(TT$PK), ]        # exclude NAs
    nTT <- length(unique(TT$subject)) # number of subjects
  }
  txt <- "" # paranoia
  if (!is.na(descr) & !is.null(descr) & length(descr) >= 1)
    txt <- paste0(strwrap(descr, width = 78), collapse="\n")
  if (logtrans) {
    txt <- paste0(txt,
                  "\nAnalysis performed on column \u2018PK\u2019 ",
                  "(data internally log-transformed)")
  } else {
    txt <- paste0(txt,
                  "\nAnalysis performed on column \u2018logPK\u2019 ",
                  "(data already log-transformed)")
  }
  txt <- paste0(txt, "\nSequences (design) : ", type)
  if (nchar(type) == 19) txt <- paste(txt, "(4-period 4-sequence full replicate)")
  if (nchar(type) == 9) txt <- paste(txt, "(4-period full replicate)")
  if (type %in% c("TRT|RTR", "TRR|RTT")) txt <- paste(txt, "(3-period full replicate)")
  if (nchar(type) == 11) txt <- paste(txt, "(partial replicate)")
  if (type == "TRR|RTR") txt <- paste(txt, "(partial replicate; extra-reference)")
  x <- paste0(Nsub.seq, collapse = "|")
  if (sum(Miss.seq) > 0) x <- c(x, paste0(Miss.seq, collapse = "|"))
  if (sum(Miss.per) > 0) x <- c(x, paste0(Miss.per, collapse = "|"))
  x.len <- nchar(x)
  x.max <- max(x.len)
  txt <- paste0(txt, "\nSubjects / sequence: ", x[1])
  if (length(unique(Nsub.seq)) == 1) {
    txt <- paste0(txt, paste0(rep(" ", x.max-x.len[1]+1), collapse=""),
                  "(balanced)")
  } else {
    txt <- paste0(txt, paste0(rep(" ", x.max-x.len[1]+1), collapse=""),
                  "(unbalanced)")
  }
  if (sum(Miss.seq) > 0) {
    txt <- paste0(txt, "\nMissings / sequence: ", paste0(x[2],
                  paste0(rep(" ", x.max-x.len[2]+1), collapse=""),
                  "(incomplete)"))
  }
  if (sum(Miss.per) > 0) {
    txt <- paste0(txt, "\nMissings / period  : ", paste0(x[3],
                  paste0(rep(" ", x.max-x.len[3]+1), collapse=""),
                  "(incomplete)"))
  }
  txt <- paste0(txt,
                "\nSubjects (total)   : ", sprintf("%3i", n),
                "\nSub\u2019s with T and R : ", sprintf("%3i", NTR),
                " (calculation of the CI)")
  if (NTR < 12) {
    txt <- paste0(txt, "\n                     ",
                  "Less than 12 as required acc. to the BE-GL.")
  }
  if (nchar(type) != 11 & type != "TRR|RTR") # full replicates only
    txt <- paste0(txt, "\nSub\u2019s with two Ts  : ", sprintf("%3i", nTT))
  txt <- paste0(txt, "\nSub\u2019s with two Rs  : ", sprintf("%3i", nRR))
  if ((type == "TRT|RTR" | type == "TRR|RTT") & nRR < 12) {
    txt <- paste(txt, "(uncertain CVwR acc. to Q&A Rev. 12)")
  }
  ret <- list(data=data, ref=ref, RR=RR, test=test, type=type, n=n,
              nTT=nTT, nRR=nRR, txt=txt, Sub.Seq=Nsub.seq,
              Miss.seq=Miss.seq, Miss.per=Miss.per, transf=logtrans,
              res.file=ifelse(print, res.file, NA),
              png.path=ifelse(plot.bxp, png.path, NA))
  return(ret)
} # end of function get.data()
