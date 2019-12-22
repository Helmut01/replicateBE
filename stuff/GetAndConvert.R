library(digest)
path <- "E:/Public/Documents/BEBAC/R/replicateBE/inst/extdata/"
file  <- "DS"
set   <- "30"
ext   <- "csv"
sep   <- ","
dec   <- "."
data  <- read.csv(file=paste0(path, file, set, ".", ext), sep=sep,
                  dec=dec, quote="", strip.white=TRUE, comment.char="#",
                  na.strings=c("NA", "ND", ".", "", "Missing"))
facs <- which(!names(data) %in% c("PK", "logPK")) # will be factors later
names(data)[facs] <- tolower(names(data)[facs])
cols       <- c("subject", "period", "sequence", "treatment")
data[cols] <- lapply(data[cols], factor)
rds30 <- data
attr(rds30, "rset") <- "rd30"
#save(rds30, file="E:/Public/Documents/BEBAC/R/replicateBE/data/RTRT.TRTR.RTTR.TRRT.rda")
