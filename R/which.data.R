############################################
# return data.frame of currently available #
# data containg variables as required by   # 
# get.data(). Don't forget to update for   #
# new reference data sets!                 #
############################################
which.data <- function(data = NULL) {
  if (missing(data) | is.null(data)) stop()
  sets     <- 21
  descr    <- c("Data set I given by the EMA (Q&A document) available at
 http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf",
                "Data set II given by the EMA (Q&A document) available at http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf",
                "Modified Data set I given by the EMA (Q&A document): Period 3 removed.",
                "Cmax data of Table II from Patterson SD, Jones B. Viewpoint: observations on scaled average bioequivalence. Pharm Stat. 2012:11(1):1\u20137. doi:10.1002/pst.498",
                "Cmax data of the Appendix from Metzler CM, Shumaker RC. The Phenytoin Trial is a Case Study of \u2018Individual\u2019 Bioequivalence. Drug Inf J. 1998:32:1063\u201372.",
                "Modified Data set I given by the EMA (Q&A document): T and R switched.",
                "Data set simulated with CVwT = CVwR = 35%, GMR = 0.90.",
                "Data set simulated with CVwT = 70%, CVwR = 80%, CVbT = CVbR = 150%, GMR = 0.85.",
                "Data set with wide numeric range (based of ref08: data of last 37 subjects multiplied by 1,000,000).",
                "Table 9.3.3 (AUC) from: Chow SC, Liu JP. Design and Analysis of Bioavailability and Bioequivalence Studies. Boca Raton: CRC Press: 3rd edition 2009. p275.",
                "Table 9.6 (Cmax) from: Hauschke D, Steinijans VW, Pigeot I. Bioequivalence Studies in Drug Development. Chichester: John Wiley: 2007. p216. (Drug 17a of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip).",
                "Data set simulated with extreme intrasubject variability.",
                "Highly incomplete data set (based of rds08. Approx. 50% of period 4 data deleted).",
                "Data set simulated with high variability and dropouts as a hazard function growing with period.",
                "Highly incomplete data set (based of rds08. Approx. 50% of period 4 data are coded as missing '.').",
                "Drug 14a, Cmax data: MAO inhibitor- IR of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip.",
                "Highly unbalanced data set (based on rds03. 12 subjects in RTR and 7 in TRT).",
                "Highly incomplete data set (based on rds14. T data of subjects 63\u201378 removed).",
                "Highly incomplete data set (based on rds18. Data of subjects 63\u201378 removed).",
                "Highly incomplete data set (based on rds19. Outlier of R (subject 1) introduced: original value \u00D7100).",
                "Modified Data set I given by the EMA (Q&A document): one extreme result of subjects 45
& 52 set to NA.")
  checksum <- c("a182455ab26d616fb0b5b60e8f959bb7",
                "d45885ce457b6102af94f8c46258453e",
                "82aed880964a33f429344118308836c3",
                "84169b4867be4b96f53193c71c83dbb3",
                "755563f0a48cf038b1e19bed73a9c3d8",
                "07c175181252f52cf0b9bb0f81a5a087",
                "efcf5ea31ef71edbb53fd8b3c454ebb4",
                "081dedd6738a832c9ba5d81b5df85a36",
                "f2d2c8dc40826cbc0a571fa8b3e811ee",
                "8b3930ed7950698bba6db26f959f3365",
                "f486afad37e9617d62a346afbcad737f",
                "3feebae7751cdab52f2a40dce24d37b0",
                "2a9758eb24ebb8dfe819072a08abb8d0",
                "546a9079736beecab68918bfade8ebe0",
                "ba7f3e7c8ed1b8e8a0d24fd4837ffca2",
                "7a69398304230399467118cd3f975682",
                "82aed880964a33f429344118308836c3",
                "9d01442657b7fc858939cec25db42016",
                "0528e9cda37aba33df190c3217c9ec9a",
                "01535af145ece037bca496d00bddc6f7",
                "fc7047448c3aee4206be2824d9fcdd45")
  file     <- rep("DS", sets)
  set      <- sprintf("%02i", 1:sets)
  ref      <- paste0("rds", set)
  id       <- data.frame(file, set, ref, checksum, descr, stringsAsFactors = FALSE)
  return(id)
}
