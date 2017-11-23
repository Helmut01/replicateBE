###########################################
# return information of the internal data #
# containg variables as required by other #
# functions. Don't forget to update for   #
# new reference data sets!                #
############################################
info.data <- function(data = NULL) {
  if (missing(data) | is.null(data)) stop()
  sets     <- 28
  descr    <- c("Data set I given by the EMA (Q&A document) available at
 http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf",
                "Data set II given by the EMA (Q&A document) available at http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf",
                "Modified Data set I given by the EMA (Q&A document): Period\u00A03 removed.",
                "Cmax data of Table II from Patterson SD, Jones B. Viewpoint: observations on scaled average bioequivalence. Pharm Stat. 2012:11(1):1\u20137. doi:10.1002/pst.498",
                "Cmax data of the Appendix from Metzler CM, Shumaker RC. The Phenytoin Trial is a Case Study of \u2018Individual\u2019 Bioequivalence. Drug Inf J. 1998:32:1063\u201372.",
                "Modified Data set I given by the EMA (Q&A document): T and R switched.",
                "Data set simulated with CVwT\u00A0=\u00A0CVwR\u00A0=\u00A035%, GMR\u00A0=\u00A00.90.",
                "Data set simulated with CVwT\u00A0=\u00A070%, CVwR\u00A0=\u00A080%, CVbT\u00A0=\u00A0CVbR\u00A0=\u00A0150%, GMR\u00A0=\u00A00.85.",
                "Data set with wide numeric range (based of rds08: Data of last 37 subjects multiplied by 1,000,000).",
                "Table 9.3.3 (AUC) from: Chow SC, Liu JP. Design and Analysis of Bioavailability and Bioequivalence Studies. Boca Raton: CRC Press; 3rd edition 2009. p275.",
                "Table 9.6 (Cmax) from: Hauschke D, Steinijans VW, Pigeot I. Bioequivalence Studies in Drug Development. Chichester: John Wiley: 2007. p216. (Drug\u00A017a of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip).",
                "Data set simulated with extreme intra- and intersubject variability, GMR\u00A0= 1.6487.",
                "Highly incomplete data set (based of rds08: Approx. 50% of period\u00A04 data deleted).",
                "Data set simulated with extreme intra- and intersubject variability, GMR\u00A0= 1. Dropouts as a hazard function growing with period.",
                "Highly incomplete data set (based of rds08: Approx. 50% of period\u00A04 data are coded as missing '.').",
                "Drug 14a, Cmax data: MAO inhibitor\u00A0- IR of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip.",
                "Highly unbalanced data set (based on rds03: 12 subjects in RTR and 7 in TRT).",
                "Highly incomplete data set (based on rds14: T data of subjects 63\u201378 removed).",
                "Highly incomplete data set (based on rds18: Data of subjects 63\u201378 removed).",
                "Highly incomplete data set (based on rds19: Outlier of R (subject\u00A01) introduced: original value \u00D7100).",
                "Modified Data set I given by the EMA (Q&A document): One extreme result of subjects 45
& 52 set to NA.",
                "Data set simulated with CVwT\u00A0= CVwR\u00A0=\u00A045%, CVbT\u00A0= CVbR\u00A0=\u00A0100%, GMR\u00A0=\u00A00.90.",
                "Drug 7a, Cmax data: Beta-adrenergic blocking agent\u00A0- IR of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip.",
                "Drug 1, Cmax data: Antianxiety agent\u00A0- IR of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip.",
                "Data set simulated with CVwT\u00A0=\u00A050%, CVwR\u00A0=\u00A080%, CVbT\u00A0=\u00A0CVbR\u00A0=\u00A0130%, GMR\u00A0=\u00A00.90.",
                "Example 4.4 (Cmax) from: Patterson SD, Jones B. Bioequivalence and Statistics in Clinical Pharmacology. Boca Raton: CRC Press; 2nd edition 2016. p105\u20136.",
                "Data set simulated with CVwT\u00A0= CVwR\u00A0=\u00A035%, CVbT = CVbR\u00A0=\u00A075%, GMR\u00A0=\u00A00.90.",
                "Data set simulated with CVwT\u00A0= CVwR\u00A0=\u00A035%, CVbT = CVbR\u00A0=\u00A075%, GMR\u00A0=\u00A00.90.")
  file     <- rep("DS", sets)
  set      <- sprintf("%02i", 1:sets)
  ref      <- paste0("rds", set)
  id       <- data.frame(file, set, ref, descr, stringsAsFactors = FALSE)
  act      <- attr(data, "rset")
  if (!act %in% ref) {
    info <- NULL
  } else {
    info <- id[as.integer(substr(act, 4, 5)), ]
  }
  return(info)
}
