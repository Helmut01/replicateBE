\encoding{utf-8}
\name{method.B}
\alias{method.B}
\title{
Comparative BA-calculation for the EMA's Average Bioequivalence with Expanding Limits (ABEL), Method B
}
\description{
This function performs the required calculations for the aggregate \acronym{BE} decision
via Average Bioequivalence with Expanding Limits (\acronym{ABEL}) based
on a linear mixed effects model with subjects as a random effect (\sQuote{Method B}) as
recommended in the \acronym{EMA}’s Q&A-document.
}
\usage{
method.B(alpha = 0.05, path.in = NULL, path.out = NULL, file, set = "",
         ext, na = ".", sep = ",", dec = ".", logtrans = TRUE,
         ola = FALSE, print = TRUE, option = 2, details = FALSE,
         verbose = FALSE, ask = FALSE, plot.bxp = FALSE, fence = 2,
         data = NULL)
}
\arguments{
  \item{alpha}{Type I Error (\acronym{TIE}) probability (nominal level of the test). Per convention commonly set to 0.05 which results in a \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2
\eqn{\alpha}{alpha})} confidence interval.
}
  \item{path.in}{
Path to the data file for import. The path \emph{must} be given with forward slashes (/) -- even on Windows (where backward slashes (\\) are the standard. If missing or not existing the user’s home folder \code{"~/"} will be used.
}
  \item{path.out}{
Path to safe the result file if \code{print=TRUE}. The path \emph{must} be given with forward slashes (/) -- even on Windows (where backward slashes (\\) are the standard. The user must have write-permission to the directory. If missing or not existing the user’s home folder \code{"~/"} will be used.
}
\item{file}{
Name of the data set for import (\emph{without} extension). Must be a string (\emph{i.e.}, enclosed in single or double quotation marks).\cr
If reading from a \acronym{CSV}-file a commentary header is allowed. Commentary line(s) must start with the hash character \code{"#"}.
}
  \item{set}{
Name of the sheet of an Excel-file (mandatory). Must be a string (\emph{i.e.}, enclosed in single or double quotation marks).
}
\item{ext}{
File-extension enclosed in single or double quotation marks. Acceptable are \code{"CSV"} and \code{"csv"} for character-separated variables (\acronym{CSV}) or \code{"XLS"}, \code{"xls"}, \code{"XLSX"}, and \code{"xlsx"} for Excel-files.
}
  \item{na}{
Character string denoting missing values. Acceptable are \code{"NA"} (not available),
\code{"ND"} (not determined), \code{"."} (\acronym{SAS}), \code{"Missing"} (Phoenix WinNonlin), and \code{""} (\acronym{Excel}; empty cell). Missings will be converted to \code{NA} in the imported data. Defaults to \code{"."}.
}
  \item{sep}{
Variable separator in the \acronym{CSV}-file. Acceptable are \code{","} (colon -- \code{ASCII 44}), \code{";"} (semicolon -- \code{ASCII 59}), and \code{"\\t"} (tabulator -- \code{ASCII 9}). Defaults to \code{","}.
}
  \item{dec}{
Decimal separator in the \acronym{CSV}-file. Acceptable are \code{"."} (period -- \code{ASCII 46}) or \code{","} (colon -- \code{ASCII 44}). Defaults to \code{"."}.
}
  \item{logtrans}{
If \code{TRUE} (default) the raw data (provided in column \code{PK}) will be
internally log-transformed and used in the calculations. If \code{FALSE} the already log-transformed data (provided in the column \code{log-PK}) will be used in the calculations.
}
\item{ola}{
Defaults to \code{FALSE}. If \code{TRUE} an outlier analysis based on the studentized and standardized residuals of the model estimating \code{CVwR} is performed.
}
  \item{print}{
If \code{TRUE} (default), the function prints its results to a file. If \code{FALSE}, returns a data.frame of results.
}
  \item{option}{
If \code{2} (default), the model will be evaluated by \code{lme()} from package \code{nlme}. The degrees of freedom of the treatment comparison will be equivalent to \acronym{SAS}’ \code{PROC MIXED DDFM=CONTAIN} and Phoenix WinNonlin’s \code{Residual}.\cr
If \code{1}, the model will be evaluated by \code{lmer()} from package \code{lmerTest}. The degrees of freedom of the treatment comparison will be equivalent to \acronym{SAS}’ \code{PROC MIXED DDFM=SATTERTHWAITE} and Phoenix WinNonlin’s \code{Satterthwaite}.\cr
}
  \item{details}{
Defaults to \code{FALSE}. If \code{TRUE}, the function sends its results in 7-digits precision to a data.frame.
}
  \item{verbose}{
Defaults to \code{FALSE}. If \code{TRUE} the ANOVA-table is send to the console. If \code{ola=TRUE} additional information about outliers are shown.
}
  \item{ask}{
Defaults to \code{FALSE}. If \code{TRUE} the user will be asked whether an already existing result file (and if outliers are found, the boxplot) should be overwritten.
}
  \item{plot.bxp}{
Only observed if \code{ola = TRUE} and at least one outlier is found. If \code{FALSE} (default) the boxplot will be shown in the graphics device. If \code{TRUE} the boxplot will be saved in \acronym{PNG} format to \code{path.out}.
}
  \item{fence}{
Only observed if \code{ola = TRUE}. The limit for outlier detection as a multiplier of the interquartile range. Defaults to 2. Less outliers will be detected with higher values. Not recommended.
}
  \item{data}{
One of internal reference data sets (\code{rds01} -- \code{rds23}). If given, \code{path.in}, \code{file}, \code{set}, and \code{ext} are ignored. For its use see the examples.\cr
Defaults to \code{NULL} (\emph{i.e.}, import external data).
}
}
\details{
The model for the estimation of \code{CVwR} is\cr
\verb{   log(response) ~ sequence + subject(sequence) + period}\cr
where all effects are fixed.\cr\cr
The model for the treatment comparison is\cr
\verb{   log(response) ~ sequence + subject(sequence) + period + treatment}\cr
where \code{sequence}, \code{period}, and \code{treatment} are fixed effects and \code{subject(sequence)} is a random effect.\cr\cr
Tested designs:
  \itemize{
    \item 4-period 2-sequence full replicates:\cr
      \code{TRTR | RTRT}\cr
      \code{TRRT | RTTR}
    \item 4-period 4-sequence full replicates:\cr
      \code{TRTR | RTRT | TRRT | RTTR}\cr
      \code{TRRT | RTTR | TTRR | RRTT}
    \item 3-period 2-sequence full replicates:\cr
      \code{TRT | RTR}\cr
      \code{TRR | RTT}
    \item 3-period (partial) replicates:\cr
      \code{TRR | RTR | RRT}\cr
      \code{TRR | RTR } (extra-reference design)
  }
Data structure:
  \itemize{
    \item Columns must have the headers \code{subject}, \code{period}, \code{sequence}, \code{treatment}, \code{PK}, and/or \code{logPK}.\cr
    Any order of columns is acceptable.\cr
    Uppercase and mixed case headers will be internally converted to lowercase headers.
    \itemize{
      \item \code{subject} must be integer numbers or (any combination of) alphanumerics \code{[A-Z, a-z, -, _, #, 0-9]}
      \item \code{period} must be integer numbers.
      \item \code{sequence} must be contained in the tested designs (numbers or \emph{e.g.}, \code{ABAB} are not acceptable).
      \item The Test treatment must be coded \code{T} and the Reference \code{R}.
    }
  }
}
\value{
Prints results to a file if argument \code{print = TRUE} (default).\cr
If argument \code{print = FALSE}, returns a data.frame with the elements:
  \tabular{ll}{
    \code{Design} \tab \emph{e.g.}, TRTR|RTRT\cr
    \code{Method} \tab B-\code{option} (\code{1} or \code{2})\cr
    \code{n} \tab total number of subjects\cr
    \code{nTT} \tab number of subjects with two treatments of \code{T} (full replicates only)\cr
    \code{nRR} \tab number of subjects with two treatments of \code{R}\cr
    \code{Sub/seq} \tab number of subjects per sequence\cr
    \code{Miss/seq} \tab if the design is unbalanced, number of missings per sequence\cr
    \code{Miss/per} \tab if the design is incomplete, number of missings per period\cr
    \code{alpha} \tab nominal level of the test\cr
    \code{DF} \tab degrees of freedom of the treatment comparison\cr
    \code{CVwT(\%)} \tab intra-subject coefficient of variation of the test treatment (full replicates only)\cr
    \code{CVwR(\%)} \tab intra-subject coefficient of variation of the reference treatment\cr
    \code{sw.ratio} \tab ratio of intrasubject variabilities of \code{T} and \code{R} (full replicates only)\cr
    \code{sw.ratio.CL} \tab upper confidence limit of \code{sw.ratio} (full replicates only)
  }
\itemize{
  \item If reference-scaling is applicable (\emph{i.e.}, \code{CVwR(\%)} >30\%):
    \tabular{ll}{
      \code{EL.lo(\%)} \tab lower expanded limit\cr
      \code{EL.hi(\%)} \tab upper expanded limit
    }
  \item If reference-scaling is not applicable (\emph{i.e.}, \code{CVwR(\%)} ≤30\%):
    \tabular{ll}{
      \code{BE.lo(\%)} \tab conventional lower limit (\emph{i.e.}, \verb{ 80})\cr
      \code{BE.hi(\%)} \tab conventional upper limit (\emph{i.e.}, \verb{125})
    }
}
  \tabular{ll}{
    \code{CI.lo(\%)} \tab lower confidence limit of the treatment comparison\cr
    \code{CI.hi(\%)} \tab upper confidence limit of the treatment comparison\cr
    \code{PE(\%)} \tab point estimate of the treatment comparison (aka \acronym{GMR})\cr
    \code{CI} \tab assessment whether the \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2
\eqn{\alpha}{alpha})} \acronym{CI} lies entirely within the acceptance range (\code{pass|fail})\cr
    \code{GMR} \tab assessment whether the \acronym{PE} lies entirely within the \acronym{GMR}-restriction 80.00\%–125.00\% (\code{pass|fail})\cr
    \code{BE} \tab mixed (aggregate) assessment whether the study demonstrates bioequivalence (\code{pass|fail})\cr
    \code{log.half-width} \tab half-width of the \acronym{CI} in log-scale
  }
If \code{ola=TRUE} and at least one outlier was detected:
  \tabular{ll}{
    \code{outlier} \tab outlying subject(s)\cr
    \code{CVwR.new(\%)} \tab intra-subject coefficient of variation of the reference treament; recalculated after exclusion of outlier(s)\cr
    \code{sw.ratio.new} \tab ratio of intrasubject variabilities of \code{T} and \code{R} after exclusion of outlier(s); full replicates only\cr
    \code{sw.ratio.new.CL} \tab upper confidence limit of \code{sw.ratio.new} (full replicates only)
  }
\itemize{
  \item If reference-scaling is applicable (\emph{i.e.}, \code{CVwR.new(\%)} >30\%):
    \tabular{ll}{
      \code{EL.new.lo(\%)} \tab recalculated lower expanded limit\cr
      \code{EL.newhi(\%)} \tab recalculated upper expanded limit
    }
  \item If reference-scaling is not applicable (\emph{i.e.}, \code{CVwR.new(\%)} ≤30\%):
    \tabular{ll}{
      \code{BE.new.lo(\%)} \tab conventional lower limit (\emph{i.e.}, \verb{ 80})\cr
      \code{BE.new.hi(\%)} \tab conventional upper limit (\emph{i.e.}, \verb{125})
    }
}
  \tabular{ll}{
    \code{CI.new} \tab assessment whether the \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2
\eqn{\alpha}{alpha})} \acronym{CI} lies entirely within the new acceptance range (\code{pass|fail})\cr
    \code{GMR.new} \tab assessment whether the \acronym{PE} lies entirely within the \acronym{GMR}-restriction 80.00\%–125.00\% (\code{pass|fail})\cr
    \code{BE.new} \tab mixed (aggregate) assessment whether the study demonstrates bioequivalence (\code{pass|fail})\cr
  }
}

\section{Warning}{
If the \acronym{CSV}-file has a commentary header, each line must start with \code{"# "} (hash-space -- \code{ASCII 35-32}).\cr
Commentary headers in \acronym{XLS}-files are not restricted.
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use.\cr
\emph{Guideline on the Investigation of Bioequivalence}\cr
London, 20 January 2010. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2010/01/WC500070039.pdf}{CPMP/EWP/QWP/1401/98 Rev. 1/ Corr **}

3\ifelse{html}{\out{<sup>rd</sup>}}{\eqn{\textsuperscript}{rd}} \acronym{EGA} Symposium on Bioequivalence\cr
\emph{Questions and Answers on the Revised \acronym{EMA} Bioequivalence Guideline}\cr
London, 1 June 2010. \href{http://www.medicinesforeurope.com/wp-content/uploads/2016/03/EGA_BEQ_QA_WEB_QA_1_32.pdf}{open access}

European Medicines Agency, Committee for Medicinal Products for Human Use.\cr
\emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP)}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}

European Medicines Agency, Committee for Medicinal Products for Human Use.\cr
\emph{Guideline on the pharmacokinetic and clinical evaluation of modified release dosage forms}\cr
London, 20 November 2014. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2014/11/WC500177884.pdf}{EMA/CPMP/EWP/280/96 Corr1}

World Health Organization, Prequalification Team: medicines\cr
\emph{Guidance Document: Application of reference-scaled criteria for AUC in bioequivalence studies conducted for submission to PQTm}\cr
Geneva, 9 June 2017. \href{https://extranet.who.int/prequal/sites/default/files/documents/AUC_criteria_June2017.pdf}{open access}

Food and Drug Administration, Office of Generic Drugs.\cr
\emph{Draft Guidance on Warfarin Sodium}\cr
December 2012. \href{https://www.fda.gov/downloads/drugs/guidancecomplianceregulatoryinformation/guidances/ucm201283.pdf}{open access}
}

\author{
Helmut \enc{Schütz}{Schuetz}, Michael Tomashevskiy, Detlew Labes
}

\note{
The \sQuote{graphical} presentation in the result file gives the confidence limits with filled black squares (■) and the point estimate as a white rhombus (◊). If a confidence limit exceeds the maximum possible expansion limit, it is shown as a triangle (◄ or ►). Expanded limits are given as double vertical lines (║). Unscaled limits, the \acronym{GMR} restriction, and 100\% are given with single vertical lines (│). The \sQuote{resolution} is approximatelly 0.5\% and therefore, not all symbols might be shown. The \acronym{CI} has presedence over the limits and the expanded limits over unscaled ones.
}

\note{
The \acronym{EMA}’s model specified in \sQuote{Method B} assumes equal [sic!] intra-subject variances of test and reference (like in 2×2×2 trials) -- even if proven false in one of the full replicate designs (were \emph{both} \code{CVwT} and \code{CVwR} can be estimated). Hence, amongst biostatisticians it is called the \dQuote{crippled model}.\cr
The method for calculating the degrees of freedom is not specified in the \acronym{SAS} code provided by the \acronym{EMA} in the Q&A document. Hence, the default in \code{PROC MIXED}, namely \code{DDFM=CONTAIN} is applied. For incomplete data (\emph{i.e.}, missing periods) Satterthwaite’s approximation of the degrees of freedom (\code{option=1}) might be a better choice -- if stated as such in the statistical analysis plan.\cr
The half-width of the CI in log-scale allows a comparison of methods (B \emph{v.s.} A) or options (2 \emph{v.s.} 1). A higher value \emph{might} point towards a more conservative decision. Quoting the Q&A-document:\itemize{
  \item A simple linear mixed model, which assumes identical within-subject variability (Method B), may be acceptable as long as results obtained with the two methods do not lead to different regulatory decisions. However, in borderline cases [\ldots] additional analysis using Method A might be required.
}
In the provided example data sets -- with one exception -- the conclusion of \acronym{BE} (based on the mixed \acronym{CI} and \acronym{GMR} criteria) agrees between \sQuote{Method A} and \sQuote{Method B}. However, for the highly incomplete data set 14 \sQuote{Method A} was \emph{liberal} (passing by \acronym{ANOVA} but failing by the mixed effects model).\cr\cr
Reference-scaling is acceptable for \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} (immediate release products) and \ifelse{html}{\out{C<sub>max,ss</sub>}}{\eqn{C\textsubscript}{max,ss}}, \ifelse{html}{\out{C<sub>τ,ss</sub>}}{\eqn{C\textsubscript}{τ,ss}}, and \ifelse{html}{\out{<sub>partial</sub>AUC}}{\eqn{\textsubscript}{partial}AUC} (modified release products). However, quoting the \acronym{BE} guideline:
\itemize{
  \item The applicant should justify that the calculated intra-subject variability is a reliable estimate and that it is not the result of outliers.}
Quoting the Q&A on the Revised EMA Bioequivalence Guideline:
\itemize{
  \item [\ldots] a study could be acceptable if the bioequivalence requirements are met both including the outlier subject (using the scaled average bioequivalence approach and the within-subject CV with this subject) and after exclusion of the outlier (using the within-subject CV without this subject).\cr
An outlier test is not an expectation of the medicines agencies but outliers could be shown by a box plot. This would allow the medicines agencies to compare the data between them.}
The \acronym{EMA}’s method of reference-scaling for highly variable drugs / drug products is currently recommended in other jurisdictions as well (\emph{e.g.}, the \acronym{WHO};
\acronym{ASEAN} States, Australia, Brazil, Egypt, the Russian Federation, the Eurasian Economic Union, New Zealand).\cr
The \acronym{WHO} opened a pilot phase exploring reference-scaling for AUC (4-period full replicate studies are mandatory in order to assess the variability associated with each product). It is an open issue how this assessment should be done. In Population Bioequivalence (\acronym{PBE}) and Individual Bioequivalence (\acronym{IBE}) the \ifelse{html}{\out{<em>s<sub>wT</sub></em>}}{\eqn{s\textsubscript}{wT}}/\ifelse{html}{\out{<em>s<sub>wR</sub></em>}}{\eqn{s\textsubscript}{wR}} ratio was assessed and similar variability was concluded for a ratio within 0.667 -- 1.500. However, the power of comparing variabilities in a study designed to demonstrate \acronym{ABE} is low. This was the main reason why \acronym{PBE} and \acronym{IBE} were not implemented in regulatory practice. An alternative approach is given in the \acronym{FDA}’s guidance on warfarin. Variabilities are considered comparable if the upper confidence limit of \ifelse{html}{\out{<em>&sigma;<sub>wT</sub></em>}}{\eqn{\sigma\textsubscript}{wT}}/\ifelse{html}{\out{<em>&sigma;<sub>wR</sub></em>}}{\eqn{\sigma\textsubscript}{wR}} is less than or equal to 2.5.\cr\cr
\strong{Program offered for Use without any Guarantees and Absolutely No Warranty.\cr
No Liability is accepted for any Loss and Risk to Public Health Resulting from Use of this R-Code.}
}

\seealso{
  \tabular{ll}{
    \code{\link{method.A}} \tab evaluation by a fixed effects model (\acronym{ANOVA})\cr
    \code{\link{ABE}} \tab evaluation for conventional (unscaled) Average Bioeqivalence
  }
}

\examples{
# Importing from a CSV-file, using most of the defaults: variable
# separator colon, decimal separator period, no outlier-analyis, print to file
# Note: You must adapt the path-variables. Write-permissions must be
# granted for 'path.out' in order to save the result file. If 'path.out' is
# not specified, results are written to the home folder.
path.in <- paste0(find.package("replicateBE"), "/extdata/")       # example files
method.B(path.in = path.in, file = "DS", set = "01", ext = "csv") # Q&A data set I
# Should result in:
#   CVwT               :  35.16\%
#   swT                :   0.34138
#   CVwR               :  46.96\% (reference-scaling applicable)
#   swR                :   0.44645
#   Expanded limits    :  71.23\% ... 140.40\% [100exp(±0.760·swR)]
#   swT / swR          :   0.7647 (similar variabilities of T and R)
#   sw-ratio (upper CL):   0.9324 (comparable variabilities of T and R)
#   Confidence interval: 107.17\% ... 124.97\%  pass
#   Point estimate     : 115.73\%              pass
#   Mixed (CI & PE)    :                      pass
#    ╟────────┼─────────────────────┼───────■────────◊─────────■───────────────╢
#
# Internal reference data set 01 used and results to the home folder.
# Additional outlier-analyis and boxplot saved as PNG (accidental overwriting prevented)
method.B(ola = TRUE, ask = TRUE, plot.bxp = TRUE, data = rds01)
# Should give the same as above. Additionally:
#   Recalculation due to presence of 2 outliers (subj. 45|52)
#   CVwR (outl. excl.) :  32.16\% (reference-scaling applicable)
#   swR  (recalc.)     :   0.31374
#   Expanded limits    :  78.79\% ... 126.93\% [100exp(±0.760·swR)]
#   swT / swR (recalc.):   1.0881 (similar variabilities of T and R)
#   sw-ratio (upper CL):   1.3282 (comparable variabilities of T and R)
#   Confidence interval: pass
#   Point estimate     : pass
#   Mixed (CI & PE)    : pass
#            ╟┼─────────────────────┼───────■────────◊─────────■─╢
#
# Same data set. Show information about outliers and the model-table
method.B(ola = TRUE, verbose = TRUE, data = rds01)
# data.frame of results (7-digits precision) shown in the console
x <- method.B(ola = TRUE, print = FALSE, details = TRUE, data = rds01)
print(x, row.names = FALSE)
# Compare Method B with Method A for all reference data sets
\dontrun{
ds <- substr(grep("rds", unname(unlist(data(package="replicateBE"))),
                  value=TRUE), 1, 5)
for (i in seq_along(ds)) {
  A <- method.A(print=FALSE, details=TRUE, data=eval(parse(text=ds[i])))$BE
  B <- method.B(print=FALSE, details=TRUE, data=eval(parse(text=ds[i])))$BE
  r <- paste0("A ", A, ", B ", B, " \u2013 ")
  cat(paste0(ds[i], ":"), r)
  if (A == B) {
    cat("Method A and B agree.\n")
  } else {
    if (A == "fail" & B == "pass") {
      cat("Method A is conservative.\n")
    } else {
      cat("Method B is conservative.\n")
    }
  }
}}
# should give
#   rds01: A pass, B pass – Method A and B agree.
#   rds02: A pass, B pass – Method A and B agree.
#   ...
#   rds14: A pass, B fail – Method B is conservative.
}
