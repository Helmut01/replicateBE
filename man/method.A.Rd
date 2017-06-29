\encoding{utf-8}
\name{method.A}
\alias{method.A}
\title{
Comparative BA-calculation for the EMA's Average Bioequivalence with Expanding Limits (ABEL), Method A
}
\description{
This function performs the required calculations for the \acronym{BE} decision
via Average Bioequivalence with Expanding Limits (\acronym{ABEL}) based
on ANOVA (\sQuote{Method A}) as recommended in the \acronym{EMA}’s Q&A-document.
}
\usage{
method.A(alpha = 0.05, path.in = NULL, path.out = NULL, file, set, ext,
         header = 0, na = ".", sep = ",", dec = ".", logtrans = TRUE,
         ola = FALSE, print = TRUE, details = FALSE, adjust = FALSE,
         verbose = FALSE, ask = FALSE, plot.bxp = FALSE, data = NULL)
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
Name of the data set for import (\emph{without} extension).
}
  \item{set}{
Name of the sheet of an Excel-file.
}
  \item{ext}{
File-extension. Acceptable are \code{"CSV"} or \code{"csv"} for character-separated variables (\acronym{CSV}) and \code{"XLS"}, \code{"xls"}, \code{"XLXS"}, and \code{"xlsx"} for Excel-files.
}
  \item{header}{
If an Excel-sheet contains an explanatory header, the number of rows to skip (\emph{e.g.,}
if the first two rows contain a comment, use \code{header=2} and the data will be imported starting with the third row). Defaults to \code{0}.
}
  \item{na}{
Character string denoting missing values. Acceptable are any of \code{"NA"} (not available),
\code{"ND"} (not determined), \code{"."} (\acronym{SAS}), \code{"Missing"} (Phoenix WinNonlin), and \code{""} (\acronym{Excel}; empty cell). Missings will be converted to \code{NA} in the imported data. Defaults to \code{"."}.
}
  \item{sep}{
Variable separator in the \acronym{CSV}-file. Acceptable are any of \code{";"} (semicolon -- \code{ASCII 59}), \code{","} (colon -- \code{ASCII 44}), and \code{"\\t"} (tabulator -- \code{ASCII 9}). Defaults to \code{","}.
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
  \item{details}{
Defaults to \code{FALSE}. If \code{TRUE}, the function sends its results in 7-digits precision to a data.frame.
}
  \item{adjust}{
Defaults to \code{FALSE}.\cr
If \code{TRUE}, the empiric Type I Error is evaluated via simulations (by \code{PowerTOST}’s function \code{scABEL.ad()}). Currently implemented designs are \code{RTRT|TRTR}, \code{RTR|TRT}, and \code{RRT|RTR|TRR}. If the \acronym{TIE} exceeeds the nominal level of the test \code{alpha}, \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} is iteratively adjusted until \acronym{TIE}\code{ = alpha ± 1E-6}.\cr
If \code{ola=TRUE} and outlier(s) found which lead to a -- lower -- recalculated \code{CVwr}, the assessment is repeated for its value.
}
  \item{verbose}{
Defaults to \code{FALSE}. If \code{TRUE} the ANOVA-table is send to the console. If \code{ola=TRUE} additional information about outliers are shown.
}
  \item{ask}{
Defaults to \code{FALSE}. If \code{TRUE} the user will be asked whether an already existing result file (and if outliers are found, the boxplot) should be overwritten.
}
  \item{plot.bxp}{
Only observed if \code{ola=TRUE} and at least one outlier is found. If \code{FALSE} (default) the boxplot will be shown in the graphics device. If \code{TRUE} the boxplot will be saved in \acronym{PNG} format to \code{path.out}.
}
  \item{data}{
One of internal reference data sets (\code{rds01} -- \code{rds22}). If given, \code{path.in}, \code{file}, \code{set}, and \code{ext} are ignored. For the use see the examples.\cr
Defaults to \code{NULL} (\emph{i.e.}, work with external data).
}
}
\details{
The model for the estimation of \code{CVwR} is\cr
\verb{   log(response) ~ sequence + subject(sequence) + period}\cr
where all effects are fixed.\cr\cr
The model for the treatment comparison is\cr
\verb{   log(response) ~ sequence + subject(sequence) + period + treatment}\cr
where all effects are fixed.\cr\cr
Supported designs:
  \itemize{
    \item 4-period 2-sequence full replicates:
      \tabular{rcl}{
        \code{RTRT} \tab \code{|} \tab \code{TRTR}\cr
        \code{RTTR} \tab \code{|} \tab \code{TRRT}
      }
    \item 3-period 2-sequence full replicates:
      \tabular{rcl}{
        \code{RTR} \tab \code{|} \tab \code{TRT}\cr
        \code{RTT} \tab \code{|} \tab \code{TRR}
      }
    \item 3-period 3-sequence (aka partial) replicate:
      \tabular{rcccl}{
        \code{RRT} \tab \code{|} \tab \code{RTR} \tab \code{|} \tab \code{TRR}
      }
  }
}
\value{
Prints results to a file if argument \code{print=TRUE} (default).\cr
If argument \code{print=FALSE}, returns a data.frame with the elements:
  \tabular{ll}{
    \code{Design} \tab \emph{e.g.}, RTRT|TRTR\cr
    \code{Method} \tab A\cr
    \code{n} \tab total number of subjects\cr
    \code{nTT} \tab number of subjects with two treatments of \code{T} (full replicates only)\cr
    \code{nRR} \tab number of subjects with two treatments of \code{R}\cr
    \code{Sub/seq} \tab number of subjects per sequence\cr
    \code{Miss/seq} \tab if the design is unbalanced, number of missings per sequence\cr
    \code{Miss/per} \tab if the design is incomplete, number of missings per period\cr
    \code{alpha} \tab nominal level of the test\cr
    \code{DF} \tab degrees of freedom of the treatment comparison\cr
    \code{CVwT(\%)} \tab intra-subject coefficient of variation of the test treatment (full replicates only)\cr
    \code{CVwR(\%)} \tab intra-subject coefficient of variation of the reference treatment
  }
\itemize{
  \item If reference-scaling is applicable (\emph{i.e.}, \code{CVwR(\%)} >30\%):
    \tabular{ll}{
      \code{EL.lo(\%)} \tab lower expanded limit\cr
      \code{EL.hi(\%)} \tab upper expanded limit
    }
  \item If reference-scaling is not applicable (\emph{i.e.}, \code{CVwR(\%)} ≤30\%):
    \tabular{ll}{
      \code{BE.lo(\%)} \tab conventional lower limit (\emph{i.e.}, 80.00\%)\cr
      \code{BE.hi(\%)} \tab conventional upper limit (\emph{i.e.}, 125.00\%)
    }
}
  \tabular{ll}{
    \code{CI.lo(\%)} \tab lower confidence limit of the treatment comparison\cr
    \code{CI.hi(\%)} \tab upper confidence limit of the treatment comparison\cr
    \code{PE(\%)} \tab point estimate of the treatment comparison (aka \acronym{GMR})\cr
    \code{CI} \tab assessment whether the \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2
\eqn{\alpha}{alpha})} \acronym{CI} lies entirely within the acceptance range (\code{pass|fail})\cr
    \code{GMR} \tab assessment whether the \acronym{PE} lies entirely within the \acronym{GMR}-restriction 80.00\%–125.00\% (\code{pass|fail})\cr
    \code{BE} \tab assessment whether the study demonstrates bioequivalence (\code{pass|fail})\cr
    \code{log.half-width} \tab half-width of the \acronym{CI} in log-scale
  }
If \code{ola=TRUE} and at least one outlier was detected:
  \tabular{ll}{
    \code{outlier} \tab outlying subject(s)\cr
    \code{CVwR.new(\%)} \tab intra-subject coefficient of variation of the reference treament; recalculated after exclusion of outlier(s)
  }
\itemize{
  \item If reference-scaling is applicable (\emph{i.e.}, \code{CVwR.new(\%)} >30\%):
    \tabular{ll}{
      \code{EL.new.lo(\%)} \tab recalculated lower expanded limit\cr
      \code{EL.newhi(\%)} \tab  recalculated upper expanded limit
    }
  \item If reference-scaling is not applicable (\emph{i.e.}, \code{CVwR.new(\%)} ≤30\%):
    \tabular{ll}{
      \code{BE.new.lo(\%)} \tab conventional lower limit (\emph{i.e.}, 80.00\%)\cr
      \code{BE.new.hi(\%)} \tab conventional upper limit (\emph{i.e.}, 125.00\%)
    }
}
  \tabular{ll}{
    \code{CI.new} \tab assessment whether the \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2
\eqn{\alpha}{alpha})} \acronym{CI} lies entirely within the new acceptance range (\code{pass|fail})\cr
    \code{GMR.new} \tab assessment whether the \acronym{PE} lies entirely within the \acronym{GMR}-restriction 80.00\%–125.00\% (\code{pass|fail})\cr
    \code{BE.new} \tab assessment whether the study demonstrates bioequivalence (\code{pass|fail})\cr
  }
}

\section{Warning}{
If the input file has a header, the word \code{subject} \emph{must not} be part of the text. Otherwise, the header will terminate before.
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

Labes D, \enc{Schütz}{Schuetz} H.\cr
\emph{Inflation of Type I Error in the Evaluation of Scaled Average Bioequivalence,
and a Method for its Control}\cr
Pharm Res. 2016;33(11):2805--14. \doi{10.1007/s11095-016-2006-1}
}

\author{
Helmut \enc{Schütz}{Schuetz}, Michael Tomashevskiy, Detlew Labes
}

\note{
The \acronym{EMA}’s model specified in \sQuote{Method A} assumes equal [sic!] intra-subject variances of test and reference (like in 2×2×2 trials) -- even if proven false in one of the full replicate designs (were \emph{both} \code{CVwT} and \code{CVwR} can be estimated). Hence, amongst biostatisticians it is called the \dQuote{crippled model}.\cr
The half-width of the \acronym{CI} in log-scale allows a comparison of methods (B \emph{v.s.} A) where a higher value \emph{might} point towards a more conservative decision. In the provided example data sets -- with one exception -- the conclusion of \acronym{BE} (based on the aggregate \acronym{CI} and \acronym{GMR} criteria) agrees between \sQuote{Method A} and \sQuote{Method B}. However, for the highly incomplete data set 14 \sQuote{Method A} was \emph{liberal} (passing by \acronym{ANOVA} but failing by the mixed effects model).\cr\cr
Reference-scaling is acceptable for \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} (immediate release products) and \ifelse{html}{\out{C<sub>max,ss</sub>}}{\eqn{C\textsubscript}{max,ss}}, \ifelse{html}{\out{C<sub>τ,ss</sub>}}{\eqn{C\textsubscript}{τ,ss}}, and \ifelse{html}{\out{<sub>partial</sub>AUC}}{\eqn{\textsubscript}{partial}AUC} (modified release products). However, quoting the \acronym{BE} guideline:
\itemize{
  \item The applicant should justify that the calculated intra-subject variability is a reliable estimate and that it is not the result of outliers.}
Quoting the Q&A on the Revised EMA Bioequivalence Guideline:
\itemize{
  \item [\ldots] a study could be acceptable if the bioequivalence requirements are met both including the outlier subject (using the scaled average bioequivalence approach and the within-subject CV with this subject) and after exclusion of the outlier (using the within-subject CV without this subject).\cr
An outlier test is not an expectation of the medicines agencies but outliers could be shown by a box plot. This would allow the medicines agencies to compare the data between them.}
The \acronym{EMA}’s method of reference-scaling for highly variable drugs / drug products is currently recommended in other jurisdictions as well (\emph{e.g.}, the \acronym{WHO};
\acronym{ASEAN} States, Australia, Brazil, Egypt, the Russian Federation, the Eurasian Economic Union, New Zealand). The \acronym{WHO} opened a pilot phase exploring reference-scaling for AUC (4-period full replicate studies are mandatory in order to assess both \code{CVwR} and \code{CVwT}).\cr\cr
\strong{Program offered for Use without any Guarantees and Absolutely No Warranty.\cr
No Liability is accepted for any Loss and Risk to Public Health Resulting from Use of this R-Code.}
}

\seealso{
  \tabular{ll}{
    \code{\link{method.B}} \tab evaluation by a linear mixed effects model and\cr
    \code{\link{ABE}} \tab evaluation for conventional (unscaled) Average Bioeqivalence
  }
}

\examples{
# Importing from a CSV-file in path, using most of the defaults: variable
# separator colon, decimal separator period, no outlier-analyis, print to file
# Note: You must adapt the path-variables. Write-permissions must be
# granted for 'path.out' in order to save the result file. If 'path.out'
# not specified, results are written to the home folder.
path.in <- paste0(find.package("replicateBE"), "/extdata/")       # example files
method.A(path.in = path.in, file = "DS", set = "01", ext = "csv") # EMA's Q&A data set I
# Should result in:
#   CVwR               :  46.96\% (reference-scaling applicable)
#   Expanded limits    :  71.23\% ... 140.40\%
#   Confidence interval: 107.11\% ... 124.89\% (pass)
#   Point estimate     : 115.66\% (pass)
#   Overall conclusion : pass
#
# Internal reference data set 01 used and results to the home folder.
# Additional outlier-analyis and boxplot saved as PNG (accidental overwriting prevented)
method.A(ola = TRUE, ask = TRUE, plot.bxp = TRUE, data = rds01)
# Should give the same as above. Additionally:
#   Recalculation due to presence of 2 outliers (subj. 45|52)
#   CVwR (outl. excl.) :  32.16\% (reference-scaling applicable)
#   Expanded limits    :  78.79\% ... 126.93\%
#   Confidence interval: pass
#   Point estimate     : pass
#   Overall conclusion : pass
#
# Same data set. Show information about outliers and the ANOVA-table
method.A(ola = TRUE, verbose = TRUE, data = rds01)
# Generate the data.frame of results (7-digits precision) and show in the console
x <- method.A(ola = TRUE, details = TRUE, print = FALSE, data = rds01)
print(x, row.names = FALSE)
#
# Assess the Type I Error and iteratively adjust alpha if necessary
# Not run: due to timing policy of CRAN for examples
\dontrun{
method.A(adjust = TRUE, data = rds01)}
# Should give in the result file:
#   TIE not > nominal 0.05; no adjustment of alpha is required.
#
# Same with recalculation based on outliers, iteratively adjust alpha if necessary
\dontrun{
method.A(ola = TRUE, adjust = TRUE, data = rds01)}
# Should give in the result file:
# Assessment of empiric Type I Error (TIE) based on original CVwR
#   TIE not > nominal 0.05; no adjustment of alpha is required.
# Assessment of empiric Type I Error (TIE) based on recalculated CVwR
#   TIE for alpha 0.033416         : 0.05000
# Repeat the evaluation with the adjusted alpha
\dontrun{
method.A(method.A(alpha = 0.033416, ola = TRUE, adjust = TRUE, data = rds01)}
# Should give in the result file:
#  alpha              : 0.033416 (93.3168\% CI)
#  Confidence interval: 106.16\% ... 126.00\% (pass)
#  Point estimate     : 115.66\% (pass)
#  Overall conclusion : pass
#  Assessment based on recalculated CVwR 32.16\%
#  Confidence interval: pass
#  Point estimate     : pass
#  Overall conclusion : pass
#  Assessment of empiric Type I Error (TIE) based on recalculated CVwR
#    TIE for alpha 0.033416         : 0.05000
}
