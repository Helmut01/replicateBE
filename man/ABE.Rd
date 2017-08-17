\encoding{utf-8}
\name{ABE}
\alias{ABE}
\title{
Comparative BA-calculation for the EMA's Average Bioequivalence (ABE)
}
\description{
This function performs the required calculations for the \acronym{BE} decision
via conventional (unscaled) Average Bioequivalence based on ANOVA as recommended in the \acronym{EMA}\enc{’}{'}s guideline.
}
\usage{
ABE(alpha = 0.05, path.in = NULL, path.out = NULL, file, set = "",
    ext, na = ".", sep = ",", dec = ".", logtrans = TRUE,
    print = TRUE, details = FALSE, verbose = FALSE, ask = FALSE,
    theta1, theta2, data = NULL)
}
\arguments{
  \item{alpha}{Type I Error (\acronym{TIE}) probability (nominal level of the test). Per convention commonly set to 0.05 which results in a \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2
\eqn{\alpha}{alpha})} confidence interval.
}
  \item{path.in}{
Path to the data file for import. The path \emph{must} be given with forward slashes (/) -- even on Windows (where backward slashes (\\) are the standard. If missing or not existing the user\enc{’}{'}s home folder \code{"~/"} will be used.
}
  \item{path.out}{
Path to safe the result file if \code{print=TRUE}. The path \emph{must} be given with forward slashes (/) -- even on Windows (where backward slashes (\\) are the standard. The user must have write-permission to the directory. If missing or not existing the user\enc{’}{'}s home folder \code{"~/"} will be used.
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
  \item{print}{
If \code{TRUE} (default), the function prints its results to a file. If \code{FALSE}, returns a data.frame of results.
}
  \item{details}{
Defaults to \code{FALSE}. If \code{TRUE}, the function sends its results in 7-digits precision to a data.frame.
}
  \item{verbose}{
Defaults to \code{FALSE}. If \code{TRUE} the ANOVA-table is send to the console. If \code{ola=TRUE} additional information about outliers are shown.
}
  \item{ask}{
Defaults to \code{FALSE}. If \code{TRUE} the user will be asked whether an already existing result file should be overwritten.
}
  \item{theta1}{
Lower bioequivalence limit. Defaults to \code{0.80}. If missing will be set to \code{1/theta2}.
}
  \item{theta2}{
Lower bioequivalence limit. Defaults to \code{1.25}. If missing will be set to \code{1/theta1}.
}
  \item{data}{
One of internal reference data sets (\code{rds01} -- \code{rds23}). If given, \code{path.in}, \code{file}, \code{set}, and \code{ext} are ignored. For its use see the examples.\cr
Defaults to \code{NULL} (\emph{i.e.}, import external data).
}
}
\details{
The model for the treatment comparison is\cr
\verb{   log(response) ~ sequence + subject(sequence) + period + treatment}\cr
where all effects are fixed.\cr\cr
Tested designs:
  \itemize{
    \item 4-period 2-sequence full replicates:\cr
      \verb{TRTR | RTRT}\cr
      \verb{TRRT | RTTR}
    \item 4-period 4-sequence full replicates:\cr
      \verb{TRTR | RTRT | TRRT | RTTR}\cr
      \verb{TRRT | RTTR | TTRR | RRTT}
    \item 3-period 2-sequence full replicates:\cr
      \verb{TRT | RTR}\cr
      \verb{TRR | RTT}
    \item 3-period (partial) replicates:\cr
      \verb{TRR | RTR | RRT}\cr
      \verb{TRR | RTR } (extra-reference design)
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
    \code{Method} \tab ABE\cr
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
    \code{BE.lo(\%)} \tab lower bioequivalence limit (\emph{e.g.}, \verb{ 80})\cr
    \code{BE.hi(\%)} \tab upper bioequivalence limit (\emph{e.g.}, \verb{125})\cr
    \code{CI.lo(\%)} \tab lower confidence limit of the treatment comparison\cr
    \code{CI.hi(\%)} \tab upper confidence limit of the treatment comparison\cr
    \code{PE(\%)} \tab point estimate of the treatment comparison (aka \acronym{GMR})\cr
    \code{BE} \tab assessment whether the \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1-2
\eqn{\alpha}{alpha})} \acronym{CI} lies entirely within the acceptance range (\code{pass|fail})
  }
}

\section{Warning}{
If the \acronym{CSV}-file has a commentary header, each line must start with \code{"# "} (hash-space -- \code{ASCII 35-32}).\cr
Commentary headers in \acronym{XLS}-files are not restricted.
}

\note{
The \sQuote{graphical} presentation in the result file gives the confidence limits with filled black squares\enc{ (■)}{} and the point estimate as a white rhombus\enc{ (◊)}{}. If a confidence limit exceeds the drawing range, it is shown as a triangle\enc{ (◄ or ►)}{}. The \acronym{BE} limits and 100\% are given with single vertical lines\enc{ (│)}{}. The \sQuote{resolution} is approximatelly 0.5\% and therefore, not all symbols might be shown. The \acronym{CI} has presedence over the limits.
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use.\cr
\emph{Guideline on the Investigation of Bioequivalence}\cr
London, 20 January 2010. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2010/01/WC500070039.pdf}{CPMP/EWP/QWP/1401/98 Rev. 1/ Corr **}

European Medicines Agency, Committee for Medicinal Products for Human Use.\cr
\emph{Guideline on the pharmacokinetic and clinical evaluation of modified release dosage forms}\cr
London, 20 November 2014. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2014/11/WC500177884.pdf}{EMA/CPMP/EWP/280/96 Corr1}
}

\author{
Helmut \enc{Schütz}{Schuetz}
}

\note{
The \acronym{EMA}\enc{’}{'}s model assumes equal [sic!] intra-subject variances of test and reference (like in \enc{2×2×2}{2x2x2} trials) -- even if proven false in one of the full replicate designs (were \emph{both} \code{CVwT} and \code{CVwR} can be estimated). Hence, amongst biostatisticians it is called the \dQuote{crippled model}.\cr
Conventional unscaled \acronym{ABE} has to be employed for \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} (if widening of the acceptance range is clinically not justifiable), \ifelse{html}{\out{AUC<sub>0&ndash;t</sub>}}{\eqn{AUC_{0-t}}}, \ifelse{html}{\out{AUC<sub>0&ndash;72</sub>}}{\eqn{AUC_{0-72}}} immediate release products) and \ifelse{html}{\out{C<sub>max,ss</sub>}}{\eqn{C_{max,ss}}},
\ifelse{html}{\out{C<sub>&tau;,ss</sub>}}{\eqn{C_{\tau,ss}}}, \ifelse{html}{\out{<sub>partial</sub>AUC}}{\eqn{_{partial}AUC}} (if widening of the acceptance range is clinically not justifiable), and
\ifelse{html}{\out{AUC<sub>0&ndash;t</sub>}}{\eqn{AUC_{0-t}}}, \ifelse{html}{\out{AUC<sub>0&ndash;&infin;</sub>}}{\eqn{AUC_{0-\infty}}}, \ifelse{html}{\out{AUC<sub>0&ndash;&tau;</sub>}}{\eqn{AUC_{0-\tau}}} (modified release products).\cr\cr
\strong{Program offered for Use without any Guarantees and Absolutely No Warranty.\cr
No Liability is accepted for any Loss and Risk to Public Health Resulting from Use of this R-Code.}
}

\seealso{
  \tabular{ll}{
    \code{\link{method.A}} \tab evaluation for \acronym{ABEL} by a fixed effects model (\acronym{ANOVA})\cr
    \code{\link{method.B}} \tab evaluation for \acronym{ABEL} by a linear mixed effects model
  }
}

\examples{
# Importing from a CSV-file, using most of the defaults: variable
# separator colon, decimal separator period, print to file
# Note: You must adapt the path-variables. Write-permissions must be
# granted for 'path.out' in order to save the result file. If 'path.out' is
# not specified, results are written to the home folder.
path.in <- paste0(find.package("replicateBE"), "/extdata/")  # example files
ABE(path.in = path.in, file = "DS", set = "02", ext = "csv") # Q&A data set II
# Should result in:
#   BE-limits          :  80.00\% ... 125.00\%
#   Confidence interval:  97.32\% ... 107.46\%  pass
#   Point estimate     : 102.26\%
#             ├──────────────────■──┼─◊─────■──────────────────┤
#
# Generate the data.frame of results (7-digits precision) and show in the console
# Use the internal data set
x <- ABE(details = TRUE, print = FALSE, data = rds02)
print(x, row.names = FALSE)
#
# Assuming a NTID and assess BE with narrower limits.
ABE(data = rds02, theta1 = 0.90)
# Should result in:
#   BE-limits          :  90.00\% ... 111.11\%
#   Confidence interval:  97.32\% ... 107.46\%  pass
#   Point estimate     : 102.26\%
#                        ├───────■──┼─◊─────■───┤
}
