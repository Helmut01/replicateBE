\encoding{utf-8}
\name{ABE}
\alias{ABE}
\title{
Comparative BA-calculation for Average Bioequivalence
}
\description{
  This function performs the required calculations for the \acronym{BE}
  decision via conventional (unscaled) Average Bioequivalence based on
  \acronym{ANOVA} as recommended in the \acronym{EMA}’s guideline.
}
\usage{
ABE(alpha = 0.05, path.in = "~/", path.out = "~/",
    file, set = "", ext, na = ".", sep = ",", dec = ".",
    logtrans = TRUE, print = TRUE, details = FALSE,
    verbose = FALSE, ask = FALSE, data = NULL, theta1, theta2)
}

\arguments{
  \item{alpha}{
    Type I Error (\acronym{TIE}) probability (nominal level of the test).
    Conventionally set to 0.05, resulting in a
    \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{\eqn{100(1-2\alpha)}{100(1-2alpha)}}
    confidence interval.
  }
  \item{path.in}{
    Path to the data file for import. If missing or not existing the
    user’s home folder \verb{"~/"} will be used.
  }
  \item{path.out}{
    Path to safe the result file if \code{print=TRUE}. The user must have
    write-permission to the directory. If missing or not existing the
    user’s home folder \verb{"~/"} will be used.
  }
  \item{file}{
    Name of the dataset for import (\emph{without} extension). Must be a string
    (\emph{i.e.}, enclosed in single or double quotation marks). The name is
    case-sensitive.
  }
  \item{set}{
    Name of the sheet of an Excel-file (mandatory). Must be a string (\emph{i.e.},
    enclosed in single or double quotation marks). The name is case-sensitive.
  }
  \item{ext}{
    File-extension ("csv" or "xls(x)") enclosed in single or double quotation marks.
  }
  \item{na}{
    Character string denoting missing values. Acceptable are \verb{"NA"}
    (not available), \verb{"ND"} (not determined), \verb{"."} (\acronym{SAS}),
    \verb{"Missing"} (Phoenix WinNonlin), and \verb{""} (\acronym{Excel};
    empty cell). Missings will be converted to \verb{NA} in the imported
    data. Defaults to \verb{"."}.
  }
  \item{sep}{
    Variable separator in the \acronym{CSV}-file. Acceptable are \verb{","}
    (comma = \verb{ASCII 44}), \verb{";"} (semicolon = \verb{ASCII 59}),
    and \verb{"\\t"} (tabulator = \verb{ASCII 9}). Defaults to \verb{","}.
  }
  \item{dec}{
    Decimal separator in the \acronym{CSV}-file. Acceptable are \verb{"."}
    (period = \verb{ASCII 46}) or \verb{","} (comma = \verb{ASCII 44}).
    Defaults to \verb{"."}.
  }
  \item{logtrans}{
    If \code{TRUE} (default) the raw data (provided in column \verb{PK})
    will be internally log-transformed and used in the calculations.
    If \code{FALSE} the already log-transformed data (provided in the
    column \verb{logPK}) will be used in the calculations.
  }
  \item{print}{
    If \code{TRUE} (default), the function prints its results to a
    file. If \code{FALSE}, returns a data.frame of results.
  }
  \item{details}{
    Defaults to \code{FALSE}. If \code{TRUE}, the function sends
    its results in 7-digits precision to a data.frame.
  }
  \item{verbose}{
    Defaults to \code{FALSE}. If \code{TRUE} the ANOVA-table is send to
    the console.
  }
  \item{ask}{
    Defaults to \code{FALSE}. If \code{TRUE} the user will be asked
    whether an already existing result file should be overwritten.
  }
  \item{data}{
    Specification of one of the internal reference datasets (\verb{rds01}
    -- \verb{rds28}). If given, \verb{path.in}, \verb{file}, \verb{set},
    and \verb{ext} are ignored. For its use see the examples.\cr
    If not given, defaults to \code{NULL} (\emph{i.e.}, import data
    from a file).
  }
  \item{theta1}{
    Lower limit of the acceptance range. Defaults to \verb{0.80}. If missing
    will be set to \code{1/theta2}.
  }
  \item{theta2}{
    Upper limit of the acceptance range. Defaults to \verb{1.25}. If missing
    will be set to \code{1/theta1}.
  }
}

\details{
  The model for the treatment comparison is\cr
  \code{lm(log(PK) ~ sequence + subject\%in\%sequence + period + treatment, data = data)}\cr
  where all effects are fixed.\cr\cr
  Tested designs
  \itemize{
    \item 4-period 2-sequence full replicates\cr
      \verb{TRTR | RTRT}\cr
      \verb{TRRT | RTTR}\cr
      \verb{TTRR | RRTT}
    \item 2-period 4-sequence replicate\cr
      \verb{TR | RT | TT | RR } (Balaam’s design)
    \item 4-period 4-sequence full replicates\cr
      \verb{TRTR | RTRT | TRRT | RTTR}\cr
      \verb{TRRT | RTTR | TTRR | RRTT}
    \item 3-period 2-sequence full replicates\cr
      \verb{TRT | RTR}\cr
      \verb{TRR | RTT}
    \item 3-period (partial) replicates\cr
      \verb{TRR | RTR | RRT}\cr
      \verb{TRR | RTR } (extra-reference design)
  }
  Data structure
  \itemize{
    \item Columns must have the headers \code{subject}, \code{period},
    \code{sequence}, \code{treatment}, \code{PK}, and/or \code{logPK}.\cr
    Any order of columns is acceptable.\cr
    Uppercase and mixed case headers will be internally converted to lowercase headers.
    \itemize{
      \item \code{subject} must be integer numbers or (any combination of) alphanumerics\cr
      \verb{[A-Z, a-z, -, _, #, 0-9]}
      \item \code{period} must be integer numbers.
      \item \code{sequence} must be contained in the tested designs (numbers or
      \emph{e.g.}, \verb{ABAB} are not acceptable).
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
    \code{BE} \tab assessment whether the \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{\eqn{100(1-2\alpha)}{100(1-2alpha)}} \acronym{CI} lies entirely within the acceptance range (\verb{pass|fail})
  }
}

\section{Warning}{
  Files may contain a commentary header. If reading from a \acronym{CSV}-file,
  \emph{each} line of the commentary header \emph{must} start with \verb{"# "}
  (hash space = \verb{ASCII 35 ASCII 32}). If reading from an Excel-file
  all lines preceding the column headers are treated as a comment.
}

\section{Clarification}{
  The \sQuote{graphical} presentation in the result file gives the confidence
  limits with filled black squares and the point estimate as a white rhombus.
  If a confidence limit exceeds the drawing range, it is shown as a triangle.
  The \acronym{BE} limits and 100\% are given with single vertical lines. The
  \sQuote{resolution} is approximatelly 0.5\% and therefore, not all symbols
  might be shown. The \acronym{CI} and \acronym{PE} take presedence over the limits.
}

\note{
  The \acronym{EMA}’s model assumes equal [\emph{sic!}] intra-subject
  variances of test and reference (like in \enc{2×2×2}{2x2x2} trials) --
  even if proven false in one of the full replicate designs (were \emph{both}
  \ifelse{html}{\out{CV<sub>wT</sub>}}{\eqn{CV_{wT}}{CVwT}} and
  \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}{CVwR}} can be estimated).
  Hence, amongst biostatisticians it is called the \dQuote{crippled model}
  because the replicative nature of the study is ignored.\cr
  Conventional unscaled \acronym{ABE} has to be employed for
  \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} (if widening of the
  acceptance range is clinically not justifiable),
  \ifelse{html}{\out{AUC<sub>0&ndash;t</sub>}}{\eqn{AUC_{0-t}}},
  \ifelse{html}{\out{AUC<sub>0&ndash;72</sub>}}{\eqn{AUC_{0-72}}} (immediate
  release products) and
  \ifelse{html}{\out{C<sub>max,ss</sub>}}{\eqn{C_{max,ss}}},
  \ifelse{html}{\out{C<sub>&tau;,ss</sub>}}{\eqn{C_{\tau,ss}}},
  \ifelse{html}{\out{<sub>partial</sub>AUC}}{\eqn{_{partial}AUC}} (if widening
  of the acceptance range is clinically not justifiable), and
  \ifelse{html}{\out{AUC<sub>0&ndash;t</sub>}}{\eqn{AUC_{0-t}}},
  \ifelse{html}{\out{AUC<sub>0&ndash;&infin;</sub>}}{\eqn{AUC_{0-\infty}}},
  \ifelse{html}{\out{AUC<sub>0&ndash;&tau;</sub>}}{\eqn{AUC_{0-\tau}}}
  (modified release products).
}

\section{Disclaimer}{
  \emph{Program offered for Use without any Guarantees and Absolutely No Warranty. No Liability is accepted for any Loss and Risk to Public Health Resulting from Use of this R-Code.}
}

\author{
Helmut \enc{Schütz}{Schuetz}
}

\references{

European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Guideline on the Investigation of Bioequivalence.} London, 20 January 2010. \href{https://www.ema.europa.eu/en/documents/scientific-guideline/guideline-investigation-bioequivalence-rev1_en.pdf}{CPMP/EWP/QWP/1401/98 Rev. 1/ Corr **}

European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Guideline on the pharmacokinetic and clinical evaluation of modified release dosage forms.} London, 20 November 2014.
\href{https://www.ema.europa.eu/en/documents/scientific-guideline/guideline-pharmacokinetic-clinical-evaluation-modified-release-dosage-forms_en.pdf}{EMA/CPMP/EWP/280/96 Corr1}
}

\seealso{
  \tabular{ll}{
    \code{\link{method.A}} \tab evaluation for \acronym{ABEL} by a fixed effects model (\acronym{ANOVA})\cr
    \code{\link{method.B}} \tab evaluation for \acronym{ABEL} by a linear mixed effects model
  }
}

\examples{
\donttest{
# Importing from a CSV-file, using most of the defaults: variable
# separator comma, decimal separator period, print to file.
# Note: You must adapt the path-variables. The example reads from
# the data provided by the library. Write-permissions must be granted
# for 'path.out' in order to save the result file. If 'path.out' is
# not specified, results are written to the home folder.
path.in <- paste0(find.package("replicateBE"), "/extdata/")
ABE(path.in = path.in, file = "DS", set = "02", ext = "csv")
# Should result in:
#   BE-limits          :  80.00\% ... 125.00\%
#   Confidence interval:  97.32\% ... 107.46\%  pass
#   Point estimate     : 102.26\%}
# Generate the data.frame of results (7-digits precision) and show
# in the console. Use an internal dataset.
x <- ABE(details = TRUE, print = FALSE, data = rds02)
print(x, row.names = FALSE)
\donttest{
# Assuming a NTID and assess BE with narrower limits.
ABE(data = rds02, theta1 = 0.90)
# Should result in:
#   BE-limits          :  90.00\% ... 111.11\%
#   Confidence interval:  97.32\% ... 107.46\%  pass
#   Point estimate     : 102.26\%}
\donttest{
# Health Canada: Only the PE of Cmax has to lie with 80.0-125.0\%
# (i.e., no CI is required).
x    <- ABE(alpha = 0.5, data = rds03, print = FALSE,
            details = TRUE)[c(13:14, 17:18)]
x[3] <- round(x[3], 1)
print(x, row.names = FALSE)
# Should result in:
# BE.lo(\%) BE.hi(\%) PE(\%)   BE
#       80      125 124.2 pass}}
