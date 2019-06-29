\encoding{utf-8}
\name{method.B}
\alias{method.B}
\title{
Comparative BA-calculation for Average Bioequivalence with Expanding Limits by the EMA's 'Method B'
}
\description{
  This function performs the required calculations for the mixed (or aggregate)
  \acronym{BE} decision via Average Bioequivalence with Expanding Limits
  (\acronym{ABEL}) based on a linear mixed effects model with subjects as a
  random effect (\sQuote{Method B}) as recommended in the \acronym{EMA}’s
  Q&A-document.
}
\usage{
method.B(alpha = 0.05, path.in = NULL, path.out = NULL,
         file, set = "", ext, na = ".", sep = ",", dec = ".",
         logtrans = TRUE, ola = FALSE, print = TRUE, details = FALSE,
         verbose = FALSE, ask = FALSE, plot.bxp = FALSE,
         fence = 2, data = NULL, option = 2)
}
\arguments{
  \item{alpha}{
    Type I Error (\acronym{TIE}) probability (nominal level of the test).
    Per convention commonly set to 0.05 which results in a
    \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2\eqn{\alpha}{alpha})}
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
    Name of the dataset for import (\emph{without} extension). Must be
    a string (\emph{i.e.}, enclosed in single or double quotation marks).
  }
  \item{set}{
    Name of the sheet of an Excel-file (mandatory). Must be a string
    (\emph{i.e.}, enclosed in single or double quotation marks).
  }
  \item{ext}{
    File-extension enclosed in single or double quotation marks.
    Acceptable are \verb{"CSV"} and \verb{"csv"} for comma delimited
    variables (\acronym{CSV}) or \verb{"XLS"}, \verb{"xls"}, \verb{"XLSX"},
    and \verb{"xlsx"} for Excel-files.
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
    (comma -- \verb{ASCII 44}), \verb{";"} (semicolon -- \verb{ASCII 59}), and
    \verb{"\\t"} (tabulator -- \verb{ASCII 9}). Defaults to \verb{","}.
  }
  \item{dec}{
    Decimal separator in the \acronym{CSV}-file. Acceptable are
    \verb{"."} (period -- \verb{ASCII 46}) or \verb{","} (comma -- \verb{ASCII 44}).
    Defaults to \verb{"."}.
  }
  \item{logtrans}{
    If \code{TRUE} (default) the raw data (provided in column \verb{PK})
    will be internally log-transformed and used in the calculations. If
    \code{FALSE} the already log-transformed data (provided in the column
    \verb{logPK}) will be used in the calculations.
  }
  \item{ola}{
    Defaults to \code{FALSE}. If \code{TRUE} an outlier analysis based
    on the studentized and standardized residuals of the model estimating
    \code{CVwR} is performed.
  }
  \item{print}{
    If \code{TRUE} (default), the function prints its results to a file.
    If \code{FALSE}, returns a data.frame of results.
  }
  \item{details}{
    Defaults to \code{FALSE}. If \code{TRUE}, the function sends its
    results in 7-digits precision to a data.frame.
  }
  \item{verbose}{
    Defaults to \code{FALSE}. If \code{TRUE} the model-table is send to
    the console. If \code{ola=TRUE} additional information about outliers
    are shown.
  }
  \item{ask}{
    Defaults to \code{FALSE}. If \code{TRUE} the user will be asked whether
    an already existing result file (and if outliers are found, the box plot)
    should be overwritten.
  }
  \item{plot.bxp}{
    Only observed if \code{ola=TRUE} and at least one outlier is found.
    If \code{FALSE} (default) the box plot will be shown in the graphics
    device. If \code{TRUE} the box plot will be saved in \acronym{PNG} format
    to \code{path.out}.
  }
  \item{fence}{
    Only observed if \code{ola=TRUE}. The limit for outlier detection as
    a multiplier of the interquartile range. Defaults to 2. Less outliers
    will be detected with higher values (not recommended).
  }
  \item{data}{
    Specification of one of the internal reference datasets (\verb{rds01}
    -- \verb{rds28}). If given, \verb{path.in}, \verb{file}, \verb{set},
    and \verb{ext} are ignored. For its use see the examples.\cr
    If not given, defaults to \code{NULL} (\emph{i.e.}, import data
    from a file).
  }
  \item{option}{
    If \code{2} (default), the model will be evaluated by \verb{lme()} of
    package \verb{nlme}. The degrees of freedom of the treatment comparison
    will be equivalent to \acronym{SAS}’ \verb{DDFM=CONTAIN} and
    Phoenix WinNonlin’s \verb{Residual}.\cr
    If \code{1} or \code{3}, the model will be evaluated by \verb{lmer()} of package
    \verb{lmerTest}. With \code{1} the degrees of freedom of the treatment comparison
    will be equivalent to \acronym{SAS}’ \verb{DDFM=SATTERTHWAITE}
    and Phoenix WinNonlin’s \verb{Satterthwaite}.\cr
    \code{3} uses the Kenward-Roger approximation (the only one available in \acronym{JMP}).
  }
}

\details{
  The model for the estimation of \code{CVwR} is\cr
  \verb{   log(response) ~ sequence + subject(sequence) + period}\cr
  where all effects are fixed.\cr
  The model for the treatment comparison is\cr
  \verb{   log(response) ~ sequence + subject(sequence) + period + treatment}\cr
  where \verb{sequence}, \verb{period}, and \verb{treatment} are
  fixed effects and \verb{subject(sequence)} is a random effect.\cr\cr
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
  \item{If reference-scaling is applicable (\emph{i.e.}, \code{CVwR(\%)} >30\%):}{
    \tabular{ll}{
      \code{EL.lo(\%)} \tab lower expanded limit\cr
      \code{EL.hi(\%)} \tab upper expanded limit
    }
  }
  \item{If reference-scaling is not applicable (\emph{i.e.}, \ifelse{html}{\out{&le;}}{\eqn{\leq}}30\%):}{
    \tabular{ll}{
      \code{BE.lo(\%)} \tab conventional lower limit (\emph{i.e.}, \verb{ 80})\cr
      \code{BE.hi(\%)} \tab conventional upper limit (\emph{i.e.}, \verb{125})
    }
  }
}
  \tabular{ll}{
    \code{CI.lo(\%)} \tab lower confidence limit of the treatment comparison\cr
    \code{CI.hi(\%)} \tab upper confidence limit of the treatment comparison\cr
    \code{PE(\%)} \tab point estimate of the treatment comparison (aka \acronym{GMR})\cr
    \code{CI} \tab assessment whether the \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2
\eqn{\alpha}{alpha})} \acronym{CI} lies entirely within the acceptance range (\verb{pass|fail})\cr
    \code{GMR} \tab assessment whether the \acronym{PE} lies entirely within the \acronym{GMR}-restriction 80.00\%\enc{–}{-}125.00\% (\verb{pass|fail})\cr
    \code{BE} \tab mixed (aggregate) assessment whether the study demonstrates bioequivalence (\verb{pass|fail})\cr
    \code{log.half-width} \tab half-width of the confidence interval in log-scale
  }
If \code{ola=TRUE} and at least one outlier was detected:
  \tabular{ll}{
    \code{outlier} \tab outlying subject(s)\cr
    \code{CVwR.new(\%)} \tab intra-subject coefficient of variation of \code{R}; recalculated after exclusion of outlier(s)\cr
    \code{sw.ratio.new} \tab ratio of intrasubject variabilities of \code{T} and \code{R} after exclusion of outlier(s); full replicates only\cr
    \code{sw.ratio.new.CL} \tab upper confidence limit of \code{sw.ratio.new} (full replicates only)
  }
\itemize{
  \item{If reference-scaling is applicable (\emph{i.e.}, \code{CVwR.new(\%)} >30\%):}{
    \tabular{ll}{
      \code{EL.new.lo(\%)} \tab recalculated lower expanded limit\cr
      \code{EL.newhi(\%)} \tab recalculated upper expanded limit
    }
  }
  \item{If reference-scaling is not applicable (\emph{i.e.}, \code{CVwR.new(\%)} \ifelse{html}{\out{&le;}}{\eqn{\leq}}30\%):}{
    \tabular{ll}{
      \code{BE.new.lo(\%)} \tab conventional lower limit (\emph{i.e.}, \verb{ 80})\cr
      \code{BE.new.hi(\%)} \tab conventional upper limit (\emph{i.e.}, \verb{125})
    }
  }
}
  \tabular{ll}{
    \code{CI.new} \tab assessment whether the \ifelse{html}{\out{100(1&ndash;2&alpha;)}}{100(1–2
\eqn{\alpha}{alpha})} \acronym{CI} lies entirely within the new acceptance range (\verb{pass|fail})\cr
    \code{GMR.new} \tab assessment whether the \acronym{PE} lies entirely within the \acronym{GMR}-restriction 80.00\%\enc{–}{-}125.00\% (\verb{pass|fail})\cr
    \code{BE.new} \tab mixed (aggregate) assessment whether the study demonstrates bioequivalence (\verb{pass|fail})\cr
  }
}

\section{Warning}{
  Files may contain a commentary header. If reading from a \acronym{CSV}-file,
  \emph{each} line of the commentary header \emph{must} start with \verb{"# "}
  (hash-space -- \verb{ASCII 35-32}). If reading from an \acronym{XLS}-file
  all lines preceding the column headers are treated as a comment.
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use.
\emph{Guideline on the Investigation of Bioequivalence.}\cr
London, 20 January 2010.
\href{https://www.ema.europa.eu/en/documents/scientific-guideline/guideline-investigation-bioequivalence-rev1_en.pdf}{CPMP/EWP/QWP/1401/98 Rev. 1/ Corr **}

3\ifelse{html}{\out{<sup>rd</sup>}}{\eqn{^{rd}}} \acronym{EGA} Symposium on Bioequivalence.
\emph{Questions and Answers on the Revised \acronym{EMA} Bioequivalence Guideline.}\cr
London, 1 June 2010.
\href{https://www.medicinesforeurope.com/wp-content/uploads/2016/03/EGA_BEQ_QA_WEB_QA_1_32.pdf}{open access}

European Medicines Agency, Committee for Medicinal Products for Human Use.
\emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015.
\href{https://www.ema.europa.eu/en/documents/scientific-guideline/questions-answers-positions-specific-questions-addressed-pharmacokinetics-working-party_en.pdf}{EMA/618604/2008 Rev. 13}

European Medicines Agency, Committee for Medicinal Products for Human Use.
\emph{Guideline on the pharmacokinetic and clinical evaluation of modified release dosage forms.}\cr
London, 20 November 2014.
\href{https://www.ema.europa.eu/en/documents/scientific-guideline/guideline-pharmacokinetic-clinical-evaluation-modified-release-dosage-forms_en.pdf}{EMA/CPMP/EWP/280/96 Corr1}

World Health Organization, Prequalification Team: medicines.
\emph{Guidance Document: Application of reference-scaled criteria for AUC in bioequivalence studies conducted for submission to PQTm.}\cr
Geneva, 22 November 2018.
\href{https://extranet.who.int/prequal/sites/default/files/documents/AUC_criteria_November2018.pdf}{open access}

Food and Drug Administration, Office of Generic Drugs.
\emph{Draft Guidance on Warfarin Sodium.}\cr
Rockville, December 2012.
\href{https://www.accessdata.fda.gov/drugsatfda_docs/psg/Warfarin_Sodium_tab_09218_RC12-12.pdf}{open access}
}

\author{
Helmut \enc{Schütz}{Schuetz}, Michael Tomashevskiy, Detlew Labes
}

\note{
  The \sQuote{graphical} presentation in the result file gives the confidence
  limits with filled black squares and the point estimate as a white rhombus.
  If a confidence limit exceeds the maximum possible expansion limit, it is
  shown as a triangle. Expanded limits are given as double vertical lines.
  Unscaled limits, the \acronym{GMR} restriction, and 100\% are given with
  single vertical lines. The \sQuote{resolution} is approximatelly 0.5\% and
  therefore, not all symbols might be shown. The \acronym{CI} and \acronym{PE} take presedence
  over the limits and the expanded limits over unscaled ones.
}

\note{
  The \acronym{EMA}’s model specified in \sQuote{Method B} assumes
  equal [\emph{sic!}] intra-subject variances of test and reference (like in
  \ifelse{html}{\out{2&times;2&times;2}}{\eqn{2 \times 2 \times 2}{2x2x2}}
  trials) -- even if proven false in one of the full replicate designs (were
  \emph{both} \ifelse{html}{\out{CV<sub>wT</sub>}}{\eqn{CV_{wT}}{CVwT}} and
  \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}{CVwR}} can be estimated).
  Hence, amongst biostatisticians it is called the \dQuote{crippled model}
  because the replicative nature of the study is ignored.\cr
  The method for calculating the degrees of freedom is not specified in the
  \acronym{SAS} code provided by the \acronym{EMA} in the Q&A document. Hence,
  the default in \code{PROC MIXED}, namely \code{DDFM=CONTAIN} is applied.\cr
  For incomplete data (\emph{i.e.}, missing periods) Satterthwaite’s
  approximation of the degrees of freedom (\code{option=1}) or Kenward-Roger (\code{option=3})
  might be better choices -- if stated as such in the statistical analysis plan.\cr
  The half-width of the confidence interval in log-scale allows a comparison of methods (B
  \emph{v.s.} A) or options (2 \emph{v.s.} 1). A higher value \emph{might}
  point towards a more conservative decision. Quoting the Q&A-document:\cr
  \cite{A simple linear mixed model, which assumes identical within-subject
    variability (Method B), may be acceptable as long as results obtained
    with the two methods do not lead to different regulatory decisions.
    However, in borderline cases [\ldots] additional analysis using Method
    A might be required.}\cr
  In the provided reference datasets -- with one exception -- the conclusion
  of \acronym{BE} (based on the mixed \acronym{CI} and \acronym{GMR} criteria)
  agrees between \sQuote{Method A} and \sQuote{Method B}. However, for the
  highly incomplete dataset 14 \sQuote{Method A} was \emph{liberal} (passing
  by \acronym{ANOVA} but failing by the mixed effects model).\cr\cr
  Reference-scaling is acceptable for
  \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} (immediate release
  products) and \ifelse{html}{\out{C<sub>max,ss</sub>}}{\eqn{C_{max,ss}}},
  \ifelse{html}{\out{C<sub>&tau;,ss</sub>}}{\eqn{C_{\tau,ss}}}, and
  \ifelse{html}{\out{<sub>partial</sub>AUC}}{\eqn{_{partial}AUC}} (modified
  release products). However, quoting the \acronym{BE} guideline:\cr
    \cite{The applicant should justify that the calculated intra-subject
    variability is a reliable estimate and that it is not the result of
    outliers.}\cr
  Quoting the Q&A on the Revised EMA Bioequivalence Guideline:\cr
    \cite{\ldots a study could be acceptable if the bioequivalence
    requirements are met both including the outlier subject (using the
    scaled average bioequivalence approach and the within-subject CV with
    this subject) and after exclusion of the outlier (using the within-subject
    CV without this subject).\cr
    An outlier test is not an expectation of the medicines agencies but
    outliers could be shown by a box plot. This would allow the medicines
    agencies to compare the data between them.  }\cr\cr
  The \acronym{EMA}’s method of reference-scaling for highly
  variable drugs / drug products is currently recommended in other jurisdictions
  as well (\emph{e.g.}, the \acronym{WHO}; \acronym{ASEAN} States, Australia,
  Brazil, Egypt, the Russian Federation, the Eurasian Economic Union,
  the East African Community, New Zealand).\cr\cr
  The \acronym{WHO} accepts reference-scaling for AUC (4-period full replicate
  studies are mandatory in order to assess the variability associated with each product).
  It is an open issue how this assessment should be done. In Population Bioequivalence (\acronym{PBE})
  and Individual Bioequivalence (\acronym{IBE}) the
  \ifelse{html}{\out{<em>s<sub>wT</sub></em>}}{\eqn{s_{wT}}}/\ifelse{html}{\out{<em>s<sub>wR</sub></em>}}{\eqn{s_{wR}}}
  ratio was assessed and similar variability was concluded for a ratio
  within 0.667–1.500. However, the power of comparing variabilities
  in a study designed to demonstrate \acronym{ABE} is low. This was one of the
  reasons why why \acronym{PBE} and \acronym{IBE} were not implemented
  in regulatory practice. An alternative approach is given in the
  \acronym{FDA}’s guidance on warfarin. Variabilities are
  considered comparable if the upper confidence limit of
  \ifelse{html}{\out{<em>&sigma;<sub>wT</sub></em>}}{\eqn{\sigma_{wT}}}/\ifelse{html}{\out{<em>&sigma;<sub>wR</sub></em>}}{\eqn{\sigma_{wR}}} is less than or equal to 2.5.\cr\cr
  \strong{Program offered for Use without any Guarantees and Absolutely No Warranty. No Liability is accepted for any Loss and Risk to Public Health Resulting from Use of this R-Code.}
}

\seealso{
  \tabular{ll}{
    \code{\link{method.A}} \tab evaluation by a fixed effects model (\acronym{ANOVA})\cr
    \code{\link{ABE}} \tab evaluation for conventional (unscaled) Average Bioeqivalence
  }
}

\examples{
\donttest{
# Importing from a CSV-file, using most of the defaults: variable
# separator comma, decimal separator period, no outlier-analyis,
# print to file.
# Note: You must adapt the path-variables. The example reads from
# the data provided by the library. Write-permissions must be granted
# for 'path.out' in order to save the result file. If 'path.out' is
# not specified, results are written to the home folder.
path.in <- paste0(find.package("replicateBE"), "/extdata/")
method.B(path.in = path.in, file = "DS", set = "01", ext = "csv")
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
#
# Internal reference dataset 01 used and results to the home
# folder. Additional outlier-analyis and box plot saved as PNG
# (accidental overwriting prevented).
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
#
# Same dataset. Show information about outliers and the model-table.
method.B(ola = TRUE, verbose = TRUE, data = rds01)}
# data.frame of results (7-digits precision) shown in the console.
x <- method.B(ola = TRUE, print = FALSE, details = TRUE, data = rds01)
print(x, row.names = FALSE)

# Compare all methods for the highly incomplete dataset rds18.
a  <- method.A(print=FALSE, details=TRUE, data=rds18)
b2 <- method.B(print=FALSE, details=TRUE, data=rds18, option=2) # GL
b1 <- method.B(print=FALSE, details=TRUE, data=rds18, option=1) # Satt.
b3 <- method.B(print=FALSE, details=TRUE, data=rds18, option=3) # KR
cs <- c(2, 10, 17:18)
x  <- rbind(a[, cs], b2[, cs], b1[, cs], b3[, cs])
x[, 3:4] <- round(x[, 3:4], 4)
x$detail <- c("GL (fixed)", "GL (random)",
              "Satterthwaite", "Kenward-Roger")
print(x, row.names=FALSE)

# Compare Method B with Method A for all reference datasets.
\donttest{
ds <- substr(grep("rds", unname(unlist(data(package="replicateBE"))),
                  value=TRUE), start=1, stop=5)
for (i in seq_along(ds)) {
  A <- method.A(print=FALSE, details=TRUE, data=eval(parse(text=ds[i])))$BE
  B <- method.B(print=FALSE, details=TRUE, data=eval(parse(text=ds[i])))$BE
  r <- paste0("A ", A, ", B ", B, " \u2013 ")
  cat(paste0(ds[i], ":"), r)
  if (A == B) {
    cat("Methods A and B agree.\n")
  } else {
    if (A == "fail" & B == "pass") {
      cat("Method A is conservative.\n")
    } else {
      cat("Method B is conservative.\n")
    }
  }
}}
# should give
#   rds01: A pass, B pass – Methods A and B agree.
#   ...
#   rds04: A fail, B fail – Methods A and B agree.
#   ...
#   rds14: A pass, B fail – Method B is conservative.
#   ...
}
