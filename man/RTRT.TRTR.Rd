\encoding{utf-8}
\name{RTRT.TRTR}
\docType{data}
\title{
Reference Data Sets for RTRT|TRTR Designs
}

\description{
Data sets from the literature, edited, or obtained by simulations to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\details{
   \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
  \code{rds01} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds06} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds08} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds09} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds12} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds13} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds14} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds15} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds18} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds19} \tab 61 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds20} \tab 61 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds21} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\format{
  \itemize{
    \item Reference Data Set 01 (\code{rds01})\cr
    77 subjects.
    \describe{
      A data frame with 298 observations on the following 6 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (3 missings in sequence RTRT and 7 in sequence TRTR). Missings / period: 0/1, 1/2, 7/3, 2/4. Two extreme outliers (subjects 45 and 52) after the reference treatment in sequence RTRT.\cr
In the source evaluated by \acronym{SAS} v9.1 for \acronym{ABEL}. Reported results:
    \tabular{ll}{
      \code{CVwR} \tab 47.0\%\cr
      \code{PE} \tab 115.66\% (Method A)\cr\tab 115.73\% (Method B)\cr
      \code{90\% CI} \tab 107.11\% – 124.89\% (Method A)\cr\tab 107.17\% – 124.97\% (Method B)
    }
  }

    \item Reference Data Set 06 (\code{rds06})\cr
    Based on \code{rds01}. 77 subjects. Responses of T and R switched.\cr
    \describe{
      A data frame with 298 observations on the following 6 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Unbalanced (39 subjects in sequence RTRT and 38 in TRTR) and incomplete (7 missings in sequence RTRT and 3 in sequence TRTR). Missings / period: 0/1, 1/2, 7/3, 2/4. No outliers.
    }

    \item Reference Data Set 08 (\code{rds08})\cr
    Simulated with \ifelse{html}{\out{C<sub>wT</sub>}}{\eqn{C\textsubscript}{wT}} = 70\%, \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} = 80\%, \ifelse{html}{\out{C<sub>bT</sub>}}{\eqn{C\textsubscript}{bT}} = \ifelse{html}{\out{C<sub>bT</sub>}}{\eqn{C\textsubscript}{bT}} = 150\%, GMR = 0.85. 222 subjects.\cr
    \describe{
      A data frame with 888 observations on the following 5 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 222 levels: 1, 2, \ldots, 222\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
      }
      Balanced (222 subjects in both sequences) and complete. No outliers.\cr
The extreme sample size results from high variability, an assumed true GMR 0.85, and target power 90\%.
    }

    \item Reference Data Set 09 (\code{rds09})\cr
    Based on \code{rds08}. Wide numeric range (data of last 37 subjects multiplied by 1,000,000). 222 subjects.\cr
    \describe{
      A data frame with 888 observations on the following 5 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 222 levels: 1, 2, \ldots, 222\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
      }
      Balanced (222 subjects in both sequences) and complete. No outliers.
    }

    \item Reference Data Set 12 (\code{rds12})\cr
    Data set simulated with extreme intrasubject variability. 77 subjects.\cr
    \describe{
      A data frame with 298 observations on the following 6 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (3 missings in sequence RTRT and 7 in sequence TRTR). Missings / period: 0/1, 1/2, 7/3, 2/4. No outliers.
    }

    \item Reference Data Set 13 (\code{rds13})\cr
    Based on \code{rds08}. Highly incomplete (approx. 50\% of period 4 data deleted). 222 subjects.\cr
    \describe{
      A data frame with 776 observations on the following 5 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 222 levels: 1, 2, \ldots, 222\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
      }
      Balanced (111 subjects in both sequences) and incomplete (56 missings in both sequences). Missings / period: 0/1, 0/2, 0/3, 112/4. No outliers.
    }

    \item Reference Data Set 14 (\code{rds14})\cr
    Data set simulated with high variability and dropouts as a hazard function growing with period. 77 subjects.
    \describe{
      A data frame with 273 observations on the following 6 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (17 missings in sequence RTRT and 18 in sequence TRTR). Missings / period: 0/1, 4/2, 12/3, 19/4. No outliers.
    }

    \item Reference Data Set 15 (\code{rds15})\cr
    Based on \code{ref08}. Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'}). 222 subjects.
    \describe{
     A data frame with 888 observations (112 \code{NA}) on the following 5 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 222 levels: 1, 2, \ldots, 222\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
      }
      Balanced (111 subjects in both sequences) and incomplete (56 missings in both sequences). Missings / period: 0/1, 0/2, 0/3, 112/4. No outliers.
    }

    \item Reference Data Set 18 (\code{rds18})\cr
    Data set based on \code{rds14}. Removed T data of subjects 63–78. 77 subjects.
    \describe{
     A data frame with 245 observations on the following 6 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (31 missings in sequence RTRT and 32 in sequence TRTR). Missings / period: 8/1, 12/2, 18/3, 25/4. No outliers.
    }

    \item Reference Data Set 19 (\code{rds19})\cr
    Data set based on \code{rds18}. Removed data of subjects 63–78. 61 subjects.
    \describe{
     A data frame with 216 observations on the following 6 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 61 levels: 1, 2, \ldots, 62\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Unbalanced (30 subjects in sequence RTRT and 31 in TRTR) and incomplete (14 missings in both sequences). Missings / period: 0/1, 4/2, 9/3, 15/4. No outliers.
    }

    \item Reference Data Set 20 (\code{rds20})\cr
    Data set based on \code{rds19}. Outlier of R (subject 1) introduced: original value ×100). 61 subjects.
    \describe{
     A data frame with 216 observations on the following 6 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 61 levels: 1, 2, \ldots, 62\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Unbalanced (30 subjects in sequence RTRT and 31 in TRTR) and incomplete (14 missings in both sequences). Missings / period: 0/1, 4/2, 9/3, 15/4. 1 outlier.
    }

    \item Reference Data Set 21 (\code{rds21})\cr
    Based on \code{ds01}. 77 subjects. One extreme result of subjects 45 & 52 set to NA.
    \describe{
     A data frame with 298 observations (2 NA) on the following 6 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 61 levels: 1, 2, \ldots, 62\cr
      \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
      \code{sequence} \tab a factor with 2 levels: RTRT, TRTR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (5 missings in sequence RTRT and 7 in sequence TRTR). Missings / period: 1/1, 1/2, 8/3, 2/4. No outliers.
    }
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds01} \tab EMA, CHMP \tab Data set I.\cr
  \code{rds06} \tab \code{rds01} edited \tab T and R switched.\cr
  \code{rds08} \tab R \tab Large simulated data set with slight heteroscedasticity.\cr
  \code{rds09} \tab \code{rds08} \tab Wide numeric range (data of last 37 subjects multiplied by 1,000,000).\cr
  \code{rds12} \tab Phoenix NLME \tab Simulated data set with extreme intrasubject variability.\cr
  \code{rds13} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data deleted).\cr
  \code{rds14} \tab Phoenix NLME \tab Simulated data set with high intrasubject variability and number of dropouts increasing with period.\cr
  \code{rds15} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'}).\cr
  \code{rds18} \tab \code{rds14} edited \tab Removed T data of subjects 63–78.\cr
  \code{rds19} \tab \code{rds18} edited \tab Removed data of subjects 63–78.\cr
  \code{rds20} \tab \code{rds19} edited \tab Outlier of R (subject 1) introduced: original value ×100.\cr
  \code{rds21} \tab \code{rds01} edited \tab One extreme result of subjects 45 & 52 set to NA.
  }
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes), Michael Tomashevskiy (simulations in Phoenix/NLME)
}

\examples{
str(rds01)
}

\keyword{datasets}
