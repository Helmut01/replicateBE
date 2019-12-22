\encoding{utf-8}
\name{TRTR.RTRT}
\alias{TRTR.RTRT}
\alias{rds01}
\alias{rds06}
\alias{rds08}
\alias{rds09}
\alias{rds12}
\alias{rds13}
\alias{rds14}
\alias{rds15}
\alias{rds18}
\alias{rds19}
\alias{rds20}
\alias{rds21}
\alias{rds25}
\alias{rds26}
\alias{rds29}
\docType{data}
\title{
Reference Datasets for TRTR|RTRT Designs
}

\description{
Datasets from the public domain, edited, or obtained by simulations to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\format{
  \itemize{
    \item Reference dataset 01 (\code{rds01})\cr
    77 subjects.\cr
    Unbalanced (39 subjects in sequence TRTR and 38 in RTRT) and incomplete (seven missings in sequence TRTR and three in sequence RTRT). Missings / period: 0/1, 1/2, 7/3, 2/4. Two outliers (subjects 45 and 52) in sequence RTRT.\cr
    A data frame with 298 observations on the following 6 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})\cr
          \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
        }
      }
      In the source evaluated by \acronym{SAS} v9.1 for \acronym{ABEL}. Reported results:
      % 2.1.1 Text between or after list items is discouraged.
      \item{}{
      \tabular{ll}{
        \code{CVwR} \tab 47.0\%\cr
        \code{PE} \tab 115.66\% (Method A)\cr\tab 115.73\% (Method B)\cr
        \code{90\% CI} \tab 107.11\% \enc{–}{-} 124.89\% (Method A)\cr\tab 107.17\% \enc{–}{-} 124.97\% (Method B)
      }
    }
  }

    \item Reference dataset 06 (\code{rds06})\cr
    Based on \code{rds01}. 77 subjects. Responses of T and R switched.\cr
    Unbalanced (39 subjects in sequence TRTR and 38 in RTRT) and incomplete (seven missings in sequence TRTR and three in sequence RTRT). Missings / period: 0/1, 1/2, 7/3, 2/4. No outliers.\cr
    A data frame with 298 observations on the following 6 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
      \item{}{}
    }

    \item Reference dataset 08 (\code{rds08})\cr
    Simulated with slight heteroscedasticity (\ifelse{html}{\out{CV<sub>wT</sub>}}{\eqn{CV_{wT}}} = 70\%, \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} = 80\%), \ifelse{html}{\out{CV<sub>bT</sub>}}{\eqn{CV_{bT}}} = \ifelse{html}{\out{CV<sub>bR</sub>}}{\eqn{CV_{bR}}} = 150\%, GMR = 0.85. 222 subjects.\cr
    Balanced (222 subjects in both sequences) and complete. No outliers.\cr
The extreme sample size results from high variability, an assumed true GMR 0.85, and target power 90\%.\cr
    A data frame with 888 observations on the following 5 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 222 levels: 1, 2, \ldots, 222\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 09 (\code{rds09})\cr
    Based on \code{rds08}. Wide numeric range (data of last 37 subjects multiplied by 1,000,000). 222 subjects.\cr
    Balanced (222 subjects in both sequences) and complete. No outliers.\cr
    A data frame with 888 observations on the following 5 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 222 levels: 1, 2, \ldots, 222\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 12 (\code{rds12})\cr
    Simulated with extreme intra- and intersubject variability, GMR = 1.6487. 77 subjects.\cr
    Unbalanced (39 subjects in sequence TRTR and 38 in RTRT) and incomplete (seven missings in sequence TRTR and three in sequence RTRT). Missings / period: 0/1, 1/2, 7/3, 2/4. No outliers.\cr
    A data frame with 298 observations on the following 6 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 13 (\code{rds13})\cr
    Based on \code{rds08}. Highly incomplete (approx. 50\% of period 4 data deleted). 222 subjects.\cr
    Balanced (111 subjects in both sequences) and incomplete (56 missings in both sequences). Missings / period: 0/0, 0/0, 0/0, 112/4. No outliers.\cr
    A data frame with 776 observations on the following 5 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 222 levels: 1, 2, \ldots, 222\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 14 (\code{rds14})\cr
    Simulated with high variability, GMR = 1. Dropouts as a hazard function growing with period. 77 subjects.\cr
    Unbalanced (39 subjects in sequence TRTR and 38 in RTRT) and incomplete (18 missings in sequence TRTR and 17 in sequence RTRT). Missings / period: 0/1, 4/2, 12/3, 19/4. No outliers.\cr
    A data frame with 273 observations on the following 6 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 15 (\code{rds15})\cr
    Based on \code{ref08}. Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'}). 222 subjects.\cr
    Balanced (111 subjects in both sequences) and incomplete (56 missings in both sequences). Missings / period: 0/1, 0/2, 0/3, 112/4. No outliers.\cr
    A data frame with 888 observations (112 \code{NA}) on the following 5 variables
    \describe{
     \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 222 levels: 1, 2, \ldots, 222\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 18 (\code{rds18})\cr
    Data set based on \code{rds14}. Removed T data of subjects 63–78. 77 subjects.\cr
    Unbalanced (39 subjects in sequence TRTR and 38 in RTRT) and incomplete (32 missings in sequence TRTR and 31 in sequence RTRT). Missings / period: 8/1, 12/2, 18/3, 25/4. No outliers.\cr
    A data frame with 245 observations on the following 6 variables:
    \describe{
     \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 19 (\code{rds19})\cr
    Data set based on \code{rds18}. Removed data of subjects 63–78. 61 subjects.\cr
    Unbalanced (31 subjects in sequence TRTR and 30 in RTRT) and incomplete (14 missings in both sequences). Missings / period: 0/1, 4/2, 9/3, 15/4. Two outliers (subjects 18 and 51 in sequence RTRT).\cr
    A data frame with 216 observations on the following 6 variables:
    \describe{
     \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 61 levels: 1, 2, \ldots, 62\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 20 (\code{rds20})\cr
    Data set based on \code{rds19}. Extreme outlier of R (subject 1) introduced: original value ×100). 61 subjects.\cr
    Unbalanced (31 subjects in sequence TRTR and 30 in RTRT) and incomplete (14 missings in both sequences). Missings / period: 0/1, 4/2, 9/3, 15/4. Two outliers (subjects 1 and 51 in sequence RTRT).\cr
    A data frame with 216 observations on the following 6 variables:
    \describe{
     \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 61 levels: 1, 2, \ldots, 62\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 21 (\code{rds21})\cr
    Based on \code{ds01}. 77 subjects. One extreme result of subjects 45 & 52 set to NA.\cr
    Unbalanced (39 subjects in sequence TRTR and 38 in RTRT) and incomplete (seven missings in sequence TRTR and five in sequence RTRT). Missings / period: 1/1, 1/2, 8/3, 2/4. No outliers.\cr
    A data frame with 298 observations (2 NA) on the following 6 variables:
    \describe{
     \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 61 levels: 1, 2, \ldots, 62\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 25 (\code{rds25})\cr
    Simulated with heteroscedasticity (\ifelse{html}{\out{CV<sub>wT</sub>}}{\eqn{CV_{wT}}} = 50\%, \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} = 80\%), \ifelse{html}{\out{CV<sub>bT</sub>}}{\eqn{CV_{bT}}} = \ifelse{html}{\out{CV<sub>bR</sub>}}{\eqn{CV_{bR}}} = 130\%, GMR = 0.85. 70 subjects.\cr
    Balanced (70 subjects in both sequences) and complete. No outliers.\cr
    A data frame with 280 observations on the following 5 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 70 levels: 1, 2, \ldots, 70\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }

    \item Reference dataset 26 (\code{rds26})\cr
    54 subjects.\cr
    Balanced (27 subjects in both sequences) and incomplete (two missings in both sequences). Missings / period: 0/1, 0/2, 2/3, 2/4. One outlier (subject 49) in sequence RTRT.\cr
    A data frame with 216 observations on the following 5 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 54 levels: 1, 2, \ldots, 57\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
      In the source evaluated by \acronym{SAS} for \acronym{ABEL}. Reported results (Method A):
      \item{}{
        \tabular{ll}{
          \code{CVwR} \tab 60.25\%\cr
          \code{PE} \tab 151.3\%\cr
          \code{90\% CI} \tab 133.5\% \enc{–}{-} 171.4\%
        }
      }
    }

    \item Reference dataset 29 (\code{rds29})\cr
    Simulated with heteroscedasticity (\ifelse{html}{\out{CV<sub>wT</sub>}}{\eqn{CV_{wT}}} = 14\%, \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} = 28\%), \ifelse{html}{\out{CV<sub>bT</sub>}}{\eqn{CV_{bT}}} = 28\%, \ifelse{html}{\out{CV<sub>bR</sub>}}{\eqn{CV_{bR}}} = 56\%, GMR = 0.90. 12 subjects.\cr
    Imbalanced (five subjects in sequence TRTR and seven in sequence RTRT) and incomplete (three missings in sequence TRTR and four in sequence RTRT). Missings / period: 0/1, 1/2, 2/3, 4/4. One outlier (subject 11) in sequence RTRT.\cr
    A data frame with 41 observations on the following 5 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 12 levels: 1, 2, \ldots, 20\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TRTR, RTRT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
        }
      }
    }
  }
}

\details{
  \tabular{crcl}{
    Dataset \tab N \tab \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} (\%) \tab Evaluation\cr
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
    \code{rds21} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
    \code{rds25} \tab 70 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
    \code{rds26} \tab 54 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
    \code{rds29} \tab 12 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RTRT", "TRTR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The library follows this convention.
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes), Michael Tomashevskiy (simulations in Phoenix NLME)
}

\source{
  \tabular{cll}{
    Dataset \tab Origin \tab Description\cr
    \code{rds01} \tab EMA \tab Annex II.\cr
    \code{rds06} \tab \code{rds01} edited \tab T and R switched.\cr
    \code{rds08} \tab R \tab Large simulated data set with slight heteroscedasticity.\cr
    \code{rds09} \tab \code{rds08} \tab Wide numeric range (data of last 37 subjects multiplied by 1,000,000).\cr
    \code{rds12} \tab Phoenix NLME \tab Simulated with extreme intra- and intersubject variability.\cr
    \code{rds13} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data deleted).\cr
    \code{rds14} \tab Phoenix NLME \tab Simulated with high intra- and intersubject and number of dropouts increasing with period.\cr
    \code{rds15} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'}).\cr
    \code{rds18} \tab \code{rds14} edited \tab Removed T data of subjects 63–78.\cr
    \code{rds19} \tab \code{rds18} edited \tab Removed data of subjects 63–78.\cr
    \code{rds20} \tab \code{rds19} edited \tab Outlier of R (subject 1) introduced: original value ×100.\cr
    \code{rds21} \tab \code{rds01} edited \tab One extreme result of subjects 45 & 52 set to NA.\cr
    \code{rds25} \tab R \tab Simulated with heteroscedasticity.\cr
    \code{rds26} \tab Patterson & Jones \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data given in Tables 4.40 and 4.31.\cr
    \code{rds29} \tab R \tab Simulated with heteroscedasticity; imbalanced and incomplete.
  }
}

\references{
European Medicines Agency. London, 21 September 2016. \href{https://www.ema.europa.eu/en/documents/other/31-annex-i-statistical-analysis-methods-compatible-ema-bioequivalence-guideline_en.pdf}{\emph{Annex I}}, \href{https://www.ema.europa.eu/en/documents/other/31-annex-ii-statistical-analysis-bioequivalence-study-example-data-set_en.pdf}{\emph{Annex II}}.

Patterson SD, Jones B. \emph{Bioequivalence and Statistics in Clinical Pharmacology.} Boca Raton: CRC Press; 2\ifelse{html}{\out{<sup>nd</sup>}}{\eqn{^{nd}}} edition 2016. p105--6.
}

\examples{
str(rds01)
summary(rds01[2:6])
}

\keyword{datasets}
