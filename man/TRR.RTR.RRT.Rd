\encoding{utf-8}
\name{TRR.RTR.RRT}
\alias{TRR.RTR.RRT}
\alias{rds02}
\alias{rds04}
\alias{rds07}
\alias{rds30}
\docType{data}
\title{
Reference Datasets for TRR|RTR|RRT (partial) Replicate Designs
}

\description{
Datasets from the public domain or simulated to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\format{
  \itemize{
    \item Reference Dataset 02\cr
    24 subjects.\cr
    Balanced (eight subjects in each of the three sequences) and complete (no missing data). No outliers.\cr
    A data frame with 72 observations on the following 6 variables:
      \describe{
          \item{rds02}{
            \tabular{ll}{
              \code{subject} \tab a factor with 24 levels: 1, 2, \ldots, 24\cr
              \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
              \code{sequence} \tab a factor with 3 levels: TRR, RTR, RRT\cr
              \code{treatment} \tab a factor with 2 levels: T, R\cr
              \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})\cr
              \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
            }
          }
          In the source evaluated by \acronym{SAS} v9.1 for \acronym{ABEL}. Reported results:
          \item{SAS Proc GLM}{
            \tabular{ll}{
              \code{CVwR} \tab 11.2\%\cr
              \code{PE} \tab 102.26\% (Method A and B)\cr
              \code{90\% CI} \tab 97.32\% – 107.46\% (Method A and B)
            }
          }
       }

    \item Reference Dataset 04\cr
    Data set of Table II given by Patterson & Jones. 51 subjects.\cr
    Balanced (17 subjects in each of the three sequences) and complete. No outliers.\cr
    A data frame with 153 observations on the following 5 variables:
      \describe{
          \item{rds04}{
            \tabular{ll}{
              \code{subject} \tab a factor with 51 levels: 1, 2, \ldots, 56\cr
              \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
              \code{sequence} \tab a factor with 3 levels: TRR, RTR, RRT\cr
              \code{treatment} \tab a factor with 2 levels: T, R\cr
              \code{PK} \tab a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
            }
          }
          In the source evaluated by \acronym{SAS} with the \acronym{FDA}’s mixed effects model (termed \sQuote{Method C} by the \acronym{EMA}; not compatible with the guideline). Reported results:
          \item{SAS Proc MIXED}{
            \tabular{ll}{
              \code{CVwR} \tab 61\%\cr
              \code{PE} \tab 137\%\cr
              \code{90\% CI} \tab 119\% – 159\%
            }
          }
      }

    \item Reference Dataset 07\cr
    Simulated with \ifelse{html}{\out{CV<sub>wT</sub>}}{\eqn{CV_{wT}}} = \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} = 35\%, GMR 0.90. 360 subjects.\cr
    Balanced (120 subjects in each of the three sequences) and complete. No outliers.\cr
    A data frame with 1,080 observations on the following 5 variables:
      \describe{
          \item{rds07}{
            \tabular{ll}{
              \code{subject} \tab a factor with 360 levels: 1, 2, \ldots, 360\cr
              \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
              \code{sequence} \tab a factor with 3 levels: TRR, RTR, RRT\cr
              \code{treatment} \tab a factor with 2 levels: T, R\cr
              \code{PK} \tab a numeric vector of pharmacokinetic responses (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
            }
          }
      }

    \item Reference Dataset 30\cr
    Simulated with heteroscedasticity (\ifelse{html}{\out{CV<sub>wT</sub> = 14\%, CV<sub>wR</sub> = 28\%, CV<sub>bT</sub> = 28\%, CV<sub>bR</sub> = 56\%}}{\eqn{CCV_{wT}=14\%,\:CV_{wR}=28\%,\:CV_{bT}=28\%,\:CV_{bR}=56\%}}), GMR = 0.90. 12 subjects. 14 subjects.\cr
    Imbalanced (six subjects in sequence TRR, five in RTR, and three RRT) and incomplete (two missings in sequences TRR and RTR and three in sequence RRT). Missings / period: 0/1, 0/2, 7/3. No outliers.\cr
    A data frame with 35 observations on the following 5 variables:
      \describe{
          \item{rds30}{
            \tabular{ll}{
              \code{subject} \tab a factor with 14 levels: 1, 2, \ldots, 39\cr
              \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
              \code{sequence} \tab a factor with 3 levels: TRR, RTR, RRT\cr
              \code{treatment} \tab a factor with 2 levels: T, R\cr
              \code{PK} \tab a numeric vector of pharmacokinetic responses (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
            }
          }
      }
   }
}

\details{
   \tabular{crcl}{
      Dataset \tab N \tab \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} (\%) \tab Evaluation\cr
      \code{rds02} \tab 24 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
      \code{rds04} \tab 51 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
      \code{rds07} \tab 360 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
      \code{rds30} \tab 14 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RRT", "RTR", "TRR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The package follows this convention.
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes)
}

\source{
  \tabular{cll}{
    Dataset \tab Origin \tab Description\cr
    \code{rds02} \tab EMA \tab Annex III.\cr
    \code{rds04} \tab Patterson & Jones \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data of Table II.\cr
    \code{rds07} \tab R \tab Large simulated data set with homoscedasticity.\cr
    \code{rds30} \tab R \tab Simulated with heteroscedasticity; imbalanced and incomplete.
  }
}

\references{
European Medicines Agency. London, 21 September 2016. \href{https://www.ema.europa.eu/en/documents/other/31-annex-i-statistical-analysis-methods-compatible-ema-bioequivalence-guideline_en.pdf}{\emph{Annex I}}, \href{https://www.ema.europa.eu/en/documents/other/statistical-method-equivalence-studies-annex-iii_en.pdf}{\emph{Annex III}}.

Patterson SD, Jones B. \emph{Viewpoint: observations on scaled average bioequivalence.} Pharm Stat. 2012; 11(1): 1--7. \doi{10.1002/pst.498}
}

\examples{
str(rds02)
row <- c(10:12, 1:3, 16:18)
rds02[row, ]
summary(rds02[2:6])
}

\keyword{datasets}
