\encoding{utf-8}
\name{TRR.RTR.RRT}
\alias{TRR.RTR.RRT}
\alias{rds02}
\alias{rds04}
\alias{rds07}
\docType{data}
\title{
Reference Data Sets for TRR|RTR|RRT (partial) Replicate Designs
}

\description{
Data sets from the literature or edited to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
   \tabular{crcl}{
      \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
      \code{rds02} \tab 24 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
      \code{rds04} \tab 51 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
      \code{rds07} \tab 360 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\source{
  \tabular{cll}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
    \code{rds02} \tab EMA, CHMP \tab Data set II.\cr
    \code{rds04} \tab \ifelse{html}{\out{Patterson&nbsp;&amp;&nbsp;Jones}}{Patterson~&~Jones} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Table II.\cr
    \code{rds07} \tab R \tab Large simulated data set with homoscedasticity.
  }
}

\format{
  \itemize{
    \item Reference Data Set 02 (\code{rds02})\cr
    Data set II of the EMA’s Q&A document. 24 subjects.
      \describe{
          A data frame with 72 observations on the following 6 variables:
          \tabular{ll}{
            \code{subject} \tab a factor with 24 levels: 1, 2, \ldots, 24\cr
            \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
            \code{sequence} \tab a factor with 3 levels: TRR, RTR, RRT\cr
            \code{treatment} \tab a factor with 2 levels: T, R\cr
            \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
            \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
          }
          Balanced (eight subjects in each of the three sequences) and complete (no missing data). No outliers.\cr\cr
          In the source evaluated by \acronym{SAS} v9.1 for \acronym{ABEL}. Reported results:
          \tabular{ll}{
            \code{CVwR} \tab 11.2\%\cr
            \code{PE} \tab 102.26\% (Method A)\cr\tab 102.26\% (Method B)\cr
            \code{90\% CI} \tab 97.32\% – 107.46\% (Method A)\cr\tab 97.32\% – 107.46\% (Method B)
          }
       }

    \item Reference Data Set 04 (\code{rds04})\cr
    Data set of Table II given by Patterson & Jones. 51 subjects.
      \describe{
          A data frame with 153 observations on the following 5 variables:
          \tabular{ll}{
            \code{subject} \tab a factor with 51 levels: 1, 2, \ldots, 56\cr
            \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
            \code{sequence} \tab a factor with 3 levels: TRR, RTR, RRT\cr
            \code{treatment} \tab a factor with 2 levels: T, R\cr
            \code{PK} \tab a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
          }
          Balanced (17 subjects in each of the three sequences) and complete. No outliers.\cr\cr
          In the source evaluated by SAS 9.1 for the FDA’s mixed effects model (\sQuote{Method C}; \strong{not} acceptable for the \acronym{EMA}). Reported results:
            \tabular{ll}{
              \code{CVwR} \tab 61\%\cr
              \code{PE} \tab 137\%\cr
              \code{90\% CI} \tab 119\% – 159\%
            }
      }

    \item Reference Data Set 07 (\code{rds07})\cr
    Simulated with \ifelse{html}{\out{C<sub>wT</sub>}}{\eqn{C\textsubscript}{wT}} = \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} = 35\%, GMR 0.90. 360 subjects.
      \describe{
          A data frame with 1,080 observations on the following 5 variables:
          \tabular{ll}{
            \code{subject} \tab a factor with 360 levels: 1, 2, \ldots, 360\cr
            \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
            \code{sequence} \tab a factor with 3 levels: TRR, RTR, RRT\cr
            \code{treatment} \tab a factor with 2 levels: T, R\cr
            \code{PK} \tab a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
          }
          Balanced (120 subjects in each of the three sequences) and complete. No outliers.
      }
  }
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}

Patterson SD, Jones B. \emph{Viewpoint: observations on scaled average bioequivalence.}\cr
Pharm Stat. 2012;11(1):1--7. \doi{10.1002/pst.498}
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RRT", "RTR", "TRR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The library follows this convention.
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes)
}

\examples{
str(rds02)
row <- c(10:12, 1:3, 16:18)
rds02[row, ]
summary(rds02[2:6])
}

\keyword{datasets}
