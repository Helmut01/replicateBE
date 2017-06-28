\encoding{utf-8}
\name{RTTR.TRRT}
\alias{RTTR.TRRT}
\alias{rds05}
\alias{rds11}
\alias{rds16}
\docType{data}
\title{
Reference Data Sets for RTTR|TRRT Replicate Designs
}

\description{
Data sets from the literature to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
  \tabular{crcl}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
    \code{rds05} \tab 26 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
    \code{rds11} \tab 37 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
    \code{rds16} \tab 38 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\source{
  \tabular{cll}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
    \code{rds05} \tab \ifelse{html}{\out{Metzler&nbsp;&amp;&nbsp;Shumaker}}{Metzler~&~Shumaker} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in the Appendix.\cr
    \code{rds11} \tab Hauschke \emph{et al.} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in Table 9.6.\cr
    \code{rds16} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Drug 14a.
  }
}

\format{
  \itemize{
    \item Reference Data Set 05 (\code{rds05})\cr
    Data set given in the Appendix of Metzler & Shumaker. 26 subjects.
      \describe{
          A data frame with 72 observations on the following 5 variables:
          \tabular{ll}{
            \code{subject} \tab a factor with 26 levels: 1, 2, \ldots, 26\cr
            \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
            \code{sequence} \tab a factor with 2 levels: RTTR, TRRT\cr
            \code{treatment} \tab a factor with 2 levels: R, T\cr
            \code{PK} a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
          }
          Balanced (13 subjects in both sequences) and complete. No outliers.\cr\cr
          In the source evaluated by SAS for the FDA’s mixed effects model (\sQuote{Method C}; \strong{not} acceptable for the \acronym{EMA}). Reported results:
            \tabular{ll}{
              \code{CVwR} \tab 5.47\%\cr
              \code{CVwT} \tab 6.75\%\cr
              \code{PE} \tab 107.90\%\cr
              \code{90\% CI} \tab 103.66\% – 112.2\%
            }
       }

    \item Reference Data Set 11 (\code{rds11})\cr
    Data set given in Table 9.6 of Hauschke \emph{et al.} 37 subjects.
      \describe{
          A data frame with 148 observations on the following 5 variables:
          \tabular{ll}{
            \code{subject} \tab a factor with 37 levels: 1, 2, \ldots, 37\cr
            \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
            \code{sequence} \tab a factor with 2 levels: RTTR, TRRT\cr
            \code{treatment} \tab a factor with 2 levels: R, T\cr
            \code{PK} a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
          }
          Unbalanced (19 subjects in sequence RTTR and 18 in TRRT) and complete. No outliers.\cr\cr
          In the source evaluated by the FDA's mixed effects model (\sQuote{Method C}; \strong{not} acceptable for the \acronym{EMA}). Reported results:
            \tabular{ll}{
              \code{PE} \tab 90.0\%\cr
              \code{90\% CI} \tab 79.6\% – 101.7\%
            }
      }

    \item Reference Data Set 16 (\code{rds16})\cr
    Data set given by the \acronym{FDA}. 38 subjects.
      \describe{
          A data frame with 152 observations on the following 5 variables:
          \tabular{ll}{
            \code{subject} \tab a factor with 38 levels: 1, 2, \ldots, 38\cr
            \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
            \code{sequence} \tab a factor with 2 levels: RTTR, TRRT\cr
            \code{treatment} \tab a factor with 2 levels: R, T\cr
            \code{PK} a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
          }
          Unbalanced (20 subjects in sequence RTTR and 18 in TRRT) and complete. No outliers.
      }
    }
}

\references{
Metzler CM, Shumaker RC. \emph{The Phenytoin Trial is a Case Study of \sQuote{Individual} Bioequivalence.}\cr
Drug Inf J. 1998;32:1063--72

Hauschke D, Steinijans VW, Pigeot I. \emph{Bioequivalence Studies in Drug Development.}\cr
Chichester: John Wiley; 2007. p216.

\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.}\cr
Rockville, 1997. \href{https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip}{bioequivalence study files}
}

\examples{
str(rds05)
head(rds11, 8)
}

\keyword{datasets}
