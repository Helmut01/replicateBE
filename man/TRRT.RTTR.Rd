\encoding{utf-8}
\name{TRRT.RTTR}
\alias{TRRT.RTTR}
\alias{rds05}
\alias{rds11}
\alias{rds16}
\docType{data}
\title{
Reference Datasets for TRRT|RTTR Replicate Designs
}

\description{
Datasets from the public domain to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\format{
  \itemize{
    \item Reference Dataset 05 (\code{rds05})\cr
    26 subjects.
      \describe{
          \item{A data frame with 104 observations on the following 5 variables:}{
            \tabular{ll}{
              \code{subject} \tab a factor with 26 levels: 1, 2, \ldots, 26\cr
              \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
              \code{sequence} \tab a factor with 2 levels: TRRT, RTTR\cr
              \code{treatment} \tab a factor with 2 levels: T, R\cr
              \code{PK} \tab a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
            }
          }
          \item{Balanced (13 subjects in both sequences) and complete. No outliers.\cr\cr
          In the source evaluated by SAS with the FDA’s mixed effects model (termed \sQuote{Method C} by the \acronym{EMA}; not compatible with the guideline). Reported results:}{
            \tabular{ll}{
              \code{CVwR} \tab 5.47\%\cr
              \code{CVwT} \tab 6.75\%\cr
              \code{PE} \tab 107.90\%\cr
              \code{90\% CI} \tab 103.66\% – 112.2\%
            }
          }
       }

    \item Reference Dataset 11 (\code{rds11})\cr
    37 subjects.
      \describe{
          \item{A data frame with 148 observations on the following 5 variables:}{
            \tabular{ll}{
              \code{subject} \tab a factor with 37 levels: 1, 2, \ldots, 37\cr
              \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
              \code{sequence} \tab a factor with 2 levels: TRRT, RTTR\cr
              \code{treatment} \tab a factor with 2 levels: T, R\cr
              \code{PK} \tab a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
            }
          }
          \item{Unbalanced (18 subjects in sequence TRRT and 19 subjects in RTTR) and complete. No outliers.\cr\cr
          In the source evaluated by \acronym{SAS} with the \acronym{FDA}’s mixed effects model (termed \sQuote{Method C} by the \acronym{EMA}; not compatible with the guideline). Reported results:}{
            \tabular{ll}{
              \code{PE} \tab 90.0\%\cr
              \code{90\% CI} \tab 79.6\% – 101.7\%
            }
          }
      }

    \item Reference Dataset 16 (\code{rds16})\cr
    38 subjects.
      \describe{
          \item{A data frame with 152 observations on the following 5 variables:}{
            \tabular{ll}{
              \code{subject} \tab a factor with 38 levels: 1, 2, \ldots, 38\cr
              \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
              \code{sequence} \tab a factor with 2 levels: TRRT, RTTR\cr
              \code{treatment} \tab a factor with 2 levels: T, R\cr
              \code{PK} \tab a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
            }
          }
          \item{Unbalanced (18 subjects in sequence TRRT and 20 in RTTR) and complete. No outliers.}{}
      }
    }
}

\details{
  \tabular{crcl}{
    Dataset \tab N \tab \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} (\%) \tab Evaluation\cr
    \code{rds05} \tab 26 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
    \code{rds11} \tab 37 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
    \code{rds16} \tab 38 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RTTR", "TRRT"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The library follows this convention.
}

\source{
  \tabular{cll}{
    Dataset \tab Origin \tab Description\cr
    \code{rds05} \tab \ifelse{html}{\out{Shumaker&nbsp;&amp;&nbsp;Metzler}}{Shumaker~&~Metzler} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data given in the Appendix.\cr
    \code{rds11} \tab Hauschke \emph{et al.} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data given in Table 9.6.\cr
    \code{rds16} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data of Drug 14a.
  }
}

\references{
Shumaker RC, Metzler CM. \emph{The Phenytoin Trial is a Case Study of \sQuote{Individual} Bioequivalence.}\cr
Drug Inf J. 1998;32(4):1063--72. \doi{10.1177/009286159803200426}

Hauschke D, Steinijans VW, Pigeot I. \emph{Bioequivalence Studies in Drug Development.}\cr
Chichester: John Wiley; 2007. p216.

\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.}\cr
Rockville, 1997. \href{https://web.archive.org/web/20170723175533/https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip
}{bioequivalence study files} (archived 2017-07-23)
}

\examples{
str(rds05)
summary(rds05[2:5])
head(rds11, 8)
}

\keyword{datasets}
