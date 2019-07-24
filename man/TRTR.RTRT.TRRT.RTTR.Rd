\encoding{utf-8}
\name{TRTR.RTRT.TRRT.RTTR}
\alias{TRTR.RTRT.TRRT.RTTR}
\alias{rds23}
\docType{data}
\title{
Reference Dataset for TRTR|RTRT|TRRT|RTTR Designs
}

\description{
Dataset from the public domain to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\format{
  \itemize{
    \item Reference Dataset 23 (\code{rds23})\cr
    22 subjects.\cr
    Unbalanced (four subjects in sequence RTRT and six in each of the other three) and complete. Two outliers (subjects 8 and 17) in sequence TRTR.\cr
    A data frame with 88 observations on the following 5 variables:
    \describe{
      \item{}{
        \tabular{ll}{
          \code{subject} \tab a factor with 22 levels: 1, 2, \ldots, 27\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 4 levels: TRTR, RTRT, TRRT, RTTR\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
          }
      }
    }
  }
}

\details{
  \tabular{crcl}{
    Dataset \tab N \tab \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} (\%) \tab Evaluation\cr
    \code{rds23} \tab 22 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RTRT", "RTTR", "TRRT", "TRTR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The library follows this convention.
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds23} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data of Drug 7.
  }
}

\references{
  \acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.} Rockville, 1997. \href{https://web.archive.org/web/20170723175533/https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip
}{bioequivalence study files} (archived 2017-07-23)
}

\examples{
str(rds23)
row <- c(25:28, 5:8, 9:12, 1:4)
rds23[row, ]
summary(rds23[2:5])
}

\keyword{datasets}
