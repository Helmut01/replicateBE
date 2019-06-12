\encoding{utf-8}
\name{TRRT.RTTR.TTRR.RRTT}
\alias{TRRT.RTTR.TTRR.RRTT}
\alias{rds24}
\docType{data}
\title{
Reference Dataset for TRRT|RTTR|TTRR|RRTT Designs
}

\description{
Dataset from the public domain to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\format{
  \itemize{
    \item Reference Dataset 24 (\code{rds24})\cr
    40 subjects (one completely missing).
    \describe{
      \item{ A data frame with 160 observations on the following 5 variables:}{
        \tabular{ll}{
          \code{subject} \tab a factor with 40 levels: 1, 2, \ldots, 932\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 4 levels: TRRT, RTTR, TTRR, RRTT\cr
          \code{treatment} \tab a factor with 2 levels: T, R\cr
          \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
          }
      }
      \item{Unbalanced (nine subjects in sequence TRRT and ten in each of the other three) and complete. Two outliers (subject 3 in sequence RTTR and subject 30 in sequence TTRR).}{}
    }
  }
}

\details{
  \tabular{crcl}{
    Dataset \tab N \tab \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} (\%) \tab Evaluation\cr
    \code{rds24} \tab 39 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RRTT", "RTTR", "TRRT", "TTRR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The library follows this convention.
}

\source{
  \tabular{cll}{
  Dataset \tab Origin \tab Description\cr
  \code{rds24} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data of Drug 1.
  }
}

\references{
\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.}\cr
Rockville, 1997.
\href{https://web.archive.org/web/20170723175533/https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip
}{bioequivalence study files} (archived 2017-07-23)
}

\examples{
str(rds24)
row <- c(13:16, 9:12, 1:4, 5:8)
rds24[row, ]
summary(rds24[2:5])
}

\keyword{datasets}
