\encoding{utf-8}
\name{TTRR.RRTT}
\alias{TTRR.RRTT}
\alias{rds28}
\docType{data}
\title{
Reference Datasets for TTRR\enc{|}{,}RRTT Designs
}

\description{
Dataset obtained by simulations to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\format{
  \itemize{
    \item Reference Dataset 28\cr
    64 subjects. Balanced (64 subjects in both sequences) and complete. No outliers.\cr
    A data frame with 256 observations on the following 5 variables:
    \describe{
      \item{rds28}{
        \tabular{ll}{
          \code{subject} \tab a factor with 64 levels: 1, 2, \ldots, 64\cr
          \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
          \code{sequence} \tab a factor with 2 levels: TTRR, RRTT\cr
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
    \code{rds28} \tab 64 \tab <30 \tab \code{method.A()}, \code{method.B()}
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RRTT", "TTRR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The package follows this convention.
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes)
}

\source{
  \tabular{cll}{
    Dataset \tab Origin \tab Description\cr
    \code{rds28} \tab R \tab Simulated with \ifelse{html}{\out{CV<sub>wT</sub>}}{\eqn{CV_{wT}}} = \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} = 35\%, \ifelse{html}{\out{CV<sub>bT</sub>}}{\eqn{CV_{bR}}} = \ifelse{html}{\out{CV<sub>bR</sub>}}{\eqn{CV_{bR}}} = 75\%, GMR 0.90.
  }
}

\examples{
str(rds28)
summary(rds28[1:5])
}

\keyword{datasets}
