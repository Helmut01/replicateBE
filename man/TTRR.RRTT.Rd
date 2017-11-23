\encoding{utf-8}
\name{TTRR.RRTT}
\alias{TTRR.RRTT}
\alias{rds28}
\docType{data}
\title{
Reference Data Sets for TTRR\enc{|}{,}RRTT Designs
}

\description{
Data set obtained by simulations to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\details{
  \tabular{crcl}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} (\%) \tab Evaluation\cr
    \code{rds28} \tab 64 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}
  }
}

\format{
  \itemize{
    \item Reference Data Set 28 (\code{rds28})\cr
    64 subjects.
    \describe{
      A data frame with 256 observations on the following 5 variables:
      \tabular{ll}{
        \code{subject} \tab a factor with 64 levels: 1, 2, \ldots, 64\cr
        \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
        \code{sequence} \tab a factor with 2 levels: TTRR, RRTT\cr
        \code{treatment} \tab a factor with 2 levels: T, R\cr
        \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
      }
      Balanced (64 subjects in both sequences) and complete. No outliers.
    }
  }
}

\source{
  \tabular{cll}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
    \code{rds28} \tab R \tab Simulated data set with homoscedasticity.
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RRTT", "TTRR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The library follows this convention.
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes)
}

\examples{
str(rds28)
summary(rds28[1:5])
}

\keyword{datasets}
