\encoding{utf-8}
\name{TRR.RTR}
\alias{TRR.RTR}
\alias{rds22}
\docType{data}
\title{
Reference Data Set for TRR\enc{|}{,}RTR (extra-reference) Designs
}

\description{
Data set simulated to be evaluated by \code{method.A()}, \code{method.B()}.
}

\usage{rds22}

\details{
   \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
    \code{rds22} \tab 42 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\format{
  \itemize{
    \item Reference Data Set 22 (\code{rds22})\cr
    Simulated with \ifelse{html}{\out{CV<sub>wT</sub>}}{\eqn{CV_{wT}}} = \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} = 45\%, \ifelse{html}{\out{CV<sub>bT</sub>}}{\eqn{CV_{bT}}} = \ifelse{html}{\out{CV<sub>bR</sub>}}{\eqn{CV_{bR}}} = 100\% GMR 0.90. 42 subjects.
    \describe{
      A data frame with 126 observations on the following 5 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 42 levels: 1, 2, \ldots, 42\cr
      \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
      \code{sequence} \tab a factor with 2 levels: TRR, RTR\cr
      \code{treatment} \tab a factor with 2 levels: T, R\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Balanced (21 subjects in each of the sequences) and complete (no missing data). No outliers.
    }
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
    \code{rds22} \tab R \tab Simulated data set with homoscedasticity.
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RTR", "TRR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The library follows this convention.\cr
This partial replicate design is also known as the \sQuote{extra-reference design}. Since the Test is not administered in all periods, lacking period effects must be assumed. In the presence of a \emph{true} period effect the treatment comparison will be biased. Hence, this design is not recommended.
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes)
}

\examples{
str(rds22)
rds22[61:66, ]
summary(rds22[2:5])
}

\keyword{datasets}
