\encoding{utf-8}
\name{RTR.TRR}
\alias{RTR.TRR}
\docType{data}
\title{
Reference Data Set for RTR|TRR (extra-reference) Designs
}

\description{
Data set simulated to be evaluated by \code{method.A()}, \code{method.B()}.
}

\usage{rds22}

\details{
   \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
    \code{rds22} \tab 36 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\format{
  \itemize{
    \item Reference Data Set 22 (\code{rds10})\cr
    Simulated with \ifelse{html}{\out{C<sub>wT</sub>}}{\eqn{C\textsubscript}{wT}} = \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} = 40\%, GMR 0.90. 36 subjects.
    \describe{
      A data frame with 108 observations on the following 5 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 36 levels: 1, 2, \ldots, 36\cr
      \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
      \code{sequence} \tab a factor with 2 levels: RTR, TRR\cr
      \code{treatment} \tab a factor with 2 levels: R, T\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
      \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
      }
      Balanced (18 subjects in each of the sequences) and complete (no missing data). No outliers.
    }
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
    \code{rds22} \code{rds22} \tab R \tab Simulated data set with homoscedasticity.
  }
}

\note{
This partial replicate design is also known as the \sQuote{extra-reference design}. Since T is not administered in all periods, lacking period effects must be assumed. In the presence of a \emph{true} period effect the treatment comparison will be biased. Hence, this design is not recommended.
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes)
}

\examples{
str(rds22)
rds22[52:57, ]
}

\keyword{datasets}
