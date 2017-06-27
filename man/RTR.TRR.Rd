\encoding{utf-8}
\name{rds22}
\alias{RTR.TRR}
\docType{data}
\title{
Reference Data Sets for RTR|TRR (extra-reference) Designs
}

\description{
Data set simulated to be evaluated by \code{method.A()}, \code{method.B()}.
}

\details{
  \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
  \code{rds22} \tab 36 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\note{
This partial replicate design is also known as the \sQuote{extra-reference design}. Since T is not administered in all periods, lacking period effects must be assumed. In the presence of a \emph{true} period effect the treatment comparison will be biased. Hence, this design is not recommended.
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds22} \tab R \tab Simulated data with homoscedasticity.
  }
}

\examples{
str(rds22)
rds22[52:57, ]
}

\keyword{datasets}
