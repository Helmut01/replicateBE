\encoding{utf-8}
\name{rds10}
\alias{TRR.RRT}
\docType{data}
\title{
Reference data sets for TRR|RTT replicate designs
}

\description{
Data sets from the literature to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
  \tabular{lll}{
  \code{rds10} \tab N=18 \tab \code{CVwR} <30\%: Evaluate by \code{method.A()}, \code{method.B()}, or \code{ABE()}
  }
}

\source{
  \tabular{ll}{
  \code{rds10} \tab Chow & Liu. 2009. AUC data given in Table 9.3.3.
  }
}

\references{
Chow SC, Liu JP. \emph{Design and Analysis of Bioavailability and Bioequivalence Studies.}\cr
Boca Raton: CRC Press; \ifelse{html}{\out{3<sup>rd</sup>}}{\eqn{3\textsuperscript}{rd}} edition 2009. p275.
}

\examples{
data("TRR.RRT")
str(rds10)
row <- c(1:3, 28:30)
rds10[row, ]
}

\keyword{datasets}
