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
  \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
  \code{rds10} \tab 18 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds10} \tab Chow & Liu \tab AUC data given in Table 9.3.3.
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
