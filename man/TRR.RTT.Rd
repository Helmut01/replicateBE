\encoding{utf-8}
\name{TRR.RTT}
\alias{TRR.RTT}
\alias{rds10}
\docType{data}
\title{
Reference Data Set for TRR|RTT Replicate Designs
}

\description{
Data set from the public domain to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\usage{rds10}

\details{
   \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
    \code{rds10} \tab 36 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}
  }
}

\format{
  \itemize{
    \item Reference Data Set 10 (\code{rds10})\cr
    18 subjects.
    \describe{
      A data frame with 54 observations on the following 5 variables:
      \tabular{ll}{
      \code{subject} \tab a factor with 18 levels: 1, 2, \ldots, 18\cr
      \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
      \code{sequence} \tab a factor with 2 levels: TRR, RTT\cr
      \code{treatment} \tab a factor with 2 levels: T, R\cr
      \code{PK} \tab a numeric vector of pharmacokinetic responses (here AUC)
      }
      Balanced (nine subjects in both sequences) and complete. No outliers.
    }
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds10} \tab Chow & Liu \tab AUC data given in Table 9.3.3.
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RTT", "TRR"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The library follows this convention.\cr
In analogy to the \acronym{EMA}’s Q&A: Uncertain estimate of \code{CVwR} since less than twelve subjects in sequence TRR.
}

\references{
Chow SC, Liu JP. \emph{Design and Analysis of Bioavailability and Bioequivalence Studies.}\cr
Boca Raton: CRC Press; \ifelse{html}{\out{3<sup>rd</sup>}}{\eqn{3\textsuperscript}{rd}} edition 2009. p275.
}

\examples{
str(rds10)
row <- c(1:3, 28:30)
rds10[row, ]
summary(rds10[2:5])
}

\keyword{datasets}
