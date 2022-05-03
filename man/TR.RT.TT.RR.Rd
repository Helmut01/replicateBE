\encoding{utf-8}
\name{TR.RT.TT.RR}
\alias{TR.RT.TT.RR}
\alias{rds27}
\docType{data}
\title{
Reference Dataset for TR\enc{|}{,}RT\enc{|}{,}TT\enc{|}{,}RR Replicate Designs
}

\description{
Dataset for Balaam’s design obtained by simulations to be evaluated by \code{method.A()}, \code{method.B()}.
}

\usage{rds27}

\format{
  \itemize{
    \item \bold{Reference Dataset 27} (\code{rds27})\cr
    312 subjects. Balanced (78 subjects in each of the four sequences) and incomplete (T of subject 111 missing in period 2 of sequence RT). No outliers.\cr
    A data frame with 624 observations on the following 5 variables:
    \describe{
      \item{rds27}{
          \tabular{ll}{
            \code{subject} \tab a factor with 312 levels: 1, 2, \ldots, 18\cr
            \code{period} \tab a factor with 2 levels: 1, 2\cr
            \code{sequence} \tab a factor with 4 levels: TR, RT, TT, RR\cr
            \code{treatment} \tab a factor with 2 levels: T, R\cr
            \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling
          }
        }
    }
  }
}

\details{
   \tabular{crcl}{
  Dataset \tab N \tab \ifelse{html}{\out{<em>CV</em><sub>wR</sub>}}{\eqn{CV_\text{wR}}} (\%) \tab Evaluation\cr
    \code{rds27} \tab 312 \tab >30 \tab \code{method.A()}, \code{method.B()}}
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RR", "RT", "TR", "TT"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The package follows this convention.
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes)
}

\source{
  \tabular{cll}{
    Dataset \tab Origin \tab Description\cr
    \code{rds27} \tab R \tab Simulated with \ifelse{html}{\out{<em>CV</em><sub>wT</sub>}}{\eqn{CV_\text{wT}}} = \ifelse{html}{\out{<em>CV</em><sub>wR</sub>}}{\eqn{CV_\text{wR}}} = 35\%, \ifelse{html}{\out{<em>CV</em><sub>bT</sub>}}{\eqn{CV_\text{bR}}} = \ifelse{html}{\out{<em>CV</em><sub>bR</sub>}}{\eqn{CV_\text{bR}}} = 75\%, GMR 0.90.
  }
}

\examples{
str(rds27)
row <- c(1:2, 157:158, 313:314, 469:470)
rds27[row, ]
summary(rds27[2:5])
}

\keyword{datasets}
