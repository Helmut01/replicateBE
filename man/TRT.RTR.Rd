\encoding{utf-8}
\name{TRT.RTR}
\alias{TRT.RTR}
\alias{rds03}
\alias{rds17}
\docType{data}
\title{
Reference Datasets for TRT|RTR Replicate Designs
}

\description{
Datasets from the public domain and edited to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\format{
  \itemize{
    \item Reference dataset 03\cr
    Based on \code{rds01}. Removed all data of period 4. 77 subjects.\cr
    Unbalanced (39 subjects in sequence TRT and 38 in RTR) and incomplete (six missings in sequence TRT and two in RTR). Missings / period: 0/1, 1/2, 7/3. Two outliers (subjects 45 and 52) in sequence RTR.\cr
    A data frame with 223 observations on the following 6 variables:
      \describe{
          \item{rds03}{
            \tabular{ll}{
              \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
              \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
              \code{sequence} \tab a factor with 2 levels: TRT, RTR\cr
              \code{treatment} \tab a factor with 2 levels: T, R\cr
              \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}})
            }
          }
       }

    \item Reference dataset 17\cr
    Based on \code{rds03}. 19 subjects.\cr
    Unbalanced (seven subjects in sequence TRT and twelve in RTR) and incomplete (one missing in sequence TRT). Missings / period: 0/1, 0/2, 1/3. One outlier (subject 18) in sequence RTR.\cr
    A data frame with 56 observations on the following 6 variables:
      \describe{
          \item{rds17}{
            \tabular{ll}{
              \code{subject} \tab a factor with 19 levels: 1, 2, \ldots, 22\cr
              \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
              \code{sequence} \tab a factor with 2 levels: TRT, RTR\cr
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
    \code{rds03} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
    \code{rds17} \tab 19 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\note{
In software sequences and treatments are ranked in lexical order. Hence, executing \code{str()} or \code{summary()} will show \code{sequence} as \code{"RTR", "TRT"} and \code{treatment} as \code{"R", "T"}. In \acronym{BE} -- by convention -- sequences are ordered with \code{T} first. The package follows this convention.
}

\author{
Helmut \enc{Schütz}{Schuetz}
}

\source{
  \tabular{cll}{
    Dataset \tab Origin \tab Description\cr
    \code{rds03} \tab \code{rds01} edited \tab Period 4 removed.\cr
    \code{rds17} \tab \code{rds03} edited \tab Highly unbalanced (seven subjects in TRT and twelve in RTR).
  }
}

\examples{
head(rds03, 6)
summary(rds03[2:5])
}

\keyword{datasets}
