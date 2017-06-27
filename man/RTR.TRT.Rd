\encoding{utf-8}
\name{RTR.TRT}
\docType{data}
\title{
Reference data sets for RTR|TRT full replicate designs
}

\description{
Data sets from the literature and edited to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\details{
  \tabular{crcl}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
    \code{rds03} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
    \code{rds17} \tab 19 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\source{
  \tabular{cll}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
    \code{rds03} \tab \code{rds01} edited \tab Period 4 removed.\cr
    \code{rds17} \tab \code{rds03} edited \tab Highly unbalanced (12 subjects in RTR and 7 in TRT).
  }
}

\format{
  \itemize{
    \item Reference Data Set 03 (\code{rds03})\cr
    Based on \code{ds01}. Removed all data of period 4. 77 subjects.
      \describe{
          A data frame with 223 observations on the following 6 variables:
          \tabular{ll}{
            \code{subject} \tab a factor with 77 levels: 1, 2, \ldots, 78\cr
            \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
            \code{sequence} \tab a factor with 2 levels: RTR, TRT\cr
            \code{treatment} \tab a factor with 2 levels: R, T\cr
            \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
            \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
          }
          Unbalanced (38 subjects in sequence RTR and 39 in TRT) and incomplete (2 missings in sequence RTR and 6 in sequence TRT). Missings / period: 0/1, 1/2, 7/3. Two extreme outliers (subjects 45 and 52) after the reference treatment in sequence RTR.
       }

    \item Reference Data Set 17 (\code{rds17})\cr
    Based on \code{rds03}. 19 subjects.
      \describe{
          A data frame with 56 observations on the following 6 variables:
          \tabular{ll}{
            \code{subject} \tab a factor with 19 levels: 1, 2, \ldots, 22\cr
            \code{period} \tab a factor with 3 levels: 1, 2, 3\cr
            \code{sequence} \tab a factor with 2 levels: RTR, TRT\cr
            \code{treatment} \tab a factor with 2 levels: R, T\cr
            \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})\cr
            \code{logPK} \tab a numeric vector of the natural logarithms of {\code{PK}}
          }
          Unbalanced (12 subjects in sequence RTR and 7 in TRT) and incomplete (1 missing in sequence TRT). Missings / period: 0/1, 0/2, 1/3. No outliers.
       }
  }
}

\author{
Helmut \enc{Schütz}{Schuetz}
}

\examples{
head(rds03, 6)
}

\keyword{datasets}
