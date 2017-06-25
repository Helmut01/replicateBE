\encoding{utf-8}
\name{rds03+rds17}
\alias{RTR.TRT}
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

\examples{
head(rds03, 6)
}

\keyword{datasets}
