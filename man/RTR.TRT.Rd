\encoding{utf-8}
\name{rds03+rds17}
\alias{RTR.TRT}
\docType{data}
\title{
Reference data sets for RTR|TRT full replicate designs
}

\description{
Data sets from the literature, edited, or obtained by simulations to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\details{
  \tabular{lll}{
  \code{rds03} \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds17} \tab N=19 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}
  }
}

\source{
  \tabular{ll}{
  \code{rds03} \tab Based on \code{rds01} (period 4 removed).\cr
  \code{rds17} \tab Based on \code{rds03}. Highly unbalanced (12 subjects in RTR and 7 in TRT).
  }
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}
}

\examples{
data("RTR.TRT")
head(rds03, 6)
}

\keyword{datasets}
