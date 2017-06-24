\encoding{utf-8}
\name{rds01+rds02+rds03+rds04+rds05+rds06+rds07+rds08+rds09+rds10+rds11+rds12+rds13+rds14+rds15+rds16+rds17+rds18+rds19+rds20+rds21}
\alias{data}
\alias{RTRT.TRTR}
\alias{RTTR.TRRT}
\alias{RRT.RTR.TRR}
\alias{TRR.RRT}
\alias{RTR.TRT}
\docType{data}
\title{
Reference data sets for replicate designs
}

\description{
Data sets from the literature, edited, or obtained by simulations to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
  \tabular{llll}{
  \code{rds01} \tab RTRT|TRTR \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds02} \tab RRT|RTR|TRR \tab N=24 \tab \code{CVwR} <30\%: Evaluate by \code{method.A()}, \code{method.B()}, or \code{ABE()}\cr
  \code{rds03} \tab RTR|TRT \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds04} \tab RRT|RTR|TRR \tab N=51 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds05} \tab RTTR|TRRT \tab N=26 \tab \code{CVwR} <30\%: Evaluate by \code{method.A()}, \code{method.B()}, or \code{ABE()}\cr
  \code{rds06} \tab RTRT|TRTR \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds07} \tab RRT|RTR|TRR \tab N=360 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds08} \tab RTRT|TRTR \tab N=222 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds09} \tab RTRT|TRTR \tab N=222 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds10} \tab TRR|RTT \tab N=18 \tab \code{CVwR} <30\%: Evaluate by \code{method.A()}, \code{method.B()}, or \code{ABE()}\cr
  \code{rds11} \tab RTTR|TRRT \tab N=37 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or\code{method.B()}\cr
  \code{rds12} \tab RTRT|TRTR \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds13} \tab RTRT|TRTR \tab N=222 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds14} \tab RTRT|TRTR \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds15} \tab RTRT|TRTR \tab N=222 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds16} \tab RTTR|TRRT \tab N=38 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds17} \tab RTR|TRT \tab N=19 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds18} \tab RTRT|TRTR \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds19} \tab RTRT|TRTR \tab N=61 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds20} \tab RTRT|TRTR \tab N=61 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds21} \tab RTRT|TRTR \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}
  }
}

\source{
  \tabular{ll}{
  \code{rds01} \tab EMA, CHMP. 2015. Data set I.\cr
  \code{rds02} \tab EMA, CHMP. 2015. Data set II.\cr
  \code{rds03} \tab Based on \code{rds01} (period 4 removed).\cr
  \code{rds04} \tab Patterson & Jones. 2012. \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Table II.\cr
  \code{rds05} \tab Metzler & Shumaker. 1998. \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in the Appendix.\cr
  \code{rds06} \tab Based on \code{rds01} (T and R switched).\cr
  \code{rds08} \tab Large simulated data set with slight heteroscedasticity.\cr
  \code{rds07} \tab Large simulated data set with homoscedasticity.\cr
  \code{rds09} \tab Based on \code{rds08}. Wide numeric range (data of last 37 subjects multiplied by 1,000,000).\cr
  \code{rds10} \tab Chow & Liu. 2009. AUC data given in Table 9.3.3.\cr
  \code{rds11} \tab Hauschke \emph{et al.} 2007. \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in Table 9.6.\cr
  \code{rds12} \tab Simulated data set with extreme intrasubject variability.\cr
  \code{rds13} \tab Based on \code{rds08}. Highly incomplete (approx. 50\% of period 4 data deleted).\cr
  \code{rds14} \tab Simulated data set with high intrasubject variability and number of dropouts increasing with period.\cr
  \code{rds15} \tab Based on \code{rds08}. Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'}).\cr
  \code{rds16} \tab FDA. 1997. \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Drug 14a.\cr
  \code{rds17} \tab Based on \code{rds03}. Highly unbalanced (12 subjects in RTR and 7 in TRT).\cr
  \code{rds18} \tab Based on \code{rds14}. Removed T data of subjects 63–78.\cr
  \code{rds19} \tab Based on \code{rds18}. Removed data of subjects 63–78.\cr
  \code{rds20} \tab Based on \code{rds19}. Outlier of R (subject 1) introduced: original value ×100.\cr
  \code{rds21} \tab Based on \code{rds01}. One extreme result of subjects 45 & 52 set to NA.
  }
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}

Patterson SD, Jones B. \emph{Viewpoint: observations on scaled average bioequivalence.}\cr
Pharm Stat. 2012;11(1):1--7. \doi{10.1002/pst.498}

Metzler CM, Shumaker RC. \emph{The Phenytoin Trial is a Case Study of \sQuote{Individual} Bioequivalence.}\cr
Drug Inf J. 1998;32:1063--72

Chow SC, Liu JP. \emph{Design and Analysis of Bioavailability and Bioequivalence Studies.}\cr
Boca Raton: CRC Press; \ifelse{html}{\out{3<sup>rd</sup>}}{\eqn{3\textsuperscript}{rd}} edition 2009. p275.

Hauschke D, Steinijans VW, Pigeot I. \emph{Bioequivalence Studies in Drug Development.}\cr
Chichester: John Wiley; 2007. p216.

\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.}\cr
Rockville, 1997. \href{https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip}{bioequivalence study files}
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}, {\link[replicateBE]{RTTR.TRRT}}, {\link[replicateBE]{TRR.RRT}}, {\link[replicateBE]{RTR.TRT}}, {\link[replicateBE]{RRT.RTR.TRR}}
}

\examples{
data("RTRT.TRTR")
str(rds01)
}

\keyword{datasets}
