\encoding{utf-8}
\name{refdata}
\alias{data}
\docType{data}

\title{
Reference Data Sets for Replicate Designs
}

\description{
Data sets from the public domain, edited, or obtained by simulations to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
  \tabular{llcrcl}{
  Design \tab Specification \tab\ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab  N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
  TRTR|RTRT \tab full \tab \code{rds01} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds06} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds12} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds14} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds18} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds21} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds19} \tab 61 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds20} \tab 61 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds08} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds09} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds13} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds15} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRRT|RTTR \tab full \tab \code{rds05} \tab 26 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  TRRT|RTTR \tab full \tab \code{rds11} \tab 37 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRRT|RTTR \tab full \tab \code{rds16} \tab 38 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT|TRRT|RTTR \tab full \tab \code{rds23} \tab 22 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRRT|RTTR|TTRR|RRTT \tab full \tab \code{rds24} \tab 39 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRT|RTR \tab full \tab \code{rds03} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRT|RTR \tab full \tab \code{rds17} \tab 19 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRR|RTT \tab full \tab \code{rds10} \tab 18 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  TRR|RTR|RRT \tab partial \tab \code{rds02} \tab 24 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  TRR|RTR|RRT \tab partial \tab \code{rds04} \tab 51 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRR|RTR|RRT \tab partial \tab \code{rds07} \tab 360 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRR|RTR \tab partial \tab \code{rds22} \tab 36 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
In full replicate designs \emph{both} R and T are administered twice (in 3-period designs to ½ of the subjects).\cr
In partial replicate designs \emph{only} R is administered twice.
}

\source{
  \tabular{cll}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
    \code{rds01} \tab EMA, CHMP \tab Data set I.\cr
    \code{rds06} \tab \code{rds01} edited \tab T and R switched.\cr
    \code{rds12} \tab Phoenix NLME \tab Simulated with extreme intra- and intersubject variability.\cr
    \code{rds14} \tab Phoenix NLME \tab Simulated with high intra- and intersubject variability and number of dropouts increasing with period.\cr
    \code{rds18} \tab \code{rds14} edited \tab Removed T data of subjects 63–78.\cr
    \code{rds21} \tab \code{rds01} edited \tab One extreme result of subjects 45 & 52 set to NA.\cr
    \code{rds19} \tab \code{rds18} edited \tab Removed data of subjects 63–78.\cr
    \code{rds20} \tab \code{rds19} edited \tab Outlier of R (subject 1) introduced: original value ×100.\cr
    \code{rds08} \tab R \tab Simulated with slight heteroscedasticity.\cr
    \code{rds09} \tab \code{rds08} \tab Wide numeric range (data of last 37 subjects multiplied by 1,000,000).\cr
    \code{rds13} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data deleted).\cr
    \code{rds15} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'}).\cr
    \code{rds05} \tab \ifelse{html}{\out{Metzler&nbsp;&amp;&nbsp;Shumaker}}{Metzler~&~Shumaker} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in the Appendix.\cr
    \code{rds11} \tab Hauschke \emph{et al.} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in Table 9.6.\cr
    \code{rds16} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Drug 14a.\cr
    \code{rds23} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Drug 7.\cr
    \code{rds24} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Drug 1.\cr
    \code{rds03} \tab \code{rds01} edited \tab Period 4 removed.\cr
    \code{rds17} \tab \code{rds03} edited \tab Highly unbalanced (twelve subjects in RTR and seven in TRT).\cr
    \code{rds10} \tab Chow & Liu \tab AUC data given in Table 9.3.3.\cr
    \code{rds02} \tab EMA, CHMP \tab Data set II.\cr
    \code{rds04} \tab \ifelse{html}{\out{Patterson&nbsp;&amp;&nbsp;Jones}}{Patterson~&~Jones} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Table II.\cr
    \code{rds07} \tab R \tab Simulated with homoscedasticity.\cr
    \code{rds22} \tab R \tab Simulated with homoscedasticity.
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
  4-period full replicates\cr
  {\link[replicateBE]{TRTR.RTRT}}, {\link[replicateBE]{TRRT.RTTR}}, {\link[replicateBE]{TRTR.RTRT.TRRT.RTTR}}, {\link[replicateBE]{TRRT.RTTR.TTRR.RRTT}}\cr
  3-period full replicates\cr
  {\link[replicateBE]{TRT.RTR}}, {\link[replicateBE]{TRR.RTT}}\cr
  3-period partial replicates\cr
  {\link[replicateBE]{TRR.RTR.RRT}}, {\link[replicateBE]{TRR.RTR}}
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes), Michael Tomashevskiy (simulations in Phoenix NLME)
}

\examples{
# show structure of all data sets
ds <- sprintf("rds\%02i", 1:24)
for (i in seq_along(ds)) {
  cat(ds[i], "\n")
  str(eval(parse(text=ds[i])))
}
}

\keyword{datasets}
