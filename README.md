# replicateBE
## Comparative BA-calculation for the EMA’s Average Bioequivalence with Expanding Limits (ABEL)

The library supports methods given in the EMA’s [Q&A document](http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf) for reference-scaling according to the EMA’s [Guideline on the Investigation of Bioequivalence](http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2010/01/WC500070039.pdf). Potential influence of outliers on the variability of the reference can be assessed by boxplots of studentized and standardized residuals as suggested at a joint [EGA/EMA workshop](http://www.medicinesforeurope.com/wp-content/uploads/2016/03/EGA_BEQ_QA_WEB_QA_1_32.pdf).


- Method A
  
  A linear model of log-transformed PK responses and effects _sequence_, _subject(sequence)_, _period_, and _treatment_ – where all effects are fixed (_i.e._, ANOVA). Estimated via function ```lm()``` of library ```stats```.
  ```Rscript
  log(PK) ~ sequence + subject(sequence) + period + treatment, data = data
  ```
  
- Method B
  
  A linear mixed model of log-transformed PK responses and effects _sequence_, _subject(sequence)_, _period_, and _treatment_ – where _subject(sequence)_ is a random effect and all others are fixed.
  ```RScript
  log(PK) ~ sequence + period + treatment, random = ~1|subject, data = data
  ```
  Two options
    - Estimated via function ```lme()``` of library ```nlme```. Uses degrees of freedom equivalent to SAS’ ```DDFM=CONTAIN``` and Phoenix/WinNonlin’s ```DF Residual```. Implicitly preferred according to the Q&A document.
    - Estimated via function ```lmer()``` of library ```lmerTest```. Uses Satterthwaite’s degrees of freedom.


- Conventional (unscaled) Average Bioequivalence (ABE) can be calculated as well.

Tested designs
- 4-period (full) replicates
  - ```TRTR | RTRT```
  - ```TRRT | RTTR```
  - ```TTRR | RRTT```
  - ```TRTR | RTRT | TRRT | RTTR```¹
  - ```TRRT | RTTR | TTRR | RRTT```¹
- 2-period (full) replicate
  - ```TR | RT | TT | RR```²
- 3-period (full) replicates
  - ```TRT | RTR```
  - ```TRR | RTT```
- 3-period (partial) replicates
  - ```TRR | RTR | RRT```
  - ```TRR | RTR```³

¹ Confounded effects (design _not recommended_).

² Balaam’s design (not recommended due to poor power characteristics).

³ Extra-reference design; biased in the presence of period effects (design _not recommended_).

Cross-validation: Results of reference data sets agree with ones obtained in SAS 9.3 and Phoenix WinNonlin (6.4, 7.0, 8.0).

**Program offered for Use without any Guarantees and Absolutely No Warranty. No Liability is accepted for any Loss and Risk to Public Health Resulting from Use of this R-Code.**
