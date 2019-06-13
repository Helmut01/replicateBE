# replicateBE
## Comparative BA-calculation for the EMA’s Average Bioequivalence with Expanding Limits (ABEL)

The library supports methods given in the EMA’s [Q&A document](https://www.ema.europa.eu/en/documents/scientific-guideline/questions-answers-positions-specific-questions-addressed-pharmacokinetics-working-party_en.pdf) for reference-scaling according to the EMA’s [Guideline on the Investigation of Bioequivalence](https://www.ema.europa.eu/en/documents/scientific-guideline/guideline-investigation-bioequivalence-rev1_en.pdf). Potential influence of outliers on the variability of the reference treatment can be assessed by box plots of studentized and standardized residuals as suggested at a joint [EGA/EMA workshop](https://www.medicinesforeurope.com/wp-content/uploads/2016/03/EGA_BEQ_QA_WEB_QA_1_32.pdf).

In full replicate designs the variability of test and reference treatment can be assessed by _s<sub>wT</sub>_/_s<sub>wR</sub>_ and the upper confidence limit of <em>σ<sub>wT</sub></em>/<em>σ<sub>wR</sub></em> (required for the [WHO’s approach](https://extranet.who.int/prequal/sites/default/files/documents/AUC_criteria_November2018.pdf) for reference-scaling of _AUC_).


- Estimation of <em>CV<sub>wR</sub></em> (and <em>CV<sub>wT</sub></em> in full replicates)

  A linear model of log-transformed PK responses and effects _sequence_, _subject(sequence)_, _period_&nbsp;– where all effects are fixed (_i.e._, ANOVA). Estimated via function ```lm()``` of library ```stats```.
  ```Rscript
  modCVwR <- lm(log(PK) ~ sequence + subject%in%sequence + period, data = data[data$treatment == "R")
  modCVwT <- lm(log(PK) ~ sequence + subject%in%sequence + period, data = data[data$treatment == "T")
  ```
 

- Method A
  
  A linear model of log-transformed PK responses and effects _sequence_, _subject(sequence)_, _period_, _treatment_&nbsp;– where all effects are fixed (_i.e._, ANOVA). Estimated via function ```lm()``` of library ```stats```.
  ```Rscript
  modA <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment, data = data)
  ```
  
- Method B
  
  A linear mixed model of log-transformed PK responses and effects _sequence_, _subject(sequence)_, _period_, _treatment_&nbsp;– where _subject(sequence)_ is a random effect and all others are fixed.
  
  Two options
    - Estimated via function ```lme()``` of library ```nlme```. Uses degrees of freedom equivalent to SAS’ ```DDFM=CONTAIN``` and Phoenix/WinNonlin’s ```DF Residual```. Implicitly preferred according to the Q&A document.
      ```Rscript
      modB <- lme(log(PK) ~ sequence +  period + treatment, random = ~1|subject, data = data)
      ```    
    - Estimated via function ```lmer()``` of library ```lmerTest```. Uses Satterthwaite’s degrees of freedom equivalent to SAS’ ```DDFM=SATTERTHWAITE``` and Phoenix/WinNonlin’s ```DF Satterthwaite```.
      ```Rscript
      modB <- lmer(log(PK) ~ sequence + period + treatment + (1|subject), data = data)
      ```


- Conventional Average Bioequivalence (ABE)&nbsp;– optionally with tighter (EMA: NTIDs) or wider (GCC: _C<sub>max</sub>_) limits
             &nbsp;– can be calculated as well.

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

² Balaam’s design (_not recommended_ due to poor power characteristics).

³ Extra-reference design; biased in the presence of period effects (design _not recommended_).

Cross-validation: Results of reference data sets agree with ones obtained in SAS (9.3, 9.4) and Phoenix WinNonlin (6.4, 7.0, 8.0).

**Program offered for Use without any Guarantees and Absolutely No Warranty. No Liability is accepted for any Loss and Risk to Public Health Resulting from Use of this R-Code.**
