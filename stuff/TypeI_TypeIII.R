data <- replicateBE:::get.data(data=rds01, print=F, plot.bxp=F)$data
modA <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment,
                     data = data)
typeIII <- stats::anova(modA) # otherwise summary of lmerTest is used
attr(typeIII, "heading")[1] <- "Type III Analysis of Variance Table\n"
MSdenom <- typeIII["sequence:subject", "Mean Sq"]
df2     <- typeIII["sequence:subject", "Df"]
fvalue  <- typeIII["sequence", "Mean Sq"] / MSdenom
df1     <- typeIII["sequence", "Df"]
typeIII["sequence", 4] <- fvalue
typeIII["sequence", 5] <- pf(fvalue, df1, df2, lower.tail = FALSE)
# Type I like in version ≤1.0.17
print(stats::anova(modA), digits = 6, signif.stars = FALSE)
print(typeIII, digits = 6, signif.stars = FALSE)
