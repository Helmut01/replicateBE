library(replicateBE)
# RTRT.TRTR (DS01, DS06, DS08, DS09, DS12, DS13, DS14, DS15, DS18, DS19, DS20, DS21, DS25, DS26, DS29)
attr(rds01, "rset") <- "rds01"
attr(rds06, "rset") <- "rds06"
attr(rds08, "rset") <- "rds08"
attr(rds09, "rset") <- "rds09"
attr(rds12, "rset") <- "rds12"
attr(rds13, "rset") <- "rds13"
attr(rds14, "rset") <- "rds14"
attr(rds15, "rset") <- "rds15"
attr(rds18, "rset") <- "rds18"
attr(rds19, "rset") <- "rds19"
attr(rds20, "rset") <- "rds20"
attr(rds21, "rset") <- "rds21"
attr(rds25, "rset") <- "rds25"
attr(rds26, "rset") <- "rds26"
attr(rds29, "rset") <- "rds29"
save(rds01, rds06, rds08, rds09, rds12,
     rds13, rds14, rds15, rds18, rds19, rds20,
     rds21, rds25, rds26, rds29,
     file="E:/Public/Documents/BEBAC/R/replicateBE/data/TRTR.RTRT.rda")

# RRT.RTR.TRR (DS02, DS04, DS07, DS30)
attr(rds02, "rset") <- "rds02"
attr(rds04, "rset") <- "rds04"
attr(rds07, "rset") <- "rds07"
attr(rds30, "rset") <- "rds30"
save(rds02, rds04, rds07, rds30,
     file="E:/Public/Documents/BEBAC/R/replicateBE/data/TRR.RTR.RRT.rda")

# RTR.TRT (DS03, DS17)
attr(rds03, "rset") <- "rds03"
attr(rds17, "rset") <- "rds17"
save(rds03, rds17,
     file="E:/Public/Documents/BEBAC/R/replicateBE/data/RTR.TRT.rda")

# RTTR.TRRT (DS05, DS11, DS16)
attr(rds05, "rset") <- "rds05"
attr(rds11, "rset") <- "rds11"
attr(rds16, "rset") <- "rds16"
save(rds05, rds11, rds16,
     file="E:/Public/Documents/BEBAC/R/replicateBE/data/RTTR.TRRT.rda")

# RRT.TRR (DS10)
attr(rds10, "rset") <- "rds10"
save(rds10,
     file="E:/Public/Documents/BEBAC/R/replicateBE/data/RRT.TRR.rda")

# RTR.TRR (DS22)
attr(rds22, "rset") <- "rds22"
save(rds22,
     file="E:/Public/Documents/BEBAC/R/replicateBE/data/RTR.TRR.rda")
