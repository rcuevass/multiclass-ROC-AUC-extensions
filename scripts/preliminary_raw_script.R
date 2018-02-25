install.packages("pROC")
library(pROC)
data(aSAH)
head(aSAH)

#======= "Traditional" AUC/ROC analysis

# reference: https://www.rdocumentation.org/packages/pROC/versions/1.10.0

roc(aSAH$outcome, aSAH$s100b)
roc(outcome ~ s100b, aSAH)

roc(outcome ~ s100b, aSAH, smooth=TRUE)

roc1 <- roc(aSAH$outcome,
            aSAH$s100b, percent=TRUE,
            # arguments for auc
            partial.auc=c(100, 90), partial.auc.correct=TRUE,
            partial.auc.focus="sens",
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)

# Add to an existing plot. Beware of 'percent' specification!
roc2 <- roc(aSAH$outcome, aSAH$wfns,
            plot=TRUE, add=TRUE, percent=roc1$percent)

coords(roc1, "best", ret=c("threshold", "specificity", "1-npv"))
coords(roc2, "local maximas", ret=c("threshold", "sens", "spec", "ppv", "npv"))


# Of the AUC
ci(roc2)

# Of the curve
sens.ci <- ci.se(roc1, specificities=seq(0, 100, 5))
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

# need to re-add roc2 over the shape
plot(roc2, add=TRUE)

# CI of thresholds
plot(ci.thresholds(roc2))

# ========= Multi-class ROC ===================
# 
# reference: https://www.rdocumentation.org/packages/pROC/versions/1.10.0/topics/multiclass.roc
# https://stats.stackexchange.com/questions/2151/how-to-plot-roc-curves-in-multiclass-classification
# http://www.cs.bris.ac.uk/~flach/ICML04tutorial/
# https://link.springer.com/article/10.1023%2FA%3A1010920819831
 
# Basic example
multiclass.roc(aSAH$gos6, aSAH$s100b)

# Select only 3 of the aSAH$gos6 levels:
multiclass.roc(aSAH$gos6, aSAH$s100b, levels=c(3, 4, 5))

# Give the result in percent
multiclass.roc(aSAH$gos6, aSAH$s100b, percent=TRUE)
