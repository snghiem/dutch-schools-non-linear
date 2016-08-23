# Author: Sony Nghiem
# Title: Step regression

# Here I use the cut() function
# use cross-validation to choose how many cuts for the step regression


library(MASS)
library(boot)

set.seed(34)
cv.error = rep(NA, 20)
# test cross-validation with 20 cuts
for (i in 2:20) { # since the number of segments should be more than 2
  fit = glm(lang ~ cut(SES,i), data=nlschools)
  cv.error[i] = cv.glm(nlschools, fit, K=10)$delta[1]
}
plot(2:20, cv.error[-1], xlab="Numbers of cuts", ylab="CV error", type="l", pch=20, lwd=2 )
which.min(cv.error[-1])
# here is 9 the best cut

fit = lm(lang ~ cut(SES, 9), data=nlschools)
coef(summary(fit))
# so for students from families living under 14.4 SES condition
# they make on average 33.4497 of language score