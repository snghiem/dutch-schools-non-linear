# Author: Sony Nghiem
# Title: polynomial regresion

library(nlschools)
library(boot)
summary(nlschools)
lm.fit = lm(lang~poly(SES, 4), data=nlschools)
summary(lm.fit)

# I see a siginicant cubic-polynomial correlation between language test score 
# and social-economic status of pupil's family

# Now I perform cross-validation to choose the optimal degree for the polynomial
set.seed(1)
cv.error = rep(NA,20)
for (degree in 1:20){
  glm.fit = glm(lang~poly(SES, degree), data=nlschools)
  cv.error[degree] = cv.glm(nlschools, glm.fit, K=10)$delta[1] 
  # for 10-fold cross-validation
}

which.min(cv.error)
# so here I get 5 as for the smallest cross-validation error
# Now I plot the data

glm.fit = glm(lang~poly(SES, 5), data=nlschools)

SES.lim =range(nlschools$SES)
SES.grid = seq(from=SES.lim[1], to = SES.lim[2])
preds = predict(glm.fit, newdata =list(SES=SES.grid), se=TRUE)
plot(nlschools$SES, nlschools$lang, col="grey")
lines(SES.grid, preds$fit, lwd=2, col="blue")
lines(SES.grid, preds$fit+2*preds$se, lty="dashed")
lines(SES.grid, preds$fit-2*preds$se, lty="dashed")
