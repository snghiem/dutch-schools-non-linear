# Author: Sony Nghiem
# Title: spline regression

library(MASS)
library(splines)

# cross validation to fit the best degree of spline regression
# here I use bs() to generate the entire matrix of basis functions for splines with 
# knots at uniform quantiles. I can also do knots = c(25, 40, 50) for example as an argument
set.seed(2)
cv.error = rep(NA, 10)
for (i in 3:10){ # the minimum is 3 or cublic spline regression
  fit = glm(lang~bs(SES, df=i), data=nlschools)
  cv.error[i] = cv.glm(nlschools, fit, K=5)$delta[1] # 5-fold cross-validation
}
plot(3:10, cv.error[-c(1,2)], xlab="Number of degrees", ylab="CV error", type="l")
which.min(cv.error[-c(1,2)])
# 1 here pretty much means 3
# well, so we stick with a cubic spline regression for this case

fit = lm(lang~poly(SES,3), data=nlschools)

SES.lim =range(nlschools$SES)
SES.grid = seq(from=SES.lim[1], to = SES.lim[2])
preds = predict(glm.fit, newdata =list(SES=SES.grid), se=TRUE)
plot(nlschools$SES, nlschools$lang, col="grey")
lines(SES.grid, preds$fit, lwd=2, col="blue")
lines(SES.grid, preds$fit+5*preds$se, lty="dashed")
lines(SES.grid, preds$fit-5*preds$se, lty="dashed")

# let's try a smoothing line
spline.fit = smooth.spline(nlschools$SES,nlschools$lang, df=16)
spline.fit2 = smooth.spline(nlschools$SES,nlschools$lang, cv=TRUE) # here I use cross-validation
spline.fit2$df
# Woohoo I get 5.884175 effective degree of freedom
lines(spline.fit2, col="red", lwd=2)

# perform local regression

fit3 = loess(lang~SES, span = .2, data=nlschools) 
#here I really choose a random span. 20% of the observations are captured in the neighborhood
lines(SES.grid, predict(fit3, data.frame(SES=SES.grid)), col="yellow", lwd=2)

# Warning messages:
#   1: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
#     pseudoinverse used at 18
#   2: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
#     neighborhood radius 2
#   3: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
#     reciprocal condition number  1.6842e-015
#   4: In simpleLoess(y, x, w, span, degree = degree, parametric = parametric,  :
#     There are other near singularities as well. 4
                                                                    