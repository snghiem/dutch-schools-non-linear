# Author: Sony Nghiem
# Title: Generalized Additive Model

library(MASS)
library(gam)

gam1 = gam(lang~s(SES,3), data=nlschools)
gam2 = gam(lang~s(SES,3)+GS, data=nlschools)
gam3 = gam(lang~s(SES,3)+ns(SES,3):ns(IQ,3)+GS, data=nlschools) 
# this one includes interaction terms but it's harder to interpret
# but it should make some intuitive sense about the correlation between
# language test score and verbal IQ of students
gam4 = gam(lang~s(SES,3)+IQ+GS, data=nlschools)
anova(gam1,gam2,gam3,gam4, test="F")

summary(gam3)
