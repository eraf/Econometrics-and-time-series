rm(list = ls())
library(AER)
data("Journals")

library(dplyr)
journals = Journals %>% select(c("subs", "price"))
journals = journals %>% mutate(citeprice = Journals$citations / 
                      Journals$price)
lm_jour = lm(log(subs) ~ log(citeprice), data = journals)
lm_jour$coefficients

#reset test
resettest(lm_jour, power = 2:3) #F = 1.44
qf(.95, 2, 176) #3.04, so we can't reject

#lagrange multiplier
u = resid(lm_jour)
x = log(journals$citeprice)
model_u = lm(u ~ x + I(x^2) + I(x^3), data = journals)
summary(model_u) #Rsquare = .01611
qchisq(.95, df = 2) #5.99
nrow(journals) * .01611 # < 5.99 so we can't reject

#non-nested tests
model1 = lm(log(subs) ~ citations, data = Journals)
model2 = lm(log(subs) ~ pages, data = Journals)

#encompassing F test
encomptest(model1, model2, data = Journals) #model 1 is better

#Davidson MacKinnon J test
jtest(model1, model2, data = Journals) #model 1 is better


AIC(model1) #smaller is better
AIC(model2)