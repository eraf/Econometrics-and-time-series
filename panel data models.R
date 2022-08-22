d <- read.csv("panel_wage.csv")
library(plm)
names(d)

paneldata = pdata.frame(d, index = c("id", "t"))

#Pooled OLS estimate
pooled_OLS = plm(lwage ~ exp+exp2+wks+ed, 
                 data = paneldata, model = "pooling")
summary(pooled_OLS)

#first difference estimator
fd = plm(lwage ~ exp + exp2 + wks + ed, 
         data = paneldata, model = "fd")
summary(fd)

#fixed effects estimator
fe = plm(lwage ~ exp + exp2 + wks + ed, 
         data = paneldata, model = "within")
summary(fe)

#random effects model
re = plm(lwage ~ exp + exp2 + wks + ed, 
             data = paneldata, model = "random")
summary(random)

#Lagrange FF multiplier test
pFtest(fe, pooled_OLS) 
pFtest(pooled_OLS, fe)
#always put fe first

#Hausman specification test
phtest(fe, re)
phtest(re, fe)
