library("plm")
library(performance)
library(nortest)
library(readxl)
library(pcse)

panel <- data_uji_regresi_panel
head(panel)
str(panel)


#deskriptif
summary(panel)
cor(panel[,-c(1:3)])
  


paneldata <-pdata.frame(panel, index=c("No", "Tahun"))
model <- Miskin~Inflasi+PDRB
##pooled
pooled <-plm(model, paneldata, model = "pooling")
summary(pooled)
check_collinearity(pooled)
check_autocorrelation(pooled)
check_heteroscedasticity(pooled)
 
##fixed effects
fixed <-plm(model,paneldata,model ="within", effect = "individual")
summary(fixed)
check_autocorrelation(fixed)
check_heteroscedasticity(fixed)
pwartest(model, data = paneldata)

##random effect
random <-plm(model,paneldata,model ="random", effect = "individual")
summary(random)
check_autocorrelation(random)
check_heteroscedasticity(random)

##chow test
chow_panel<-pFtest(fixed,pooled)
chow_panel
##hausman test
hausman_panel<-phtest(fixed,random)
hausman_panel
##bp test
bp_panel<-plmtest(pooled, type = ("bp"))
bp_panel

##MODEL GLS
gls <- pggls(Miskin~Inflasi+PDRB, data = paneldata, model = "pooling")
summary(gls)
check_autocorrelation(gls)
check_heteroscedasticity(gls)

ranef(gls)

