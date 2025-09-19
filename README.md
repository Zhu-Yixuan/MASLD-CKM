#Code of the Cox proportional hazards model
setwd("C://Users//HW//Desktop")
library(survival)
library(foreign)
library(rmda)
library(survminer)
library(openxlsx)
library(RISCA)
library(ipw)
bc<-read.xlsx("CKM.xlsx")
str(bc)

bc$SEX<-as.factor(bc$SEX)
bc$Race<-as.factor(bc$Race)
bc$CKM01<-as.factor(bc$CKM01)
bc$smoking<-as.factor(bc$smoking)
#all cause
#CKM01
fit=coxph(Surv(time,status) ~CKM01,data=bc)
summary(fit)

fit=coxph(Surv(time,status) ~CKM01+Age+SEX+PIR+Race+FIB4,data=bc)
summary(fit)
fit=coxph(Surv(time,status) ~CKM01+Age+SEX+PIR+Race+FIB4+smoking,data=bc)
summary(fit)

w1<- ipwpoint(
  exposure = CKM01,
  family = "multinomial",
  numerator = ~ 1,
  denominator = ~Age+SEX+Race+PIR+FIB4,
  data = bc)

bc$w1<-w1$ipw.weights
fit.IPTW=coxph(Surv(time,status)~CKM01, data=bc,weights=bc$w1)
summary(fit.IPTW)
fit.IPTW=coxph(Surv(time,status)~CKM01+Age+SEX+Race+PIR+FIB4, data=bc,weights=bc$w1)
summary(fit.IPTW)

w1<- ipwpoint(
  exposure = CKM01,
  family = "multinomial",
  numerator = ~ 1,
  denominator = ~Age+SEX+Race+PIR+FIB4+smoking,
  data = bc)
fit.IPTW=coxph(Surv(time,status)~CKM01+Age+SEX+Race+PIR+FIB4+smoking, data=bc,weights=bc$w1)
summary(fit.IPTW)

#Code of the Kaplan-Meier survival curves
library(survival)
library(survminer)
library(foreign)
library("openxlsx")
setwd("C://Users//HW//Desktop")
dt<-read.xlsx("CKM.xlsx")


fit1<-survfit(Surv(time, status) ~ CKM01,  data=dt)
summary(fit1)
plot(fit1)
ggsurvplot(fit1,
           xlab="Time (years)",ylab="cif",
           title="",
           surv.scale="percent" ,
           data = dt,     
           pval=TRUE,         
           pval.coord = c(1, 0.4),
           pval.method.coord=c(1,0.55),
           pval.method=TRUE,  
           palette = "lancet",
           legend.title = "",
           risk.table = T, 
           risk.table.y.text = FALSE,
           tables.theme = theme_cleantable(),
           legend.labs = c("Stages 0-1", "Stage 2", "Stages 3 and 4"),
           xlim = c(0,30), ylim = c(0,1),
           break.x.by = 5,
           conf.int = TRUE,
           font.y = c(16, "plain", "black"),
           font.x = c(16, "plain", "black"),
           pval.size = 5,
           pval.method.size = 5,
           risk.table.fontsize=4)
res.sum<-surv_summary(fit1)
head(res.sum)
attr(res.sum,"table")
