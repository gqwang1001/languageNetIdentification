library(tidyverse)
library(lmtest)
library(AER)
# Load Data ---------------------------------------------------------------
dat<-read.csv("data/fsmri.csv")
dat1 <- readxl::read_xlsx("data/Master1.xlsx", sheet = 2)
dat.handedness = dat1 %>% select(`subject ID`, Handedness) 
colnames(dat.handedness) = c("subject", "Handedness")
dat.handedness = dat.handedness[1:43, ]
dat.handedness$Handedness = factor(dat.handedness$Handedness,levels = c("L", "R", "B"), labels = c("L", "R", "B"))
dat.handedness$subject = dat.handedness$subject %>% as.factor()

dat.logit <- dat
dat.logit$Reviewer = factor(dat.logit$Reviewer, levels = c("1", "2", "3"), labels = c("R1", 'R2',"R3"))
dat.logit$Type = factor(dat.logit$Type, levels = c("20", "50"), labels = paste0("ICA", c(20,50)))
dat.logit$Criterion = factor(dat.logit$Criterion, levels = c("1","2","3"), labels = c('Top1', 'Top2', 'Top3'))
dat.logit$subject = dat.logit$subjec1.ID
dat.logit<- left_join(dat.logit, dat.handedness, by="subject")


# Explore Data  Analysis --------------------------------------------------
plotdata <- dat.logit %>% 
  group_by(Reviewer, Criterion, Type) %>% 
  summarize(n = n(), n_postive = sum(Result)) %>% 
  mutate(pct = n_postive/n,
         lbl = scales::percent(pct), 
         se = sqrt(pct*(1-pct)/n))

p1 <- ggplot(data = plotdata, aes(x = Reviewer, y = pct , fill = Criterion))
edaplot = 
  p1 +
  geom_bar(stat = 'identity', position = "dodge")+
  geom_text(aes(label = lbl),  size = 3, position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = pct-1.96*se, ymax = pct+1.96*se), width = 0.2, position = position_dodge(width = 0.9))+
  facet_wrap(~Type)+
  scale_y_continuous(breaks = seq(0, 1, .2), labels = scales::percent)+
  ylab("Percent")
ggsave('figs/EDA.png', edaplot, height = 5, width = 11, units = 'in')

p2 <- ggplot(data = plotdata, aes(x = Reviewer, y = pct , fill = Type))
edaplot1 = 
  p2 +
  geom_bar(stat = 'identity', position = "dodge")+
  geom_text(aes(label = lbl),  size = 3, position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = pct-1.96*se, ymax = pct+1.96*se), width = 0.2, position = position_dodge(width = 0.9))+
  facet_wrap(~Criterion)+
  scale_y_continuous(breaks = seq(0, 1, .2), labels = scales::percent)+
  ylab("Percent")
ggsave('figs/EDA1.png', edaplot1, height = 4, width = 11, units = 'in')

p3 <- ggplot(data = plotdata, aes(x = Criterion, y = pct , fill = Reviewer))
edaplot2 = 
  p3 +
  geom_bar(stat = 'identity', position = "dodge")+
  geom_text(aes(label = lbl),  size = 3, position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = pct-1.96*se, ymax = pct+1.96*se), width = 0.2, position = position_dodge(width = 0.9))+
  facet_wrap(~Type)+
  scale_y_continuous(breaks = seq(0, 1, .2), labels = scales::percent)+
  ylab("Percent")
ggsave('figs/EDA2.png', edaplot2, height = 5, width = 11, units = 'in')

fit0.0 <- glm(data = dat.logit, Result ~ Reviewer + Criterion + subject, family = binomial)
summary(fit0.0)

fit0 <- glm(data = dat.logit, Result ~ Reviewer + Criterion, family = binomial)
summary(fit0)
fit1 <- glm(data = dat.logit, Result ~ Reviewer + Type + Criterion, family = binomial)
summary(fit1)
fit2 <- glm(data = dat.logit, Result ~ Reviewer * Criterion + Type, family = binomial)
summary(fit2)

lrtest(fit0, fit1)

plot(fit0)
dispersiontest(fit0)


# Conditional Logistic Regression -----------------------------------------
library(survival)
clogit.fit0 = clogit(Result ~ Reviewer + Criterion + strata(ID), data = dat.logit)
summary(clogit.fit0)
clogit.fit1 = clogit(Result ~ Reviewer + Criterion + Type+ strata(ID), data = dat.logit)
summary(clogit.fit1)

AIC(clogit.fit0)
AIC(fit0)
# logistic random intercept model ----------------------------------------

library(lme4)
ri.fit = glmer(data = dat.logit, Result ~ (1|subject) + Reviewer + Criterion, family = binomial)
summs = summary(ri.fit)
exp(summs$coefficients[,1])

pbgmm <- pbnm::pbnm(ri.fit,fit0, cores = 8, tasks = 10, seed = 1)
summary(pbgmm)
confs = confint(ri.fit)
exp(confs)

ri.fit1 = glmer(data = dat.logit, Result ~ (1|subject) + Reviewer * Criterion, family = binomial)
summary(ri.fit1)

ri.fit2 = glmer(data = dat.logit, Result ~ (1|subject) + Reviewer + Criterion + Type, family = binomial)
summary(ri.fit2)
lmtest::lrtest(ri.fit, ri.fit2)

plot(ri.fit)
ri.fit.1 = glmer(data = dat.logit, Result ~ (1|subject) + Reviewer + Criterion+Handedness, family = binomial)
summary(ri.fit.1)

ri.fit1.2 = glmer(data = dat.logit, Result ~ (1|subject) + relevel(Reviewer, 2) + relevel(Criterion,2), family = binomial)
summs2 = summary(ri.fit1.2)
exp(summs2$coefficients[,1])
confs2 = confint(ri.fit1.2)
exp(confs2)

# loglinear model ---------------------------------------------------------

dat.loglinear = dat.logit %>% 
  filter(Criterion=="Top1", Type == "ICA20") %>% 
  spread(Reviewer, Result)
ctg.table = table(dat.loglinear$R1, dat.loglinear$R2, dat.loglinear$R3)
with(dat.loglinear, cohen.kappa(cbind(R1, R2, R3)))

ctg.table[,,1]
dat.ll = data.frame(n = as.vector(ctg.table), R1 = c(0,0,1,1, 0,0,1,1), R2 = c(0,1,0,1,0,1,0,1), R3 = c(rep(0,4), rep(1, 4)))

fig.ll = glm (data = dat.ll, n ~ R1*R2+ R2*R3, family = "poisson")
summary(fig.ll)

dat.lg = dat.logit %>% 
  filter(Criterion == "Top1", Type == "ICA20")
fig.lg = glm(data = dat.lg, Result ~ Reviewer, family = binomial)
summary(fig.lg)

dat.lg = dat.logit %>% 
  filter(Criterion == "Top1", Type == "ICA20")
fig.lg = clogit(data = dat.lg, Result ~ Reviewer+strata(ID))
summary(fig.lg)
AIC(fig.lg)

dat.loglinear = dat.logit %>% 
  spread(Reviewer, Result)

# model for logit regression with confidence rating -----------------------

dat.cr = dat.logit %>% 
  filter(Criterion == 'Top1')
dat.ratings = readxl::read_xlsx('data/Master.xlsx', sheet = 1)[1:43, ]
dat.ratings = dat.ratings[, c(1, 5, 7, 9, 11, 13, 15)]
colnames(dat.ratings) = c('subject', paste0(rep(c('R1_', 'R2_', 'R3_'), each =2), rep(c('ICA20', 'ICA50'), 3)))
dat.ratings.long = dat.ratings %>% gather("methods", "confRate", R1_ICA20:R3_ICA50)
reviewers <- apply(dat.ratings.long, 1, function(x) {
  sx = strsplit(x[2], "_")
  sx[[1]][1]})
Criterions <- apply(dat.ratings.long, 1, function(x) {
  sx = strsplit(x[2], "_")
  sx[[1]][2]})
reviewers <- factor(reviewers, levels = c("R1", "R2", "R3"), labels = c("R1", "R2", "R3"))
types <- factor(Criterions, levels = paste0("ICA", c(20, 50)), labels =paste0("ICA", c(20, 50)))
dat.ratings.long$Reviewer = reviewers
dat.ratings.long$Type= types

dat.cr = left_join(dat.cr, dat.ratings.long, by = c("subject", "Reviewer", "Type"))


ri.fit.cr = glmer(data = dat.cr, Result ~ (1|subject) + Reviewer + confRate, family = binomial)
summary(ri.fit.cr)

library(survival)
fit.cr.clogit = clogit(data = dat.cr, Result ~ Reviewer + confRate + strata(subject))
summary(fit.cr.clogit)


# plot ROC ----------------------------------------------------------------
library(caret)
library(pROC)

subjID = unique(dat.logit$ID)
predsArray = c()
for (i in subjID) {
  data.train = dat.logit[dat.logit$ID!=i, ]
  data.test = dat.logit[dat.logit$ID==i, ]
  ri.fit.train = glmer(data = data.train, Result ~ (1|subject) + Reviewer + Criterion, family = binomial)
  preds = predict(ri.fit, newdata = data.test, type = "response")
  
  predsArray = rbind(predsArray, cbind(data.test$Result, preds))
}

png(filename = 'figs/ROCcurve.png', width = 500, height = 500)
proc.obj <- roc(predsArray[,1], predsArray[,2], smooth = F, ci = T, plot = T, print.auc = T)
sens.ci <- ci.se(proc.obj)
plot(sens.ci, type = 'shape', col = "lightblue")
dev.off()
