library(gmodels)
library(psych)
library(sandwich)
library("exact2x2")
####
dat<-read.csv("data/fsmri.csv")
##################Function#############
sign.test<-function(x=0,y=NULL,alternative="two.sided"){
  n<-sum((x-y)!=0)
  T<-sum(x<y)
  if (alternative=="less") {
    p.value<-pbinom(T,n,0.5)}
  if (alternative=="greater"){
    p.value<- 1-pbinom(T-1,n,0.5)}
  if (alternative=="two.sided"){
    p.value<-2*min(1-pbinom(T-1,n,0.5),pbinom(T,n,0.5))}
  list(n=n,alternative=alternative,T=T,p.value=p.value)}
##########subset 1
r1n20c1<-subset(dat,Reviewer==1&Type==20&Criterion==1)[,1:6]

n1 = length(r1n20c1$Result)    # valid responses count 
k1 = sum(r1n20c1$Result == 1) 
pbar = k/n

prop.test(c(2*k2,2*k1),c(43,43), alternative="greater") 
prop.test(c(k1,k2),c(n1,n2)) 

##########subset 2
r1n20c2<-subset(dat,Reviewer==1&Type==20&Criterion==2)[,1:6]

n2 = length(r1n20c2$Result)    # valid responses count 
k2 = sum(r1n20c2$Result == 1) 
prop.test(k2, n2) 

##########subset 3
r1n20c3<-subset(dat,Reviewer==1&Type==20&Criterion==3)

n3 = length(r1n20c3$Result)    # valid responses count 
k3 = sum(r1n20c3$Result == 1) 
prop.test(k3, n3) 



##########subset 4
r1n50c1<-subset(dat,Reviewer==1&Type==50&Criterion==1)

n4 = length(r1n50c1$Result)    # valid responses count 
k4 = sum(r1n50c1$Result == 1) 
prop.test(k4, n4) 

##########subset 5
r1n50c2<-subset(dat,Reviewer==1&Type==50&Criterion==2)

n5 = length(r1n50c2$Result)    # valid responses count 
k5 = sum(r1n50c2$Result == 1) 
prop.test(k5, n5) 

##########subset 6
r1n50c3<-subset(dat,Reviewer==1&Type==50&Criterion==3)

n6 = length(r1n50c3$Result)    # valid responses count 
k6 = sum(r1n50c3$Result == 1) 
prop.test(k6, n6) 

##########subset 7
r2n20c1<-subset(dat,Reviewer==2&Type==20&Criterion==1)

n7 = length(r2n20c1$Result)    # valid responses count 
k7 = sum(r2n20c1$Result == 1) 
prop.test(k7, n7) 

##########subset 8
r2n20c2<-subset(dat,Reviewer==2&Type==20&Criterion==2)

n8 = length(r2n20c2$Result)    # valid responses count 
k8 = sum(r2n20c2$Result == 1) 
prop.test(k8, n8)

##########subset 9
r2n20c3<-subset(dat,Reviewer==2&Type==20&Criterion==3)

n9 = length(r2n20c3$Result)    # valid responses count 
k9 = sum(r2n20c3$Result == 1) 
prop.test(k9, n9)

##########subset 10
r2n50c1<-subset(dat,Reviewer==2&Type==50&Criterion==1)

n10 = length(r2n50c1$Result)    # valid responses count 
k10 = sum(r2n50c1$Result == 1) 
prop.test(k10, n10)

##########subset 11
r2n50c2<-subset(dat,Reviewer==2&Type==50&Criterion==2)

n11 = length(r2n50c2$Result)    # valid responses count 
k11 = sum(r2n50c2$Result == 1) 
prop.test(k11, n11)

##########subset 12
r2n50c3<-subset(dat,Reviewer==2&Type==50&Criterion==3)

n12 = length(r2n50c3$Result)    # valid responses count 
k12 = sum(r2n50c3$Result == 1) 
prop.test(k12, n12)

##########subset 13
r3n20c1<-subset(dat,Reviewer==3&Type==20&Criterion==1)

n13 = length(r3n20c1$Result)    # valid responses count 
k13 = sum(r3n20c1$Result == 1) 
prop.test(k13, n13)

##########subset 14
r3n20c2<-subset(dat,Reviewer==3&Type==20&Criterion==2)

n14 = length(r3n20c2$Result)    # valid responses count 
k14 = sum(r3n20c2$Result == 1) 
prop.test(k14, n14)

##########subset 15
r3n20c3<-subset(dat,Reviewer==3&Type==20&Criterion==3)

n15 = length(r3n20c3$Result)    # valid responses count 
k15 = sum(r3n20c3$Result == 1) 
prop.test(k15, n15)

##########subset 16
r3n50c1<-subset(dat,Reviewer==3&Type==50&Criterion==1)

n16 = length(r3n50c1$Result)    # valid responses count 
k16 = sum(r3n50c1$Result == 1) 
prop.test(k16, n16)

##########subset 17
r3n50c2<-subset(dat,Reviewer==3&Type==50&Criterion==2)

n17 = length(r3n50c2$Result)    # valid responses count 
k17 = sum(r3n50c2$Result == 1) 
prop.test(k17, n17)


##########subset 18
r3n50c3<-subset(dat,Reviewer==3&Type==50&Criterion==3)

n18 = length(r3n50c3$Result)    # valid responses count 
k18 = sum(r3n50c3$Result == 1) 
prop.test(k18, n18)

############Vs top
prop.test(x = c(k2, k1), n = c(43, 43), correct = FALSE)
sign.test(x=r1n20c1$Result,y=r1n20c2$Result,alternative="greater")
SIGN.test(x=r1n20c1$Result,y=r1n20c2$Result,alternative="less")
mcnemar.exact(r1n20c1$Result,r1n20c2$Result)
x<-cbind(r1n20c1$Result,r1n20c2$Result)
prop.test(x = c(k3, k1), n = c(43, 43), correct = FALSE)
(k3/n-k1/n)*100
sign.test(x=r1n20c1$Result,y=r1n20c3$Result,alternative="greater")

prop.test(x = c(k3, k2), n = c(43, 43), correct = FALSE)
(k3/n-k2/n)*100
sign.test(x=r1n20c2$Result,y=r1n20c3$Result,alternative="greater")



prop.test(x = c(k5, k4), n = c(43, 43), correct = FALSE)
(k5/n-k4/n)*100
sign.test(x=r1n50c1$Result,y=r1n50c2$Result,alternative="greater")

prop.test(x = c(k6, k4), n = c(43, 43), correct = FALSE)
(k6/n-k4/n)*100
sign.test(x=r1n50c1$Result,y=r1n50c3$Result,alternative="greater")

prop.test(x = c(k6, k5), n = c(43, 43), correct = FALSE)
(k6/n-k5/n)*100
sign.test(x=r1n50c2$Result,y=r1n50c3$Result,alternative="greater")

prop.test(x = c(k8, k7), n = c(43, 43), correct = FALSE)
(k8/n-k7/n)*100
sign.test(x=r2n20c1$Result,y=r2n20c2$Result,alternative="greater")

prop.test(x = c(k9, k7), n = c(43, 43), correct = FALSE)
(k9/n-k7/n)*100
sign.test(x=r2n20c1$Result,y=r2n20c3$Result,alternative="greater")

prop.test(x = c(k9, k8), n = c(43, 43), correct = FALSE)
(k9/n-k8/n)*100
sign.test(x=r2n20c2$Result,y=r2n20c3$Result,alternative="greater")

prop.test(x = c(k11,k10), n = c(43, 43), correct = FALSE)
(k11/n-k10/n)*100
sign.test(x=r2n50c1$Result,y=r2n50c2$Result,alternative="greater")

prop.test(x = c(k12, k10), n = c(43, 43), correct = FALSE)
(k12/n-k10/n)*100
sign.test(x=r2n50c1$Result,y=r2n50c3$Result,alternative="greater")

prop.test(x = c(k12, k11), n = c(43, 43), correct = FALSE)
(k12/n-k11/n)*100
sign.test(x=r2n50c2$Result,y=r2n50c3$Result,alternative="greater")

prop.test(x = c(k14,k13), n = c(43, 43), correct = FALSE)
(k14/n-k13/n)*100
sign.test(x=r3n20c1$Result,y=r3n20c2$Result,alternative="greater")

prop.test(x = c(k15, k13), n = c(43, 43), correct = FALSE)
(k15/n-k13/n)*100
sign.test(x=r3n20c1$Result,y=r3n20c3$Result,alternative="greater")

prop.test(x = c(k15, k14), n = c(43, 43), correct = FALSE)
(k15/n-k14/n)*100
sign.test(x=r3n20c2$Result,y=r3n20c3$Result,alternative="greater")

prop.test(x = c(k17,k16), n = c(43, 43), correct = FALSE)
(k17/n-k16/n)*100
sign.test(x=r3n50c1$Result,y=r3n50c2$Result,alternative="greater")

prop.test(x = c(k18, k16), n = c(43, 43), correct = FALSE)
(k18/n-k16/n)*100
sign.test(x=r3n50c1$Result,y=r3n50c3$Result,alternative="greater")

prop.test(x = c(k18, k17), n = c(43, 43), correct = FALSE)
(k18/n-k17/n)*100
sign.test(x=r3n50c2$Result,y=r3n50c3$Result,alternative="greater")



### 20 vs 50
prop.test(x = c(k1,k4), n = c(43, 43), correct = FALSE)
(k1/n-k4/n)*100

prop.test(x = c(k2, k5), n = c(43, 43), correct = FALSE)
(k2/n-k5/n)*100
prop.test(x = c(k3, k6), n = c(43, 43), correct = FALSE)
(k3/n-k6/n)*100


prop.test(x = c(k7,k10), n = c(43, 43), correct = FALSE)
(k7/n-k10/n)*100
prop.test(x = c(k8, k11), n = c(43, 43), correct = FALSE)
(k8/n-k11/n)*100
prop.test(x = c(k9, k12), n = c(43, 43), correct = FALSE)
(k9/n-k12/n)*100


prop.test(x = c(k13,k16), n = c(43, 43), correct = FALSE)
(k13/n-k16/n)*100
prop.test(x = c(k14, k17), n = c(43, 43), correct = FALSE)
(k14/n-k17/n)*100
prop.test(x = c(k15, k18), n = c(43, 43), correct = FALSE)
(k15/n-k18/n)*100

###########################
###Topchoice
sub1<-subset(dat,Reviewer==1&Criterion==1)
sub2<-subset(dat,Reviewer==2&Criterion==1)
sub3<-subset(dat,Reviewer==3&Criterion==1)

cohen.kappa(cbind(sub1$Result,sub3$Result,sub2$Result))
###Top tow
sub1<-subset(dat,Reviewer==1&Criterion==2)
sub2<-subset(dat,Reviewer==2&Criterion==2)
sub3<-subset(dat,Reviewer==3&Criterion==2)

cohen.kappa(cbind(sub1$Result,sub3$Result,sub2$Result))
###Top three
sub1<-subset(dat,Reviewer==1&Criterion==3)
sub2<-subset(dat,Reviewer==2&Criterion==3)
sub3<-subset(dat,Reviewer==3&Criterion==3)

cohen.kappa(cbind(sub1$Result,sub3$Result,sub2$Result))
cohen.kappa(sub3$Result,sub2$Result)

###########Kappa by reviewer
cohen.kappa(cbind(subset(dat,Reviewer==1&Type==20)$Result,subset(dat,Reviewer==1&Type==50)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==1&Type==20&Criterion==1)$Result,subset(dat,Reviewer==1&Type==50&Criterion==1)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==1&Type==20&Criterion==2)$Result,subset(dat,Reviewer==1&Type==50&Criterion==2)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==1&Type==20&Criterion==3)$Result,subset(dat,Reviewer==1&Type==50&Criterion==3)$Result))

cohen.kappa(cbind(subset(dat,Reviewer==2&Type==20)$Result,subset(dat,Reviewer==2&Type==50)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==2&Type==20&Criterion==1)$Result,subset(dat,Reviewer==2&Type==50&Criterion==1)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==2&Type==20&Criterion==2)$Result,subset(dat,Reviewer==2&Type==50&Criterion==2)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==2&Type==20&Criterion==3)$Result,subset(dat,Reviewer==2&Type==50&Criterion==3)$Result))

cohen.kappa(cbind(subset(dat,Reviewer==3&Type==20)$Result,subset(dat,Reviewer==3&Type==50)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==3&Type==20&Criterion==1)$Result,subset(dat,Reviewer==3&Type==50&Criterion==1)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==3&Type==20&Criterion==2)$Result,subset(dat,Reviewer==3&Type==50&Criterion==2)$Result))
cohen.kappa(cbind(subset(dat,Reviewer==3&Type==20&Criterion==3)$Result,subset(dat,Reviewer==3&Type==50&Criterion==3)$Result))

###########regression
reg1<-glm(dat$Result~as.factor(dat$Reviewer)+as.factor(dat$Type)+as.factor(dat$Criterion), family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])

R1<-cbind(c(k1,k2,k3)/n,c(1,2,3))
R2<-cbind(c(k4,k5,k6)/n,c(1,2,3))
R3<-cbind(c(k7,k8,k9)/n,c(1,2,3))
R4<-cbind(c(k10,k11,k12)/n,c(1,2,3))
R5<-cbind(c(k13,k14,k15)/n,c(1,2,3))
R6<-cbind(c(k16,k17,k18)/n,c(1,2,3))

plot(R1[,2],R1[,1], type ="l", col="blue", xlab = "criteria", ylab ="Proportion of correctness", ylim =c(0.3,0.8))
lines(R2[,2],R2[,1], type ="l", col="red", xlab = "criteria", ylab ="Proportion of correctness")
lines(R3[,2],R3[,1], type ="l", col="dark green", xlab = "criteria", ylab ="Proportion of correctness")
lines(R4[,2],R4[,1], type ="l", col="purple", xlab = "criteria", ylab ="Proportion of correctness")
lines(R5[,2],R5[,1], type ="l", col="dark orange", xlab = "criteria", ylab ="Proportion of correctness")
lines(R6[,2],R6[,1], type ="l", col="black", xlab = "criteria", ylab ="Proportion of correctness")

plot(sub1$Result,sub1$Criterion)


#######
prop.test(x = c(k2, k1), n = c(43, 43), correct = FALSE, alternative ="greater")
prop.test(x = c(k3, k1), n = c(43, 43), correct = FALSE, alternative ="greater")
(k3/n-k1/n)*100
prop.test(x = c(k3, k2), n = c(43, 43), correct = FALSE, alternative ="greater")
(k3/n-k2/n)*100



prop.test(x = c(k5, k4), n = c(43, 43), correct = FALSE, alternative ="greater")
(k5/n-k4/n)*100
prop.test(x = c(k6, k4), n = c(43, 43), correct = FALSE, alternative ="greater")
(k6/n-k4/n)*100
prop.test(x = c(k6, k5), n = c(43, 43), correct = FALSE, alternative ="greater")
(k6/n-k5/n)*100

prop.test(x = c(k8, k7), n = c(43, 43), correct = FALSE, alternative ="greater")
(k8/n-k7/n)*100
prop.test(x = c(k9, k7), n = c(43, 43), correct = FALSE, alternative ="greater")
(k9/n-k7/n)*100
prop.test(x = c(k9, k8), n = c(43, 43), correct = FALSE, alternative ="greater")
(k9/n-k8/n)*100

prop.test(x = c(k11,k10), n = c(43, 43), correct = FALSE, alternative ="greater")
(k11/n-k10/n)*100
prop.test(x = c(k12, k10), n = c(43, 43), correct = FALSE, alternative ="greater")
(k12/n-k10/n)*100
prop.test(x = c(k12, k11), n = c(43, 43), correct = FALSE, alternative ="greater")
(k12/n-k11/n)*100

prop.test(x = c(k14,k13), n = c(43, 43), correct = FALSE, alternative ="greater")
(k14/n-k13/n)*100
prop.test(x = c(k15, k13), n = c(43, 43), correct = FALSE, alternative ="greater")
(k15/n-k13/n)*100
prop.test(x = c(k15, k14), n = c(43, 43), correct = FALSE, alternative ="greater")
(k15/n-k14/n)*100

prop.test(x = c(k17,k16), n = c(43, 43), correct = FALSE, alternative ="greater")
(k17/n-k16/n)*100
prop.test(x = c(k18, k16), n = c(43, 43), correct = FALSE, alternative ="greater",conf.level = 0.95)
(k18/n-k16/n)*100
prop.test(x = c(k18, k17), n = c(43, 43), correct = FALSE, alternative ="greater",conf.level = 0.95)
(k18/n-k17/n)*100

##########Naive model regression

reg1<-glm(dat$Result~as.factor(dat$Reviewer),family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])

reg1<-glm(dat$Result~as.factor(dat$Type), family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])

reg1<-glm(dat$Result~as.factor(dat$Criterion), family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])


##########Robust variance
reg1<-glm(dat$Result~as.factor(dat$Reviewer),family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])
vcovHC(reg1,type="HC")
sandwich_se <- diag(vcovHC(reg1, type = "HC"))^0.5
 
exp (rbind((coef(reg1)-1.96*sandwich_se),(coef(reg1)+1.96*sandwich_se)))
z_stat <- coef(reg1)/sandwich_se
p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE)
p_values

reg1<-glm(dat$Result~as.factor(dat$Type), family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])
vcovHC(reg1,type="HC")
sandwich_se <- diag(vcovHC(reg1, type = "HC"))^0.5

exp (rbind((coef(reg1)-1.96*sandwich_se),(coef(reg1)+1.96*sandwich_se)))
z_stat <- coef(reg1)/sandwich_se
p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE)
p_values

reg1<-glm(dat$Result~relevel(as.factor(dat$Criterion),2), family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])
vcovHC(reg1,type="HC")
sandwich_se <- diag(vcovHC(reg1, type = "HC"))^0.5

exp (rbind((coef(reg1)-1.96*sandwich_se),(coef(reg1)+1.96*sandwich_se)))
z_stat <- coef(reg1)/sandwich_se
p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE)
p_values


reg1<-glm(dat$Result~as.factor(dat$Reviewer)+as.factor(dat$Type)+relevel(as.factor(dat$Criterion),2),family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])
vcovHC(reg1,type="HC")
sandwich_se <- diag(vcovHC(reg1, type = "HC"))^0.5

exp (rbind((coef(reg1)-1.96*sandwich_se),(coef(reg1)+1.96*sandwich_se)))
z_stat <- coef(reg1)/sandwich_se
p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE)
p_values
##########Overall
r1<-subset(dat,Reviewer==1)[,1:6]

n1 = length(r1$Result)    # valid responses count 
k1 = sum(r1$Result == 1) 
prop.test(k1, n1)

r2<-subset(dat,Reviewer==2)[,1:6]


n2 = length(r2$Result)    # valid responses count 
k2 = sum(r2$Result == 1) 
prop.test(k2, n2)


r3<-subset(dat,Reviewer==3)[,1:6]

n3 = length(r3$Result)    # valid responses count 
k3 = sum(r3$Result == 1) 
prop.test(k3, n3)


ica20<-subset(dat,Type==20)[,1:6]

n1 = length(ica20$Result)    # valid responses count 
k1 = sum(ica20$Result == 1) 
prop.test(k1, n1)

ica50<-subset(dat,Type==50)[,1:6]
n1 = length(ica50$Result)    # valid responses count 
k1 = sum(ica50$Result == 1) 
prop.test(k1, n1)

dat$age<-"."
dat$age[dat$Reviewer==1]<-17
dat$age[dat$Reviewer==2]<-2
dat$age[dat$Reviewer==3]<-6

reg1<-glm(dat$Result~as.numeric(dat$age)+as.factor(dat$Type)+relevel(as.factor(dat$Criterion),2),family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])
vcovHC(reg1,type="HC")
sandwich_se <- diag(vcovHC(reg1, type = "HC"))^0.5

exp (rbind((coef(reg1)-1.96*sandwich_se),(coef(reg1)+1.96*sandwich_se)))
z_stat <- coef(reg1)/sandwich_se
p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE)
p_values


reg1<-glm(dat$Result~(as.numeric(dat$age)),family="binomial")
summary (reg1)
cbind(exp(coefficients(reg1)),exp(confint(reg1)),coef(summary(reg1))[,4])
vcovHC(reg1,type="HC")
sandwich_se <- diag(vcovHC(reg1, type = "HC"))^0.5

exp (rbind((coef(reg1)-1.96*sandwich_se),(coef(reg1)+1.96*sandwich_se)))
z_stat <- coef(reg1)/sandwich_se
p_values <- pchisq(z_stat^2, 1, lower.tail=FALSE)
p_values