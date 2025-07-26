###################################
###################################
## CONJOINT EXPERIMENT ANALYSIS: ##
## EXTRA TEST OF HYPOTHESIS 1    ##
###################################
###################################

#######################################
## NON-MONETARY CONJOINT EXPERIMENT: ##
## APPENDIX SECTION F, pp. 8-9       ##
#######################################

# load necessary functions and libraries
install.packages("FindIt") 
library(FindIt)
install.packages("list")
install.packages("GEEmediate")
library("GEEmediate")
library("list")
library(foreach) 
install.packages("doParallel")
library(doParallel)
library("mvtnorm")
install.packages("Amelia")
require(Amelia)
install.packages("mlogit")
library("mlogit")
install.packages("binomlogit")
library("binomlogit")
install.packages("nnet")
library(nnet)
install.packages("gmodels")
library(gmodels)
install.packages("aod")
library(aod)
install.packages("ggplot2")
library(ggplot2)
library(Rcpp)
library("MASS")
library("foreign")
logistic <- function(x) exp(x)/(1+exp(x)) 
utils::install.packages(pkgs = "ggstatsplot")
install.packages("multiwayvcov")
library(multiwayvcov)
library(arm)
library(mvtnorm)
library(lme4)
library(multiwayvcov)
library(dplyr)

# Open log file
sink("[Your file path goes here]/Appendix_F_log", split=TRUE)

# Load and subset data
setwd("[Your file path goes here]")
mx.panel.2018 <- read.dta(file = "mx_2018_eqd_stata12.dta", convert.underscore = TRUE, convert.factors = FALSE)

mx.panel.2018 <- subset(mx.panel.2018, select = c(Z9, Z10, Q10A, Q10B, Q23B, Q18, edad, Q8, D4, Q1, D12, Q3, 
                                                  T.Q9.5, T.Q9.6, T.Q9.7, T.Q9.8, Q21, Q27A, T.Q27B.1, T.Q27B.2, 
                                                  SbjNum, Q18.w2, edad, Q8A.w2, Q8B.w2, D4, 
                                                  Q1, D12, Q3.w2, T.Q9.5.w2, T.Q9.6.w2, T.Q9.7.w2, T.Q9.8.w2, Q21.w2, 
                                                  Q27A.w2, T.Q27B.1.w2, T.Q27B.2.w2, c11.w2, c21.w2, c12.w2, c22.w2, 
                                                  c13.w2, c23.w2, c14.w2, c24.w2, SbjNum.w2, munpres.pan.in.2017, 
                                                  munpres.pri.in.2017, munpres.prd.in.2017, munpres.morena.in.2017,
                                                  munpres.pan.prd.in.2017, Q20))

mx.panel.2018$spoke.spanish.w1 <- ifelse(mx.panel.2018$Z9==2 & mx.panel.2018$Z10==1,0,1)
mx.panel.2018 <- mx.panel.2018[mx.panel.2018$spoke.spanish.w1==1,]

# Code variables for analysis
mx.panel.2018$panft.w1 <- mx.panel.2018$T.Q9.5
mx.panel.2018$panft.w1 <- ifelse(mx.panel.2018$T.Q9.5==99,NA,mx.panel.2018$T.Q9.5)
mx.panel.2018$prift.w1 <- mx.panel.2018$T.Q9.6
mx.panel.2018$prift.w1 <- ifelse(mx.panel.2018$T.Q9.6==99,NA,mx.panel.2018$T.Q9.6)
mx.panel.2018$morenaft.w1 <- mx.panel.2018$T.Q9.7
mx.panel.2018$morenaft.w1 <- ifelse(mx.panel.2018$T.Q9.7==99,NA,mx.panel.2018$T.Q9.7)
mx.panel.2018$prdft.w1 <- mx.panel.2018$T.Q9.8
mx.panel.2018$prdft.w1 <- ifelse(mx.panel.2018$T.Q9.8==99,NA,mx.panel.2018$T.Q9.8)

# Expand data 
mx.panel.2018$ce.code <- 0 # create id code in dataframe 1
mx.panel.2018.2 <- mx.panel.2018 # create second dataframe
mx.panel.2018.2$ce.code <- 1 # create id code in datafrane 2
mx.panel.2018.w2.ce <- rbind(mx.panel.2018, mx.panel.2018.2) # bind two dataframes into new one

# Code conjoint vars 
mx.panel.2018.w2.ce$ce.choice.w2 <- ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$Q27A.w2==2,0, 
                                    ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$Q27A.w2==1,1,
                                    ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$Q27A.w2==2,1, 
                                    ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$Q27A.w2==1,0,NA))))

mx.panel.2018.w2.ce$ce.pri.w2 <- ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c12.w2!=2,0, 
                                 ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c12.w2==2,1,
                                 ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c22.w2==2,1, 
                                 ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c22.w2!=2,0,NA))))

mx.panel.2018.w2.ce$ce.pan.w2 <- ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c12.w2!=1,0, 
                                 ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c12.w2==1,1,
                                 ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c22.w2==1,1, 
                                 ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c22.w2!=1,0,NA))))

mx.panel.2018.w2.ce$ce.morena.w2 <- ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c12.w2!=3,0, 
                                    ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c12.w2==3,1,
                                    ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c22.w2==3,1, 
                                    ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c22.w2!=3,0,NA))))

d <- 5
e <- 2
f <- 2

mx.panel.2018.w2.ce$voter.type.w2 <- 
  ifelse((mx.panel.2018.w2.ce$prift.w1>mx.panel.2018.w2.ce$panft.w1+d & 
          mx.panel.2018.w2.ce$prift.w1>mx.panel.2018.w2.ce$morenaft.w1+d & mx.panel.2018.w2.ce$prift.w1>e & mx.panel.2018.w2.ce$ce.pri.w2==1) | 
         (mx.panel.2018.w2.ce$panft.w1>mx.panel.2018.w2.ce$prift.w1+d & 
          mx.panel.2018.w2.ce$panft.w1>mx.panel.2018.w2.ce$morenaft.w1+d & mx.panel.2018.w2.ce$panft.w1>e & mx.panel.2018.w2.ce$ce.pan.w2==1) | 
         (mx.panel.2018.w2.ce$morenaft.w1>mx.panel.2018.w2.ce$panft.w1+d & 
          mx.panel.2018.w2.ce$morenaft.w1>mx.panel.2018.w2.ce$prift.w1+d & mx.panel.2018.w2.ce$morenaft.w1>e & mx.panel.2018.w2.ce$ce.morena.w2==1),0,
  ifelse((mx.panel.2018.w2.ce$prift.w1<mx.panel.2018.w2.ce$panft.w1 & mx.panel.2018.w2.ce$prift.w1<mx.panel.2018.w2.ce$prdft.w1 & 
           mx.panel.2018.w2.ce$prift.w1<mx.panel.2018.w2.ce$morenaft.w1 & mx.panel.2018.w2.ce$ce.pri.w2==1) | 
          (mx.panel.2018.w2.ce$panft.w1<mx.panel.2018.w2.ce$prift.w1 & mx.panel.2018.w2.ce$panft.w1<mx.panel.2018.w2.ce$prdft.w1 & 
           mx.panel.2018.w2.ce$panft.w1<mx.panel.2018.w2.ce$morenaft.w1 & mx.panel.2018.w2.ce$ce.pan.w2==1) | 
          (mx.panel.2018.w2.ce$morenaft.w1<mx.panel.2018.w2.ce$panft.w1 & mx.panel.2018.w2.ce$morenaft.w1<mx.panel.2018.w2.ce$prdft.w1 & 
           mx.panel.2018.w2.ce$morenaft.w1<mx.panel.2018.w2.ce$prift.w1 & mx.panel.2018.w2.ce$ce.morena.w2==1) | 
          (mx.panel.2018.w2.ce$prift.w1<=f & mx.panel.2018.w2.ce$panft.w1<=f & mx.panel.2018.w2.ce$morenaft.w1<=f & mx.panel.2018.w2.ce$prdft.w1<=f) |
          (mx.panel.2018.w2.ce$prdft.w1>mx.panel.2018.w2.ce$prift.w1 & mx.panel.2018.w2.ce$prdft.w1>mx.panel.2018.w2.ce$panft.w1 &
           mx.panel.2018.w2.ce$prdft.w1>mx.panel.2018.w2.ce$morenaft.w1),2,
  ifelse((mx.panel.2018$T.Q9.5==99 & mx.panel.2018$T.Q9.6==99 & mx.panel.2018$T.Q9.7==99 & mx.panel.2018$T.Q9.8==99),NA
         ,1)))

mx.panel.2018.w2.ce$ce.male.w2 <- ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c11.w2==2,0, 
                                  ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c11.w2==1,1,
                                  ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c21.w2==2,1, 
                                  ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c21.w2==1,0,NA))))

mx.panel.2018.w2.ce$ce.experience.w2 <- ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c13.w2==1,0, 
                                        ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c13.w2==2,1,
                                        ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c23.w2==2,1, 
                                        ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c23.w2==1,0,NA))))

mx.panel.2018.w2.ce$ce.payoff2.w2 <- ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c14.w2==1,0, 
                                     ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c14.w2==2,1,
                                     ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c14.w2==3,2, 
                                     ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c24.w2==1,0,
                                     ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c24.w2==2,1,
                                     ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c24.w2==3,2,NA))))))

mx.panel.2018.w2.ce$ce.payoff4.w2 <- ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c14.w2==1,0, 
                                     ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c14.w2==2,1,
                                     ifelse(mx.panel.2018.w2.ce$ce.code==0 & mx.panel.2018.w2.ce$c14.w2==3,1, 
                                     ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c24.w2==1,0,
                                     ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c24.w2==2,1,
                                     ifelse(mx.panel.2018.w2.ce$ce.code==1 & mx.panel.2018.w2.ce$c24.w2==3,1,NA))))))

mx.panel.2018.w2.ce$ce.payoff2.w2 <- factor(mx.panel.2018.w2.ce$ce.payoff2.w2,ordered=TRUE,levels=c("0","1","2"))
mx.panel.2018.w2.ce$ce.payoff4.w2 <- factor(mx.panel.2018.w2.ce$ce.payoff4.w2,ordered=TRUE,levels=c("0","1"))
mx.panel.2018.w2.ce$ce.male.w2 <- factor(mx.panel.2018.w2.ce$ce.male.w2,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.w2.ce$ce.experience.w2 <- factor(mx.panel.2018.w2.ce$ce.experience.w2,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.w2.ce$ce.pri.w2 <- factor(mx.panel.2018.w2.ce$ce.pri.w2,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.w2.ce$ce.pan.w2 <- factor(mx.panel.2018.w2.ce$ce.pan.w2,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.w2.ce$ce.morena.w2 <- factor(mx.panel.2018.w2.ce$ce.morena.w2,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.w2.ce$ce.code.w2 <- factor(mx.panel.2018.w2.ce$ce.code, ordered=FALSE,levels=c("0","1"))
mx.panel.2018.w2.ce$voter.type.w2 <- factor(mx.panel.2018.w2.ce$voter.type.w2, ordered=TRUE,levels=c("0", "1", "2"))

# Create dataframe for conjoint analysis
mx.panel.2018.w2.ce <- subset(mx.panel.2018.w2.ce, select = c(ce.choice.w2, ce.payoff2.w2, ce.payoff4.w2, voter.type.w2, ce.male.w2, ce.experience.w2, voter.type.w2, ce.code.w2, SbjNum.w2))
mx.panel.2018.w2.ce <- mx.panel.2018.w2.ce[complete.cases(mx.panel.2018.w2.ce),]

# Model
model.tables.section.6 <- CausalANOVA(formula=ce.choice.w2 ~ ce.payoff2.w2 + voter.type.w2 + ce.male.w2 + ce.experience.w2, 
                    int2.formula = ~ ce.payoff2.w2:voter.type.w2,
                    data=mx.panel.2018.w2.ce, pair.id = mx.panel.2018.w2.ce$ce.code.w2, diff = TRUE, cluster = mx.panel.2018.w2.ce$SbjNum.w2, 
                    nway=2, family = "binomial")

# The AME portion is Table A19 and the AMIE portion is Table A20 in Appendix F, p. 8
# ce.payoff2 = "Benefit"
# voter.type = "Supporter" if 0, "Weakly opposed" if 1, and "Strongly opposed" if 2.
summary(model.tables.section.6)

# Code for figures
my.amies.w2 <- model.tables.section.6$CI.table[[5]][, 1]

my.amie.diffs.doctor.w2 <- c(my.amies.w2[2]-my.amies.w2[1],
                             my.amies.w2[5]-my.amies.w2[4],
                             my.amies.w2[8]-my.amies.w2[7])

my.sds.w2 <- model.tables.section.6$CI.table[[5]][, 2] 

my.sds.diffs.doctor.w2 <- c(sqrt(my.sds.w2[2]^2+my.sds.w2[1]^2),
                            sqrt(my.sds.w2[5]^2+my.sds.w2[4]^2),
                            sqrt(my.sds.w2[8]^2+my.sds.w2[7]^2))

my.ci.lower.doctor.w2 <- c(my.amie.diffs.doctor.w2[1]-1.96*my.sds.diffs.doctor.w2[1], 
                           my.amie.diffs.doctor.w2[2]-1.96*my.sds.diffs.doctor.w2[2], 
                           my.amie.diffs.doctor.w2[3]-1.96*my.sds.diffs.doctor.w2[3])

my.ci.upper.doctor.w2 <- c(my.amie.diffs.doctor.w2[1]+1.96*my.sds.diffs.doctor.w2[1], 
                           my.amie.diffs.doctor.w2[2]+1.96*my.sds.diffs.doctor.w2[2], 
                           my.amie.diffs.doctor.w2[3]+1.96*my.sds.diffs.doctor.w2[3])

my.amie.data.frame.doctor.w2 <- data.frame(voter.type.w2=c("Supporters: Doctor visit", "Weakly opposed: Doctor visit", "Strongly opposed: Doctor visit"), 
                                AMIE.Differences=my.amie.diffs.doctor.w2, ci.lower.w2=my.ci.lower.doctor.w2, ci.upper.w2=my.ci.upper.doctor.w2)

my.amie.data.frame.doctor.w2$voter.type.w2 <- factor(my.amie.data.frame.doctor.w2$voter.type.w2, levels = c("Supporters: Doctor visit", "Weakly opposed: Doctor visit", "Strongly opposed: Doctor visit"))

# Figure A3 Panel A: Doctor's Visit in Appendix F, p. 9
pdf("[Your file path goes here]/Figure_A3_Panel_A.pdf")

amie.diff.plot <- ggplot() + geom_pointrange(data=my.amie.data.frame.doctor.w2, mapping=aes
                  (x=voter.type.w2, y=AMIE.Differences, ymin=ci.lower.w2, ymax=ci.upper.w2)) + 
                  coord_flip() + theme_bw() + theme(panel.background = element_rect(fill = "white"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = rel(1.5)),
                  axis.text = element_text(size = rel(1.5)))
amie.diff.plot <- amie.diff.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
                  amie.diff.plot + scale_y_continuous(breaks=seq(-.2,.3,.1)) + 
                  geom_hline(yintercept=0, linetype="dashed", color="black") 
dev.off()

my.amie.diffs.food.w2 <- c(my.amies.w2[3]-my.amies.w2[1],
                           my.amies.w2[6]-my.amies.w2[4],
                           my.amies.w2[9]-my.amies.w2[7])

my.sds.w2 <- model.tables.section.6$CI.table[[5]][, 2] 

my.sds.diffs.food.w2 <- c(sqrt(my.sds.w2[3]^2+my.sds.w2[1]^2),
                          sqrt(my.sds.w2[6]^2+my.sds.w2[4]^2),
                          sqrt(my.sds.w2[9]^2+my.sds.w2[7]^2))

my.ci.lower.food.w2 <- c(my.amie.diffs.food.w2[1]-1.96*my.sds.diffs.food.w2[1], 
                         my.amie.diffs.food.w2[2]-1.96*my.sds.diffs.food.w2[2], 
                         my.amie.diffs.food.w2[3]-1.96*my.sds.diffs.food.w2[3])

my.ci.upper.food.w2 <- c(my.amie.diffs.food.w2[1]+1.96*my.sds.diffs.food.w2[1], 
                         my.amie.diffs.food.w2[2]+1.96*my.sds.diffs.food.w2[2], 
                         my.amie.diffs.food.w2[3]+1.96*my.sds.diffs.food.w2[3])

my.amie.data.frame.food.w2 <- data.frame(voter.type.w2=c("Supporters: Groceries", "Weakly opposed: Groceries", "Strongly opposed: Groceries"), 
                              AMIE.Differences=my.amie.diffs.food.w2, ci.lower.w2=my.ci.lower.food.w2, ci.upper.w2=my.ci.upper.food.w2)

my.amie.data.frame.food.w2$voter.type.w2 <- factor(my.amie.data.frame.food.w2$voter.type.w2, levels = c("Supporters: Groceries", "Weakly opposed: Groceries", "Strongly opposed: Groceries"))

# Figure A3 Panel B: Groceries in Appendix F, p. 9
pdf("[Your file path goes here]/Figure_A3_Panel_B.pdf")

amie.diff.plot <- ggplot() + geom_pointrange(data=my.amie.data.frame.food.w2, mapping=aes
                  (x=voter.type.w2, y=AMIE.Differences, ymin=ci.lower.w2, ymax=ci.upper.w2)) + 
                  coord_flip() + theme_bw() + theme(panel.background = element_rect(fill = "white"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = rel(1.5)),
                  axis.text = element_text(size = rel(1.5)))
amie.diff.plot <- amie.diff.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
                  amie.diff.plot + scale_y_continuous(breaks=seq(-.2,.3,.1)) + 
                  geom_hline(yintercept=0, linetype="dashed", color="black") 
dev.off()

# Close log file
sink()

#############################
## NOT RUN - combined plot ##
#############################
                  
#my.amie.diffs.w2 <- c(my.amies.w2[2]-my.amies.w2[1],my.amies.w2[3]-my.amies.w2[1],
#                      my.amies.w2[5]-my.amies.w2[4],my.amies.w2[6]-my.amies.w2[4],
#                      my.amies.w2[8]-my.amies.w2[7],my.amies.w2[9]-my.amies.w2[7])
#my.sds.w2 <- fit2$CI.table[[5]][, 2] 
#my.sds.diffs.w2 <- c(sqrt(my.sds.w2[2]^2+my.sds.w2[1]^2), sqrt(my.sds.w2[3]^2+my.sds.w2[1]^2),
#                     sqrt(my.sds.w2[5]^2+my.sds.w2[4]^2), sqrt(my.sds.w2[6]^2+my.sds.w2[4]^2),
#                     sqrt(my.sds.w2[8]^2+my.sds.w2[7]^2), sqrt(my.sds.w2[9]^2+my.sds.w2[7]^2))  
#my.ci.lower.w2 <- c(my.amie.diffs.w2[1]-1.96*my.sds.diffs.w2[1], 
#                    my.amie.diffs.w2[2]-1.96*my.sds.diffs.w2[2], 
#                    my.amie.diffs.w2[3]-1.96*my.sds.diffs.w2[3],
#                    my.amie.diffs.w2[4]-1.96*my.sds.diffs.w2[4],
#                    my.amie.diffs.w2[5]-1.96*my.sds.diffs.w2[5],
#                    my.amie.diffs.w2[6]-1.96*my.sds.diffs.w2[6])
#my.ci.upper.w2 <- c(my.amie.diffs.w2[1]+1.96*my.sds.diffs.w2[1], 
#                    my.amie.diffs.w2[2]+1.96*my.sds.diffs.w2[2], 
#                    my.amie.diffs.w2[3]+1.96*my.sds.diffs.w2[3],
#                    my.amie.diffs.w2[4]+1.96*my.sds.diffs.w2[4],
#                    my.amie.diffs.w2[5]+1.96*my.sds.diffs.w2[5],
#                    my.amie.diffs.w2[6]+1.96*my.sds.diffs.w2[6])
#my.amie.data.frame.w2 <- data.frame(voter.type.w2=c("Supporters: Doctor visit", "Supporters: Groceries", "Weakly opposed: Doctor visit", "Weakly opposed: Groceries", "Strongly opposed: Doctor visit", "Strongly opposed: Groceries"), 
#                         AMIE.Differences=my.amie.diffs.w2, ci.lower.w2=my.ci.lower.w2, ci.upper.w2=my.ci.upper.w2)
#my.amie.data.frame.w2$voter.type.w2 <- factor(my.amie.data.frame.w2$voter.type.w2, levels = c("Supporters: Doctor visit", "Supporters: Groceries", "Weakly opposed: Doctor visit", "Weakly opposed: Groceries", "Strongly opposed: Doctor visit", "Strongly opposed: Groceries"))
#amie.diff.plot <- ggplot() + geom_pointrange(data=my.amie.data.frame.w2, mapping=aes
#                  (x=voter.type.w2, y=AMIE.Differences, ymin=ci.lower.w2, ymax=ci.upper.w2)) + 
#                  coord_flip() + theme_bw() + theme(panel.background = element_rect(fill = "white"),
#                  axis.title.y = element_blank(),
#                  axis.title.x = element_text(size = rel(1.2)),
#                  axis.text = element_text(size = rel(1.2)))
#amie.diff.plot <- amie.diff.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
#                  amie.diff.plot + scale_y_continuous(breaks=seq(-.2,.3,.1)) + 
#                  geom_hline(yintercept=0, linetype="dashed", color="black") 

                  
                  
