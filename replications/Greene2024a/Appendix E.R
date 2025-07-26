###################################
###################################
## CONJOINT EXPERIMENT ANALYSIS: ##
## TESTS OF HYPOTHESIS 1         ##
###################################
###################################

###################################
## MONETARY CONJOINT EXPERIMENT: ##
## APPENDIX SECTION E, pp. 6-8   ##
###################################

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
sink("[Your file path goes here]/Appendix_E_log", split=TRUE)

#####################################################
## TESTING HYPOTHESIS 1                            ##            
## USING FEELING THERMOMETER MEASURE OF VOTER.TYPE ##
## APPENDIX E, pp. 6-8                             ##
#####################################################

# Load and subset dataframe
setwd("[Your file path goes here]")
mx.panel.2018 <- read.dta(file = "mx_2018_eqd_stata12.dta", convert.underscore = TRUE, convert.factors = FALSE)

mx.panel.2018 <- subset(mx.panel.2018, select = c(D4, Q1, D12, T.Q9.5, T.Q9.6, T.Q9.7, T.Q9.8, Q27A, c11, c21, c12, c22, c13, c23, c14, c24, c15, c25, SbjNum))

mx.panel.2018$educ <- ifelse(mx.panel.2018$D4==1,0, ifelse(mx.panel.2018$D4==2,1, ifelse(mx.panel.2018$D4==3,2,
                      ifelse(mx.panel.2018$D4==4,3, ifelse(mx.panel.2018$D4==5,4, ifelse(mx.panel.2018$D4==6,5,
                      ifelse(mx.panel.2018$D4==7,6, ifelse(mx.panel.2018$D4==8,7, ifelse(mx.panel.2018$D4==9,8,                             
                      ifelse(mx.panel.2018$D4==10,9, ifelse(mx.panel.2018$D4==11,10, ifelse(mx.panel.2018$D4==12,11,
                      ifelse(mx.panel.2018$D4==13,12, NA)))))))))))))

mx.panel.2018$female <- ifelse(mx.panel.2018$Q1==1,0, ifelse(mx.panel.2018$Q1==2,1,NA))

mx.panel.2018$ses <- ifelse(mx.panel.2018$D12==1,0, ifelse(mx.panel.2018$D12==2,1, ifelse(mx.panel.2018$D12==3,2,
                     ifelse(mx.panel.2018$D12==4,3, ifelse(mx.panel.2018$D12==5,4, ifelse(mx.panel.2018$D12==3,2,
                     ifelse(mx.panel.2018$D12==98,NA, ifelse(mx.panel.2018$D12==99,NA,NA))))))))

mx.panel.2018$panft.w1 <- mx.panel.2018$T.Q9.5
mx.panel.2018$panft.w1 <- ifelse(mx.panel.2018$T.Q9.5==99,NA,mx.panel.2018$T.Q9.5)
mx.panel.2018$prift.w1 <- mx.panel.2018$T.Q9.6
mx.panel.2018$prift.w1 <- ifelse(mx.panel.2018$T.Q9.6==99,NA,mx.panel.2018$T.Q9.6)
mx.panel.2018$morenaft.w1 <- mx.panel.2018$T.Q9.7
mx.panel.2018$morenaft.w1 <- ifelse(mx.panel.2018$T.Q9.7==99,NA,mx.panel.2018$T.Q9.7)
mx.panel.2018$prdft.w1 <- mx.panel.2018$T.Q9.8
mx.panel.2018$prdft.w1 <- ifelse(mx.panel.2018$T.Q9.8==99,NA,mx.panel.2018$T.Q9.8)

mx.panel.2018$prift.minusothers.w1 <- mx.panel.2018$prift.w1-(mx.panel.2018$panft.w1-mx.panel.2018$morenaft.w1-mx.panel.2018$prdft.w1)
mx.panel.2018$panft.minusothers.w1 <- mx.panel.2018$panft.w1-(mx.panel.2018$prift.w1-mx.panel.2018$morenaft.w1-mx.panel.2018$prdft.w1)
mx.panel.2018$morenaft.minusothers.w1 <- mx.panel.2018$morenaft.w1-(mx.panel.2018$prift.w1-mx.panel.2018$panft.w1-mx.panel.2018$prdft.w1)
mx.panel.2018$prdft.minusothers.w1 <- mx.panel.2018$prdft.w1-(mx.panel.2018$prift.w1-mx.panel.2018$panft.w1-mx.panel.2018$morenaft.w1)

mx.panel.2018$ce.code <- 0 # create id code in dataframe 1
mx.panel.2018.2 <- mx.panel.2018 # create second dataframe
mx.panel.2018.2$ce.code <- 1 # create id code in datafrane 2
mx.panel.2018.ce <- rbind(mx.panel.2018, mx.panel.2018.2) # bind two dataframes into new one

# Code conjoint vars #
mx.panel.2018.ce$ce.choice <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$Q27A==2,0, 
                              ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$Q27A==1,1,
                              ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$Q27A==2,1, 
                              ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$Q27A==1,0,NA))))

mx.panel.2018.ce$ce.pri <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12!=1,0, 
                           ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12==1,1,
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22==1,1, 
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22!=1,0,NA))))

mx.panel.2018.ce$ce.pan <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12!=2,0, 
                           ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12==2,1,
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22==2,1, 
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22!=2,0,NA))))

mx.panel.2018.ce$ce.prd <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12!=3,0, 
                           ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12==3,1,
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22==3,1, 
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22!=3,0,NA))))

mx.panel.2018.ce$ce.morena <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12!=4,0, 
                              ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12==4,1,
                              ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22==4,1, 
                              ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22!=4,0,NA))))

d <- 5
e <- 3
f <- 3

mx.panel.2018.ce$voter.type.ftorderD <- 
  ifelse((mx.panel.2018$prift.w1>mx.panel.2018$panft.w1+d & mx.panel.2018$prift.w1>mx.panel.2018$prdft.w1+d &
          mx.panel.2018$prift.w1>mx.panel.2018$morenaft.w1+d & mx.panel.2018$prift.w1>e & mx.panel.2018.ce$ce.pri==1) | 
         (mx.panel.2018$panft.w1>mx.panel.2018$prift.w1+d & mx.panel.2018$panft.w1>mx.panel.2018$prdft.w1+d &
          mx.panel.2018$panft.w1>mx.panel.2018$morenaft.w1+d & mx.panel.2018$panft.w1>e & mx.panel.2018.ce$ce.pan==1) | 
         (mx.panel.2018$prdft.w1>mx.panel.2018$panft.w1+d & mx.panel.2018$prdft.w1>mx.panel.2018$prift.w1+d &
          mx.panel.2018$prdft.w1>mx.panel.2018$morenaft.w1+d & mx.panel.2018$prdft.w1>e & mx.panel.2018.ce$ce.prd==1) |
         (mx.panel.2018$morenaft.w1>mx.panel.2018$panft.w1+d & mx.panel.2018$morenaft.w1>mx.panel.2018$prdft.w1+d &
          mx.panel.2018$morenaft.w1>mx.panel.2018$prift.w1+d & mx.panel.2018$morenaft.w1>e & mx.panel.2018.ce$ce.morena==1),0,
  ifelse((mx.panel.2018$prift.w1<mx.panel.2018$panft.w1 & mx.panel.2018$prift.w1<mx.panel.2018$prdft.w1 &
          mx.panel.2018$prift.w1<mx.panel.2018$morenaft.w1 & mx.panel.2018.ce$ce.pri==1) | 
         (mx.panel.2018$panft.w1<mx.panel.2018$prift.w1 & mx.panel.2018$panft.w1<mx.panel.2018$prdft.w1 &
          mx.panel.2018$panft.w1<mx.panel.2018$morenaft.w1 & mx.panel.2018.ce$ce.pan==1) | 
         (mx.panel.2018$prdft.w1<mx.panel.2018$panft.w1 & mx.panel.2018$prdft.w1<mx.panel.2018$prift.w1 &
          mx.panel.2018$prdft.w1<mx.panel.2018$morenaft.w1 &mx.panel.2018.ce$ce.prd==1) |
         (mx.panel.2018$morenaft.w1<mx.panel.2018$panft.w1 & mx.panel.2018$morenaft.w1<mx.panel.2018$prdft.w1 &
          mx.panel.2018$morenaft.w1<mx.panel.2018$prift.w1 & mx.panel.2018.ce$ce.morena==1) | 
         (mx.panel.2018$prift.w1<=f & mx.panel.2018$panft.w1<=f & mx.panel.2018$prdft.w1<=f & mx.panel.2018$morenaft.w1<=f),2,1))

mx.panel.2018.ce$ce.male <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c11==1,0, 
                            ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c11==2,1,
                            ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c21==2,1, 
                            ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c21==1,0,NA))))

mx.panel.2018.ce$ce.experience <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c13==1,0, 
                                  ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c13==2,1,
                                  ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c23==2,1, 
                                  ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c23==1,0,NA))))

mx.panel.2018.ce$ce.corrupt <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c14==2,0, 
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c14==1,1,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c24==1,1, 
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c24==2,0,NA))))

mx.panel.2018.ce$ce.payoff2 <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==1,0, 
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==2,500,
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==3,1000, 
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==1,0,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==2,500,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==3,1000,NA))))))

mx.panel.2018.ce$ce.payoff4 <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==1,0, 
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==2,1,
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==3,1, 
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==1,0,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==2,1,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==3,1,NA))))))

mx.panel.2018.ce$ce.payoff2 <- factor(mx.panel.2018.ce$ce.payoff2,ordered=TRUE,levels=c("0","500","1000"))
mx.panel.2018.ce$ce.payoff4 <- factor(mx.panel.2018.ce$ce.payoff4,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.male <- factor(mx.panel.2018.ce$ce.male,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.experience <- factor(mx.panel.2018.ce$ce.experience,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.corrupt <- factor(mx.panel.2018.ce$ce.corrupt,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.pri <- factor(mx.panel.2018.ce$ce.pri,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.pan <- factor(mx.panel.2018.ce$ce.pan,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.prd <- factor(mx.panel.2018.ce$ce.prd,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.morena <- factor(mx.panel.2018.ce$ce.morena,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.code <- factor(mx.panel.2018.ce$ce.code, ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$voter.type.ftorderD <- factor(mx.panel.2018.ce$voter.type.ftorderD, ordered=TRUE,levels=c("0", "1", "2"))
mx.panel.2018.ce$male <- factor(mx.panel.2018.ce$ce.male, ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$educ <- factor(mx.panel.2018.ce$educ, ordered=TRUE,levels=c("0", "1", "2", "3", "4", "5", "6"))
mx.panel.2018.ce$ses <- factor(mx.panel.2018.ce$ses, ordered=TRUE,levels=c("0", "1", "2", "3", "4"))

# Create dataframe for analysis
mx.panel.2018.ce <- subset(mx.panel.2018.ce, select = c(ce.choice, ce.payoff2, ce.payoff4, voter.type.ftorderD, ce.male, ce.experience, ce.corrupt, ce.code, SbjNum))
mx.panel.2018.ce <- mx.panel.2018.ce[complete.cases(mx.panel.2018.ce),]

# Model with TWO levels of payoff
fit2 <- CausalANOVA(formula=ce.choice ~ ce.payoff4 + voter.type.ftorderD + ce.male + ce.experience + ce.corrupt, 
                    int2.formula = ~ ce.payoff4:voter.type.ftorderD,
                    data=mx.panel.2018.ce, pair.id = mx.panel.2018.ce$ce.code, diff = TRUE, cluster = mx.panel.2018.ce$SbjNum, 
                    nway=2, family = "binomial")

# The AME portion is Table A17 and the AMIE portion is Table A18 in Appendix E, pp. 17-18
# ce.payoff4 = "Benefit"
# voter.type = "Supporter" if 0, "Weakly opposed" if 1, and "Strongly opposed" if 2.
summary(fit2)

my.amies <- fit2$CI.table[[6]][, 1]
my.amie.diffs <- c(my.amies[2]-my.amies[1],my.amies[4]-my.amies[3], my.amies[6]-my.amies[5])
my.sds <- fit2$CI.table[[6]][, 2] 
my.sds.diffs <- c(sqrt(my.sds[1]^2+my.sds[2]^2), sqrt(my.sds[3]^2+my.sds[4]^2), sqrt(my.sds[5]^2+my.sds[6]^2))  
my.ci.lower <- c(my.amie.diffs[1]-1.65*my.sds.diffs[1], my.amie.diffs[2]-1.65*my.sds.diffs[2], my.amie.diffs[3]-1.65*my.sds.diffs[3] )
my.ci.upper <- c(my.amie.diffs[1]+1.65*my.sds.diffs[1], my.amie.diffs[2]+1.65*my.sds.diffs[2], my.amie.diffs[3]+1.65*my.sds.diffs[3] )
my.amie.data.frame <- data.frame(voter.type=c("Supporters", "Weakly opposed", "Strongly opposed"), AMIE.Differences=my.amie.diffs, ci.lower=my.ci.lower, ci.upper=my.ci.upper)
my.amie.data.frame$voter.type <- factor(my.amie.data.frame$voter.type, levels = c("Supporters", "Weakly opposed", "Strongly opposed"))

# Figure A2 Panel A in Appendix E, p. 8 - Average Marginal Interaction Effects (AMIEs)
pdf("[Your file path goes here]/Figure_A2_Panel_A.pdf")

amie.diff.plot <- ggplot() + geom_pointrange(data=my.amie.data.frame, mapping=aes
                  (x=voter.type, y=AMIE.Differences, ymin=ci.lower, ymax=ci.upper)) + 
                  coord_flip() + theme_bw() + 
                  theme(panel.background = element_rect(fill = "white"),
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = rel(1.5)),
                  axis.text = element_text(size = rel(1.5)))
amie.diff.plot <- amie.diff.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
                  amie.diff.plot + scale_y_continuous(breaks=seq(-.2,.3,.1)) + 
                  geom_hline(yintercept=0, linetype="dashed", color="black") 
dev.off()

## Model for monetary conjoint analysis using THREE levels of payoff
fit3 <- CausalANOVA(formula=ce.choice ~ ce.payoff2 + voter.type.ftorderD + ce.male + ce.experience + ce.corrupt, 
                    int2.formula = ~ ce.payoff2:voter.type.ftorderD,
                    data=mx.panel.2018.ce, pair.id = mx.panel.2018.ce$ce.code, diff = TRUE, cluster = mx.panel.2018.ce$SbjNum, 
                    nway=2, family = "binomial")

## The AME portion is Table A16 in Appendix E, p. 7
# ce.payoff2 = "Benefit"
# voter.type = "Supporter" if 0, "Weakly opposed" if 1, and "Strongly opposed" if 2.
summary(fit3)

my.ames <- c(fit3$CI.table[[1]][, 1], fit3$CI.table[[2]][, 1], fit3$CI.table[[3]][, 1], fit3$CI.table[[4]][, 1], fit3$CI.table[[5]][, 1]) ## tables 1 - 5 have AMEs
my.ame.ci.lower <- c(fit3$CI.table[[1]][, 3], fit3$CI.table[[2]][, 3], fit3$CI.table[[3]][, 3], fit3$CI.table[[4]][, 3], fit3$CI.table[[5]][, 3]) 
my.ame.ci.upper <- c(fit3$CI.table[[1]][, 4], fit3$CI.table[[2]][, 4], fit3$CI.table[[3]][, 4], fit3$CI.table[[4]][, 4], fit3$CI.table[[5]][, 4]) 
my.ame.data.frame <- data.frame(voter.type=c("No material offer", "500 pesos", "1,000 pesos", "Supporters", "Weakly opposed", "Strongly opposed", "Female candidate", "Male candidate", "Candidate has no experience", "Candidate has 15 years experience", "Candidate not corrupt", "Candidate corrupt"), AME=my.ames, ci.lower=my.ame.ci.lower, ci.upper=my.ame.ci.upper)
my.ame.data.frame$voter.type <- factor(my.ame.data.frame$voter.type, levels = c("No material offer", "500 pesos", "1,000 pesos", "Supporters", "Weakly opposed", "Strongly opposed", "Female candidate", "Male candidate", "Candidate has no experience", "Candidate has 15 years experience", "Candidate not corrupt", "Candidate corrupt"))

## Figure A2 Panel B - Average Marginal Effects (AMEs)
pdf("[Your file path goes here]/Figure_A2_Panel_B.pdf")

ame.plot <- ggplot() + geom_pointrange(data=my.ame.data.frame, mapping=aes
            (x=voter.type, y=AME, ymin=ci.lower, ymax=ci.upper)) + 
            coord_flip() + theme_bw() + 
            theme(panel.background = element_rect(fill = "white"),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)))
ame.plot <- ame.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
            ame.plot + scale_y_continuous(breaks=seq(-.2,.3,.1)) + geom_hline(yintercept=0, linetype="dashed", color="black") 
dev.off()

            
#############################################
## TESTING HYPOTHESIS 1                    ##            
## USING VOTE CHOICE MEASURE OF VOTER.TYPE ##
## APPENDIX SECTION E, pp. 6-8             ##
#############################################
            
# Load and subset data
setwd("[Your file path goes here]")

mx.panel.2018 <- read.dta(file = "mx_2018_eqd_stata12.dta", convert.underscore = TRUE, convert.factors = FALSE)

mx.panel.2018 <- subset(mx.panel.2018, select = c(Z9, Z10, Q10A, Q10B, Q23B, Q18, edad, Q8, D4, Q1, D12, Q3, 
                                                  T.Q9.5, T.Q9.6, T.Q9.7, T.Q9.8, Q21, Q27A, T.Q27B.1, T.Q27B.2, 
                                                  T.Q27C.1, T.Q27C.2, c11, c21, c12, c22, c13, c23, c14, c24, c15, 
                                                  c25, SbjNum, munpres.pan.in.2017, 
                                                  munpres.pri.in.2017, munpres.prd.in.2017, munpres.morena.in.2017,
                                                  munpres.pan.prd.in.2017, Q20))

mx.panel.2018$spoke.spanish.w1 <- ifelse(mx.panel.2018$Z9==2 & mx.panel.2018$Z10==1,0,1)
mx.panel.2018 <- mx.panel.2018[mx.panel.2018$spoke.spanish.w1==1,]

# Code variables to create voter.type using municipal vote choice #
mx.panel.2018$munvote.w1 <- ifelse(mx.panel.2018$Q8==2,"PRI", ifelse(mx.panel.2018$Q8==12,"PRI", 
                            ifelse(mx.panel.2018$Q8==1,"PAN",
                            ifelse(mx.panel.2018$Q8==11,"PAN", ifelse(mx.panel.2018$Q8==3,"PRD", 
                            ifelse(mx.panel.2018$Q8==9,"Other", 
                            ifelse(mx.panel.2018$Q8==5,"Other", 
                            ifelse(mx.panel.2018$Q8==6,"Other",  
                            ifelse(mx.panel.2018$Q8==7,"Other", ifelse(mx.panel.2018$Q8==8,"Other", 
                            ifelse(mx.panel.2018$Q8==4,"MORENA", 
                            ifelse(mx.panel.2018$Q8==10,"MORENA", ifelse(mx.panel.2018$Q8==13,"None", 
                            ifelse(mx.panel.2018$Q8==14,"None", 
                            ifelse(mx.panel.2018$Q.8.S=="SANSORES", "MORENA",
                            ifelse(mx.panel.2018$Q.8.S=="paty chio", "MORENA",
                            ifelse(mx.panel.2018$Q.8.S=="por la compa\xf1\xeda del peje", "MORENA",
                            ifelse(mx.panel.2018$Q.8.S=="Alejandro Barrales", "PRD",
                            ifelse(mx.panel.2018$Q.8.S=="ba\xf1ales", "PRD",
                            ifelse(mx.panel.2018$Q.8.S=="Chucho Nader", "PAN",
                            ifelse(mx.panel.2018$Q.8.S=="la coalicion del pan", "PAN",
                            ifelse(mx.panel.2018$Q.8.S=="roberto de los santos", "PAN",
                            ifelse(mx.panel.2018$Q.8.S=="LENIN", "PAN",
                            ifelse(mx.panel.2018$Q.8.S=="ENRIQUE VARGAS", "PRI", 
                            ifelse(mx.panel.2018$Q.8.S=="la chata", "PRI", 
                            ifelse(mx.panel.2018$Q.8.S=="maria elena limon", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="maria elena limin", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="alianza", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="Genaro", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="ALFONSO", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="ALIANZA", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="NUEVA ALIANZA", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="nueva alianza", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="PARTIDO SOCIAL DEMOCTARA PSD", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="UNIDAD POPULAR", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="udc", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="partido verde", "Other",  
                            ifelse(mx.panel.2018$Q.8.S=="independiente", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="INDEPENDIENTE", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="CANDIDATO INDEPENDIENTE", "Other",    
                            ifelse(mx.panel.2018$Q.8.S=="CANDIDATO INDEPENDIENTE", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="FIERRO", "Other",
                            ifelse(mx.panel.2018$Q.8.S=="NINGUNO", "None",
                            NA)))))))))))))))))))))))))))))))))))))))))))

mx.panel.2018$educ <- ifelse(mx.panel.2018$D4==1,0, ifelse(mx.panel.2018$D4==2,1, ifelse(mx.panel.2018$D4==3,2,
                            ifelse(mx.panel.2018$D4==4,3, ifelse(mx.panel.2018$D4==5,4, ifelse(mx.panel.2018$D4==6,5,
                            ifelse(mx.panel.2018$D4==7,6, ifelse(mx.panel.2018$D4==8,7, ifelse(mx.panel.2018$D4==9,8,                             
                            ifelse(mx.panel.2018$D4==10,9, ifelse(mx.panel.2018$D4==11,10, ifelse(mx.panel.2018$D4==12,11,
                            ifelse(mx.panel.2018$D4==13,12, NA)))))))))))))
                             
mx.panel.2018$female <- ifelse(mx.panel.2018$Q1==1,0, ifelse(mx.panel.2018$Q1==2,1,NA))

mx.panel.2018$ses <- ifelse(mx.panel.2018$D12==1,0, ifelse(mx.panel.2018$D12==2,1, ifelse(mx.panel.2018$D12==3,2,
                     ifelse(mx.panel.2018$D12==4,3, ifelse(mx.panel.2018$D12==5,4, ifelse(mx.panel.2018$D12==3,2,
                     ifelse(mx.panel.2018$D12==98,NA, ifelse(mx.panel.2018$D12==99,NA,NA))))))))

mx.panel.2018$polinterest.w1 <- ifelse(mx.panel.2018$Q3==4,0, ifelse(mx.panel.2018$Q3==3,1, ifelse(mx.panel.2018$Q3==2,2,
                                ifelse(mx.panel.2018$Q3==1,3, ifelse(mx.panel.2018$Q3==9,NA,NA)))))

mx.panel.2018$natcorrupt.w1 <- ifelse(mx.panel.2018$Q23B==99,NA,mx.panel.2018$Q23B)

mx.panel.2018$panft.w1 <- mx.panel.2018$T.Q9.5
mx.panel.2018$panft.w1 <- ifelse(mx.panel.2018$T.Q9.5==99,NA,mx.panel.2018$T.Q9.5)
mx.panel.2018$prift.w1 <- mx.panel.2018$T.Q9.6
mx.panel.2018$prift.w1 <- ifelse(mx.panel.2018$T.Q9.6==99,NA,mx.panel.2018$T.Q9.6)
mx.panel.2018$morenaft.w1 <- mx.panel.2018$T.Q9.7
mx.panel.2018$morenaft.w1 <- ifelse(mx.panel.2018$T.Q9.7==99,NA,mx.panel.2018$T.Q9.7)
mx.panel.2018$prdft.w1 <- mx.panel.2018$T.Q9.8
mx.panel.2018$prdft.w1 <- ifelse(mx.panel.2018$T.Q9.8==99,NA,mx.panel.2018$T.Q9.8)

mx.panel.2018$prift.minusavg.w1 <- mx.panel.2018$prift.w1-(mx.panel.2018$panft.w1+mx.panel.2018$morenaft.w1+mx.panel.2018$prdft.w1)/3
mx.panel.2018$panft.minusavg.w1 <- mx.panel.2018$panft.w1-(mx.panel.2018$prift.w1+mx.panel.2018$morenaft.w1+mx.panel.2018$prdft.w1)/3
mx.panel.2018$morenaft.minusavg.w1 <- mx.panel.2018$morenaft.w1-(mx.panel.2018$prift.w1+mx.panel.2018$panft.w1+mx.panel.2018$prdft.w1)/3
mx.panel.2018$prdft.minusavg.w1 <- mx.panel.2018$prdft.w1-(mx.panel.2018$prift.w1+mx.panel.2018$panft.w1+mx.panel.2018$morenaft.w1)/3


mun.model <- multinom(munvote.w1 ~  natcorrupt.w1 + munpres.pri.in.2017 + munpres.pan.in.2017 + munpres.prd.in.2017 +
                                    munpres.morena.in.2017 + educ + female + ses + prdft.minusavg.w1 + prift.minusavg.w1 + 
                                    panft.minusavg.w1 + morenaft.minusavg.w1 + polinterest.w1, data = mx.panel.2018)

summary(mun.model)
(1 - pnorm(abs(summary(mun.model)$coefficients/summary(mun.model)$standard.errors), 0, 1)) * 2 #2-tailed sig#

morena.prob <- mun.model$fitted.values[,1]
mean(morena.prob)
morena.prob[is.na(mx.panel.2018$munvote.w1)] <- NA
mx.panel.2018$morena.prob <- morena.prob
mx.panel.2018$morena.type[is.na(mx.panel.2018$munvote.w1)] <- NA

none.prob <- mun.model$fitted.values[,2]
mean(none.prob)
none.prob[is.na(mx.panel.2018$munvote.w1)] <- NA
mx.panel.2018$none.prob <- none.prob
mx.panel.2018$none.prob[is.na(mx.panel.2018$munvote.w1)] <- NA

other.prob <- mun.model$fitted.values[,3]
mean(other.prob)
other.prob[is.na(mx.panel.2018$munvote.w1)] <- NA
mx.panel.2018$other.prob <- other.prob
mx.panel.2018$other.prob[is.na(mx.panel.2018$munvote.w1)] <- NA

pan.prob <- mun.model$fitted.values[,4]
mean(pan.prob)
pan.prob[is.na(mx.panel.2018$munvote.w1)] <- NA
mx.panel.2018$pan.prob <- pan.prob
mx.panel.2018$pan.type[is.na(mx.panel.2018$munvote.w1)] <- NA

prd.prob <- mun.model$fitted.values[,5]
mean(prd.prob)
prd.prob[is.na(mx.panel.2018$munvote.w1)] <- NA
mx.panel.2018$prd.prob <- prd.prob
mx.panel.2018$prd.type[is.na(mx.panel.2018$munvote.w1)] <- NA

pri.prob <- mun.model$fitted.values[,6]
mean(pri.prob)
pri.prob[is.na(mx.panel.2018$munvote.w1)] <- NA
mx.panel.2018$pri.prob <- pri.prob
mx.panel.2018$pri.type[is.na(mx.panel.2018$munvote.w1)] <- NA

# Expand data #
mx.panel.2018$ce.code <- 0 # create id code in dataframe 1
mx.panel.2018.2 <- mx.panel.2018 # create second dataframe
mx.panel.2018.2$ce.code <- 1 # create id code in datafrane 2
mx.panel.2018.ce <- rbind(mx.panel.2018, mx.panel.2018.2) # bind two dataframes into new one

# Code conjoint vars #
mx.panel.2018.ce$ce.choice <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$Q27A==2,0, 
                              ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$Q27A==1,1,
                              ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$Q27A==2,1, 
                              ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$Q27A==1,0,NA))))

mx.panel.2018.ce$ce.pri <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12!=1,0, 
                           ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12==1,1,
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22==1,1, 
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22!=1,0,NA))))

mx.panel.2018.ce$ce.pan <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12!=2,0, 
                           ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12==2,1,
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22==2,1, 
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22!=2,0,NA))))

mx.panel.2018.ce$ce.prd <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12!=3,0, 
                           ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12==3,1,
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22==3,1, 
                           ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22!=3,0,NA))))

mx.panel.2018.ce$ce.morena <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12!=4,0, 
                              ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c12==4,1,
                              ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22==4,1, 
                              ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c22!=4,0,NA))))

p1 <- .5
p2 <- .25

mx.panel.2018.ce$voter.type <- ifelse((mx.panel.2018$pri.prob>p1 & mx.panel.2018.ce$ce.pri==1) |
                                      (mx.panel.2018$pan.prob>p1 & mx.panel.2018.ce$ce.pan==1) |
                                      (mx.panel.2018$prd.prob>p1 & mx.panel.2018.ce$ce.prd==1) |
                                      (mx.panel.2018$morena.prob>p1 & mx.panel.2018.ce$ce.morena==1),0,
                               ifelse((mx.panel.2018$pri.prob<=p1 & mx.panel.2018$pri.prob>=p2 & mx.panel.2018.ce$ce.pri==1) |
                                      (mx.panel.2018$pan.prob<=p1 & mx.panel.2018$pan.prob>=p2 & mx.panel.2018.ce$ce.pan==1) |
                                      (mx.panel.2018$prd.prob<=p1 & mx.panel.2018$prd.prob>=p2 & mx.panel.2018.ce$ce.prd==1) |
                                      (mx.panel.2018$morena.prob<=p1 & mx.panel.2018$morena.prob>=p2 & mx.panel.2018.ce$ce.morena==1) |
                                      (mx.panel.2018$none.prob>=p2) | (mx.panel.2018$other.prob>=p2),1,
                               ifelse((mx.panel.2018$pri.prob<p2 & mx.panel.2018.ce$ce.pri==1) |
                                      (mx.panel.2018$pan.prob<p2 & mx.panel.2018.ce$ce.pan==1) |
                                      (mx.panel.2018$prd.prob<p2 & mx.panel.2018.ce$ce.prd==1) |
                                      (mx.panel.2018$morena.prob<p2 & mx.panel.2018.ce$ce.morena==1) 
                                      ,2,NA)))

mx.panel.2018.ce$ce.male <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c11==1,0, 
                            ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c11==2,1,
                            ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c21==2,1, 
                            ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c21==1,0,NA))))

mx.panel.2018.ce$ce.experience <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c13==1,0, 
                                  ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c13==2,1,
                                  ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c23==2,1, 
                                  ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c23==1,0,NA))))

mx.panel.2018.ce$ce.corrupt <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c14==2,0, 
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c14==1,1,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c24==1,1, 
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c24==2,0,NA))))

mx.panel.2018.ce$ce.payoff2 <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==1,0, 
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==2,500,
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==3,1000, 
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==1,0,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==2,500,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==3,1000,NA))))))

mx.panel.2018.ce$ce.payoff4 <- ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==1,0, 
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==2,1,
                               ifelse(mx.panel.2018.ce$ce.code==0 & mx.panel.2018.ce$c15==3,1, 
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==1,0,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==2,1,
                               ifelse(mx.panel.2018.ce$ce.code==1 & mx.panel.2018.ce$c25==3,1,NA))))))

mx.panel.2018.ce$ce.payoff2 <- factor(mx.panel.2018.ce$ce.payoff2,ordered=TRUE,levels=c("0","500","1000"))
mx.panel.2018.ce$ce.payoff4 <- factor(mx.panel.2018.ce$ce.payoff4,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.male <- factor(mx.panel.2018.ce$ce.male,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.experience <- factor(mx.panel.2018.ce$ce.experience,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.corrupt <- factor(mx.panel.2018.ce$ce.corrupt,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.pri <- factor(mx.panel.2018.ce$ce.pri,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.pan <- factor(mx.panel.2018.ce$ce.pan,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.prd <- factor(mx.panel.2018.ce$ce.prd,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.morena <- factor(mx.panel.2018.ce$ce.morena,ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$ce.code <- factor(mx.panel.2018.ce$ce.code, ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$voter.type <- factor(mx.panel.2018.ce$voter.type, ordered=TRUE,levels=c("0", "1", "2"))
mx.panel.2018.ce$female <- factor(mx.panel.2018.ce$female, ordered=FALSE,levels=c("0","1"))
mx.panel.2018.ce$educ <- factor(mx.panel.2018.ce$educ, ordered=TRUE,levels=c("0", "1", "2", "3", "4", "5", "6"))
mx.panel.2018.ce$ses <- factor(mx.panel.2018.ce$ses, ordered=TRUE,levels=c("0", "1", "2", "3", "4"))

# Create dataframe for analysis #
mx.panel.2018.ce <- subset(mx.panel.2018.ce, select = c(ce.choice, ce.payoff2, ce.payoff4, voter.type, ce.male, ce.experience, ce.corrupt, voter.type, ce.code, SbjNum))
mx.panel.2018.ce <- mx.panel.2018.ce[complete.cases(mx.panel.2018.ce),]

# Model with THREE levels of payoff
fit3 <- CausalANOVA(formula=ce.choice ~ ce.payoff2 + voter.type + ce.male + ce.experience + ce.corrupt, 
                    int2.formula = ~ ce.payoff2:voter.type,
                    data=mx.panel.2018.ce, pair.id = mx.panel.2018.ce$ce.code, diff = TRUE, cluster = mx.panel.2018.ce$SbjNum, 
                    nway=2, family = "binomial")

# The AME portion is Table A13 in Appendix E, p. 6
# ce.payoff2 = "Benefit"
# voter.type = "Supporter" if 0, "Weakly opposed" if 1, and "Strongly opposed" if 2.
summary(fit3)

my.ames <- c(fit3$CI.table[[1]][, 1], fit3$CI.table[[2]][, 1], fit3$CI.table[[3]][, 1], fit3$CI.table[[4]][, 1], fit3$CI.table[[5]][, 1]) ## tables 1 - 5 have AMEs
my.ame.ci.lower <- c(fit3$CI.table[[1]][, 3], fit3$CI.table[[2]][, 3], fit3$CI.table[[3]][, 3], fit3$CI.table[[4]][, 3], fit3$CI.table[[5]][, 3]) 
my.ame.ci.upper <- c(fit3$CI.table[[1]][, 4], fit3$CI.table[[2]][, 4], fit3$CI.table[[3]][, 4], fit3$CI.table[[4]][, 4], fit3$CI.table[[5]][, 4]) 
my.ame.data.frame <- data.frame(voter.type=c("No material offer", "500 pesos", "1,000 pesos", "Supporters", "Weakly opposed", "Strongly opposed", "Female candidate", "Male candidate", "Candidate has no experience", "Candidate has 15 years experience", "Candidate not corrupt", "Candidate corrupt"), AME=my.ames, ci.lower=my.ame.ci.lower, ci.upper=my.ame.ci.upper)
my.ame.data.frame$voter.type <- factor(my.ame.data.frame$voter.type, levels = c("No material offer", "500 pesos", "1,000 pesos", "Supporters", "Weakly opposed", "Strongly opposed", "Female candidate", "Male candidate", "Candidate has no experience", "Candidate has 15 years experience", "Candidate not corrupt", "Candidate corrupt"))

# Figure A1 Panel B in Appendix E, p. 7 - Average Marginal Effects (AMEs)
pdf("[Your file path goes here]/Figure_A1_Panel_B.pdf")

ame.plot <- ggplot() + geom_pointrange(data=my.ame.data.frame, mapping=aes
            (x=voter.type, y=AME, ymin=ci.lower, ymax=ci.upper)) + 
            coord_flip() + theme_bw() + 
            theme(panel.background = element_rect(fill = "white"),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)))
ame.plot <- ame.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
            ame.plot + scale_y_continuous(breaks=seq(-.2,.3,.1)) + 
            geom_hline(yintercept=0, linetype="dashed", color="black") 
dev.off()

# Model with TWO levels of payoff
fit2 <- CausalANOVA(formula=ce.choice ~ ce.payoff4 + voter.type + ce.male + ce.experience + ce.corrupt , 
                    int2.formula = ~ ce.payoff4:voter.type,
                    data=mx.panel.2018.ce, pair.id = mx.panel.2018.ce$ce.code, diff = TRUE, cluster = mx.panel.2018.ce$SbjNum, 
                    nway=2, family = "binomial")

# The AME portion is Table A14 and the AMIE portion is Table A15 in Appendix E, pp. 6-7 - Average Marginal Interaction Effects (AMIEs)
# ce.payoff4 = "Benefit"
# voter.type = "Supporter" if 0, "Weakly opposed" if 1, and "Strongly opposed" if 2.
summary(fit2)

my.amies <- fit2$CI.table[[6]][, 1]
my.amie.diffs <- c(my.amies[2]-my.amies[1],my.amies[4]-my.amies[3], my.amies[6]-my.amies[5])
my.sds <- fit2$CI.table[[6]][, 2] 
my.sds.diffs <- c(sqrt(my.sds[1]^2+my.sds[2]^2), sqrt(my.sds[3]^2+my.sds[4]^2), sqrt(my.sds[5]^2+my.sds[6]^2))  
my.ci.lower <- c(my.amie.diffs[1]-1.96*my.sds.diffs[1], my.amie.diffs[2]-1.96*my.sds.diffs[2], my.amie.diffs[3]-1.96*my.sds.diffs[3] )
my.ci.upper <- c(my.amie.diffs[1]+1.96*my.sds.diffs[1], my.amie.diffs[2]+1.96*my.sds.diffs[2], my.amie.diffs[3]+1.96*my.sds.diffs[3] )
my.amie.data.frame <- data.frame(voter.type=c("Supporters", "Weakly opposed", "Strongly opposed"), AMIE.Differences=my.amie.diffs, ci.lower=my.ci.lower, ci.upper=my.ci.upper)
my.amie.data.frame$voter.type <- factor(my.amie.data.frame$voter.type, levels = c("Supporters", "Weakly opposed", "Strongly opposed"))

# Figure A1 Panel A in Appendix E, p. 7
pdf("[Your file path goes here]/Figure_A1_Panel_A.pdf")

amie.diff.plot <- ggplot() + geom_pointrange(data=my.amie.data.frame, mapping=aes
                  (x=voter.type, y=AMIE.Differences, ymin=ci.lower, ymax=ci.upper)) + 
                  coord_flip() + theme_bw() + 
                  theme(panel.background = element_rect(fill = "white"),
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