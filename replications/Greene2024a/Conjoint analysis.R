###################################
###################################
## CONJOINT EXPERIMENT ANALYSIS: ##
## TESTS OF HYPOTHESIS 1         ##
###################################
###################################

###################################
## MONETARY CONJOINT EXPERIMENT: ##
## FIGURE 4 AND APPENDIX D       ##
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
sink("[Your file path goes here]/Figure_4_log", split=TRUE)

# Load and subset dataframe
setwd("[Your file path goes here]")
mx.panel.2018 <- read.dta(file = "mx_2018_eqd_stata12.dta", convert.underscore = TRUE, convert.factors = FALSE)
mx.panel.2018 <- subset(mx.panel.2018, select = c(Q10A, Q23B, Q18, Q27A, c11, c21, c12, c22, c13, c23, c14, c24, c15, c25, SbjNum))

# Expand data for conjoint analysis #
mx.panel.2018$ce.code <- 0 # create id code in dataframe 1
mx.panel.2018.2 <- mx.panel.2018 # copy dataframe
mx.panel.2018.2$ce.code <- 1 # create id code in datafrane 2
mx.panel.2018.ce <- rbind(mx.panel.2018, mx.panel.2018.2) # bind two dataframes

# Code variables for conjoint analysis #
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

mx.panel.2018.ce$voter.type <- 
           ifelse((mx.panel.2018$Q10A==1 & mx.panel.2018.ce$ce.pan==1) |
                  (mx.panel.2018$Q10A==3 & mx.panel.2018.ce$ce.pri==1) |
                  (mx.panel.2018$Q10A==5 & mx.panel.2018.ce$ce.prd==1) |
                  (mx.panel.2018$Q10A==7 & mx.panel.2018.ce$ce.morena==1),0,
           ifelse((mx.panel.2018$Q10A==1 & (mx.panel.2018.ce$ce.pri==1 | mx.panel.2018.ce$ce.prd==1 | mx.panel.2018.ce$ce.morena==1)) |
                  (mx.panel.2018$Q10A==3 & (mx.panel.2018.ce$ce.pan==1 | mx.panel.2018.ce$ce.prd==1 | mx.panel.2018.ce$ce.morena==1)) |
                  (mx.panel.2018$Q10A==5 & (mx.panel.2018.ce$ce.pan==1 | mx.panel.2018.ce$ce.pri==1 | mx.panel.2018.ce$ce.morena==1)) |
                  (mx.panel.2018$Q10A==7 & (mx.panel.2018.ce$ce.pan==1 | mx.panel.2018.ce$ce.pri==1 | mx.panel.2018.ce$ce.prd==1)),2,
           ifelse((mx.panel.2018$Q10A==2 & mx.panel.2018.ce$ce.pan==1) |
                  (mx.panel.2018$Q10A==4 & mx.panel.2018.ce$ce.pri==1) |
                  (mx.panel.2018$Q10A==6 & mx.panel.2018.ce$ce.prd==1) |
                  (mx.panel.2018$Q10A==8 & mx.panel.2018.ce$ce.morena==1),1,
           ifelse((mx.panel.2018$Q10A==2 & (mx.panel.2018.ce$ce.pri==1 | mx.panel.2018.ce$ce.prd==1 | mx.panel.2018.ce$ce.morena==1)) |
                  (mx.panel.2018$Q10A==4 & (mx.panel.2018.ce$ce.pan==1 | mx.panel.2018.ce$ce.prd==1 | mx.panel.2018.ce$ce.morena==1)) |
                  (mx.panel.2018$Q10A==6 & (mx.panel.2018.ce$ce.pan==1 | mx.panel.2018.ce$ce.pri==1 | mx.panel.2018.ce$ce.morena==1)) |
                  (mx.panel.2018$Q10A==8 & (mx.panel.2018.ce$ce.pan==1 | mx.panel.2018.ce$ce.pri==1 | mx.panel.2018.ce$ce.prd==1)),2,
           ifelse(mx.panel.2018$Q10A==9,2,
           ifelse(mx.panel.2018$Q10A==10,1,
           ifelse(mx.panel.2018$Q10A==99,NA,
           NA)))))))

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

## Create dataframe for conjoint analysis ##
mx.panel.2018.ce <- subset(mx.panel.2018.ce, select = c(ce.choice, ce.payoff2, ce.payoff4, voter.type, ce.male, ce.experience, ce.corrupt, voter.type, ce.code, SbjNum))
mx.panel.2018.ce <- mx.panel.2018.ce[complete.cases(mx.panel.2018.ce),]

## Model for monetary conjoint analysis using THREE levels of payoff
fit3.table.a10 <- CausalANOVA(formula=ce.choice ~ ce.payoff2 + voter.type + ce.male + ce.experience + ce.corrupt, 
                    int2.formula = ~ ce.payoff2:voter.type,
                    data=mx.panel.2018.ce, pair.id = mx.panel.2018.ce$ce.code, diff = TRUE, cluster = mx.panel.2018.ce$SbjNum, 
                    nway=2, family = "binomial")

## The AME portion is Table A10 in Appendix D, p. 6 - Monetary conjoint using THREE levels of payoff
# ce.payoff2 = "Benefit"
# voter.type = "Supporter" if 0, "Weakly opposed" if 1, and "Strongly opposed" if 2.
summary(fit3.table.a10)

my.ames <- c(fit3.table.a10$CI.table[[1]][, 1], fit3.table.a10$CI.table[[2]][, 1], fit3.table.a10$CI.table[[3]][, 1], fit3.table.a10$CI.table[[4]][, 1], fit3.table.a10$CI.table[[5]][, 1]) ## tables 1 - 5 have AMEs
my.ame.ci.lower <- c(fit3.table.a10$CI.table[[1]][, 3], fit3.table.a10$CI.table[[2]][, 3], fit3.table.a10$CI.table[[3]][, 3], fit3.table.a10$CI.table[[4]][, 3], fit3.table.a10$CI.table[[5]][, 3]) 
my.ame.ci.upper <- c(fit3.table.a10$CI.table[[1]][, 4], fit3.table.a10$CI.table[[2]][, 4], fit3.table.a10$CI.table[[3]][, 4], fit3.table.a10$CI.table[[4]][, 4], fit3.table.a10$CI.table[[5]][, 4]) 
my.ame.data.frame <- data.frame(voter.type=c("No material offer", "500 pesos", "1,000 pesos", "Supporters", "Weakly opposed", "Strongly opposed", "Female candidate", "Male candidate", "Candidate has no experience", "Candidate has 15 years experience", "Candidate not corrupt", "Candidate corrupt"), AME=my.ames, ci.lower=my.ame.ci.lower, ci.upper=my.ame.ci.upper)
my.ame.data.frame$voter.type <- factor(my.ame.data.frame$voter.type, levels = c("No material offer", "500 pesos", "1,000 pesos", "Supporters", "Weakly opposed", "Strongly opposed", "Female candidate", "Male candidate", "Candidate has no experience", "Candidate has 15 years experience", "Candidate not corrupt", "Candidate corrupt"))

## Figure 4 Panel B in the main text - Average Marginal Effects (AMEs)
pdf("[Your file path goes here]/Figure_4_Panel_B.pdf")

ame.plot <- ggplot() + geom_pointrange(data=my.ame.data.frame, mapping=aes
            (x=voter.type, y=AME, ymin=ci.lower, ymax=ci.upper)) + 
            coord_flip() + theme_bw() + 
            theme(panel.background = element_rect(fill = "white"),
            axis.title.y = element_blank(),
            axis.title.x =  element_blank(),
            axis.text = element_text(size = rel(1.4)))
ame.plot <- ame.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
            ame.plot + scale_y_continuous(breaks=seq(-.2,.3,.1)) + geom_hline(yintercept=0, linetype="dashed", color="black") 
dev.off()

## ## Model for monetary conjoint analysis using TWO levels of payoff
fit2.table.a11 <- CausalANOVA(formula=ce.choice ~ ce.payoff4 + voter.type + ce.male + ce.experience + ce.corrupt , 
                    int2.formula = ~ ce.payoff4:voter.type,
                    data=mx.panel.2018.ce, pair.id = mx.panel.2018.ce$ce.code, diff = TRUE, cluster = mx.panel.2018.ce$SbjNum, 
                    nway=2, family = "binomial")

## The AME portion is Table A11 and the AMIE portion is Table A12 in Appendix D, p. 6 - Monetary conjoint using TWO levels of payoff
# ce.payoff4 = "Benefit"
# voter.type = "Supporter" if 0, "Weakly opposed" if 1, and "Strongly opposed" if 2.
summary(fit2.table.a11)

my.amies <- fit2.table.a11$CI.table[[6]][, 1]
my.amie.diffs <- c(my.amies[2]-my.amies[1],my.amies[4]-my.amies[3], my.amies[6]-my.amies[5])
my.sds <- fit2.table.a11$CI.table[[6]][, 2] 
my.sds.diffs <- c(sqrt(my.sds[1]^2+my.sds[2]^2), sqrt(my.sds[3]^2+my.sds[4]^2), sqrt(my.sds[5]^2+my.sds[6]^2))  
my.ci.lower <- c(my.amie.diffs[1]-1.96*my.sds.diffs[1], my.amie.diffs[2]-1.96*my.sds.diffs[2], my.amie.diffs[3]-1.96*my.sds.diffs[3] )
my.ci.upper <- c(my.amie.diffs[1]+1.96*my.sds.diffs[1], my.amie.diffs[2]+1.96*my.sds.diffs[2], my.amie.diffs[3]+1.96*my.sds.diffs[3] )
my.amie.data.frame <- data.frame(voter.type=c("Supporters", "Weakly opposed", "Strongly opposed"), AMIE.Differences=my.amie.diffs, ci.lower=my.ci.lower, ci.upper=my.ci.upper)
my.amie.data.frame$voter.type <- factor(my.amie.data.frame$voter.type, levels = c("Supporters", "Weakly opposed", "Strongly opposed"))

## Differences associated with Figure 4 cited on p. 21 of the main text ##
my.amie.data.frame

## Figure 4 Panel A in the main text - Average Marginal Interaction Effects (AMIEs)
pdf("[Your file path goes here]/Figure_4_Panel_A.pdf")

amie.diff.plot <- ggplot() + geom_pointrange(data=my.amie.data.frame, mapping=aes
                  (x=voter.type, y=AMIE.Differences, ymin=ci.lower, ymax=ci.upper)) + 
                  coord_flip() + theme_bw() + theme(panel.background = element_rect(fill = "white"),
                  axis.title.y = element_blank(),
                  axis.title.x =  element_blank(),
                  axis.text = element_text(size = rel(1.4)))
amie.diff.plot <- amie.diff.plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))
                  amie.diff.plot + scale_y_continuous(breaks=seq(-.2,.3,.1)) + 
                  geom_hline(yintercept=0, linetype="dashed", color="black") 

dev.off()

# Turn off log
sink()

