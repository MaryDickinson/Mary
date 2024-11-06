#Script Name: FactorialANOVAandBayesPractice.R
#Created on: 10/10/2022
#Author: Mary Dickinson
#Purpose: Factorial repeated-measures two-way ANOVA and Bayesian Factorial Two-Way ANOVA
#Version: 1.0
#Notes: 

#Research question... Do people rate hostility to kill a bug differently depending on frightfulness and disgustingness? Does hostility rating depend on interaction between these two factors?

#Analysis plan = Factorial repeated-measures two-way ANOVA followed by bayesian factorial two-way ANOVA
#DV = Numeric variable of hostility rating (hostility, 0-10)
#IV1 = Categorical variable of frightfulness level (frig, with 2 levels: high and low)
#IV2 = Categorical variable of disgustingness level (disg, with 2 levels: high and low)

#Hypotheses...
#Set 1 (main effect of frightfulness) H0 = mean hostility rating is equal for all frightfulness levels, H1 = at least one is different
#Set 2 (main effect of disgustingness) H0 = mean hostility rating is equal for all disgustingness levels, H1 = at least one is different
#Set 3 (interaction) H0 = mean hostility rating is equal for all frightfulness and disgustingness levels, H1 = at least one different 

#No planned contrasts. 

#Installation of required packages...
packages = c("ggplot2", "dplyr", "afex", "ez", "rstatix", "psych", "weights", "BayesFactor", "readr")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#Uploading dataset onto R...
databugs = read_csv("bugs.csv")
View(databugs)

#Inspecting data set...
head(databugs, 20) #long format data 
glimpse(databugs) 

#Changing character variables to factors...
databugs$frig = as.factor(databugs$frig)
databugs$disg = as.factor(databugs$disg)
databugs$gender = as.factor(databugs$gender)
glimpse(databugs)

#Check for missing values...
is.na(databugs) #no missing values

#Visualising data using boxplot...
ggplot(databugs, aes(x = frig, y = hostility, fill = disg)) +
  geom_boxplot() +
  ggtitle("The effect of frightfulness and disgustingness on hostility") +
  xlab('Frightfulness') +
  ylab('Hostility rating (0-10)')+
  labs(fill = "Disgustingness")

#OR Visualising data using barchart...
ggplot(databugs, aes(y = hostility,x = frig, fill = disg)) + 
  stat_summary(fun=mean, geom="bar", position="dodge") +
  stat_summary(fun.data=mean_se, geom="errorbar",position=position_dodge(width=0.9),width= 0.2) +
  ggtitle("The effect of frightfulness and disgustingness on hostility") +
  xlab('Frightfulness') +
  ylab('Hostility rating (0-10)')+
  labs(fill="Disgustingness")

#Interaction plot for hypotheses...
interaction.plot(trace.factor = databugs$frig,
                 x.factor = databugs$disg,
                 response = databugs$hostility,
                 fun = mean,
                 xlab = "Frightfulness",
                 ylab = "Hostility rating 0-10",
                 trace.label = "Disgustingness",
                 type="b",
                 col=c("blue","red"),   
                 pch=c(19, 17, 15),             
                 fixed=TRUE,                    
                 leg.bty = "o",
                 cex.axis = 1.4,
                 cex.lab = 1.6, lwd = 6)
#Plot indicates interaction is unlikely to be present between frightfulness and disgustingness for hostility rating.

#Descriptive statistics...

#Grand mean
mean(databugs$hostility)

#Marginal means and SD
aggregate(hostility ~ frig, databugs, mean) #Marginal means for frig
aggregate(hostility ~ frig, databugs, sd) #Marginal SDs for frig

aggregate(hostility ~ disg, databugs, mean) #Marginal means for disg
aggregate(hostility ~ disg, databugs, sd) #Marginal SDs for disg

#Cell means and SD
aggregate(hostility ~ frig + disg, databugs, mean)
aggregate(hostility ~ frig + disg, databugs, sd)

#Factorial Two-Way repeated-measures ANOVA
MyAFEX  = aov_ez(data = databugs, id = "sub", dv = "hostility", within = c("frig", "disg"), detailed = TRUE, return_aov = TRUE)

#Testing ANOVA assumptions...

#Normality of residuals...
hist(x = MyAFEX$lm$residuals) 
qqnorm(y = MyAFEX$lm$residuals) 
shapiro.test(x = MyAFEX$lm$residuals) #p < .05
#Residuals are not normally distributed but group size are equal so can be ignored and no need for log transformation. 

#(Sphericity for within-subjects variable if more than two levels) and inspect correction done automatically in print of ANOVA.

#Examine factorial ANOVA results...
summary(MyAFEX) 
eta_squared(MyAFEX) #effect size of partial eta^2 

#Post-hoc investigation into significant interaction...

#Testing the effect of disg when frig == high
t.test(hostility ~ disg, databugs[databugs$frig == "high",], paired = T) #Significant as p<.05

#Testing the effect of disg when frig == low
t.test(hostility ~ disg, databugs[databugs$frig == "low",], paired = T) #Significant as p<.05

#Testing the effect of frig wen disg == high
t.test(hostility ~ frig, databugs[databugs$disg == "high",], paired = T) #Not significant as p = 0.68

#Testing the effect of frig wen disg == low
t.test(hostility ~ frig, databugs[databugs$disg == "low",], paired = T) #Significant as p<.05

#Bayesian Factorial Two-Way repeated measures ANOVA
BayesANOVA = anovaBF(hostility ~ frig*disg + sub, whichRandom = "sub", databugs)
summary(BayesANOVA)

#Write up...
cat("The effect of frightfulness and disgustingness (high or low) on hostility 
    rating (0-10) is shown through the visualisation of data (Figure 1). Based
    on this,  high frightfulness (M = 7.2, SD =  3.13) appears to be associated with 
    higher thresholds on average than low frighfulness (M = 6.8, SD =  3.1).
    Similarly, the average hostility rating appears to be higher with high 
    disgustingness (M = 7.81, SD = 2.77) as compared to low disgustingness
    (M = 6.2, SD =  3.25). There did not appear to be an interaction
    between frightfulness and disgustingness in the interaction plot.")

cat("To evaluate whether these differences are statistically significant, I 
    conducted a factorial repeated-measures two-way ANOVA, followed by a
    bayesian factorial ANOVA.I tested the assumption of normality among the 
    residuals using a Shapiro-Wilk test (W = 0.94, p < .05), indicating sufficient
    evidence to reject the null that the residuals were normally distributed. 
    I continued with the analysis without any correction as ANOVAs have been 
    argued to be robust to violations of the assumption of normality when the 
    group-sizes are equal which is true for this design.
    (Sphericity was automatically corrected with a correction factor of x).")

cat("I observed a significant main effect of frightfulness [F(1,175)= 6.6595,
    p = 0.0107, 𝜂p2= .04], indicating that the hostility rating following
    low frightfulness is significantly lower compared to high frighfulness.
    I also observed a significant main effect of disgustingness [F(2, 57)=147.222,
    [F(1,175)= 6.6595, p < .001, 𝜂p2= .38], indicating that the hostility rating 
    following low disgustingness is significantly lower compared to high 
    disgustingness.")

cat("Finally, I observed a significant interaction between frightfulness and
    disgustingness [F(1,175) = 5.4153, p = 0.0211, 𝜂p2=.03]. Following this I 
    conducted post-hoc tests to understand the interaction better. For less
    disgusting insects, there was a significant difference in hostility 
    depending on how frightful the insect was, with higher frightfulness
    leading to higher hostility. In contrast, for highly disgusting insects,
    frightfulness has no impact on hostility ratings.")

cat("I conducted a Bayesian two-way ANOVA to estimate the effect of frightfulness
    and disgustingness on hostility rating. The data are 2.901141e+19 (BFy)
    times more likely with interaction model as compared with intercept-only
    model, suggesting this is the best model. This aligns with our normal ANOVA results.")

