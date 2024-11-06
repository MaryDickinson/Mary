#Script Name: LinearMixedModelsPractice.R
#Created on: 28/11/2022
#Author: Mary Dickinson
#Purpose: Linear mixed models practice 
#Version: 1.0
#Notes:

#Research question = Does donut type predict timBieb donut sales, after controlling for baseline sales and taking into account that this may vary across different sales locations across Canada?

#Analysis plan = Linear mixed model regression
#DV = Continuous numerical variable of new timBieb donut sales (timBiebSales, $)
#IV1 = Fixed effects variable of type of donut (donutType, with 3 levels: BirthdayCakeWaffle, ChocolateWhiteFudge, SourCreamChocChip), 
#IV2 = Continuous numerical variable of baseline donut sales (baselineSales, $)
#IV3 = Random effects variable of store location with random intercept and slope per location (storeLocation, with 6 levels: Alberta, BritishColumbia, NovaScotia, Ontario, PrinceEdwardIsland and Quebec)

#Hypotheses...
#H0 = There is no effect in donut sales with new timBiebs donut types.
#H1 = Change in donut sales with new timBiebs donut types.

#Installation of required packages...
packages = c("ggplot2", "dplyr", "afex", "ez", "rstatix", "psych", "weights", "BayesFactor", "readr", "afex", "BayesFactor", "ggpubr", "corrplot")
package.check = lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#Uploading dataset onto R...
donut = read_csv("donut.csv")
View(donut)

#Inspecting data set...
head(donut, 20) #long format data 
glimpse(donut) 

#Changing character variables to factors...
donut$donutType = as.factor(donut$donutType)
donut$storeLocation = as.factor(donut$storeLocation)
glimpse(cocoadata)

#Cleaning data by removing columns...
donut = donut %>% 
  select(-...1)

#Checking the number of data points per cell...
xtabs(formula = ~  storeLocation, data = cocoadata)

#Check for missing values...
is.na(donut) #no missing values

#Descriptive statistics...
skim(unicorns)

#Overall means and SDs per donut type...
aggregate(timBiebSales ~ donutType, donut, mean)
aggregate(timBiebSales ~ donutType, donut, sd)

#Overall means and SDS per donut type separated by store location...
aggregate(timBiebSales ~ donutType + storeLocation, donut, mean)
aggregate(timBiebSales ~ donutType + storeLocation, donut, sd)

#Visualising data using boxplots...
ggplot(donut, aes(x = donutType, y = timBiebSales, color = storeLocation)) + 
  geom_boxplot() +
  geom_smooth(method = "lm", se = F) +
  xlab("Donut Type") +
  ylab("Sales") +
  labs(title = "Donut type, Location and Sales") +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 20))

#OR another boxplot option...
ggplot(donut, aes(x=donutType, y=timBiebSales)) + 
  geom_boxplot() +
  facet_wrap(.~storeLocation) +
  xlab("Donut Type") +
  ylab("Sales") +
  labs(title = "Donut type, Location and Sales") +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 20))

#Linear mixed effects analysis...

#Comparing a random intercept-only model to a random-intercept and random-slope model...
#Random intercept...
randomintercept = lmer(timBiebSales ~ donutType + baselineSales + (1|storeLocation), data = donut)
summary(randomintercept)
fixef(randomintercept) #intercept = -5.26 
ranef(randomintercept)
donut$fitted.mod = fitted(randomintercept)
ggplot(data = donut, mapping = aes(y = resid(randomintercept), x = donutType)) + 
  geom_hline(yintercept = 0) +
  geom_point() + 
  geom_line() + 
  facet_wrap(. ~ storeLocation)

#Random slope...
randomSlope = lmer(timBiebSales ~ donutType + baselineSales  + (1|storeLocation) + (0 + donutType|storeLocation), data = donut, control = lmerControl(optimizer = "bobyqa"))
summary(randomSlope)
fixef(randomSlope) #intercept = 9.61
ranef(randomSlope)
ggplot(data = donut, mapping = aes(y = resid(randomSlope), x = donutType)) + 
  geom_hline(yintercept = 0) +
  geom_point() + 
  geom_line() + 
  facet_wrap(. ~ storeLocation)

#Chi-sq for comparison of random intercept and random slope models...
anova(randomintercept, randomSlope, test = "Chisq")
#The difference model fit is significant with the random slope model fitting better (X^2(6) = 329.89, p < .05).

#To quantify whether donut type has an effect on sales use comparison of random slope model with simpler model excluding fixed effect of donut type...
nodonutModel = lmer(timBiebSales ~ baselineSales + (1|storeLocation) + (0 + donutType|storeLocation), donut, control = lmerControl(optimizer = "bobyqa"))
anova(randomSlope, nodonutModel, test = "Chisq")
#The difference in model fit is not significant (X^2(2)= 1.8, p = 0.41).
#So donut type does not have an effect on sales, after controlling for baseline and allowing for random intercept and slope for sales location. 

#Checking assumptions of linear mixed models...

#Normality of residuals...
hist(residuals(randomSlope))
qqnorm(residuals(randomSlope))
qqline(residuals(randomSlope))
shapiro.test(residuals(randomSlope)) #p < .05
#The residuals are not normally distributed, but the distribution doesn't look really systematically skewed
#So no transformation is performed and assumption may be ignored. 

#Linearity between dependent and independent (quantative) variables...
ggplot(data = donut, aes(y = timBiebSales, x=baselineSales)) + 
  geom_point() +
  geom_smooth(method = lm)

#Linearity between dependent and independent (qualitative) variables...
donut$donutType_num = as.numeric(donut$donutType)
ggplot(data = donut, aes(y = timBiebSales, x=donutType_num)) + 
  geom_point() +
  geom_smooth(method = lm) #Line should be near to horizontal.  
#Data appear to be vaguely linear.

#With more than one fixed effect variable test multicollinearity. 

#Write up...

cat("I performed a linear mixed model using lmer, which is part of the package 
lme4 (Bates et al., 2021). New donut sales was entered as my outcome variable and 
new donut types for timBiebs (with 3 levels: BirthdayCakeWaffle, 
ChocolateWhiteFudge, SourCreamChocChip) was my fixed effect of interest, after 
controlling for baseline donut sales. Store location where the donuts were 
purchased was entered as a random effect. These were entered such that both 
parameters (intercept and slope) of sales were allowed to vary as a function of 
location.")

cat("A random intercept only model was compared to a full model with random slope 
included. The difference in model fit was significant with the random slope model
fitting better in a Chi-Square difference test (X^2(6) = 329.89, p < .05).")

cat("This model was compared to a null model that was identical to the full model
with the exception of the donut type as a fixed effect. A Chi-Square difference
test revealed no significant difference between the full model and the null model
(X^2(2)= 1.8, p = 0.41).So donut type does not have an effect on sales, after 
controlling for baseline and allowing for random intercept and slope for sales 
location.")
