#Script Name: RegressionandBayesRegression.R
#Created on: 14/11/2022
#Author: Mary Dickinson
#Purpose: To practice regression and bayesian regression
#Version: 1.0
#Notes: 

#Research question... How well do bitterness, cocoa content, aroma, thickness, alcohol, presence of marshmallows and flavour predict rating of hot chocolate?

#Analysis plan = Multiple regression followed by Bayesian regression
#Outcome variable = Numerical variable of rating of hot chocolate (rating, 0-100)
#Predictor variables = 
#PV1 = Numerical variable of bitterness (bitterness, 0-10, where 0 = carrots, 10 = bitter orange peel)
#PV2 = Numerical variable of cocoa content (cocoaContent, 40 - 90%)
#PV3 = Numerical variable of aroma (aroma, scale of 1-5, where 1 = smells like diesel fuel, 5 = smells like an alpine meadow in spring)
#PV4 = Numerical variable of thickness (thickness, scale of 1-5, where 1 = like water, 5 = thicker than toothpaste)
#PV5 = Numerical variable of alcohol (alcohol, 0 - 8%)
#PV6 = Categorical variable of presence of marshmallows (marshmallows, with 2 levels: yes and no)
#PV7 = Categorical variable of flavour (flavour, with 4 levels: Plain, Hot Chili, Irish Cream, or Peppermint)

#Hypotheses...
#H0 = Our predictors do not fit the data better than intercept-only model.
#H1 = Our predictors do fit the data better than intercept-only model.

#No planned contrasts. 

#Installation of required packages...
packages = c("ggplot2", "dplyr", "afex", "ez", "rstatix", "psych", "weights", "BayesFactor", "readr", "afex", "BayesFactor", "ggpubr", "corrplot")
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
cocoadata = read_csv("~/cocoadata.csv")
View(cocoadata)

#Inspecting data set...
head(cocoadata, 20) #long format data 
glimpse(cocoadata) 

#Changing character variables to factors...
cocoadata$marshamallows = as.factor(cocoadata$marshmallows)
cocoadata$flavour = as.factor(cocoadata$flavour)
glimpse(cocoadata)

#Cleaning data by removing columns...
cocoadata = cocoadata %>% 
  select(-...1)

#Check for missing values...
is.na(cocoadata) #no missing values

#Descriptive statistics... 
skim(cocoadata)
mn = mean(cocoadata$rating)
sd = sd(cocoadata$rating)

#Visualising data using scattergraphs for numerical predictors and boxplots for categorical predictors against rating...
p1 = ggplot(cocoadata, aes(x = alcohol, y = rating)) + geom_point() + geom_smooth(method = lm) #strong relationship 
p2 = ggplot(cocoadata, aes(x = bitterness, y = rating)) + geom_point() + geom_smooth(method = lm) 
p3 = ggplot(cocoadata, aes(x = aroma, y = rating)) + geom_point() + geom_smooth(method = lm) 
p4 = ggplot(cocoadata, aes(x = cocoaContent, y = rating)) + geom_point() + geom_smooth(method = lm) #strong relationship
p5 = ggplot(cocoadata, aes(x = thickness, y = rating)) + geom_point() + geom_smooth(method = lm) 

p6 = ggplot(cocoadata, aes(x = flavour, y = rating)) + geom_boxplot()
p7 = ggplot(cocoadata, aes(x = marshmallows, y = rating)) + geom_boxplot()

ggarrange(p1, p2, p3, p4, p5, p6, p7)

#Creating full linear multiple regression model...
cocoadatareg = lm(formula = rating ~ alcohol + bitterness + aroma + cocoaContent + marshmallows + thickness + flavour, data = cocoadata)
summary.lm(cocoadatareg)

#Checking assumptions of full linear regression model...

#Normality of residuals...
hist(cocoadatareg$residuals)
qqnorm(y = cocoadatareg$residuals) 
shapiro.test(cocoadatareg$residuals) #p = 0.7
#Residuals are normally distributed so the assumption is met. 

#Multicollinearity...
newcdata = cocoadata[,c("alcohol","bitterness","aroma", "cocoaContent", "thickness")]
cormat = round(cor(newcdata),2)
corrplot.mixed(cormat) #Possible relationship between alcohol and cocoa content? Further investigation is needed with variance inflation factor. 

vif(cocoadatareg) #Multicollinearity assumption is met as values < 5. 

#Homogeneity of variances...
ncvTest(cocoadatareg) #Assumption is met as p >.05

#Extra investigation into assumptions using plots...
plot(cocoadatareg)
performance::check_model(cocoadatareg)
#Linearity and homogeneity of variance assumptions met as line is flat and horizontal.
#Multicollinearity between predictors is insignificant. 
#Residuals are normally distributed.  

#Creating Bayesian regression model... 
cocoadatabayes = lmBF( rating ~ alcohol + bitterness + aroma + cocoaContent + marshmallows + thickness + flavour, data = cocoadata)
summary(cocoadatabayes)

#Individual parameter estimates...
parambayes = posterior(cocoadatabayes, iterations = 10000)
summary(parambayes) #suggests full model is better with very large BFy. 

#Run Bayesian regression model but only including continuous predictors...
contbayes = regressionBF(rating ~ alcohol + bitterness + aroma + cocoaContent + thickness, cocoadata)
head(contbayes/max(contbayes))
#Out of all possible models including the continuous variables, the model including only alcohol and cocoa content is the best. 

#Write up...
cat("The predictor variables of bitterness, cocoa content, aroma, thickness, 
alcohol, presence of marshmallows and flavour predict rating of hot chocolate is 
shown through the visualisation of data (Figure 1). I performed a multiple 
regression and Bayesian regression using R to quantify this. My outcome variable
was rating of hot chocolate (0-100) and my predictor variables were bitterness 
(0-10, where 0 = carrots, 10 = bitter orange peel), cocoa content (40 - 90%),
aroma (scale of 1-5, where 1 = smells like diesel fuel, 5 = smells like an 
alpine meadow in spring), thickness (scale of 1-5, where 1 = like water, 5 = 
thicker than toothpaste), alcohol (0 - 8%), presence of marshmallows (with 2 
levels: yes and no) and flavour (with 4 levels: Plain, Hot Chili, Irish Cream, 
or Peppermint).")

cat("All assumptions were met for normality of residuals (W = 1, p = 0.7), 
multicollinearity (VIF < 5) and homogeneity of variance (X^2(1) = 0.18, p = 0.67). ")

cat("In the multiple linear regression, I observed a significant regression 
equation [equation 1, F(9, 490) = 249.4, p<.001, adjusted R2=.8175]. Alcohol, 
cocoa content and marshmallow predictor variables were significant predictors of
rating (p’s < .05).")

cat("I performed a Bayesian regression using regressionBF, which is part of the 
BayesFactor package (Morey et al., 2018), to predict rating as a function of the
predictor variables. The best-performing model (compared to an intercept-only 
model) included all predictors (BF10 = 6.19 x 10^174). This model was estimated 
to be 2.5 times more likely than the full model containing all predictors (BF01 = 0.389).
Investigation into model with only continuous variables found, out of all 
possible models including the continuous variables, the model including only 
alcohol and cocoa content is the best.")


