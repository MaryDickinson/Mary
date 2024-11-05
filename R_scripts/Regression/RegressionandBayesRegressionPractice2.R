#Script Name: RegressionandBayesRegressionPractice2.R
#Created on: 11/01/2022
#Author: K21014303
#Purpose: Regression and bayesian regression
#Version: 1.0
#Notes: 

#Research question... How well do gender of spokesperson, likeability of spokesperson, gender match with participant, year level of participant and self-confidence of participant predict change in attitude towards underage drinking in teenagers? 

#Analysis plan = Multiple regression followed by Bayesian regression 
#Outcome variable = Numerical variable of change in attitude of participant towards underage drinking (attitude, -5 - 5)
#Predictor variables = 
#PV1 = Categorical variable of gender of spokesperson (genderSpokesperson, with 3 levels: female, male and non-bin_unspec)
#PV2 = Categorical variable of gender of participant (genderParticipant, with 3 levels: female, male and non-bin_unspec)
#PV3 = Numerical variable of gender matched between spokesperson and participant (genderMatch, 0 or 1, where 0 = no match, 1 = match)
#PV4 = Numerical variable of self-confidence of participant (selfConfidence, scale of 0-100, where 0 = lacking self-confidence, 100 = very confident),
#PV5 = Numerical variable of likeability of spokesperson (likeability, scale of 0-10, where 0 = very unlikable, 10 = super likeable)
#PV6 = Numerical variable of year level of participant (yearLevel, with 4 levels: 9, 10, 11 and 12)

#Hypotheses...
#H0 = Our predictors do not fit the data better than intercept-only model.
#H1 = Our predictors do fit the data better than intercept-only model.

#No planned contrasts. 

#Installation of required packages...
packages = c("ggplot2", "dplyr", "afex", "ez", "rstatix", "psych", "weights", "BayesFactor", "readr", "afex", "BayesFactor", "ggpubr", "corrplot", "skimr", "car", "performance")
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
alcohol = read_csv("~/question_alcohol.csv")
View(alcohol)

#Inspecting data set...
head(alcohol, 20) #long format data 
glimpse(alcohol) 

#Changing character variables to factors...
alcohol$genderSpokesperson = as.factor(alcohol$genderSpokesperson)
alcohol$genderParticipant = as.factor(alcohol$genderParticipant)
glimpse(alcohol)

#Check for missing values...
is.na(alcohol) #no missing values

#Descriptive statistics... 
skim(alcohol)
mn = mean(alcohol$attitude)
print(mn)
sd = sd(alcohol$attitude)
print(sd)

#Marginal means...
#By year group
with(alcohol, tapply(X = attitude,
                     INDEX = yearLevel,
                     FUN = pastecs::stat.desc))

#By gender match
with(alcohol, tapply(X = attitude,
                     INDEX = genderMatch,
                     FUN = pastecs::stat.desc))

#Cell means..
cellstable = alcohol %>%
  dplyr::group_by(genderMatch, yearLevel) %>%
  dplyr::summarise(mean = round(mean(attitude), 2),
                   sd = round(stats::sd(attitude), 2))
print(cellstable)

#Visualising data using scattergraphs for numerical predictors and boxplots for categorical predictors against rating...
p1 = ggplot(alcohol, aes(x = genderSpokesperson, y = attitude)) + geom_boxplot()
p2 = ggplot(alcohol, aes(x = genderParticipant, y = attitude)) + geom_boxplot()

p3 = ggplot(alcohol, aes(x = genderMatch, y = attitude)) + geom_point() + geom_smooth(method = lm) 
p4 = ggplot(alcohol, aes(x = selfConfidence, y = attitude)) + geom_point() + geom_smooth(method = lm) 
p5 = ggplot(alcohol, aes(x = likeability, y = attitude)) + geom_point() + geom_smooth(method = lm) 

ggarrange(p1, p2, p3, p4, p5)

#Scattergraph for self-confidence and gender match on attitude for year level...
ggplot(alcohol, aes(x = selfConfidence, y = attitude, colour = genderMatch)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(vars(yearLevel))

#Scattergraph for likeability and gender match on attitude for year level...
ggplot(alcohol, aes(x = likeability, y = attitude, colour = genderMatch)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(yearLevel))

#Scattergraph for self-confidence and participant gender on attitude for year level...
ggplot(alcohol, aes(x = selfConfidence, y = attitude, colour = genderParticipant)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(yearLevel))

#Scattergraph for likeability and participant gender on attitude for year level...
ggplot(alcohol, aes(x = likeability, y = attitude, colour = genderParticipant)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(yearLevel))

#Scattergraph for self-confidence and gender match on attitude for participant gender...
ggplot(alcohol, aes(x = selfConfidence, y = attitude, colour = genderMatch)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(genderParticipant))

#Scattergraph for likeability and gender match on attitude for participant gender...
ggplot(alcohol, aes(x = likeability, y = attitude, colour = genderMatch)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(vars(genderParticipant))

#Creating full linear multiple regression model...
alcoholreg = lm(formula = attitude ~ genderMatch + selfConfidence + likeability + yearLevel, data = alcohol)
summary.lm(alcoholreg)
performance::performance(alcoholreg) #Values indicate not optimal fit, could create another model with less predictors if you have time. 

#Effect sizes...
rsq::rsq.partial(alcoholreg)

#Checking assumptions of full linear regression model...

#Normality of residuals...
hist(alcoholreg$residuals)
qqnorm(y = alcoholreg$residuals) 
shapiro.test(alcoholreg$residuals) #p = 0.03
#p>.05 then there is insufficient evidence to reject null hypothesis that residuals are normally distributed. 
#However, with equal group size I am choosing to ignore as histogram shows no significant skew in distribution.

#Multicollinearity...
newalc = alcohol[,c("genderMatch", "selfConfidence", "likeability")]
cormat = round(cor(newalc),2)
corrplot.mixed(cormat) #No appearance of multicollinearity between numerical variables.

vif(alcoholreg) #Multicollinearity assumption is met as values < 5. 

#Homogeneity of variances...
ncvTest(alcoholreg) #Assumption of homogeneity is met as p >.05.

#Extra investigation into assumptions using plots...
plot(alcoholreg)
performance::check_model(alcoholreg)
#Linearity and homogeneity of variance assumptions met as line is flat and horizontal.
#Multicollinearity between predictors is insignificant. 
#Residuals are normally distributed.  

#Creating Bayesian regression model... 
alcoholbayes = lmBF(formula = attitude ~ genderMatch + selfConfidence + likeability + yearLevel, data = alcohol)
summary(alcoholbayes)

#Individual parameter estimates...
parambayes = posterior(alcoholbayes, iterations = 10000)
summary(parambayes)

#Run Bayesian regression model but only including continuous predictors...
contbayes = regressionBF(formula = attitude ~ selfConfidence + likeability, data = alcohol)
print(contbayes)
head(contbayes/max(contbayes)) 
1/0.1345065 #1/second most likely model
#Out of all possible models including the continuous variables, the model including only self-confidence and likeability is the best. 

#Write up...
cat("The predictor variables of likeability of spokesperson, gender match between
spokesperson and participant, year level and self-confidence of participant on
prediction for attitude towards underage alcohol consumption is 
shown through the visualisation of data (Figure 1). I performed a multiple 
regression and Bayesian regression using R to quantify this. My outcome variable
was attitude of participant towards underage drinking (ranging from -5 - 5) and my 
predictor variables were gender matched between spokespersonand participant 
(0 or 1, where 0 = no match, 1 = match),, year level (with 4 levels: 9, 10, 11 and 12),
self-confidence of participant (scale of 0-100, where 0 = lacking self-confidence,
100 = very confident) and likeability of spokesperson (scale of 0-10, where 0 = 
very unlikable, 10 = super likeable).")

cat("All assumptions were met for multicollinearity (VIF < 5) and homogeneity of
variance (X^2(1) = 1.79, p = 0.18). There was insufficient evidence to reject 
null hypothesis for assumption of normality of residuals (W = 0.98, p < .05). 
However, with equal group size and no significant skew in distribution shown, 
violations of assumption are ignored.")

cat("In the multiple linear regression, I observed a significant regression 
equation [equation 1, F(4, 195) = 20.98, p<.001, adjusted R2=.2865], suggesting 
the model fit is better than chance. However, with low F-statistic and adjusted 
R2, the model fit does not account for a large proportion of variance. 
Self-confidence of participant and likeability of spokesperson predictor variables 
were significant predictors of rating (p’s < .05).")

cat("I performed a Bayesian regression using regressionBF, which is part of the 
BayesFactor package (Morey et al., 2018), to predict attitude as a function of the
predictor variables. Our data is 1.255773e+12 more likely under a model including only 
likeability than under the null model (intercept only), which is extreme evidence
for this simple model. The model including both likeability and self-confidence 
also receives extreme evidence, as our data is 9.336152e+12 more likely under it
than under the null model. Furthermore, our data is 7.43 more likely under this 
model with two predictors than under the model with only Likeability, which is 
considered moderate evidence.")



