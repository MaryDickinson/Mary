#Script Name: MixedAnovaPractice.R
#Created on: 2/01/2022
#Author: Mary Dickinson
#Purpose: Mixed ANOVA with planned contrasts practice question
#Version: 1.0
#Notes:

#Research question = Does new coke energy drink improve brain excitability more than coke classic and sparkling water?

#Analysis plan = 2x3 mixed ANOVA 
#DV = Continuous variable of motor evoked potential threshold (MEP)
#IV1 = Between-subjects categorical variable for type of drink (Drink, with 3 levels: water, coke energy, coke classic), 
#IV2 = Within-subjects categorical variable for transcranial magnetic stimulation region (Site, with 2 levels: M1 and vertex)

#Hypotheses...
#Set 1 (main effect of Drink), H0 = There is no difference in mean MEP as a function of drink consumed. H1 = There is difference in mean MEP as a function of drink consumed.
#Set 2 (main fixed effect of Site), H0 = There is no difference in mean MEP as a function of TMS site. H1 = There is difference in mean MEP as a function of TMS site.
#Set 3 (interaction between Drink and Site), H0 = The effect of drink on mean MEP is not affected by TMS site. H1 = The effect of drink on mean MEP is affected by TMS site.

#Planned contrast 1 = (Water vs Cokes, within M1 and vertex separately) = Cokes will show reduced MEP compared to sparkling water.
#Planned contrast 2 = (Classic vs Energy, within M1 and vertex separately) = Coke classic will show reduced MEP compared to coke energy. 

#Installation of required packages...
packages = c("ggplot2", "dplyr", "afex", "ez", "rstatix", "psych", "weights", "BayesFactor", "readr", "lsr", "car", "tidyverse", "reshape", "lsmeans", "skimr", "effectsize")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#Uploading data set to R...
CokeEnergy = read_csv("CokeEnergy.csv")
View(CokeEnergy)

#Inspecting data set...
head(CokeEnergy, 20) #long format data 
glimpse(CokeEnergy) 

#Changing character variables to factors...
CokeEnergy$Drink = as.factor(CokeEnergy$Drink)
CokeEnergy$Site = as.factor(CokeEnergy$Site)
glimpse(CokeEnergy)

#Check for missing values...
is.na(CokeEnergy) #no missing values

#Visualising data with boxplot within violin plot graph...
ggplot(CokeEnergy, aes(x = Drink, y = MEP, fill = Site, color = Site)) + 
  geom_boxplot(alpha = 0, width = .1, position = position_dodge(.9)) +
  geom_violin(alpha = .4) +
  geom_jitter(aes(fill = Site), position = position_jitterdodge(dodge.width = 1),
              size = 1, colour = "black", alpha = .3) +
  xlab("Type of Drink") +
  ylab("MEP (mV)") +
  labs(title = "MEP as a function of drink consumed and TMS site") +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 20))

#Interaction plot for hypotheses...
interaction.plot(trace.factor = CokeEnergy$Site,
                 x.factor = CokeEnergy$Drink,
                 response = CokeEnergy$MEP,
                 fun = mean,
                 xlab = "Type of Drink",
                 ylab = "MEP (mV)",
                 trace.label = "Site",
                 type="b",
                 col=c("blue","red"),   
                 pch=c(19, 17, 15),             
                 fixed=TRUE,                    
                 leg.bty = "o",
                 cex.axis = 1.4,
                 cex.lab = 1.6, lwd = 6)
#Plot indicates interaction is likely to be present between drink and site for MEP.

#Descriptive statistics...

#For means...
cellmeans = with(CokeEnergy, tapply(MEP, INDEX = list(Drink, Site), mean))
print(cellmeans) #cell means 

marginalmeans_drink = with(CokeEnergy, tapply(MEP, Drink, mean))
print(marginalmeans_drink) #marginal means for drink

marginalmeans_site = with(CokeEnergy, tapply(MEP, Site, mean))
print(marginalmeans_site) #marginal means for site

grandmean = mean(CokeEnergy$MEP) 
print(grandmean) #grand mean 

#For SDs...
cellsd = with(CokeEnergy, tapply(MEP, INDEX = list(Drink, Site), sd))
print(cellsd) #cell SDs 

marginalsd_drink = with(CokeEnergy, tapply(MEP, Drink, sd))
print(marginalsd_drink) #marginal SDs for drink

marginalsd_site = with(CokeEnergy, tapply(MEP, Site, sd))
print(marginalsd_site) #marginal SDs for site

grandsd = sd(CokeEnergy$MEP) 
print(grandsd) #grand mean 

#Running the analysis for 2x3 mixed ANOVA...
MixedANOVA = aov_ez(data = CokeEnergy,
                    id = "ParticipantID",
                    dv = "MEP",
                    between = "Drink",
                    within = "Site",
                    detailed = TRUE,
                    return_aov = TRUE)

#Testing ANOVA assumptions...

#Normality of residuals...
hist(x = MixedANOVA$lm$residuals) 
qqnorm(y = MixedANOVA$lm$residuals) 
shapiro.test(x = MixedANOVA$lm$residuals) #p < .05
#Residuals are not normally distributed but group size are equal so can be ignored and no need for log transformation. 

#Homogeneity of variance for between-subject variable of Drink...
leveneTest(CokeEnergy$MEP, CokeEnergy$Drink)
#There is insufficient evidence to reject the null hypothesis that there is homogeneity of variance within the data. 

#(Sphericity for within-subjects variable if more than two levels) and inspect correction done automatically in print of ANOVA.

#Examine mixed ANOVA results...
summary(MixedANOVA) 
eta_squared(MixedANOVA) #effect size of partial eta^2 

#Planned contrasts...
ANOVAcellmeans = lsmeans(MixedANOVA, specs = c("Drink", "Site"))
print(ANOVAcellmeans) #Anova cell means for order of cells

#Contrast vectors...
coke_vs_water_M1 = c(-1, -1, 2, 0, 0, 0) #comparing coke drinks vs water for M1 site only
coke_vs_water_V = c(0, 0, 0, -1, -1, 2) #comparing coke drinks vs water for vertex site only
classic_vs_energy_M1 = c(1, -1, 0, 0 , 0, 0) #comparing coke classic vs coke energy for M1 site only
classic_vs_energy_V = c(0, 0, 0, 1 , -1, 0) #comparing coke classic vs coke energy for vertex site only

#Running planned contrasts...
summary(contrast(ANOVAcellmeans, list(coke_vs_water_M1, coke_vs_water_V, classic_vs_energy_M1, classic_vs_energy_V)))

#Write up...
cat("The effect of TMS site and drink consumed on threshold for a motor-evoked 
    potential (MEP) is shown through the visualisation of data (Figure 1). Based
    on this, sparkling water (M = 70.4, SD =  12) appears to be associated with 
    higher thresholds on average than Coke Energy (M = 36.0, SD =  15) or Coke
    Classic (M = 40.0, SD =  16). Similarly, the average threshold appears to be
    higher when TMS was applied to the vertex (M = 54.0, SD =  12) as 
    compared to M1 (M = 43.6 SD =  26). There also appears to be an interaction
    between drink and site in the interaction plot, supported by the difference
    between the two TMS sites (M1 and Vertex) is greater for sugary 
    drinks (Coke Energy and Coke Classic) as compared to sparkling water.")

cat("To evaluate whether these differences are statistically significant, I 
    conducted a 2x3 mixed ANOVA, with TMS site as my within-subjects factor 
    (M1 vs. Vertex) and drink consumed as my between-subjects factor 
    (Coke Energy, Coke Classic, Sparkling Water). I tested the assumption of
    normality among the residuals using a Shapiro-Wilk test (W = 0.97, p=.009),
    indicating sufficient evidence to reject the null that the residuals were 
    normally distributed. I continued with the analysis without any correction 
    as ANOVAs have been argued to be robust to violations of the assumption of 
    normality when the group-sizes are equal which is true for this design. I 
    used Levene’s test to assess the assumption of homogeneity of variance 
    within my between-subjects factor, which was not significant (F=0.97, p=.38). 
    (Sphericity was automatically corrected with a correction factor of x).")

cat("I observed a significant main effect of TMS site [F(1, 57)=53.223, p<.001 ,
    𝜂p2=.48], indicating that the threshold for evoking an MEP following
    stimulation to M1 is significantly lower compared to the threshold required
    for the vertex. I also observed a significant main effect of drink consumed 
    [F(2, 57)=147.222, p<.001 , 𝜂p2=.84], confirming that the thresholds for 
    the three drinks are not equal.")

cat("Finally, I observed a significant interaction between TMS site and drink 
    [F(2, 57)=96.975, p<.001 , 𝜂p2=.77]. Following this I conducted planned 
    contrasts focusing on data where the vertex or M1 was the site of 
    stimulation to understand the interaction better. Where stimulation was 
    applied to the vertex, sparkling water was associated with significantly 
    higher thresholds as compared to the average of the two Coke drinks, 
    Coke Classic or Coke Energy [t(57)=22.9, p=.0005]. No difference was
    observed between the two Coke drinks at this site [t(57)=3.59, p=.32]. 
    Where stimulation was applied to M1, sparkling water was associated with 
    significantly higher thresholds as compared to the average of the two Coke
    drinks, Coke Classic or Coke Energy [t(57)=35.6, p<.001]. Within the two 
    Coke drinks, Coke Energy had significantly lower thresholds as compared to 
    Coke Classic [t(57)=4.43, p=.013].")
