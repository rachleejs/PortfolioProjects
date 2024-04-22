# EB Research Report Statistical Analysis
# Created on April 9, 2023
# Author: RL

setwd("~/Desktop/Research Report")

MeanAmplitude <- read.csv("5channels_grandaverage.csv", header = T ) # Grand average mean amplitude of 5 channels
data_summary <- read.csv("gonogo_summary.csv", header = T ) # Behavioural data summary
AppendedAmplitude <- read.csv("Appended_MeanAmplitude.csv", header = T ) # Grand average mean amplitude of all channels
ppData <- read.csv("NotAverage_5channels.csv", header = T)


#BIN1 = Go, correct 
#BIN2 = NoGo, correct 
#BIN3 = Go, incorrect 
#BIN4 = NoGo, incorrect 


library(ggplot2)
library(tidyverse)
library(tidyr)
library(corrplot)
library(lsr)
library(car)
library(afex)


# Extracting only correct condition from the grand average mean amplitude data
Correct_Data <- ppData %>%
  filter(binlabel == "              Go,_correct" | binlabel == "            NoGo,_correct")
 
# Calculating cell means, marginal means, and grand mean

#Grand mean
(grandMean <- mean(Correct_Data$value))

#Marginal Means
(marginalMeansCondition<- tapply(Correct_Data$value, Correct_Data$binlabel, mean))

(marginalMeansChannel <- tapply(Correct_Data$value, Correct_Data$chlabel, mean))

#SD calculation
tapply(Correct_Data$value, Correct_Data$binlabel, sd)
tapply(Correct_Data$value, Correct_Data$chlabel, sd)

#Individual means
(individualMeans <- aggregate(value ~ binlabel + chlabel, data = Correct_Data, mean))

# Visualising the data
(plot <- ggplot(Correct_Data, aes(x = chlabel , y = value, fill = binlabel, color = binlabel)) +
    geom_boxplot() + xlab("Condition") + ylab("Mean Amplitude")) + 
  ggtitle("Effects of Condition and Channel on Mean Amplitude")

# Setting the independent variables as factor
Correct_Data$binlabel<- as.factor(Correct_Data$binlabel)
Correct_Data$chlabel <- as.factor(Correct_Data$chlabel)

# Carrying out with ANOVA 

## Main Effects:  Conditions (Go/NoGo) + Channels (5)
## Interaction: If the condition affects the effect of channels or vice versa


condition_anova <- aov(value~binlabel*chlabel, Correct_Data)

#Testing the assumptions

# Normal distribution of Residuals
hist(x = condition_anova$residuals)
qqnorm(y = condition_anova$residuals)
shapiro.test(x = condition_anova$residuals) 

leveneTest(condition_anova)

# Run ANOVA
summary(condition_anova)

#Effect sizes
etaSquared(condition_anova)


#Multiple comparison (post-hoc)

TukeyHSD(condition_anova)



