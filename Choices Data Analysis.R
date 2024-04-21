#CW Data Analysis
#Date: November 25th 2022

#Open data
framing_data <- read.csv("choices_framing_data.csv", header = T)

#Call for packages
library(ggplot2)
library(car)
library(lsr)
library(tidyverse)


#Have a look at the data
str(framing_data)
summary(framing_data)

#Descriptive Statistics for Demographics

summary(framing_data$age)

#Remove participants with age<18, as it is unknown

AgeData <- framing_data %>%
  filter(age >= 18)

summary(AgeData)
sd(AgeData$age, na.rm = T)

#-> Age Mean = 28.39, SD = 13.65

framing_data %>%
  count(gender)

framing_data %>%
  count(framing)

framing_data %>%
  count(norm)

#-> Gender: Female 249, Male 115, Non-binary 11, Prefer not to say 2, NA 2

#Descriptive Statistics

Dyn_Pos <- framing_data %>%
  filter(norm == "dyn" & framing == "pos")

Stat_Pos <- framing_data %>%
  filter(norm == "stat" & framing == "pos")

Stat_Neg <- framing_data %>%
  filter(norm == "stat" & framing == "neg")

Dyn_Neg <- framing_data %>%
  filter(norm == "dyn" & framing == "neg")

summary(Dyn_Pos)
summary(Stat_Pos)
summary(Stat_Neg)
summary(Dyn_Neg)

sd(Dyn_Pos$intn)
sd(Stat_Pos$intn)
sd(Dyn_Neg$intn)
sd(Stat_Neg$intn)



#Visualising the data using box plot

Dataplot = ggplot(framing_data, mapping = aes(x = norm , y = intn, fill=framing, color= framing)) +
  geom_boxplot(alpha = 0, width = .5)+ xlab("Norms") + ylab("Interest in Reducing Meat Consumption") +
  ggtitle("Effect of Norms and Framing on Interest in Reducing Meat Consumption")+ 
  scale_x_discrete(labels=c("Dynamic", "Static")) 
  
Dataplot

#Setting the independent variables as factor
framing_data$norm <- as.factor(framing_data$norm)
framing_data$framing <- as.factor(framing_data$framing)

#Set up ANOVA
full_factorial <- aov(intn ~ norm * framing, framing_data )

#Testing the assumptions

# Normal distribution of Residuals
hist(x = full_factorial$residuals)
qqnorm(y = full_factorial$residuals)
shapiro.test(x = full_factorial$residuals) 
#-> Not OK, violated. Could try log transformation

# Homogeneity of Variances

leveneTest(full_factorial)
# -> OK

#Carry out with ANOVA

summary(full_factorial)


#Effect sizes

etaSquared(full_factorial)




