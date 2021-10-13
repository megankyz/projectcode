library(readr)
library(dplyr)

aafund.raw <- read_csv("aafundusd.csv")
etfund.raw <- read_csv("etfundusd.csv")

summary(aafund.raw)

#to convert esg_indicator into numeric and to see how many different fund names and categories there are
aafund.ienc <- aafund.raw %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

#removal of isin, fund names and categories tentatively, can just edit the code if want to keep
aafund.cleaned <- aafund.ienc[-c(1:3)]

#i didn't select the e, s, g scores cuz they're not in the etf
#dataset, i'm not sure if using those factors in the lm model will lead to an issue when 
#we use the regression model on the etf dataset
sel.var <- aafund.cleaned[c(1:11, 15:16)]

#to see the values when we use all the variables 
aafund.mlm <- lm(sustainability_score ~., data = sel.var)
summary(aafund.mlm)

aafund.mlm.fws <- step(aafund.mlm, direction = "forward")
summary(aafund.mlm.fws)

aafund.mlm.bwe <- step(aafund.mlm, direction = "backward")
summary(aafund.mlm.bwe)

aafund.mlm.step <- step(aafund.mlm, direction = "both")
summary(aafund.mlm.step)

#WOW all the R2 values damn small 

sel.var2 <- aafund.cleaned[c(2, 4:5, 8:11, 15:16)]
aafund.mlm2 <- lm(sustainability_score ~., data = sel.var2)
summary(aafund.mlm2)

# adjusted R2 19.24%...
