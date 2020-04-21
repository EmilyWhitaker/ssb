## Clear workspace and console
rm(list=ls()); cat("\014")

# Load required packages
library(FSA); library(dplyr);library(magrittr);library(tidyr) # data management
library(mgcv);library(nlme); library(lme4) # modeling
library(viridisLite); library(gridExtra); library(ggplot2) # data viz 
library(lubridate) # dealing with dates
library(pscl) #for 2 hurdle modeling

#=========
totals_and_genus = read.csv('data/clean_abiotic_genus_03262020.csv', stringsAsFactors = F)
totals_and_genus$sampledate = mdy(totals_and_genus$sampledate)

#add those cols 
totals_and_genus$one.cbv = totals_and_genus$CellBioVol+1
totals_and_genus$log.cbv = log(totals_and_genus$one.cbv)
totals_and_genus$int.cbv=as.integer(totals_and_genus$CellBioVol)
totals_and_genus$oneint.cbv = totals_and_genus$int.cbv+1
totals_and_genus$logint.cbv =as.integer(log(totals_and_genus$oneint.cbv))

##======
#Armored Dinoflagellate during ice-on

ice.on = subset(totals_and_genus, ice.pres == 1)
gen.keep1=c("Armored Dinoflagellate")
genus.sub1 = subset(totals_and_genus, Genus %in% gen.keep1)
AD.ice.on = subset(genus.sub1, ice.pres == 1) #correct

hist(AD.ice.on$log.cbv)

install.packages("pscl")
library(pscl)
mod.hurdle <- hurdle(log.cbv ~ ., data = AD.ice.on)
# same as this:
mod.hurdle <- hurdle(log.cbv ~ ., data = AD.ice.on, dist = "poisson", zero.dist = "binomial")
summary(mod.hurdle)



install.packages("AER") 
library(AER)
data("NMES1988")

# select certain columns; Col 1 is number of visits
#nmes <- NMES1988[, c(1, 6:8, 13, 15, 18)]
ades<- AD.ice.on[,c(7,38, 39, 41, 45, 53)]
#plot(table(nmes$visits))
plot(table(ades$logint.cbv))
#mod1 <- glm(visits ~ ., data = nmes, family = "poisson")
mod2<- glm(logint.cbv ~., data = ades, family= "poisson")
#mu <- predict(mod1, type = "response")
mu2 <- predict(mod2, type = "response")
#exp <- sum(dpois(x = 0, lambda = mu)) 
exp2 <- sum(dpois(x = 0, lambda = mu2)) 
#round(exp)
round(exp2)

# install.packages("pscl")
library(pscl)
mod.hurdle <- hurdle(visits ~ ., data = nmes)

mod.hurdle2 <- hurdle(logint.cbv ~ ., data = ades)


# same as this:
mod.hurdle <- hurdle(visits ~ ., data = nmes, 
                     dist = "poisson", 
                     zero.dist = "binomial")

summary(mod.hurdle)

summary(mod.hurdle2)



#Call:
#hurdle(formula = visits ~ ., data = nmes, dist = "poisson", zero.dist = "binomial")
#sum(predict(mod.hurdle, type = "prob")[,1])


hurdle(formula = logint.cbv ~ ., data = ades, dist = "poisson", zero.dist = "binomial")
sum(predict(mod.hurdle2, type = "prob")[,1])

