#### Packages possibly needed
rm(list= ls())
library(foreign)
library(plyr)
library(dplyr)
library(scales)
library(zoo)
library(tidyr)
library(stringr)

### Import data
acs2 <- read.dta(file=paste("your/working/directory/usa_00007.dta"))

### Change variable types and create new variables
acs2$ageN<-as.numeric(as.character(acs2$age))
acs2$degfieldN <- as.factor(acs2$degfield)
acs2$strata = paste(as.numeric(acs2$statefip)*100000, acs2$puma, sep = "")

#### Build "allc" a table that has proportions nationally
all <- acs2 %>%
  filter(ageN > 22) %>%
  group_by(degfieldd) %>%
  summarize(totnum = sum(perwt)) %>%
  mutate(percent = totnum / sum(totnum)) %>%
  arrange(-percent)
all1 <- acs2 %>%
  filter(ageN > 22, degfieldd != "N/A") %>%
  group_by(degfieldd) %>%
  summarize(totnum_noNA = sum(perwt)) %>%
  mutate(percent_noNA = totnum_noNA / sum(totnum_noNA)) %>%
  arrange(-percent_noNA)
allc <- left_join(all, all1, by = "degfieldd" )


### Build the proportion for states
all_state <- acs2 %>%
  filter(ageN > 22) %>%
  group_by(stateicp, degfieldd) %>%
  summarize(totnum_state = sum(perwt)) %>%
  mutate(percent_state = totnum_state / sum(totnum_state)) %>%
  arrange(-percent_state)
all1_state <- acs2 %>%
  filter(ageN > 22, degfieldd != "N/A") %>%
  group_by(stateicp, degfieldd) %>%
  summarize(totnum_noNA_state = sum(perwt)) %>%
  mutate(percent_noNA_state = totnum_noNA_state / sum(totnum_noNA_state)) %>%
  arrange(-percent_noNA_state)
allc_state <- left_join(all_state, all1_state, by = c("degfieldd", "stateicp"))

### Combine state and national
allc_state_national <- left_join(allc_state, allc, by = "degfieldd")

### Keep only top 100 ranked majors
x <- all1[1:100, 1]
all_top100 <- filter(allc_state_national, degfieldd %in% x$degfieldd)

### Create rankings for major
library(plyr)
all_top100 <- ddply(all_top100, "stateicp", transform, rank = rank(-percent_noNA_state,ties.method ="random"))
all_top100 <- ddply(all_top100, "stateicp", transform, rank_unusual = rank(-major_ratio,ties.method ="random"))
detach("package:plyr", unload=TRUE)
library(dplyr)

### Creat ratio and round ratio for presentation
all_top100$major_ratio <- all_top100$percent_noNA_state/all_top100$percent_noNA
all_top100$Ratio <- round(all_top100$major_ratio, 1)

### Create datasets necessary for google visualization
x1 <- all_top100 %>%
  filter(rank_unusual == 1) %>%
  group_by(stateicp) %>%
  mutate(statemajor = paste(stateicp,": ", degfieldd, sep=""),
         description = paste(degfieldd,": ", roundedratio, sep="")) %>%
  select(stateicp, degfieldd,  major_ratio, rank_unusual, Ratio, statemajor)
x5 <- all_top100 %>%
  filter(rank_unusual < 6) %>%
  select(stateicp, degfieldd,  roundedratio, rank_unusual, Ratio) %>%
  group_by(stateicp) %>%
  mutate(description = paste(degfieldd,": ", roundedratio, sep="")) %>%
  ungroup() %>%
  arrange(stateicp, rank_unusual)

### Create google vis chart
names(x1)[1] <- "State"
x1$State <- as.character(x1$State)
x1$degfieldd <- as.character(x1$degfieldd)
G <- gvisGeoChart(x1, locationvar = "State", colorvar = "Ratio", hovervar = "degfieldd",
                  options=list(region="US", 
                               displayMode="regions", 
                               resolution="provinces",
                               width=600, height=400))

### Copy and paste this to get chart into wordpress
print(G, tag='chart')

### Create google vis table
names(x5)[1] <- "State"
x5$State <- as.character(x5$State)
x5$degfieldd <- as.character(x5$degfieldd)
x5_small <- x5 %>%
  rename(Major = degfieldd,
         Rank = rank_unusual) %>%
  select(State, Major, Ratio) %>%
  arrange(-Ratio)
str(x5_small)
x5_small$State <- str_trim(x5_small$State, side = "both")
x5_small$Major <- str_trim(x5_small$Major, side = "both")
chart_3 <- gvisTable(data = x5_small, options=list(width = 600))

### Copy and paste this to put table in wordpress
print(chart_3, "chart")

### Examples of checking for estimates using survey
library(survey)
acs2$strata <- as.factor(acs2$strata)
acs2$perwt <- as.numeric(acs2$perwt)

### Look at a particular major, in this example Environmental Science
acs2$major <- ifelse(acs2$DEGFIELDD == "Environmental Science", 1, 0)
acs2$major[is.na(acs2$major)] <- 0
summary(acs2$major)

### Look at a particular state verus national sample.
### I have to sample here because survey cannot handle the entire population
state <- filter(acs2, stateicp == "Vermont", degfieldd != "N/A", ageN > 22)
national <- acs2 %>%
  filter(degfieldd != "N/A", ageN > 22) %>%
  sample_n(500000)

### Establish survey design
state.design <- svydesign(id=~serial, weights=~perwt, data=state)
national.design <- svydesign(id=~serial, weights=~perwt, data=national)

### Get estimates from survey
state_estimate <- svymean(~major, state.design, data=state, na.rm=TRUE)
national_estimate <- svymean(~major, national.design, data=national, na.rm=TRUE)
state_estimate[1]/national_estimate[1]

### Get quick and dirty estimate
### In this case, the difference is enough to make the ration move from slightly above 6 to above 7
xx <- sum(state$perwt[state$major == 1], na.rm = TRUE)/sum(state$perwt)
yy <- sum(national$perwt[national$major == 1], na.rm = TRUE)/sum(national$perwt)
xx/yy

### If we do the same for Ohio and Arts and Music Education.
acs2$major <- ifelse(acs2$DEGFIELDD == "Art and Music Education", 1, 0)
acs2$major[is.na(acs2$major)] <- 0
summary(acs2$major)
state <- filter(acs2, stateicp == "Ohio", degfieldd != "N/A", ageN > 22)
national <- acs2 %>%
  filter(degfieldd != "N/A", ageN > 22) %>%
  sample_n(500000)
state.design <- svydesign(id=~serial, weights=~perwt, data=state)
national.design <- svydesign(id=~serial, weights=~perwt, data=national)
state_estimate <- svymean(~major, state.design, data=state, na.rm=TRUE)
national_estimate <- svymean(~major, national.design, data=national, na.rm=TRUE)
xx <- sum(state$perwt[state$major == 1], na.rm = TRUE)/sum(state$perwt)
yy <- sum(national$perwt[national$major == 1], na.rm = TRUE)/sum(national$perwt)
state_estimate[1]/national_estimate[1]
xx/yy

### Function for getting the distribution of the ratio of two normals
ratio2normals <- function(x, mean1,mean2,sd1,sd2,rho){
  # A function to compute ratio of 2 normals
  # R code written by Ravi Varadhan 
  # May 25, 2007
  # Based on the paper by Pham Gia et al., Comm in Stats (2006)
  A <- 1 / (2*pi*sd1*sd2*sqrt(1-rho^2))
  exponent.num <- -sd2^2*mean1^2 - sd1^2*mean2^2 + 2*rho*sd1*sd2*mean1*mean2
  exponent.denom <- 2*(1-rho^2)*sd1^2*sd2^2
  K <- A * exp(exponent.num/exponent.denom)
  t2x.num <- -sd2^2*mean1*x - sd1^2*mean2 + rho*sd1*sd2*(mean2*x + mean1)
  t2x.denom <- sd1*sd2*sqrt(2*(1-rho^2)*(sd2^2*x^2 - 2*rho*x*sd1*sd2 + sd1^2))
  t2x <- t2x.num / t2x.denom
  erf.term <- 2 * pnorm(sqrt(2) * t2x) - 1
  Ft2x <- sqrt(pi) * t2x * exp(t2x^2) * erf.term + 1
  fx <- K * Ft2x * 2 * (1 - rho^2) * sd1^2 * sd2^2 / (sd2^2 * x^2 + sd1^2 - 2*x*rho*sd1*sd2)
  return(fx)
}

#### Input an example.  
### Here we use the data for Environmental from Vermont to National
mean1 <- 0.0061747
mean2 <- 0.00093748
sd1 <- 0.0013
sd2 <- .0001
rho <- 0

### Look at the distribution visually
x <- seq(1,10, length=100)
y <- ratio2normals(x,mean1,mean2, sd1,sd2,rho)
plot(x,y, type="l")


#### Code taken from Ravi Varadhan to calculate errors
m1 <- function(x, mean1, mean2, sd1, sd2, rho){
  x * ratio2normals(x, mean1, mean2, sd1, sd2, rho)
}
m2 <- function(x, mean1, mean2, sd1, sd2, rho){
  x^2 * ratio2normals(x, mean1, mean2, sd1, sd2, rho)
}
m.1 <- integrate(m1, lower=-Inf, upper=Inf, mean1=mean1, mean2=mean2, sd1=sd1, sd2=sd2, rho=rho, rel.tol=1.e-07)$val
m.2 <- integrate(m2, lower=-Inf, upper=Inf, mean1=mean1, mean2=mean2, sd1=sd1, sd2=sd2, rho=rho, rel.tol=1.e-07)$val

### Get the mean ratio and standard error using the ratio distribution and
mean <- m.1
mean
variance <- m.2 - m.1^2
sqrt(variance)
