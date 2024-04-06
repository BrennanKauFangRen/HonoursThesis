# Brennan Kau _ Honours Thesis _ Code

##### PRE-PROCESSING

library(readxl)
library(dplyr)
library(ggplot2)
library(plm)
library(stats)
library(ExPanDaR)

##### 1. SUMMARY STATISTICS

dat1 <- read_excel("Brennan Kau _ Honours Thesis _ Submission 25.2.xlsx", sheet = 1)

## 1a. Observations in the dataset

nrow(dat1) # 2845 observations

## 1b. Countries in the dataset

n_distinct(dat1$Country) # 190 countries

## 1c. Developing and developed countries

table(dat1$dev) # 585 developed, 2260 developing country observations
summary(dat1$dev) # 79.4% of observations are for developing countries
sd(dat1$dev) # standard deviation 0.404

## 1d. Shareholder protection variable (shp) statistics

# code developing country status as discrete variable
dat1$dev.country = NA
dat1$dev.country[dat1$dev == 0] = "Developed"
dat1$dev.country[dat1$dev == 1] = "Developing"

summary(dat1$shp2) # min 0.0, max 92.0, median 50.0, mean 47.7
sd(dat1$shp2) # standard deviation 19.4

tapply(dat1$shp2, dat1$dev.country, mean, na.rm = TRUE) # show differing means

# create histogram
p1 <- ggplot(dat1, aes(x = shp2)) +
  geom_histogram(aes(fill = dev.country), binwidth = 10, color = "black") +
  stat_bin(binwidth = 10, geom = "text", aes(label = after_stat(if_else(
    count > 0, as.character(count), ""
  )), group = dev), vjust = -.3) +
  ggtitle(label = "Histogram of minority investor protection scores") +
  scale_x_continuous(name = "Minority investor protection score") +
  scale_y_continuous(name = "Count") +
  guides(fill = guide_legend(title = "Status")) +
  theme_bw()

# display histogram
p1

p2 <- ggplot(dat1, aes(x = as.factor(Year), y = shp2, fill = dev.country)) +
  geom_boxplot() +
  geom_smooth(aes(group = dev.country), method = "lm", se = FALSE, color = "black") +
  ggtitle(label = "Progression of minority investor protection scores over time") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Minority investor protection score") +
  guides(fill = guide_legend(title = "Status")) +
  theme_bw()

# display chart
p2

p3 <- ggplot(dat1, aes(x = as.factor(Year), y = shp2, group = Country, colour = Country)) +
  geom_line() +
  ggtitle(label = "Progression of minority investor protection scores over time") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Minority investor protection score") +
  theme_bw() +
  theme(legend.position = "none")

# display chart
p3

## 1e. Other variable tests

summary(dat1$smc)
sd(dat1$smc, na.rm = TRUE)
summary(dat1$weq)
sd(dat1$weq, na.rm = TRUE)
summary(dat1$law)
summary(dat1$ind)
sd(dat1$ind, na.rm = TRUE)
summary(dat1$sho)
sd(dat1$sho, na.rm = TRUE)
summary(dat1$coa)
sd(dat1$coa, na.rm = TRUE)

##### 2. ENDOGENEITY TESTS

dat2 <- read_excel("Brennan Kau _ Honours Thesis _ Submission 25.2.xlsx", sheet = 2)

## 2a. Preliminary observations

nrow(dat2) # 522 observations
n_distinct(dat2$Country) # 36 countries
summary(dat2$dev) # 17.1% of observations for developing countries

## 2b. Run tests

# create panel data frame
p.dat2 <- pdata.frame(dat2, index = c("Country", "Year"))
p.dat2.dpd <- subset(p.dat2, dev == 0)
p.dat2.dpg <- subset(p.dat2, dev == 1)

# create panel data models for feedback loop
e.1 <- plm(shp2 ~ lag(weq), data = p.dat2, model = "within", effect = "twoways")
e.2 <- plm(weq ~ lag(shp2), data = p.dat2, model = "within", effect = "twoways")
e.dpd.1 <- plm(shp2 ~ lag(weq), data = p.dat2.dpd, model = "within", effect = "twoways")
e.dpd.2 <- plm(weq ~ lag(shp2), data = p.dat2.dpd, model = "within", effect = "twoways")
e.dpg.1 <- plm(shp2 ~ lag(weq), data = p.dat2.dpg, model = "within", effect = "twoways")
e.dpg.2 <- plm(weq ~ lag(shp2), data = p.dat2.dpg, model = "within", effect = "twoways")

# summary of results
summary(e.1) # 0.06 (p-value 0.2223)
summary(e.2) # 0.08 (p-value 0.0515)
summary(e.dpd.1) # 0.09 (p-value 0.0953)
summary(e.dpd.2) # 0.14 (p-value 0.0022) **
summary(e.dpg.1) # -0.14 (p-value 0.1265)
summary(e.dpg.2) # -0.22 (p-value 0.0602)

# create panel data models for residual autocorrelation test
e <- plm(shp2 ~ weq, data = p.dat2, model = "within", effect = "twoways")
e.dpd <- plm(shp2 ~ weq, data = p.dat2.dpd, model = "within", effect = "twoways")
e.dpg <- plm(shp2 ~ weq, data = p.dat2.dpg, model = "within", effect = "twoways")

# autocorrelation results
acf(e$residuals, type = "correlation")
acf(e.dpd$residuals, type = "correlation")
acf(e.dpg$residuals, type = "correlation")

# Durbin-Watson results
pdwtest(e) # 0.72 (p-value < 2.2e-16)
pdwtest(e.dpd) # 0.54 (p-value < 2.2e-16)
pdwtest(e.dpg) # 1.54 (p-value 0.01)

dat3 <- read_excel("Brennan Kau _ Honours Thesis _ Submission 25.2.xlsx", sheet = 3)

# preliminary observations
nrow(dat3) # 1062 observations
n_distinct(dat3$Country) # 87 countries
summary(dat3$dev) # 63.1% of observations for developing countries

# create panel data frame
p.dat3 <- pdata.frame(dat3, index = c("Country", "Year"))
p.dat3.dpd <- subset(p.dat3, dev == 0)
p.dat3.dpg <- subset(p.dat3, dev == 1)

# create panel data models for stock market growth
sm <- plm(smc ~ shp2, data = p.dat3, model = "within", effect = "twoways")
sm.dpd <- plm(smc ~ shp2, data = p.dat3.dpd, model = "within", effect = "twoways")
sm.dpg <- plm(smc ~ shp2, data = p.dat3.dpg, model = "within", effect = "twoways")

# summary of results
summary(sm) # -0.09 (p-value 0.7435)
summary(sm.dpd) # -1.56 (p-value 0.0564)
summary(sm.dpg) # 0.36 (p-value 0.1071)

##### 3. MULTIVARIATE MODELS

dat4 <- read_excel("Brennan Kau _ Honours Thesis _ Submission 25.2.xlsx", sheet = 4)

## 3a. Preliminary observations

nrow(dat4) # 452 observations
n_distinct(dat4$Country) # 36 countries
summary(dat4$dev) # 16.6% of observations for developing countries

## 3b. Run tests

# re-code data
p.dat4 <- pdata.frame(dat4, index = c("Country", "Year"))
p.dat4.dpd <- subset(p.dat4, dev == 0)
p.dat4.dpg <- subset(p.dat4, dev == 1)

# create panel data models
model.p <- plm(shp2 ~ weq + law + ind + sho + coa, data = p.dat4, 
                model = "pooling")
model.dpd.p <- plm(shp2 ~ weq + law + ind + sho + coa, data = p.dat4.dpd, 
                    model = "pooling")
model.dpg.p <- plm(shp2 ~ weq + law + ind + sho + coa, data = p.dat4.dpg, 
                    model = "pooling")

# summary of results
summary(model.p)
summary(model.dpd.p)
summary(model.dpg.p)

# create panel data models
model.re <- plm(shp2 ~ weq + law + ind + sho + coa, data = p.dat4, 
             model = "random", effect = "twoways")
model.dpd.re <- plm(shp2 ~ weq + law + ind + sho + coa, data = p.dat4.dpd, 
             model = "random", effect = "twoways")
model.dpg.re <- plm(shp2 ~ weq + ind + sho + coa, data = p.dat4.dpg, 
             model = "random", effect = "twoways")

# summary of results
summary(model.re)
summary(model.dpd.re)
summary(model.dpg.re)

# create panel data models
model.fe <- plm(shp2 ~ weq + sho + coa, data = p.dat4,
                  model = "within", effect = "twoways")
model.dpd.fe <- plm(shp2 ~ weq + sho + coa, data = p.dat4.dpd,
                    model = "within", effect = "twoways")
model.dpg.fe <- plm(shp2 ~ weq + sho + coa, data = p.dat4.dpg,
                    model = "within", effect = "twoways")

# summary of results
summary(model.fe)
summary(model.dpd.fe)
summary(model.dpg.fe)

# tool for exploratory data analysis, opens separate window
ExPanD(p.dat4)
