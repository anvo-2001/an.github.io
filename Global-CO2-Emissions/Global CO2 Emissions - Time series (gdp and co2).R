setwd("C:/Users/PC - MSI/OneDrive - The University of Liverpool/An - 2025/2024 - UoL/Sem 1/ECON 705 Data Mgt/Exercise/FINAL")
library(readxl)
library(writexl)
library(tidyverse)
library(ggplot2)
library(tseries)
library(urca) # for cointegration test
library(forecast) # for auto.arima
library(vars)

# ======================
# 1. IMPORT & PREP DATA
# ======================

# Import data from Excel
rawdata_co2_emissions <- read_excel("ECON705_Final_Exam_Data.xlsx", sheet= "annual-co2-emissions")
rawdata_co2_emissions_captia <- read_excel("ECON705_Final_Exam_Data.xlsx", sheet= "co-emissions-per-capita")
rawdata_consumption_co2_gdp <- read_excel("ECON705_Final_Exam_Data.xlsx", sheet= "consumption-co2-per-capita-vs-g")
rawdata_continents <- read_excel("ECON705_Final_Exam_Data.xlsx", sheet= "continents-according-to-our-wor")

# Merge data
raw_data <- left_join(rawdata_co2_emissions, rawdata_co2_emissions_captia, by = c("Entity","Year")) %>%
  left_join(rawdata_consumption_co2_gdp, by = c("Entity","Year")) %>%
  rename (
    CO2_emissions = "Annual CO2 emissions",
    CO2_emissions_capita = "Annual CO2 emissions per capita",
    Consumption_CO2_emissions_capita = "Per capita consumption-based CO2 emissions",
    GDP_capita = "GDP per capita, PPP (constant 2017 international $)",
    Population = "Population (historical)"
  ) %>%
  dplyr::select(Entity, Code, Year,CO2_emissions, CO2_emissions_capita
         ,Consumption_CO2_emissions_capita,GDP_capita, Population)

# Subset: World data from 1990 onwards
data <- raw_data %>%
  filter(Year >= 1990, Entity == "World") %>%
  mutate(
    GDP = GDP_capita * Population,
    CO2_emissions = CO2_emissions / 10^6,  # Convert to million tonnes
    lgdp = log(GDP),
    lco2 = log(CO2_emissions)
  ) %>%
  drop_na()

# ============================
# 2. PRE-ESTIMATION CHECKS
# ============================

# Scatterplot of log(GDP) vs log(CO2)
ggplot(data) +
  geom_point(aes(x = lgdp, y = lco2)) +
  theme_minimal() +
  labs(title = "Scatter plot of Log GDP and Log CO2 Emissions", 
       x = "log(GDP)", y = "log(CO2Emissions)") 

# ADF Test for Stationarity
adf_gdp <- adf.test(data$lgdp)
adf_co2 <- adf.test(data$lco2)

adf_gdp
adf_co2

# Since both log gdp and log co2 emissions are found to be nonstationary
# we need Cointegration test before proceeding to proper time series modelling

# Johansen Cointegration Test
gdp_co2_matrix <- cbind(data$lgdp, data$lco2)
colnames(gdp_co2_matrix) <- c("lgdp", "lco2")
coint_test <- ca.jo(gdp_co2_matrix, type = "trace", ecdet = "const", K = 2)
summary(coint_test)

# No cointegration → transform data to achieve stationarity, first-differencing

# First differences of logs
data <- data %>%
  mutate(
    diff_lgdp = c(NA, diff(lgdp)),
    diff_lco2 = c(NA, diff(lco2))
  )%>%
  drop_na()

# Line plot of differenced series
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = diff_lgdp, colour = "Log Difference of GDP")) +
  geom_line(aes(y = diff_lco2, colour = "Log Difference of CO2 Emissions")) +
  theme_minimal() +
  labs(title = "Log Differences of GDP and CO2 Emissions",
       x = "Year", y = "Log Difference") +
  scale_colour_manual(values = c("Log Difference of GDP" = "blue", "Log Difference of CO2 Emissions" = "red"))

# ========================
# 3. ARIMA MODEL FITTING
# ========================
auto_model <- auto.arima(data$diff_lco2, xreg = data$diff_lgdp, allowmean = TRUE, ic = "aic")
summary(auto_model)

# ===========================
# 4. POST-ESTIMATION DIAGNOSTICS
# ===========================

# Residuals and squared residuals
residuals <- residuals(auto_model)
squared_residuals <- residuals^2
# Plot residuals
plot(residuals, main = "Residuals of ARMA Model", ylab = "Residuals")
abline(h = 0, col = "red")

# ACF/PACF of residuals
acf(residuals, main = "ACF of Residuals")
pacf(residuals, main = "PACF of Residuals")

# ACF/PACF of squared residuals
# If the squared residuals exhibit patterns or significant autocorrelations, 
# it might indicate heteroskedasticity suggesting that the variance of the residuals changes over time. 
# In such cases, models that account for changing variance (like GARCH) could be considered.
acf(squared_residuals, main = "ACF of Squared Residuals")
pacf(squared_residuals, main = "PACF of Squared Residuals")

# ===========================
# CONCLUSION
# ===========================
# The absence of significant autocorrelation in both residuals and squared residuals
# suggests that the current ARIMA model adequately captures the dynamics
# between percentage changes in GDP and CO2 emissions.
