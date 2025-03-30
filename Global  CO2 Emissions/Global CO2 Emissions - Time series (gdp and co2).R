setwd("C:/Users/PC - MSI/OneDrive - The University of Liverpool/An - 2025/2024 - UoL/Sem 1/ECON 705 Data Mgt/Exercise/FINAL")
library(readxl)
library(writexl)
library(tidyverse)
library(ggplot2)
library(tseries)
library(urca) # for cointegration test
library(forecast) # for auto.arima
library(vars)

# Import data
rawdata_co2_emissions = read_excel("ECON705_Final_Exam_Data.xlsx", sheet= "annual-co2-emissions")
rawdata_co2_emissions_captia = read_excel("ECON705_Final_Exam_Data.xlsx", sheet= "co-emissions-per-capita")
rawdata_consumption_co2_gdp = read_excel("ECON705_Final_Exam_Data.xlsx", sheet= "consumption-co2-per-capita-vs-g")
rawdata_continents = read_excel("ECON705_Final_Exam_Data.xlsx", sheet= "continents-according-to-our-wor")

# Merge 3 sheets to 1 data table
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

# Subset data 
data <- raw_data %>% 
  filter(Year >= 1990 & Entity == 'World') %>%
  mutate(GDP = GDP_capita*Population, CO2_emissions=CO2_emissions/10^6
         ,lgdp = log(GDP)
         ,lco2 = log(CO2_emissions))
data <- data[complete.cases(data),] # Remove the obs with missing values

# Step 1: Pre-estimation Checks (Visualize and check non-stationarity)
ggplot(data) +
  geom_point(aes(x = lgdp, y = lco2)) +
  theme_minimal() +
  labs(title = "Scatter plot of Log GDP and Log CO2 Emissions", 
       x = "log(GDP)", y = "log(CO2Emissions)") 
# Unit root test
# Augmented Dickey-Fuller test for GDP and CO2Emissions
adf_gdp <- adf.test(data$lgdp)
adf_co2 <- adf.test(data$lco2)

# Output the results
adf_gdp
adf_co2

# Cointegration Test
# Since both log gdp and log co2 emissions are found to be nonstationary
# we need to test whether they are cointegrated before proceeding to 
# proper time seires modelling

# Combine lgdp and lco2 into a matrix
gdp_co2_matrix <- cbind(data$lgdp, data$lco2)

# Set column names explicitly
colnames(gdp_co2_matrix) <- c("lgdp", "lco2")

coint_test <- ca.jo(gdp_co2_matrix, type = "trace", ecdet = "const", K = 2)
summary(coint_test)

# Given that the test does not support the presence of cointegration, 
# you should not use cointegration regression methods for these data. 
# Instead, you might consider other approaches, such as: 
# Transforming the data to achieve stationarity (e.g., differencing) and then using standard time series models.

## 2. Modelling the log difference of GDP on log difference of CO2 Emissions

# Calculate the differences of the logs
data <- data %>%
  mutate(
    diff_lgdp = c(NA, diff(lgdp)),
    diff_lco2 = c(NA, diff(lco2))
  )

# Remove NA values that result from differencing
data <- na.omit(data)

# Time series plot for the log differences data
ggplot(data, aes(x = Year)) +
  geom_line(aes(y = diff_lgdp, colour = "Log Difference of GDP")) +
  geom_line(aes(y = diff_lco2, colour = "Log Difference of CO2 Emissions")) +
  theme_minimal() +
  labs(title = "Log Differences of GDP and CO2 Emissions",
       x = "Year", y = "Log Difference") +
  scale_colour_manual(values = c("Log Difference of GDP" = "blue", "Log Difference of CO2 Emissions" = "red"))

# Fit a basic ARMA model
# Automatic ARIMA modeling
auto_model <- auto.arima(data$diff_lco2, xreg = data$diff_lgdp, allowmean = TRUE, ic = "aic")

# View the model summary
summary(auto_model)

## 3. Post-estimation diagnostics

# Extract residuals
residuals <- residuals(auto_model)
squared_residuals <- residuals^2
# Basic plot of residuals
plot(residuals, main = "Residuals of ARMA Model", ylab = "Residuals")
abline(h = 0, col = "red")

# ACF and PACF plots of residuals
# The ACF and PACF plots are employed to check if 
# there is any autocorrelation left in the residuals
# which would imply that the model has not fully captured the data's structure

acf(residuals, main = "ACF of Residuals")
pacf(residuals, main = "PACF of Residuals")

# ACF and PACF plots of squared residuals
# If the squared residuals exhibit patterns or significant autocorrelations (above the blue error bounds), 
# it might indicate heteroskedasticity suggesting that the variance of the residuals changes over time. 
# In such cases, models that account for changing variance (like GARCH) could be considered.
acf(squared_residuals, main = "ACF of Squared Residuals")
pacf(squared_residuals, main = "PACF of Squared Residuals")

# The acf and pacf on both residuals and squared residuals show no significant autocorrelations, 
# indicating that the current arma model can explain the relationship between the percentage change of GDP
# and the percentage change of CO2 emissions adequately. 
