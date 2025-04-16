#### Set working directory and Load necessary libraries
setwd("C:/Users/PC - MSI/OneDrive - The University of Liverpool/An - 2025/2024 - UoL/Sem 1/ECON 705 Data Mgt/Exercise/Assignment1/Assignment1_Data")
library(readxl)
library(writexl)
library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)

# =========================
# DATA PREPROCESSING
# =========================

# Import required Excel sheets
raw_house_price_region = read_excel("house_price_earnings.xlsx", sheet= "1a", skip=1, na = ":")
raw_earnings_region = read_excel("house_price_earnings.xlsx", sheet= "1b", skip=1, na = ":")
raw_house_price_district = read_excel("house_price_earnings.xlsx", sheet= "5a", skip=1, na = ":")
raw_earnings_district = read_excel("house_price_earnings.xlsx", sheet= "5b", skip=1, na = ":")
missing_local_data = read_excel("Local_Authorities_Missing_Data.xlsx")
housing_supply = read_excel("Data_Housing_Supply_England_2022.xlsx", skip=2)
uk_map <- st_read("Local_Authority_Districts_May_2024_Boundaries_UK_BFE/LAD_MAY_2024_UK_BFE.shp")

# FUNCTION TO CLEAN LONG FORMAT
clean_long_format <- function(data, col_range, value_name) {
  data %>%
    pivot_longer(cols = all_of(col_range), names_to = "Year", values_to = value_name) %>%
    mutate(
      Year = as.numeric(gsub("[^0-9]", "", Year)),
      !!value_name := as.numeric(.data[[value_name]])
    )
}

# Tranform Data
house_price_region <- clean_long_format(raw_house_price_region, starts_with("Year"), "Median_House_Price")
earnings_region <- clean_long_format(raw_earnings_region, names(raw_earnings_region)[3:28], "Median_Earnings")
house_price_district <- clean_long_format(raw_house_price_district, starts_with("Year"), "Median_House_Price")
earnings_district <- clean_long_format(raw_earnings_district, names(raw_earnings_district)[5:30], "Median_Earnings")

# rm(raw_house_price_region,raw_earnings_region,raw_house_price_district,raw_earnings_district)

# Impute missing earning data with median of the region in that year
earnings_district <- earnings_district %>%
  group_by(`Region code`, Year) %>%
  mutate(Median_Earnings = ifelse(is.na(Median_Earnings), median(Median_Earnings, na.rm = TRUE), Median_Earnings)) %>%
  ungroup()

# Merge and clean region data
region_data <- left_join(house_price_region, earnings_region, by = c("Code", "Year", "Name")) %>%
  mutate(
    Affordability_Ratio = Median_House_Price / Median_Earnings,
    Region_Code = Code,
    Region_Name = Name
  ) %>%
  filter(!Region_Name %in% c("England and Wales", "Wales"))

# Merge and clean district data
district_data <- left_join(house_price_district, earnings_district, 
                           by = c("Local authority code" = "Code", "Local authority name" = "Name", "Year")) %>%
  select(-`Region code.y`, -`Region name.y`) %>%
  mutate(
    Affordability_Ratio = Median_House_Price / Median_Earnings,
    Region_Code = `Region code.x`,
    Region_Name = `Region name.x`,
    Local_Authority_Code = `Local authority code`,
    Local_Authority_Name = `Local authority name`
  ) %>%
  filter(!Region_Name %in% c("England and Wales", "Wales")) %>%
  bind_rows(missing_local_data)

## Save two tables to 2 Excel sheets
# output_data <- list(
#   "Region_Data" = region_data_cleaned,
#   "Local_District_Data" = local_district_data_cleaned
# )
# write_xlsx(output_data, path = "Exercise/Assignment1/Assignment1_Data/Data_Cleaned.xlsx")

# =========================
# DATA ANALYSIS
# =========================

# =========================
# FIGURE 1. Affordability ratio over time for England
region_data %>%
  filter(Region_Name == 'England') %>%
  ggplot( aes(x = Year, y = Affordability_Ratio)) +
  # Highlight periods
  geom_rect(aes(xmin = 1997, xmax = 2008, ymin = -Inf, ymax = Inf), 
            fill = "#f0f0f0", alpha = 0.3, data = NULL) +
  geom_rect(aes(xmin = 2008, xmax = 2013, ymin = -Inf, ymax = Inf), 
            fill = "#e0ffff", alpha = 0.2, data = NULL) +
  geom_rect(aes(xmin = 2013, xmax = 2022, ymin = -Inf, ymax = Inf), 
            fill = "#fffaf0", alpha = 0.3, data = NULL) +  
  geom_line(color = "darkblue", linewidth = 1) +  # Affordability Ratio line
  # Add threshold line
  geom_hline(yintercept = 5, color = "darkblue", linetype = "dashed", linewidth = 0.8) +  # Set x-axis to show each year
  scale_x_continuous(breaks = seq(1998, 2022, by = 2)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_point(data = subset(region_data, Region_Name == 'England' & Year %in% c(2002,2009, 2021)),
             aes(x = Year, y = Affordability_Ratio), color = "red", size = 2) +
  labs(title = "Affordability Ratio Over Time in England") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
  )

# =========================
# FIGURE 2. House prices, earnings over time for England
region_data[region_data$Region_Name == 'England',] %>%
  ggplot() +
  geom_line(aes(x = Year, y = Median_House_Price / 1000, color = "House Price (\u00a3000s)"), linewidth = 1) +
  geom_line(aes(x = Year, y = 5 * Median_Earnings / 1000, color = "5-Year Earnings (\u00a3000s)"), linewidth = 1) +
  geom_point(data = subset(region_data, Region_Name == 'England' & Year %in% c(2008, 2021)), 
             aes(x = Year, y = Median_House_Price / 1000), color = "darkblue", size = 3) +
  geom_point(data = subset(region_data, Region_Name == 'England' & Year %in% c(2008, 2021)), 
             aes(x = Year, y = 5 * Median_Earnings / 1000), color = "#56B4E9", size = 3) +
  geom_text(data = subset(region_data, Region_Name == 'England' & Year %in% c(2008, 2021)),
            aes(x = Year, y = Median_House_Price / 1000, label = round(Median_House_Price / 1000, 1)),
            vjust = -1, color = "darkblue", size = 3) +
  geom_text(data = subset(region_data, Region_Name == 'England' & Year %in% c(2008, 2021)),
            aes(x = Year, y = 5 * Median_Earnings / 1000, label = round(5 * Median_Earnings / 1000, 1)),
            vjust = 1.5, color = "#56B4E9", size = 3) +
  scale_color_manual(values = c("House Price (\u00a3000s)" = "darkblue", 
                                "5-Year Earnings (\u00a3000s)" = "#56B4E9")) +
  scale_x_continuous(breaks = seq(1998, 2022, by = 2)) +
  scale_y_continuous(limits = c(0, 300)) +
  labs(title = "House Prices, Earnings in England",
       x = "Year",
       y = "Amount (\u00a3000s)") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )


## 2. Regional Analysis for 2022
## Filter data 2022
district_data_2022 <- filter(district_data,Year == 2022)%>%
  group_by(Region_Name) %>%
  mutate(
    Median_Value = median(Affordability_Ratio),
  )

# =========================
## FIGURE 3. Range of Housing Affordability Ratio by Region - 2022
district_summary_data_2022 <- district_data_2022 %>%
  group_by(Region_Name) %>%
  summarize(
    min_value = min(Affordability_Ratio, na.rm = TRUE),
    max_value = max(Affordability_Ratio, na.rm = TRUE),
    median_value = median(Affordability_Ratio, na.rm = TRUE),
    min_district = Local_Authority_Name[which.min(Affordability_Ratio)],
    max_district = Local_Authority_Name[which.max(Affordability_Ratio)],
  )
# Plot
ggplot(district_summary_data_2022, aes(y = reorder(Region_Name, median_value))) +
  geom_linerange(aes(xmin = min_value, xmax = max_value), color = "darkblue", linewidth = 7) +
  geom_point(aes(x = median_value), color = "skyblue", size = 3) +
  geom_text(aes(x = min_value, label = min_district), hjust = 1.1, color = "darkblue", size = 4) +
  geom_text(aes(x = max_value, label = max_district), hjust = -0.1, color = "darkblue", size = 4) +
  labs(
    title = "Range of Housing Affordability Ratio by Region (Min to Max) - 2022",
    x = "Affordability Ratio",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(), 
    axis.text.y = element_text(size = 12),
  ) +
  xlim(-5,48) 

# =========================
# TABLE 1. Top 10 affordable
top_10_affordable <- district_data_2022 %>%
  arrange(Affordability_Ratio) %>%
  select(Local_Authority_Name, Region_Name, Affordability_Ratio) %>%
  head(10)
# TABLE 2. Top 10 unaffordable
top_10_unaffordable <- district_data_2022 %>%
  arrange(desc(Affordability_Ratio)) %>%
  select(Local_Authority_Name, Region_Name, Affordability_Ratio) %>%
  head(10)

# =========================
## FIGURE 4. Net additional dwellings over time
ggplot(housing_supply, aes(x = Year, y = Net_Additional_Dwellings/1000)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  # Add threshold line at 300,000
  geom_hline(yintercept = 300, color = "red", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(1998, 2022, by = 2)) +
  # Labels and theme
  labs(
    title = "Net Additional Dwellings in England by Year",
    x = "Year",
    y = "(000s)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# =========================
### APPENDIX
## Appendix A. Divergence between House prices and 5-year earnings by region
region_data %>% filter(!Region_Name == 'England') %>%
  ggplot() +
  geom_line(aes(x = Year, y = Median_House_Price / 1000, color = "House Price (\u00a3000s)"), linewidth = 1) +
  geom_line(aes(x = Year, y = 5 * Median_Earnings / 1000, color = "5-year Earnings (\u00a3000s)"), linewidth = 1) +
  scale_color_manual(values = c("House Price (\u00a3000s)" = "darkblue", 
                                "5-year Earnings (\u00a3000s)" = "#56B4E9")) +
  
  # Facet by region name to create a separate plot for each region
  facet_wrap(~ Region_Name, ncol=3)+
  labs(title = "House prices and 5-year earnings by region",
       x = "Year",
       y = "Amount (\u00a3000s)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
  )

## Appendix B. Affordability Ratio by Local Authorities

# Merge spatial data with affordability data by local authority code
map_data_2022 <- uk_map %>%
  left_join(district_data_2022, by = c("LAD24CD" = "Local_Authority_Code")) %>%
  filter(!is.na(Affordability_Ratio) & Year == 2022)

# Spatial map
tm_shape(map_data_2022) +
  tm_fill(
    col = "Affordability_Ratio",
    palette = "YlGnBu",
    title = "Affordability Ratio",
    style = "fixed",
    breaks = c(0, 5, 8, 12, 24, 44),
    popup.vars = c(
      "Region" = "Region_Name",
      "Affordability Ratio" = "Affordability_Ratio"
    )
  ) +
  tm_borders(col = "#2F4F4F") +
  tm_layout(
    title = "Affordability Ratio by Region (2022)",
    legend.outside = TRUE,
    legend.outside.size = 0.3
  )
