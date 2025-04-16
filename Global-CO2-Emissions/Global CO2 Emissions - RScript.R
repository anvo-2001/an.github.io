#### Set working directory and Load necessary libraries
setwd("C:/Users/PC - MSI/OneDrive - The University of Liverpool/An - 2025/2024 - UoL/Sem 1/ECON 705 Data Mgt/Exercise/FINAL")
library(readxl)
library(writexl)
library(tidyverse)
library(ggplot2)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(ggpubr)    # For stat_cor()

# =========================
# DATA PREPROCESSING
# =========================

# Import required Excel sheets
rawdata_co2_emissions <- read_excel("ECON705_Final_Exam_Data.xlsx", sheet = "annual-co2-emissions")
rawdata_co2_emissions_capita <- read_excel("ECON705_Final_Exam_Data.xlsx", sheet = "co-emissions-per-capita")
rawdata_consumption_co2_gdp <- read_excel("ECON705_Final_Exam_Data.xlsx", sheet = "consumption-co2-per-capita-vs-g")
rawdata_continents <- read_excel("ECON705_Final_Exam_Data.xlsx", sheet = "continents-according-to-our-wor")

# Merge sheets
data_merged <- left_join(rawdata_co2_emissions, rawdata_co2_emissions_capita, by = c("Entity","Year")) %>%
  left_join(rawdata_consumption_co2_gdp, by = c("Entity","Year")) %>%
  rename (
    CO2_emissions = "Annual CO2 emissions",
    CO2_emissions_capita = "Annual CO2 emissions per capita",
    Consumption_CO2_emissions_capita = "Per capita consumption-based CO2 emissions",
    GDP_capita = "GDP per capita, PPP (constant 2017 international $)",
    Population = "Population (historical)"
  ) %>%
  select(Entity, Code, Year,CO2_emissions, CO2_emissions_capita
         ,Consumption_CO2_emissions_capita,GDP_capita, Population)

# Subset and prepare main data
main_data <- data_merged %>%
  filter(Year >= 1972 & Year <= 2022) %>%
  mutate(CO2_emissions_tons = CO2_emissions / 1e9)

# rm(data_merged,rawdata_co2_emissions,rawdata_co2_emissions_captia,rawdata_consumption_co2_gdp)
# write_xlsx(data_merged, path = "Data_Merged.xlsx")


# =========================
# DATA ANALYSIS
# =========================

# Calculate percentage change in emissions
main_data <- main_data %>% 
  group_by(Entity) %>%
  arrange(Entity, Year) %>%
  mutate(Pct_change_emissions = (CO2_emissions - lag(CO2_emissions)) / lag(CO2_emissions) * 100) %>%
  ungroup()
# Extract World data
world_data <- main_data %>%
  filter(Entity == "World") %>%
  arrange(Year)

# =========================
# FIGURE 1: Global Emissions Trend
ggplot(world_data, aes(x = Year)) +
  geom_rect(aes(xmin = 1973.5, xmax = 1976, ymin = -Inf, ymax = Inf), fill = "#f0f0f0", alpha = 0.3) +
  geom_rect(aes(xmin = 1979.5, xmax = 1982, ymin = -Inf, ymax = Inf), fill = "#f0f0f0", alpha = 0.3) +
  geom_rect(aes(xmin = 1990, xmax = 1992.5, ymin = -Inf, ymax = Inf), fill = "#f0f0f0", alpha = 0.3) +
  geom_rect(aes(xmin = 2007.5, xmax = 2009.5, ymin = -Inf, ymax = Inf), fill = "#f0f0f0", alpha = 0.3) +
  geom_rect(aes(xmin = 2018.5, xmax = 2021, ymin = -Inf, ymax = Inf), fill = "#f0f0f0", alpha = 0.3) +
  geom_line(aes(y = CO2_emissions_tons), color = "skyblue", size = 1.5) +
  geom_bar(aes(y = Pct_change_emissions), stat = "identity", fill = "darkblue", alpha = 0.7, color = "white") +
  scale_x_continuous(breaks = seq(1972, 2022, 5)) +
  scale_y_continuous(
    name = "CO2 Emissions (billion tons)",
    sec.axis = sec_axis(~ ., name = "% Change in Emissions"),
    breaks = seq(0, 40, 5)
  ) +
  labs(x = "Year") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

# =========================
# FIGURE 2: Top Emitters 2022

# Data for Figure 2
regions <- c(
  "China" = "#FFAA00",  # Bright orange for visibility and prominence
  "Asia (excl. China and India)" = "#32ca32",  # Soft indigo for distinctiveness
  "United States" = "#4682B4",  # Steel blue for clarity
  "India" = "#FF6347",  # Tomato red for vibrancy
  "European Union (27)" = "darkblue",  # Dodger blue for freshness
  "Europe (excl. EU-27)" = "#87CEEB",  # Sky blue for contrast with EU-27
  "Africa" = "#6A5ACD",  # Lime green for energy
  "North America (excl. USA)" = "pink"  # Hot pink for uniqueness
)

data_2022 <- main_data %>% 
  filter(Year == 2022 & (!is.na(Code))) %>%
  rename("iso_a3" = Code)

data2 <- main_data %>% filter(Year==2022) %>%
  filter(Entity %in% names(regions)) %>%  # Exclude "World"
  arrange(desc(CO2_emissions_tons)) %>%
  select(Entity,CO2_emissions_tons)%>%
  head(5)

total_emissions <- data_2022$CO2_emissions_tons[data_2022$Entity == "World"]
rest_of_world_emissions <- total_emissions - sum(data2$CO2_emissions_tons)
data2 <- data2 %>%
  add_row(
    Entity = "Rest of the World",
    CO2_emissions_tons = rest_of_world_emissions) %>%
  arrange(desc(CO2_emissions_tons)) %>%
  mutate(Percentage_of_Total = (CO2_emissions_tons / total_emissions) * 100,
    Label = paste0(round(CO2_emissions_tons, 2), " (", round(Percentage_of_Total, 1), "%)"),
    BarColor = c("#FFAA00","black", "#32ca32","#4682B4","#FF6347", "darkblue")
  )

# Create the bar chart
ggplot(data2, aes(x = reorder(Entity, CO2_emissions_tons), y = CO2_emissions_tons, fill = BarColor)) +
  geom_bar(stat = "identity", alpha = 0.8,size=3) +
  geom_text(
    aes(label = Label),
    hjust = 1.2,
    color = "white",
    size = 4
  ) +
  labs(
    title = "CO2 Emissions by Country (2022)",
    x = NULL,
    y = "CO2 (billion tons)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Add space on the right for labels
  scale_fill_identity() +  # Use colors as defined in the dataset
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  coord_flip()


# =========================
# FIGURE 3: Emissions per Capita Map

world_map <- ne_countries(scale = "medium", returnclass = "sf")  # Load world map data
world_map <- left_join(world_map, data_2022, by = c("iso_a3" = "iso_a3"))

# Plot the map with updated palette
ggplot(data = world_map) +
  geom_sf(aes(fill = CO2_emissions_capita), color = "white", size = 0.2) +
  scale_fill_distiller(
    name = "CO2 Emissions\nper capita (tons)",
    palette = "YlGnBu", 
    trans = "log",  # Apply log scale for better differentiation
    na.value = "grey90",  # Color for missing data
    direction = 1 
  ) +
  labs(
    title = "World Map of CO2 Emissions per Capita (2022)",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# =========================
# FIGURE 4: Cumulative Emissions Over Time

data4 <- main_data %>%
  filter(Entity %in% names(regions)) %>%
  select(Entity, Code, Year, CO2_emissions_tons, CO2_emissions_capita,Pct_change_emissions) %>%
  group_by(Entity) %>%
  arrange(Year) %>%  # Ensure data is sorted by year for cumulative calculation
  mutate(Cumulative_CO2_emissions = cumsum(CO2_emissions_tons)) %>%  # Calculate cumulative sum
  ungroup() 

order <- data4 %>%
  filter(Year == 2022) %>%
  arrange(desc(Cumulative_CO2_emissions)) %>% 
  pull(Entity)
# Reorder the Entity factor based on 2022 values
data4 <- data4 %>%
  mutate(Entity = factor(Entity, levels = order))

# Determine the final values (2022) for labeling
label_positions <- data4 %>%
  filter(Year == 2022) %>%
  arrange(desc(Cumulative_CO2_emissions)) 

# Plot the area chart
ggplot(data4, aes(x = Year, y = Cumulative_CO2_emissions, fill = Entity)) +
  geom_area(alpha = 0.8) +  # Add transparency to areas
  scale_fill_manual(values = regions) +  # Use the predefined region colors
  labs(
    title = "Cumulative CO2 Emissions by Region Over Time",
    x = "Year",
    y = "Cumulative CO2 Emissions (billion tons)",
    fill = NULL  # Remove the default legend title
  ) +
  scale_x_continuous(breaks = seq(1972, 2022, by = 5)) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )
