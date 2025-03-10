#setwd("E:/Capstone-Project") #Set working directory
library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)
library(lmtest)
library(car)
library(caret)
library(readxl)
library(rworldmap)
library(ggcorrplot)

getwd() #Get working directory
setwd("/Users/lipikabania/Documents/Capstone-Project")
trade_data <- read.csv("Impact.csv")
colSums(is.na(trade_data)) #na values in each column
nrow(trade_data) #number of observations

-----------------------------------------------------------------------------------------------------
#SUB-GROUPING
-----------------------------------------------------------------------------------------------------
# High-income subgroups
high_income_europe_na <- c("United Kingdom", "France", "Canada", "Sweden")
high_income_asia_pacific <- c("Singapore", "New Zealand", "Japan")

# Upper-middle-income subgroups
upper_middle_asia <- c("Malaysia", "Thailand", "Indonesia", "Turkiye")
upper_middle_latin_america <- c("Mexico", "Brazil", "Costa Rica")

# Lower-middle-income subgroups
low_middle_asia <- c("Bangladesh", "India", "Pakistan", "Philippines")
low_middle_africa_mena <- c("Morocco", "Kenya", "Egypt, Arab Rep.")

# IG as a categorical variable
trade_data <- trade_data %>%
  mutate(IG = case_when(
    Country.Name %in% high_income_europe_na ~ "High Income Europe Na",
    Country.Name %in% high_income_asia_pacific ~ "High Income Asia Pacific",
    Country.Name %in% upper_middle_asia ~ "Upper Middle Asia",
    Country.Name %in% upper_middle_latin_america ~ "Upper Middle Latin America",
    Country.Name %in% low_middle_asia ~ "Low Middle Asia",
    Country.Name %in% low_middle_africa_mena ~ "Low Middle Africa Mena",
    TRUE ~ "Other"
  ))

-----------------------------------------------------------------------------------------------------
# CONVERT COLUMNS TO NUMERIC
-----------------------------------------------------------------------------------------------------
#Standardize Exchange rate in USD by country
trade_data <- trade_data %>%
  mutate(
    Exchange_rate = as.numeric(Official.exchange.rate..LCU.per.US...period.average...PA.NUS.FCRF.),
    USD_per_LCU = as.numeric(1 / Official.exchange.rate..LCU.per.US...period.average...PA.NUS.FCRF.) ,
    Inflation = as.numeric(Inflation..GDP.deflator..annual.....NY.GDP.DEFL.KD.ZG.),
    FDI_netinflow = as.numeric(Foreign.direct.investment..net.inflows....of.GDP...BX.KLT.DINV.WD.GD.ZS.),
    FDI_netoutflow = as.numeric(Foreign.direct.investment..net.outflows....of.GDP...BM.KLT.DINV.WD.GD.ZS.),
    Exports_USD = as.numeric(Exports.of.goods.and.services....of.GDP...NE.EXP.GNFS.ZS.),
    Imports_USD = as.numeric(Imports.of.goods.and.services....of.GDP...NE.IMP.GNFS.ZS.),
    Net_Balance = Exports_USD - Imports_USD
  ) 

-----------------------------------------------------------------------------------------------------
##DATA WRANGLING
-----------------------------------------------------------------------------------------------------
#Inflation
#Detect outliers and impute missing values
impute_inflation <- function(data, country, income_group) {
    
# Get the years where inflation is missing
  missing_years <- data %>%
    filter(Country.Name == country & is.na(Inflation)) %>%
    pull(Time)
    
  for (year in missing_years) {
   # Get inflation values for the same year & IG
    group_data <- data %>%
      filter(Time == year, IG == income_group, !is.na(Inflation))
      
  # Outlier detection using IQR
    Q1 <- quantile(group_data$Inflation, 0.25, na.rm = TRUE)
    Q3 <- quantile(group_data$Inflation, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
      
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
      
    # Remove outliers
    filtered_data <- group_data %>%
      filter(Inflation >= lower_bound & Inflation <= upper_bound)
      
    # Compute median inflation after removing outliers
    imputed_value <- median(filtered_data$Inflation, na.rm = TRUE)
      
    #Impute the missing value in the original dataset
    data <- data %>%
      mutate(Inflation = ifelse(Country.Name == country & Time == year & is.na(Inflation), 
                                  imputed_value, Inflation))
  }
  
  return(data)
}
  
# Apply function for Brazil and Bangladesh
trade_data <- impute_inflation(trade_data, "Brazil", "Upper Middle Latin America")
trade_data <- impute_inflation(trade_data, "Bangladesh", "Low Middle Asia")

#Check imputed data
trade_data %>%
  filter((Country.Name == "Brazil" & Time >= 1974 & Time <= 1980) | 
           (Country.Name == "Bangladesh" & Time >= 1974 & Time <= 1986)) %>%
  select(Country.Name, Time, IG, Inflation) %>%
  print()

# Check missing inflation values
missing_inflation <- trade_data %>%
  filter(is.na(Inflation) ) %>%
  select(Country.Name, Time, IG, Inflation) %>%
  print()

-----------------------------------------------------------------------------------------------------
#FDI net-outflow
# Detect outliers and impute missing values
missing_netoutflow <- trade_data %>%
  filter(is.na(FDI_netoutflow) ) %>%
  select(Country.Name, Time, IG, FDI_netoutflow) %>%
  print()

#Function to detect outliers and impute missing values
impute_fdi_outflow <- function(data, country, income_group, start_year = 1974, end_year = 1979) {
  
  # Get the years where FDI_netoutflow is missing within the specified range
  missing_years <- data %>%
    filter(Country.Name == country, Time >= start_year, Time <= end_year, is.na(FDI_netoutflow)) %>%
    pull(Time)
  
  for (year in missing_years) {
    
    # Get FDI_netoutflow for the same year & IG
    group_data <- data %>%
      filter(Time == year, IG == income_group, !is.na(FDI_netoutflow))
    
    # Outlier detection using IQR
    Q1 <- quantile(group_data$FDI_netoutflow, 0.25, na.rm = TRUE)
    Q3 <- quantile(group_data$FDI_netoutflow, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    # Remove outliers
    filtered_data <- group_data %>%
      filter(FDI_netoutflow >= lower_bound & FDI_netoutflow <= upper_bound)
    
    # Compute median FDI_netoutflow after removing outliers
    imputed_value <- median(filtered_data$FDI_netoutflow, na.rm = TRUE)
    
    # Impute the missing value in the original dataset
    data <- data %>%
      mutate(FDI_netoutflow = ifelse(Country.Name == country & Time == year & is.na(FDI_netoutflow), 
                                     imputed_value, FDI_netoutflow))
  }
  
  return(data)
}

# Apply function for countries missing values from 1974 - 1979
trade_data <- impute_fdi_outflow(trade_data, "Indonesia", "Upper Middle Asia")
trade_data <- impute_fdi_outflow(trade_data, "Mexico", "Upper Middle Latin America")
trade_data <- impute_fdi_outflow(trade_data, "Costa Rica", "Upper Middle Latin America")
trade_data <- impute_fdi_outflow(trade_data, "Thailand", "Upper Middle Asia")

# Filter country to find missing values
missing_netoutflow_1 <- trade_data %>%
  filter(is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IG, FDI_netoutflow) %>%
  print()

#Impute missing values country level
impute_fdi_country_level <- function(data, country, start_year = 1995, end_year = 2003) {
  
  # Get the years where FDI_netoutflow is missing for the country within the specified range
  missing_years <- data %>%
    filter(Country.Name == country, Time >= start_year, Time <= end_year, is.na(FDI_netoutflow)) %>%
    pull(Time)
  
  for (year in missing_years) {
    
    # Get past available data for the country before the missing year
    past_data <- data %>%
      filter(Country.Name == country, Time < year, !is.na(FDI_netoutflow))
    
    # Outlier detection using IQR
    Q1 <- quantile(past_data$FDI_netoutflow, 0.25, na.rm = TRUE)
    Q3 <- quantile(past_data$FDI_netoutflow, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    # Remove outliers
    filtered_data <- past_data %>%
      filter(FDI_netoutflow >= lower_bound & FDI_netoutflow <= upper_bound)
    
    # Compute median FDI_netoutflow after removing outliers
    imputed_value <- median(filtered_data$FDI_netoutflow, na.rm = TRUE)
    
    # Impute the missing value in the original dataset
    data <- data %>%
      mutate(FDI_netoutflow = ifelse(Country.Name == country & Time == year & is.na(FDI_netoutflow), 
                                     imputed_value, FDI_netoutflow))
  }
  
  return(data)
}

#Apply filter country wise
trade_data <- impute_fdi_country_level(trade_data, "Mexico")
trade_data <- impute_fdi_country_level(trade_data, "Indonesia")

# Ensure no missing values left 
missing_netoutflow_1 <- trade_data %>%
  filter(is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IG, FDI_netoutflow) %>%
  print()

# Impute 0 for missing FDI_netoutflow values for Low IG countries
trade_data <- trade_data %>%
  mutate(FDI_netoutflow = ifelse(IG == "Low Middle Asia" & is.na(FDI_netoutflow), 0, FDI_netoutflow))

trade_data <- trade_data %>%
  mutate(FDI_netoutflow = ifelse(IG == "Low Middle Africa Mena" & is.na(FDI_netoutflow), 0, FDI_netoutflow))

# Ensure no missing values left for FDI_netoutflow in Low income countries
missing_netoutflow_1 <- trade_data %>%
  filter(is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IG, FDI_netoutflow) 

# Print missing data for verification
print(missing_netoutflow_1)

--------------------------------------------------------------------------------------------------------------------------
#Export
# Detect outliers and impute missing values
missing_export <- trade_data %>%
  filter(is.na(Exports_USD)) %>%
  select(Country.Name, Time, IG, Exports_USD) %>%
  print()
  
# Function to detect outliers and impute missing values
impute_export <- function(data, country, income_group) {

    # Get the years where exports are missing
    missing_years <- data %>%
      filter(Country.Name == country & is.na(Exports_USD)) %>%
      pull(Time)
    
    for (year in missing_years) {
      # Get exports for the same year & IG
      group_data <- data %>%
        filter(Time == year, IG == income_group, !is.na(Exports_USD))
      
      # Check if there are enough data points
      if (nrow(group_data) > 1) {
        # Outlier detection using IQR
        Q1 <- quantile(group_data$Exports_USD, 0.25, na.rm = TRUE)
        Q3 <- quantile(group_data$Exports_USD, 0.75, na.rm = TRUE)
        IQR_value <- Q3 - Q1
        
        lower_bound <- Q1 - 1.5 * IQR_value
        upper_bound <- Q3 + 1.5 * IQR_value
        
        # Remove outliers
        filtered_data <- group_data %>%
          filter(Exports_USD >= lower_bound & Exports_USD <= upper_bound)
        
        # Compute median exports after removing outliers
        imputed_value <- median(filtered_data$Exports_USD, na.rm = TRUE)
        
        # Impute the missing value in the original dataset
        data <- data %>%
          mutate(Exports_USD = ifelse(Country.Name == country & Time == year & is.na(Exports_USD), 
                                  imputed_value, Exports_USD))
      }
    }
    
    return(data)
}

# Apply function to fill missing export values 
trade_data <- impute_export(trade_data, "Philippines", "Low Middle Asia")

# Filter country to find missing values
missing_exports <- trade_data %>%
  filter(is.na(Exports_USD)) %>%
  select(Country.Name, Time, IG, Exports_USD) %>%
  print()

-----------------------------------------------------------------------------------------------------
#Imports
# Detect outliers and impute missing values
missing_import <- trade_data %>%
  filter(is.na(Imports_USD)) %>%
  select(Country.Name, Time, IG, Imports_USD) %>%
  print()

# Function to detect outliers and impute missing values
impute_import <- function(data, country, income_group) {
  
  # Get the years where exports are missing
  missing_years <- data %>%
    filter(Country.Name == country & is.na(Imports_USD)) %>%
    pull(Time)
  
  for (year in missing_years) {
    # Get exports for the same year & IG
    group_data <- data %>%
      filter(Time == year, IG == income_group, !is.na(Imports_USD))
    
    # Check if there are enough data points
    if (nrow(group_data) >= 1) {
      # Outlier detection using IQR
      Q1 <- quantile(group_data$Imports_USD, 0.25, na.rm = TRUE)
      Q3 <- quantile(group_data$Imports_USD, 0.75, na.rm = TRUE)
      IQR_value <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR_value
      upper_bound <- Q3 + 1.5 * IQR_value
      
      # Remove outliers
      filtered_data <- group_data %>%
        filter(Exports_USD >= lower_bound & Imports_USD <= upper_bound)
      
      # Compute median exports after removing outliers
      imputed_value <- median(filtered_data$Imports_USD, na.rm = TRUE)
      
      # Impute the missing value in the original dataset
      data <- data %>%
        mutate(Imports_USD = ifelse(Country.Name == country & Time == year & is.na(Imports_USD), 
                                    imputed_value, Imports_USD))
    }
  }
  
  return(data)
}

# Apply function to fill missing export values 
trade_data <- impute_import(trade_data, "Philippines", "Upper Middle Asia")

# Filter country to find missing values
missing_imports <- trade_data %>%
  filter(is.na(Imports_USD)) %>%
  select(Country.Name, Time, IG, Imports_USD) %>%
  print()

-----------------------------------------------------------------------------------------------------
#Impute Trade Balance
# Compute Trade Balance
trade_data <- trade_data %>%
  mutate(Net_Balance = Exports_USD - Imports_USD)  

missing <- trade_data %>%
  filter(is.na(Net_Balance)) %>%
  select(Country.Name, Time, IG, Net_Balance) %>%
  print()

-----------------------------------------------------------------------------------------------------
##DATA NORMALIZATION and SUMMARY
-----------------------------------------------------------------------------------------------------
summary(trade_data)
sd(trade_data$Exchange_rate)
sd(trade_data$Inflation)
sd(trade_data$FDI_netinflow)
sd(trade_data$FDI_netoutflow)
sd(trade_data$Exports_USD)
sd(trade_data$Imports_USD)

data_normalized <- trade_data %>%
  mutate(
    USD_per_LCU = log1p(USD_per_LCU),  # Log transformation before standardization
    ER = (USD_per_LCU - mean(USD_per_LCU, na.rm = TRUE)) / sd(USD_per_LCU, na.rm = TRUE),
    TB = (Net_Balance - mean(Net_Balance, na.rm = TRUE)) / sd(Net_Balance, na.rm = TRUE),
    IF = (Inflation - mean(Inflation, na.rm = TRUE)) / sd(Inflation, na.rm = TRUE),
    FDI_NO = (FDI_netinflow - mean(FDI_netinflow, na.rm = TRUE)) / sd(FDI_netinflow, na.rm = TRUE),
    FDI_NI = (FDI_netoutflow - mean(FDI_netoutflow, na.rm = TRUE)) / sd(FDI_netoutflow, na.rm = TRUE)
  ) %>%
  ungroup()

summary(data_normalized)

--------------------------------------------------------------------------------------------------
#EXPLORATORY DESCRIPTIVE ANALYSIS
--------------------------------------------------------------------------------------------------
#Which IG has been most affected by trade-based inflation?
ggplot(panel_data, aes(x = Inflation, y = Net_Balance, color = IG)) +
  geom_point(alpha = 0.6) +
  labs(title = "Effect of IF on TB by IG",
       x = "IF",
       y = "TB",
       color = "IG") +
  theme_minimal()

# Find countries with positive net trade balance
trade_surplus_countries <- trade_data %>%
  group_by(Country.Name) %>%
  summarize(avg_net_balance = mean(Net_Balance, na.rm = TRUE)) %>%
  filter(avg_net_balance > 0)

print(trade_surplus_countries)

# Aggregate Net Balance by Country
trade_balance_map <- panel_data %>%
  group_by(Country.Name) %>%
  summarize(avg_net_balance = mean(Net_Balance, na.rm = TRUE))

# Arrange data in descending order of trade balance
trade_balance_map <- trade_balance_map %>%
  arrange(desc(avg_net_balance))

# Create a bar chart
ggplot(trade_balance_map, aes(x = reorder(Country.Name, avg_net_balance), y = avg_net_balance, fill = avg_net_balance)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip to make it horizontal for better readability
  scale_fill_gradient(low = "red", high = "green") +  # Color scale similar to the map
  labs(title = "Trade Balance by Country",
       x = "Country",
       y = "Trade Balance") +
  theme_minimal()


# Aggregate Netinflow by Country
fdi_netinflow <- panel_data %>%
  group_by(Country.Name) %>%
  summarize(avg_netinflow = mean(FDI_NI, na.rm = TRUE)) %>%
  arrange(desc(avg_netinflow)) %>%
  slice_head(n = 5)  # Select only the top 10 countries

# Create a bar chart for the top 10 countries
ggplot(fdi_netinflow, aes(x = reorder(Country.Name, avg_netinflow), y = avg_netinflow, fill = avg_netinflow)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for better readability
  scale_fill_gradient(low = "blue", high = "darkblue") +  # Color scale
  labs(title = "Top 5 Countries with Highest FDI Net Inflow",
       x = "Country",
       y = "Average FDI Net Inflow") +
  theme_minimal()


#fdi outflow

# Aggregate outflow by Country
fdi_netoutflow <- panel_data %>%
  group_by(Country.Name) %>%
  summarize(avg_netoutflow = mean(FDI_NO, na.rm = TRUE)) %>%
  arrange(desc(avg_netoutflow)) %>%
  slice_head(n = 5)  # Select only the top 10 countries

# Create a bar chart for the top 10 countries
ggplot(fdi_netoutflow, aes(x = reorder(Country.Name, avg_netoutflow), y = avg_netoutflow, fill = avg_netoutflow)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for better readability
  scale_fill_gradient(low = "blue", high = "darkblue") +  # Color scale
  labs(title = "Top 5 Countries with Highest FDI Net Outflow",
       x = "Country",
       y = "Average FDI Net Outflow") +
  theme_minimal()


# Create a bar chart
ggplot(trade_balance_map, aes(x = reorder(Country.Name, avg_net_balance), y = avg_net_balance, fill = avg_net_balance)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip to make it horizontal for better readability
  scale_fill_gradient(low = "red", high = "green") +  # Color scale similar to the map
  labs(title = "Trade Balance by Country",
       x = "Country",
       y = "Trade Balance") +
  theme_minimal()

#Scatterplot Exchange Rate vs Net Balance IG wise  
ggplot(data_normalized, aes(x = USD_per_LCU, y = Net_Balance)) +
geom_point() +
facet_wrap(~ IG) +  # This will create a separate plot for each IG
theme_minimal() +  # Minimal theme for clean visualization
labs(title = "Trade Balance vs Exchnage Rate", 
       x = "Exchange Rate", 
       y = "Net Export")

ggplot(data_normalized, aes(x = USD_per_LCU, y = Net_Balance)) +
  geom_point() +
  theme_minimal() +  # Minimal theme for clean visualization
  labs(title = "Trade Balance vs Exchange Rate", 
       x = "Exchange Rate", 
       y = "Trade Balance")


#Scatterplot Inflation vs Net Balance IG wise 
ggplot(data_normalized, aes(x = Inflation, y = Net_Balance)) +
  geom_point() +
  facet_wrap(~ IG) +  # This will create a separate plot for each IG
  theme_minimal() +  # Minimal theme for clean visualization
  labs(title = "Net Export vs Inflation by IG", 
       x = "Inflation", 
       y = "Net Export")

ggplot(trade_data, aes(x = Inflation, y = Net_Balance)) +
  geom_point() +
  theme_minimal() +  # Minimal theme for clean visualization
  labs(title = "Trade Balance vs Inflation", 
       x = "Inflation", 
       y = "Trade Balance")


#Scatterplot FDI_netinflow vs Net Balance IG wise 
ggplot(trade_data, aes(x = FDI_netinflow, y = Net_Balance)) +
  geom_point() +
  facet_wrap(~ IG) +  # This will create a separate plot for each IG
  theme_minimal() +  # Minimal theme for clean visualization
  labs(title = "Net Export vs FDI_netinflow by IG", 
       x = "FDI_netinflow", 
       y = "Net Export")

ggplot(trade_data, aes(x = FDI_netinflow, y = Net_Balance)) +
  geom_point() +
  theme_minimal() +  # Minimal theme for clean visualization
  labs(title = "Trade Balance vs FDI Net Inflow", 
       x = "FDI_netinflow", 
       y = "Trade Balance")

#Scatterplot FDI_netoutflow vs Net Balance IG wise 
ggplot(trade_data, aes(x = FDI_netoutflow, y = Net_Balance)) +
  geom_point() +
  facet_wrap(~ IG) +  # This will create a separate plot for each IG
  theme_minimal() +  # Minimal theme for clean visualization
  labs(title = "Net Export vs FDI_netoutflow by IG", 
       x = "FDI_netoutflow", 
       y = "Net Export")

ggplot(trade_data, aes(x = FDI_netoutflow, y = Net_Balance)) +
  geom_point() +
  theme_minimal() +  # Minimal theme for clean visualization
  labs(title = "Trade Balance vs FDI_netoutflow", 
       x = "FDI_netoutflow", 
       y = "Trade Balance")
  
-----------------------------------------------------------------------------------------------------
##PEARSON COORELATION
-----------------------------------------------------------------------------------------------------
# Function to compute correlation for each IG
correlation_matrix <- data_normalized%>%
  select(Net_Balance, USD_per_LCU, Inflation, FDI_netinflow, FDI_netoutflow) %>%
  cor(use = "complete.obs")

print(correlation_matrix)

correlation_matrix <- trade_data%>%
  select(Net_Balance, Exchange_rate, Inflation, FDI_netinflow, FDI_netoutflow) %>%
  cor(use = "complete.obs")

print(correlation_matrix)

corr <- trade_data %>%
  select(Imports_USD, Exchange_rate, Inflation, FDI_netinflow, FDI_netoutflow) %>%
  cor(use = "complete.obs")

print(corr)

corr_exports <- trade_data %>%
  select(Exports_USD, Exchange_rate, Inflation, FDI_netinflow, FDI_netoutflow) %>%
  cor(use = "complete.obs")

print(corr_exports)




ggcorrplot(correlation_matrix, 
           method = "circle", 
           type = "lower",  # Use "lower" to display only the lower half of the matrix
           lab = TRUE,       # Show correlation coefficients
           title = "Correlation Matrix of Trade Balance with Economic Variables",
           colors = c("red", "white", "blue"))

-----------------------------------------------------------------------------------------------------
##HYPOTHESIS
-----------------------------------------------------------------------------------------------------
#Convert data into panel data
panel_data <- pdata.frame(data_normalized, index = c( "Country.Name", "Time"))

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Effect of Exchange Rate on Trade Balance
#Hypothesis 1
  
# Filter data to include only low- and middle-income countries
panel_data_low_middle <- panel_data %>%
  filter(IG %in% c("Low Middle Asia", "Low Middle Africa Mena", "Upper Middle Asia", "Upper Middle Latin America"))

#Fixed Effects Model
fe_model_1 <- plm(TB ~ ER * IG, 
                  data = panel_data_low_middle, 
                  index = c("Country.Name", "Time"), 
                  model = "within", effect = "individual")
summary(fe_model_1)

#Convert to OLS Model for Diagnostics
ols_model_1 <- lm(Net_Balance ~ USD_per_LCU * IG + as.factor(Country.Name), 
                               data = panel_data_low_middle)

# Durbin-Watson Test for Autocorrelation
dw_test_1 <- durbinWatsonTest(ols_model_1)  # Run on OLS model
print(dw_test_1)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values_1 <- vif(lm(Net_Balance ~ USD_per_LCU * IG, 
                     data = panel_data_low_middle), type = "predictor")  # Run on OLS model
print(vif_values_1)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2. Effect of Inflation on Trade Balance
#Hypothesis 2
  
# Fixed Effects Model for Inflation Impact on Trade Balance
fe_model_2 <- plm(TB ~ IF * IG, 
                  data = panel_data, 
                  index = c("Country.Name", "Time"), 
                  model = "within", effect = "individual")

summary(fe_model_2)

#Convert to OLS Model for Diagnostics
ols_model_2 <- lm(Net_Balance ~ Inflation * IG, 
               data = panel_data)

# Durbin-Watson Test for Autocorrelation
dw_test_2 <- durbinWatsonTest(ols_model_2)  # Run on OLS model
print(dw_test_2)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values_2 <- vif(lm(Net_Balance ~ Inflation * IG, 
                     data = panel_data), type = "predictor")  # Run on OLS model
print(vif_values_2)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#3. Effect of FDI on Trade Balance
#Hypothesis 3
fe_model_3 <- plm(TB ~ FDI_NI * IG + FDI_NO * IG, 
                  data = panel_data, 
                  index = c("Country.Name", "Time"), 
                  model = "within", effect = "individual")

summary(fe_model_3)

#Convert to OLS Model for Diagnostics
ols_model_3 <- lm(Net_Balance ~ FDI_netinflow * IG + FDI_netoutflow * IG, 
                  data = panel_data)

# Durbin-Watson Test for Autocorrelation
dw_test_3 <- durbinWatsonTest(ols_model_3)  # Run on OLS model
print(dw_test_3)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values_3 <- vif(lm(Net_Balance ~ FDI_netinflow * IG + FDI_netoutflow * IG, 
                       data = panel_data), type = "predictor")  # Run on OLS model
print(vif_values_3)



-----------------------------------------------------------------------------------------------------
##IMPACT OF FLUCTUATIONS ACROSS DIFFERENT IGS
-----------------------------------------------------------------------------------------------------
#LOW MIDDLE ASIA
#Filter IG
data_low_middle_asia <- data_normalized %>% filter(IG == "Low Middle Asia")

#Convert to panel data
panel_data_low_middle_asia <- pdata.frame(data_low_middle_asia, index = c("Time", "Country.Name"))


#Fixed Effects Model
fe_model_low_middle_asia <- plm(Net_Balance ~  USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow,
                data = panel_data_low_middle_asia ,
                index = c("Country.Name", "Time"),  # Panel identifiers
                model = "within",   # Fixed Effects (Within Estimator)
                effect = "individual")  # Country-level fixed effects)
summary(fe_model_low_middle_asia)

#Convert to OLS Model for Diagnostics
ols_mode_low_middle_asia <- lm(Net_Balance ~ USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow + as.factor(Country.Name), 
                data = data_low_middle_asia)

# Durbin-Watson Test for Autocorrelation
dw_test <- durbinWatsonTest(ols_mode_low_middle_asia)  # Run on OLS model
print(dw_test)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values <- vif(ols_mode_low_middle_asia)  # Run on OLS model
print(vif_values)

---------------------------------------------------------------------------------------------------
#LOW MIDDLE AFRICA MENA
#Filter IG
data_low_middle_africa_mena <- data_normalized %>% filter(IG == "Low Middle Africa Mena")

#Convert to panel data
panel_data_low_middle_africa_mena <- pdata.frame(data_low_middle_africa_mena, index = c("Time", "Country.Name"))


#Fixed Effects Model
fe_model_low_middle_africa_mena <- plm(Net_Balance ~  USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow,
                                data = panel_data_low_middle_africa_mena,
                                index = c("Country.Name", "Time"),  # Panel identifiers
                                model = "within",   # Fixed Effects (Within Estimator)
                                effect = "individual")  # Country-level fixed effects)
summary(fe_model_low_middle_africa_mena)

#Convert to OLS Model for Diagnostics
ols_model_low_middle_africa_mena <- lm(Net_Balance ~ USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow + as.factor(Country.Name), 
                data = panel_data_low_middle_africa_mena )

# Durbin-Watson Test for Autocorrelation
dw_test <- durbinWatsonTest(ols_model_low_middle_africa_mena)  # Run on OLS model
print(dw_test)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values <- vif(ols_model_low_middle_africa_mena)  # Run on OLS model
print(vif_values)

---------------------------------------------------------------------------------------------------
#HIGH INCOME EUROPE NA
#Filter IG
data_high_income_europe_na <- data_normalized %>% filter(IG == "High Income Europe Na")

#Convert to panel data
panel_data_high_income_europe_na <- pdata.frame(data_high_income_europe_na, index = c("Time", "Country.Name"))


#Fixed Effects Model
fe_model_high_income_europe_na <- plm(Net_Balance ~  USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow,
                                       data = panel_data_high_income_europe_na,
                                       index = c("Country.Name", "Time"),  # Panel identifiers
                                       model = "within",   # Fixed Effects (Within Estimator)
                                       effect = "individual")  # Country-level fixed effects)
summary(fe_model_high_income_europe_na)

#Convert to OLS Model for Diagnostics
ols_model_high_income_europe_na <- lm(Net_Balance ~ USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow + as.factor(Country.Name), 
                                       data = panel_data_high_income_europe_na)

# Durbin-Watson Test for Autocorrelation
dw_test <- durbinWatsonTest(ols_model_high_income_europe_na)  # Run on OLS model
print(dw_test)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values <- vif(ols_model_high_income_europe_na)  # Run on OLS model
print(vif_values)


---------------------------------------------------------------------------------------------------
#HIGH INCOME ASIA PACIFIC
#Filter IG
data_high_income_asia_pacific <- data_normalized %>% filter(IG == "High Income Asia Pacific")

#Convert to panel data
panel_data_high_income_asia_pacific <- pdata.frame(data_high_income_europe_na, index = c("Time", "Country.Name"))


#Fixed Effects Model
fe_model_high_income_asia_pacific <- plm(Net_Balance ~  USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow,
                                      data = panel_data_high_income_asia_pacific,
                                      index = c("Country.Name", "Time"),  # Panel identifiers
                                      model = "within",   # Fixed Effects (Within Estimator)
                                      effect = "individual")  # Country-level fixed effects)
summary(fe_model_high_income_asia_pacific )

#Convert to OLS Model for Diagnostics
ols_model_high_income_asia_pacific <- lm(Net_Balance ~ USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow + as.factor(Country.Name), 
                                      data = panel_data_high_income_asia_pacific)

# Durbin-Watson Test for Autocorrelation
dw_test <- durbinWatsonTest(ols_model_high_income_asia_pacific)  # Run on OLS model
print(dw_test)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values <- vif(ols_model_high_income_asia_pacific)  # Run on OLS model
print(vif_values)


---------------------------------------------------------------------------------------------------
#UPPER MIDDLE ASIA
#Filter IG
data_upper_middle_asia <- data_normalized %>% filter(IG == "Upper Middle Asia")

#Convert to panel data
panel_data_upper_middle_asia <- pdata.frame(data_high_income_europe_na, index = c("Time", "Country.Name"))


#Fixed Effects Model
fe_model_upper_middle_asia <- plm(Net_Balance ~  USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow,
                                         data = panel_data_upper_middle_asia,
                                         index = c("Country.Name", "Time"),  # Panel identifiers
                                         model = "within",   # Fixed Effects (Within Estimator)
                                         effect = "individual")  # Country-level fixed effects)
summary(fe_model_upper_middle_asia)

#Convert to OLS Model for Diagnostics
ols_model_upper_middle_asia <- lm(Net_Balance ~ USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow + as.factor(Country.Name), 
                                         data = panel_data_upper_middle_asia )

# Durbin-Watson Test for Autocorrelation
dw_test <- durbinWatsonTest(ols_model_upper_middle_asia)  # Run on OLS model
print(dw_test)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values <- vif(ols_model_upper_middle_asia)  # Run on OLS model
print(vif_values)

----------------------------------------------------------------------------------------------
#UPPER MIDDLE LATIN AMERICA
#Filter IG
data_upper_middle_latin_america <- data_normalized %>% filter(IG == "Upper Middle Latin America")

#Convert to panel data
panel_data_upper_latin_america <- pdata.frame(data_upper_middle_latin_america, index = c("Time", "Country.Name"))


#Fixed Effects Model
fe_model_upper_latin_america <- plm(Net_Balance ~  USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow,
                                  data = panel_data_upper_latin_america,
                                  index = c("Country.Name", "Time"),  # Panel identifiers
                                  model = "within",   # Fixed Effects (Within Estimator)
                                  effect = "individual")  # Country-level fixed effects)
summary(fe_model_upper_latin_america )

#Convert to OLS Model for Diagnostics
ols_model_upper_latin_america <- lm(Net_Balance ~ USD_per_LCU + Inflation + FDI_netinflow + FDI_netoutflow + as.factor(Country.Name), 
                                  data = panel_data_upper_latin_america)

# Durbin-Watson Test for Autocorrelation
dw_test <- durbinWatsonTest(ols_model_upper_latin_america)  # Run on OLS model
print(dw_test)

#Variance Inflation Factor (VIF) for Multicollinearity
vif_values <- vif(ols_model_upper_latin_america)  # Run on OLS model
print(vif_values)


---------------------------------------------------------------------------------------------------
#EXTRAS
---------------------------------------------------------------------------------------------------
trade_data <- impute_import(trade_data, "Indonesia", "Upper Middle Asia")
trade_data <- impute_import(trade_data, "Mexico", "Upper Middle Asia")
trade_data <- impute_import(trade_data, "India", "Upper Middle Latin America")
trade_data <- impute_import(trade_data, "Costa Rica", "Upper Middle Latin America")
trade_data <- impute_import(trade_data, "France", "High Income Europe Na")
trade_data <- impute_import(trade_data, "Thailand", "Upper Middle Asia")

# Impute 0 for missing Exports values for Low IG countries
trade_data <- trade_data %>%
  mutate(Imports_USD = ifelse(IG == "Low Middle Asia" & is.na(Imports_USD), 0, Imports_USD))

trade_data <- trade_data %>%
  mutate(Imports_USD = ifelse(IG == "Low Middle Africa Mena" & is.na(Imports_USD), 0, Imports_USD))

trade_data <- trade_data %>%
  mutate(Imports_USD = ifelse(IG == "Upper Middle Latin America" & is.na(Imports_USD), 0, Imports_USD))
