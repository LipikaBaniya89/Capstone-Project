library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)
library(lmtest)

getwd() #Get working directory

setwd("E:/Capstone-Project") #Set working directory

#setwd("/Users/lipikabania/Documents/Capstone-Project")

trade_data<-read.csv("ImportExportNew.csv") #load data

colSums(is.na(trade_data)) #na values in each column

nrow(trade_data) #number of observations

summary(trade_data) #summary of data

-----------------------------------------------------------------------------------------------------
# Convert columns to numeric
trade_data <- trade_data %>%
  mutate(
    ExchangeRate = as.numeric(Official.exchange.rate..LCU.per.US...period.average...PA.NUS.FCRF.),
    Inflation = as.numeric(Inflation..consumer.prices..annual.....FP.CPI.TOTL.ZG.),
    FDI_netinflow = as.numeric(Foreign.direct.investment..net.inflows....of.GDP...BX.KLT.DINV.WD.GD.ZS.),
    FDI_netoutflow = as.numeric(Foreign.direct.investment..net.outflows....of.GDP...BM.KLT.DINV.WD.GD.ZS.),
    Exports = as.numeric(Exports.of.goods.and.services..constant.LCU...NE.EXP.GNFS.KN.),
    Imports = as.numeric(Imports.of.goods.and.services..constant.LCU...NE.IMP.GNFS.KN.),
    TradeBalance = Exports - Imports  # Calculated TB
  ) 

# Country income groups
low_middle_income <- c("Bangladesh", "India", "Pakistan", "Phillipines")
upper_middle_income <- c("Belgium", "Costa Rica", "Malaysia", "Thailand", "Indonesia", "Brazil", "Turkiye", "Mexico")
high_income <- c( "United Kingdom", "France", "Canada", "Australia", "Singapore", "Japan")

# Income group as a categorical variable
trade_data <- trade_data %>%
  mutate(IncomeGroup = case_when(
    Country.Name %in% low_middle_income ~ "Lower Middle Income",
    Country.Name %in% upper_middle_income ~ "Upper Middle Income",
    Country.Name %in% high_income ~ "High Income",
    TRUE ~ "Other"
  ))

-----------------------------------------------------------------------------------------------------
##DATA CLEANING
-----------------------------------------------------------------------------------------------------
#Check outliers for Exchange Rate
Q1_ExchangeRate <- quantile(trade_data$ExchangeRate, 0.25, na.rm = TRUE)
Q3_ExchangeRate <- quantile(trade_data$ExchangeRate, 0.75, na.rm = TRUE)
IQR_ExchangeRate <- Q3_ExchangeRate - Q1_ExchangeRate

lower_exchangerate <- Q1_ExchangeRate - 1.5 * Q3_ExchangeRate
upper_exchangerate <- Q1_ExchangeRate + 1.5 * Q3_ExchangeRate

trade_data<- trade_data %>%
  filter(ExchangeRate >= lower_exchangerate & ExchangeRate <= upper_exchangerate )

print(trade_data)


# Function to detect outliers and impute missing values
impute_inflation <- function(data, country, income_group) {
    
# Get the years where inflation is missing
  missing_years <- data %>%
    filter(Country.Name == country & is.na(Inflation)) %>%
    pull(Time)
    
for (year in missing_years) {
   # Get inflation values for the same year & income group
    group_data <- data %>%
      filter(Time == year, IncomeGroup == income_group, !is.na(Inflation))
      
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
  
  # **Remove outliers from the entire dataset** before returning it
  Q1_all <- quantile(data$Inflation, 0.25, na.rm = TRUE)
  Q3_all <- quantile(data$Inflation, 0.75, na.rm = TRUE)
  IQR_all <- Q3_all - Q1_all
    
  lower_bound_all <- Q1_all - 1.5 * IQR_all
  upper_bound_all <- Q3_all + 1.5 * IQR_all
    
  cleaned_data <- data %>%
    filter(Inflation >= lower_bound_all & Inflation <= upper_bound_all)
    
  return(cleaned_data)
}

# Apply function for Brazil and Bangladesh
trade_data <- impute_inflation(trade_data, "Brazil", "Upper Middle Income")
trade_data <- impute_inflation(trade_data, "Bangladesh", "Lower Middle Income")

trade_data %>%
  filter((Country.Name == "Brazil" & Time >= 1974 & Time <= 1980) | 
           (Country.Name == "Bangladesh" & Time >= 1974 & Time <= 1986)) %>%
  select(Country.Name, Time, IncomeGroup, Inflation) %>%
  print()


-----------------------------------------------------------------------------------------------------
#FDI net-outflow
  
# Check missing net-outflow values
missing_netoutflow <- trade_data %>%
  filter(is.na(FDI_netoutflow) ) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

#Function to detect outliers and impute missing values
impute_fdi_outflow <- function(data, country, income_group, start_year = 1974, end_year = 1979) {
  
  # Get the years where FDI_netoutflow is missing within the specified range
  missing_years <- data %>%
    filter(Country.Name == country, Time >= start_year, Time <= end_year, is.na(FDI_netoutflow)) %>%
    pull(Time)
  
  for (year in missing_years) {
    
    # Get FDI_netoutflow for the same year & income group
    group_data <- data %>%
      filter(Time == year, IncomeGroup == income_group, !is.na(FDI_netoutflow))
    
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
trade_data <- impute_fdi_outflow(trade_data, "Indonesia", "Upper Middle Income")
trade_data <- impute_fdi_outflow(trade_data, "Mexico", "Upper Middle Income")
trade_data <- impute_fdi_outflow(trade_data, "Phillipines", "Lower Middle Income")
trade_data <- impute_fdi_outflow(trade_data, "Costa Rica", "Upper Middle Income")
trade_data <- impute_fdi_outflow(trade_data, "Bangladesh", "Lower Middle Income")
trade_data <- impute_fdi_outflow(trade_data, "Thailand", "Upper Middle Income")
trade_data <- impute_fdi_outflow(trade_data, "Pakistan", "Lower Middle Income")
trade_data <- impute_fdi_outflow(trade_data, "India", "Lower Middle Income")

-----------------------------------------------------------
# Filter country to find missing values
missing_netoutflow_1 <- trade_data %>%
  filter(is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
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
trade_data <- impute_fdi_country_level(trade_data, "Indonesia")
trade_data <- impute_fdi_country_level(trade_data, "Mexico")

# Ensure no missing values left 
missing_netoutflow_1 <- trade_data %>%
  filter(is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Impute 0 for missing FDI_netoutflow values for Low income group countries
trade_data <- trade_data %>%
  mutate(FDI_netoutflow = ifelse(IncomeGroup == "Lower Middle Income" & is.na(FDI_netoutflow), 0, FDI_netoutflow))

# Ensure no missing values left for FDI_netoutflow in Low income countries
missing_netoutflow_1 <- trade_data %>%
  filter(is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) 

# Print missing data for verification
print(missing_netoutflow_1)

# Display the updated dataframe
print(trade_data)

-----------------------------------------------------------------------------------------------------
#Export
  
#Find missing export values
missing_export <- trade_data %>%
  filter(is.na(Exports)) %>%
  select(Country.Name, Time, IncomeGroup, Exports) %>%
  print()
  
# Function to detect outliers and impute missing values
impute_turkiye_export <- function(data, country, income_group) {

    # Get the years where exports are missing
    missing_years <- data %>%
      filter(Country.Name == country & is.na(Exports)) %>%
      pull(Time)
    
    for (year in missing_years) {
      # Get exports for the same year & income group
      group_data <- data %>%
        filter(Time == year, IncomeGroup == income_group, !is.na(Exports))
      
      # Check if there are enough data points
      if (nrow(group_data) > 2) {
        # Outlier detection using IQR
        Q1 <- quantile(group_data$Exports, 0.25, na.rm = TRUE)
        Q3 <- quantile(group_data$Exports, 0.75, na.rm = TRUE)
        IQR_value <- Q3 - Q1
        
        lower_bound <- Q1 - 1.5 * IQR_value
        upper_bound <- Q3 + 1.5 * IQR_value
        
        # Remove outliers
        filtered_data <- group_data %>%
          filter(Exports >= lower_bound & Exports <= upper_bound)
        
        # Compute median exports after removing outliers
        imputed_value <- median(filtered_data$Exports, na.rm = TRUE)
        
        # Impute the missing value in the original dataset
        data <- data %>%
          mutate(Exports = ifelse(Country.Name == country & Time == year & is.na(Exports), 
                                  imputed_value, Exports))
      }
    }
    
    return(data)
  }

# Apply function to fill missing export values for Türkiye
trade_data <- impute_turkiye_export(trade_data, "Turkiye", "Upper Middle Income")

# Filter country to find missing values
missing_exports <- trade_data %>%
  filter(is.na(Exports)) %>%
  select(Country.Name, Time, IncomeGroup, Exports) %>%
  print()

View(trade_data)

-----------------------------------------------------------------------------------------------------
#Imports

# Filter country to find missing values
missing_imports <- trade_data %>%
  filter(is.na(Imports)) %>%
  select(Country.Name, Time, IncomeGroup, Imports) %>%
  print()
  
# Function to detect outliers and impute missing values
impute_turkiye_import <- function(data, country, income_group) {
    
    # Get the years where imports are missing
    missing_years <- data %>%
      filter(Country.Name == country & is.na(Imports)) %>%
      pull(Time)
    
    for (year in missing_years) {
      
      # Get imports for the same year & income group
      group_data <- data %>%
        filter(Time == year, IncomeGroup == income_group, !is.na(Imports))
      
      # Check if there are enough data points
      if (nrow(group_data) > 2) {
        # Outlier detection using IQR
        Q1 <- quantile(group_data$Imports, 0.25, na.rm = TRUE)
        Q3 <- quantile(group_data$Imports, 0.75, na.rm = TRUE)
        IQR_value <- Q3 - Q1
        
        lower_bound <- Q1 - 1.5 * IQR_value
        upper_bound <- Q3 + 1.5 * IQR_value
        
        # Remove outliers
        filtered_data <- group_data %>%
          filter(Imports >= lower_bound & Imports <= upper_bound)
        
        # Compute median exports after removing outliers
        imputed_value <- median(filtered_data$Imports, na.rm = TRUE)
        
        # Impute the missing value in the original dataset
        data <- data %>%
          mutate(Imports = ifelse(Country.Name == country & Time == year & is.na(Imports), 
                                  imputed_value, Imports))
      }
    }
    
    return(data)
  }

# Apply function to fill missing export values for Türkiye
trade_data <- impute_turkiye_import(trade_data, "Turkiye", "Upper Middle Income")

# Filter country to find missing values
missing_imports <- trade_data %>%
  filter(is.na(Imports)) %>%
  select(Country.Name, Time, IncomeGroup, Imports) %>%
  print()


----------------------------------------------------------------------------
#Fill missing trade balance values
  
# Compute Trade Balance
trade_data <- trade_data %>%
  mutate(TradeBalance = Exports - Imports)  

missing <- trade_data %>%
  filter(is.na(TradeBalance)) %>%
  select(Country.Name, Time, IncomeGroup, TradeBalance) %>%
  print()

View(trade_data)


----------------------------------------------------------------------------
#Descriptive Statistics
  
# Compute summary statistics for each income group
summary_stats <- trade_data %>%
  group_by(IncomeGroup) %>%
  summarise(
    Mean_ExchangeRate = mean(ExchangeRate, na.rm = TRUE),
    Median_ExchangeRate = median(ExchangeRate, na.rm = TRUE),
    SD_ExchangeRate = sd(ExchangeRate, na.rm = TRUE),
    Min_ExchangeRate = min(ExchangeRate, na.rm = TRUE),
    Max_ExchangeRate = max(ExchangeRate, na.rm = TRUE),
    
    Mean_Inflation = mean(Inflation, na.rm = TRUE),
    Median_Inflation = median(Inflation, na.rm = TRUE),
    SD_Inflation = sd(Inflation, na.rm = TRUE),
    
    Mean_FDI_netinflow = mean(FDI_netinflow, na.rm = TRUE),
    Median_FDI_netinflow = median(FDI_netinflow, na.rm = TRUE),
    SD_FDI_netinflow = sd(FDI_netinflow, na.rm = TRUE),
    
    Mean_FDI_netoutflow = mean(FDI_netoutflow, na.rm = TRUE),
    Median_FDI_netoutflow = median(FDI_netoutflow, na.rm = TRUE),
    SD_FDI_netoutflow = sd(FDI_netoutflow, na.rm = TRUE),
    
    Mean_Exports = mean(Exports, na.rm = TRUE),
    Median_Exports = median(Exports, na.rm = TRUE),
    SD_Exports = sd(Exports, na.rm = TRUE),
    
    Mean_Imports = mean(Imports, na.rm = TRUE),
    Median_Imports = median(Imports, na.rm = TRUE),
    SD_Imports = sd(Imports, na.rm = TRUE),
    
    Mean_TradeBalance = mean(TradeBalance, na.rm = TRUE),
    Median_TradeBalance = median(TradeBalance, na.rm = TRUE),
    SD_TradeBalance = sd(TradeBalance, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats)


----------------------------------------------------------------------------
#Coorelation
# Function to compute correlation for each income group
correlation_by_income <- trade_data %>%
  group_by(IncomeGroup) %>%
  summarise(
    Cor_TradeBalance_ExchangeRate = cor(TradeBalance, ExchangeRate, use = "complete.obs"),
    Cor_TradeBalance_Inflation = cor(TradeBalance, Inflation, use = "complete.obs"),
    Cor_TradeBalance_FDI_Outflow = cor(TradeBalance, FDI_netinflow, use = "complete.obs"),
    Cor_TradeBalance_FDI_Inflow = cor(TradeBalance, FDI_netoutflow, use = "complete.obs")
  )

# Print correlation results for each income group
print(correlation_by_income)

# Function to run panel regression by income group
run_panel_regression <- function(income_group, data) {
  
  # Filter data for the given income group
  group_data <- data %>% filter(IncomeGroup == income_group)
  
  # Convert to panel data format
  panel_data <- pdata.frame(group_data, index = c("Country.Name", "Time"))
  
  # Fixed Effects Model (FE)
  fe_model <- plm(TradeBalance ~ ExchangeRate + Inflation + FDI_netoutflow + FDI_netinflow, 
                  data = panel_data, model = "within")

  
  # Hausman Test to decide between FE and RE
  hausman_test <- phtest(fe_model, re_model)
  
  # Print results
  cat("\n==============================\n")
  cat("Income Group:", income_group, "\n")
  cat("==============================\n")
  print(summary(fe_model))  # Fixed Effects model results
  print(summary(re_model))  # Random Effects model results
  print(hausman_test)  # Hausman test results
}

# Run regression for each Income Group
unique_income_groups <- unique(trade_data$IncomeGroup)

for (group in unique_income_groups) {
  run_panel_regression(group, trade_data)
}


-----------------------------------------------------------------------------------------------
#Extras
  
  #Outliers
  
  #Check outliers for Inflation
  # **Remove outliers from the entire dataset** before returning it
  Q1_inflation <- quantile(trade_data$Inflation, 0.25, na.rm = TRUE)
Q3_inflation <- quantile(trade_data$Inflation, 0.75, na.rm = TRUE)
IQR_inflation <- Q3_inflation - Q1_inflation

lower_inflation <- Q1_inflation - 1.5 * IQR_inflation
upper_inflation <- Q3_inflation + 1.5 * IQR_inflation

cleaned_data <- trade_data %>%
  filter(!is.na(Inflation) & Inflation >= lower_inflation & Inflation <= upper_inflation)

print(cleaned_data)


#Check outliers for Exchange Rate
Q1_ExchangeRate <- quantile(cleaned_data$ExchangeRate, 0.25, na.rm = TRUE)
Q3_ExchangeRate <- quantile(cleaned_data$ExchangeRate, 0.75, na.rm = TRUE)
IQR_ExchangeRate <- Q3_ExchangeRate - Q1_ExchangeRate

lower_exchangerate <- Q1_ExchangeRate - 1.5 * Q3_ExchangeRate
upper_exchangerate <- Q1_ExchangeRate + 1.5 * Q3_ExchangeRate

cleaned_data_exchangerate<- cleaned_data %>%
  filter(!is.na(ExchangeRate) & Inflation >= lower_exchangerate & ExchangeRate <= upper_exchangerate ) %>%
  select(Country.Name, Time, ExchangeRate)

print(cleaned_data_exchangerate)


#Inflation
# Filter data for the year 1974 and only middle-income groups
middle_income_inflation_1974 <- trade_data %>%
  filter(Time == 1974 & IncomeGroup %in% c( "Upper Middle Income")) %>%
  select(Country.Name, Time, IncomeGroup, Inflation)

# View the result
print(middle_income_inflation_1974)

# Filter data for the year 1974 and only high_income
high_income_inflation_1974 <- trade_data %>%
  filter(Time == 1974 & IncomeGroup %in% c("High Income")) %>%
  select(Country.Name, Time, IncomeGroup, Inflation)

# View the result
print(high_income_inflation_1974)

# Filter data for the year 1974 and only low_middle_income
low_income_inflation_1974 <- trade_data %>%
  filter(Time == 1974 & IncomeGroup %in% c("Lower Middle Income")) %>%
  select(Country.Name, Time, IncomeGroup, Inflation)

# View the result
print(low_income_inflation_1974)


-----------------------------------------------------------------------------------------------------
  # Filter data for the year 1974
  trade_1974 <- trade_data %>%
  filter(Time == 1974, !is.na(Inflation))  # Remove missing inflation values

# Create the box plot
ggplot(trade_1974, aes(x = IncomeGroup, y = Inflation, fill = IncomeGroup)) +
  geom_boxplot() +
  labs(title = "Inflation Distribution by Income Group (1974)",
       x = "Income Group",
       y = "Inflation Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since x-axis already shows the groups

-----------------------------------------------------------
  #Check net-outflow country-wise
  
  # Filter data for the country Indonesia
  indonesia_netoutflow <- trade_data %>%
  filter(Country.Name == "Indonesia" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Indonesia's net FDI outflow
ggplot(indonesia_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Indonesia",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

-----------------------------------------------------------
  # Filter data for the country Mexico
  mexico_netoutflow <- trade_data %>%
  filter(Country.Name == "Mexico" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Mexico's net FDI outflow
ggplot(mexico_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Mexico",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

-----------------------------------------------------------
  # Filter data for the country Costa Rica
  costarica_netoutflow <- trade_data %>%
  filter(Country.Name == "Costa Rica" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Costa Rica's net FDI outflow
ggplot(costarica_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Costa Rica",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

-----------------------------------------------------------
  # Filter data for the country Phillipines
  phillipines_netoutflow <- trade_data %>%
  filter(Country.Name == "Phillipines" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Phillipines's net FDI inflow
ggplot(phillipines_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Phillipines",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

-----------------------------------------------------------
  # Filter data for the country Bangladesh
  bangladesh_netoutflow <- trade_data %>%
  filter(Country.Name == "Bangladesh" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Bangladesh's net FDI inflow
ggplot(phillipines_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Bangladesh",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

-----------------------------------------------------------
  # Filter data for the country Pakistan
  pakistan_netoutflow <- trade_data %>%
  filter(Country.Name == "Pakistan" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Pakistan's net FDI inflow
ggplot(pakistan_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Pakistan",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

-----------------------------------------------------------
  # Filter data for the country Thailand
  thailand_netoutflow <- trade_data %>%
  filter(Country.Name == "Thailand" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Thailand's net FDI inflow
ggplot(thailand_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Thailand",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()


-----------------------------------------------------------
  # Filter data for the country India
  india_netoutflow <- trade_data %>%
  filter(Country.Name == "India" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for India's net FDI inflow
ggplot(india_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in India",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

-----------------------------------------------------------

-----------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
  



