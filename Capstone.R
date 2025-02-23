library(dplyr)
library(ggplot2)
library(tidyr)

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
high_income <- c("United States", "United Kingdom", "France", "Canada", "Australia", "Singapore", "Japan")

# Income group as a categorical variable
trade_data <- trade_data %>%
  mutate(IncomeGroup = case_when(
    Country.Name %in% low_middle_income ~ "Lower Middle Income",
    Country.Name %in% upper_middle_income ~ "Upper Middle Income",
    Country.Name %in% high_income ~ "High Income",
    TRUE ~ "Other"
  ))

-----------------------------------------------------------------------------------------------------
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


-----------------------------------------------------------------------------------------------------
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
    
    # Impute the missing value in the original dataset
    data <- data %>%
      mutate(Inflation = ifelse(Country.Name == country & Time == year & is.na(Inflation), 
                                imputed_value, Inflation))
  }
  
  return(data)
}

# Apply function for Brazil and Bangladesh
trade_data <- impute_inflation(trade_data, "Brazil", "Upper Middle Income")
trade_data <- impute_inflation(trade_data, "Bangladesh", "Lower Middle Income")

trade_data %>%
  filter((Country.Name == "Brazil" & Time >= 1974 & Time <= 1980) | 
           (Country.Name == "Bangladesh" & Time >= 1974 & Time <= 1986)) %>%
  select(Country.Name, Time, IncomeGroup, Inflation) %>%
  print()

View(trade_data)
summary(trade_data) #summary of data

-----------------------------------------------------------------------------------------------------
#FDI net-outflow

# Missing net-outflow values
missing_netoutflow <- trade_data %>%
  filter(is.na(FDI_netoutflow) ) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()
  
# Filter data for the country Indonesia
indonesia_netoutflow <- trade_data %>%
  filter(Country.Name == "Indonesia" & is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Indonesia's net FDI inflow
ggplot(indonesia_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Indonesia",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

# Filter data for the country Mexico
mexico_netoutflow <- trade_data %>%
  filter(Country.Name == "Mexico" & is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Mexico's net FDI inflow
ggplot(mexico_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Mexico",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

# Filter data for the country Costa Rica
costarica_netoutflow <- trade_data %>%
  filter(Country.Name == "Costa Rica" & !is.na(FDI_netoutflow)) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

# Line graph for Costa Rica's net FDI inflow
ggplot(costarica_netoutflow, aes(x = Time, y = FDI_netoutflow)) +
  geom_line(color = "blue", size = 1) +  # Line for trend
  geom_point(color = "red", size = 2) +  # Points for individual data
  labs(
    title = "FDI Net Outflow Trend in Costa Rica",
    x = "Year",
    y = "FDI Net Outflow (% of GDP)"
  ) +
  theme_minimal()

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

# Apply function for Indonesia and Mexico
trade_data <- impute_fdi_outflow(trade_data, "Indonesia", "Upper Middle Income")
trade_data <- impute_fdi_outflow(trade_data, "Mexico", "Lower Middle Income")

trade_data %>%
  filter((Country.Name == "Indonesia") | 
           (Country.Name == "Mexico" )) %>%
  select(Country.Name, Time, IncomeGroup, FDI_netoutflow) %>%
  print()

View(trade_data)
summary(trade_data) #summary of data





