library(dplyr)
library(ggplot2)

getwd() #Get working directory

setwd("C:/Users/Lipika/Downloads") #Set working directory

trade_data<-read.csv("ImportExport(1).csv") #load data

colSums(is.na(trade_data)) #na values in each column

nrow(trade_data) #number of observations
rowSums(is.na(trade_data)) #na values in each column

summary(trade_data) #summary of data

# Convert columns to numeric
trade_data <- trade_data %>%
  mutate(
    GVA = as.numeric(Gross.value.added.at.basic.prices..GVA...current.LCU...NY.GDP.FCST.CN.),
    ExchangeRate = as.numeric(Official.exchange.rate..LCU.per.US...period.average...PA.NUS.FCRF.),
    Inflation = as.numeric(Inflation..consumer.prices..annual.....FP.CPI.TOTL.ZG.),
    FDI_netinflow = as.numeric(Foreign.direct.investment..net.inflows....of.GDP...BX.KLT.DINV.WD.GD.ZS.),
    FDI_netoutflow = as.numeric(Foreign.direct.investment..net.outflows....of.GDP...BM.KLT.DINV.WD.GD.ZS.),
    Exports = as.numeric(Exports.of.goods.and.services..constant.LCU...NE.EXP.GNFS.KN.),
    Imports = as.numeric(Imports.of.goods.and.services..constant.LCU...NE.IMP.GNFS.KN.),
    TradeBalance = Exports - Imports  # Calculated TB
  ) 

# Country income groups
low_middle_income <- c("Bangladesh", "India", "Pakistan", "Philippines")
upper_middle_income <- c("Argentina", "Belgium", "Costa Rica", "Malaysia", "Thailand", "Indonesia", "Brazil", "Turkiye", "Mexico")
high_income <- c("United States", "United Kingdom", "France", "Canada", "Australia", "Singapore", "Japan")

# Income group as a categorical variable
trade_data <- trade_data %>%
  mutate(IncomeGroup = case_when(
    Country.Name %in% low_middle_income ~ "Lower Middle Income",
    Country.Name %in% upper_middle_income ~ "Upper Middle Income",
    Country.Name %in% high_income ~ "High Income",
    TRUE ~ "Other"
  ))

# Correlation matrix
cor(trade_data %>% select(ExchangeRate, TradeBalance, Inflation, FDI_netinflow, FDI_netoutflow, GVA), use = "complete.obs")

ggplot(trade_data, aes(x = ExchangeRate, y = TradeBalance, color = IncomeGroup)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Exchange Rate vs Trade Balance", x = "Exchange Rate", y = "Trade Balance")

