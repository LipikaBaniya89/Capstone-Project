# Capstone-Project

## Project Overview
An analysis of the impact of exchange rates, inflation, and FDI on trade balances across income groups using the World Bank’s World Development Indicators dataset. The study applies multiple linear regression on data from 22 countries across three income categories to test key economic hypotheses.

## Dataset Information
- **Data source** : World Development Indicators - World Bank (https://databank.worldbank.org/source/world-development-indicators)
- **Country selection criteria**: Selected from three income categories, which were already pre-categorized by the World Bank as lower-middle-income countries, upper-middle-income countries, and high-income countries
- **Variables used**</br>
**Dependent**:
   - Trade Balance
**Independent**:</br>
    - Exchange Rate
    - FDI Net-inflow
    - FDI Net-outflow
    - Inflation

## Medthodology
- **Statistical model**: Multiple Linear Regression
- **Hypotheses tested**
  > Hypothesis (H1): The exchange rate has a significant impact on lower and middle-income countries.</br>
  > Hypothesis (H2): Inflation negatively affects the trade balance in lower, middle and high-income countries.</br>
  > Hypothesis (H3): FDI inflows and outflows have a positive impact on the trade balance of high-income, low- and middle-income countries.</br>
- **Tools/libraries** : R ((ggplot2, dplyr, caret, rpart, car)
