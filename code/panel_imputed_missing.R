# Using the method of KNNs to impute the missing values for variables
#Load the corresponding libraries
library("caret") # install.packages("RANN")
library(RANN)
library(tidyverse)



# Load the dataset

panel_data_final <- read.table("../Knowledge-Economy-and-Economic-growth/data/panel_data_full2.csv", header = TRUE, sep = ",", fill = TRUE)
panel_data_final$X <- NULL

# Filter observations for year >= 1970

panel_data_final <-subset(panel_data_final, year>=1970)

#subset of countries, african countries and emerging countries using a robust method

afemerge <- c("Algeria", "Angola", "Argentina", "Benin", "Botswana",  "Brazil", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "China", "Colombia", 
              "Comoros", "Cote d'Ivoire", "Congo, Dem. Rep", "Czech Republic", "Djibouti",
              "Egypt, Arab Rep.", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Ghana", "Greece", "Guinea", "Hungary", "India", "Indonesia", "Israel", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malaysia", "Malawi", "Mali", "Mauritania", "Mauritius", "Mexico",
              "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Congo, Rep.", "Pakistan", "Peru", "Philippines", "Poland", "Qatar", "Russian Federation", "Rwanda", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Seychelles", "Sierra Leone", 
              "South Africa", "Sudan", "Tanzania", "Thailand", "Turkey", "Gambia, The", "Togo", "Tunisia", "Uganda", "United Arab Emirates", "Zambia", "Zimbabwe")

# Panel data description

panel_ke<-panel_data_final[panel_data_final$country %in% afemerge,]
dim(panel_ke)


# Type of variables

lapply(panel_ke, class)


# Select for imputation of KNNs

panel_ke <- panel_ke %>% 
  select(country, year, fdi, irspread, domcredit, investment, trade, inflation, gdppc, tariff, hdi, High_Exports)

# inpute data

k_value=5


knn_imput = function(panel_ke) {
  numeric_columns = (sapply(panel_ke, class) %in% c("numeric", "integer"))
  means = apply(panel_ke[, numeric_columns], 2, mean, na.rm = TRUE)
  sds = apply(panel_ke[, numeric_columns], 2, sd, na.rm = TRUE)
  
  panel_ke[, numeric_columns] = sweep(panel_ke[, numeric_columns], 2, means)
  panel_ke[, numeric_columns] = sweep(panel_ke[, numeric_columns], 2, sds, "/")
  
  imputer = preProcess(panel_ke, method = "knnImpute", k=k_value)
  
  imputed_data =  predict(imputer, newdata = panel_ke)
  
  imputed_data[, numeric_columns] = sweep(
    imputed_data[, numeric_columns], 2, sds, "*"
  )
  imputed_data[, numeric_columns] = sweep(
    imputed_data[, numeric_columns], 2, means, "+"
  )
  
  imputed_data
}

imputed_data_economic = knn_imput(panel_ke)
