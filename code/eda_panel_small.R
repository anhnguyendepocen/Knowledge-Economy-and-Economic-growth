library(tidyverse)  
library(naniar) # Missing-values package
library(corrplot)  #Generate correlation matrix
library(GGally)  # Generate histograms, scatter plots, etc

# Load the dataset

panel_data_final <- read.table("../Knowledge-Economy-and-Economic-growth/data/panel_data_full2.csv", header = TRUE, sep = ",", fill = TRUE)
panel_data_final$X <- NULL

# Filter observations for year >= 2002

panel_data_final <-subset(panel_data_final, year>=2002)

#subset of countries, african countries and emerging countries using a robust method

afemerge <- c("Algeria", "Angola", "Argentina", "Benin", "Botswana",  "Brazil", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "China", "Colombia", 
              "Comoros", "Cote d'Ivoire", "Congo, Dem. Rep", "Czech Republic", "Djibouti",
              "Egypt, Arab Rep.", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Ghana", "Greece", "Guinea", "Hungary", "India", "Indonesia", "Israel", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malaysia", "Malawi", "Mali", "Mauritania", "Mauritius", "Mexico",
              "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Congo, Rep.", "Pakistan", "Peru", "Philippines", "Poland", "Qatar", "Russian Federation", "Rwanda", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Seychelles", "Sierra Leone", 
              "South Africa", "Sudan", "Tanzania", "Thailand", "Turkey", "Gambia, The", "Togo", "Tunisia", "Uganda", "United Arab Emirates", "Zambia", "Zimbabwe")

# Panel data description

panel_ke_2<-panel_data_final[panel_data_final$country %in% afemerge,]
dim(panel_ke_2)
names(panel_ke_2)
countrylist_ke <- length(unique(panel_ke_2$country))
levels(panel_ke_2$country)

# Original KEI and KEI predicted
kei_data <- read.table("../Knowledge-Economy-and-Economic-growth/data/KEIPRedicted.csv", header = TRUE, sep = ",")
dim(kei_data)

# Filter observations per year >= 2002

kei_data<-subset (kei_data, year>= 2002)
names(kei_data)
countrylist_kei<-length(unique(kei_data$country))
levels(kei_data$country)

#Merge with different levels for the country variable

kei_combined_data <-full_join(kei_data, panel_ke_2)

# Generate patents and trademarks per capita

kei_combined_data$patens_pc <- (kei_combined_data$patents/kei_combined_data$population)*1000
kei_combined_data$trademarks_pc <-(kei_combined_data$trademark/kei_combined_data$population)*1000


# Proportion of missing values

anyNA(kei_combined_data)
table(is.na(kei_combined_data))
prop.table(table(is.na(kei_combined_data)))*100

#Visualizing missing values with
# Missing visualizations

gg_miss_var(kei_combined_data[, c(3:33)])
           