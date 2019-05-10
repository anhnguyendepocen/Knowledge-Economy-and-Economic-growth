# Restrict the sample from 2002 and including the original  KEI and KEI predicted
#using neuron networks. The value of KEI is predicted by using a set of variables. Output (KEI)
# We get a mean squared error of 0.45. 

library(tidyverse)  
library(naniar) # Missing-values package
library(corrplot)  #Generate correlation matrix
library(GGally)  # Generate histograms, scatter plots, etc
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.2/")
library(rJava)
library(xlsx)

# Load the dataset. Reading names containing apostrophes. Using quotes

panel_data_final <- read.table("../Knowledge-Economy-and-Economic-growth/data/panel_data_full2.csv", header = TRUE, sep = ";", fill = TRUE, quote="\"")
panel_data_final$X <- NULL

# Filter observations for year >= 2002. On this way, we loose lots of observations
#it would be better to impute missing values using KNNs. Try with different values of k = 5, k = 4

panel_data_final <-subset(panel_data_final, year>=2002)

#subset of countries, african countries and emerging countries using a robust method
# This is in line with our research project when including these countries
afemerge <- c("Algeria", "Angola", "Argentina", "Benin", "Botswana",  "Brazil", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", 
              "Central African Republic", "Chad", "China", "Colombia", 
              "Comoros", "Cote d'Ivoire", "Congo.Dem.Rep.", "Czech Republic", "Djibouti",
              "Egypt, Arab Rep.", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Ghana", "Greece", "Guinea", "Hungary", "India", "Indonesia", "Israel", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malaysia", "Malawi", "Mali", "Mauritania", "Mauritius", "Mexico",
              "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Pakistan", "Peru", "Philippines", "Poland", "Qatar", "Russian Federation", "Rwanda", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Seychelles", "Sierra Leone", 
              "South Africa", "Sudan", "Tanzania", "Thailand", "Turkey", "Gambia, The", "Togo", "Tunisia", "Uganda", "United Arab Emirates", "Zambia", "Zimbabwe")

# Panel data description. 1072 observations and 31 variables per columns. 67 countries
# are included in our sample. 16 years (2002-2017)

panel_ke_2<-panel_data_final[panel_data_final$country %in% afemerge,]
panel_ke_2 <- panel_ke_2[order(panel_ke_2$country),] # Order by alphabetical order
dim(panel_ke_2)
names(panel_ke_2)
countrylist_ke <- length(unique(panel_ke_2$country))
levels(panel_ke_2$country)
yearlist_ke <-length(unique(panel_ke_2$year))

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

# Generate patents and trademarks per capita. From the WB, we only get counts (total numbers)

kei_combined_data$patens_pc <- (kei_combined_data$patents/kei_combined_data$population)*1000
kei_combined_data$trademarks_pc <-(kei_combined_data$trademark/kei_combined_data$population)*1000

# Final version of the data in panel format. Ready to estimate. Save in different formats.
write.csv(kei_combined_data, file = "panel_kei.csv")
write.xlsx(kei_combined_data, file = "panel_kei.xlsx", sheetName = "data")


# Proportion of missing values 20 % of missing values. Mostly missing values for
# innovation variables, KEI, etc. This plot reflects absolute numbers

anyNA(kei_combined_data)
table(is.na(kei_combined_data))
prop.table(table(is.na(kei_combined_data)))*100

#Visualizing missing values

gg_miss_var(kei_combined_data[, c(3:35)])
# Check missing values per variable (absolute and percentage). Below, we have the
# proportion of missing values per variable

library(naniar)
miss_var_summary (kei_combined_data[, c(3:35)]) %>% 
  print(n = 30)


# Missing values per country (total numbers)
kei_combined_data %>%
  split(.$country) %>%
  map_df(n_miss) %>%
  gather(key = "country",
         value = "n_miss") %>% 
  print(n = 76)

           