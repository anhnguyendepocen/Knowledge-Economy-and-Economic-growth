library(tidyverse)  
library(naniar) # Missing-values package
library(corrplot)  #Generate correlation matrix
library(GGally)  # Generate histograms, scatter plots, etc

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


# Variables included in the dataset

names(panel_ke)

# Build up a dictionary and labels for variables

library(Hmisc)

var.labels <- c(iso3c = "Country code", year = "Year", country = "country name", fdi = "Foreign Direct Investment (% GDP)", irspread = "Interest rate spread", domcredit = "Domestic credit to private sector", articles ="Number of scientific articles", patents = "Number of patents",  
                inflation = "inflation as measured by the annual growth rate of the GDP implicit deflator", pop_growth = "Population growth", population = "Total population", hdi = "human development index", trade = "Sum of exports plus imports relative to GDP", 
                High_Exports = "High Technology % of manufactured exports", tariff = "Simple mean applied tariff", trademark = "trademark applications", mobile = "Mobile cellular subscriptions per 100 people", telephone = "Fixed telephone subscriptions per 100 people", 
                internet = "Internet (Individuals using the internet as a % of population)", investment = "gross fixed capital formation relative to GDP", gdppc = " Real GDP per capita",
                primary = "Gross enrollment ratio, primary", secondary = "Gross enrollment ratio, secondary", tertiary = "Gross enrollment ratio, tertiary", exp_educ = "public spending on education", Voice_Account = "Voice and accountability", Polit_Stability = "Political stability", 
                Govern_Effect = "Government effectiveness", Rule_of_Law = "Rule of law", Cont_corruption = "Control of corruption", Reg_qual = "Regulatory quality")

panel_ke <- upData(panel_ke, labels = var.labels)
label(panel_ke)
contents(panel_ke)

# Countries and years included in the sample. 67 countries, and 58 years

countrylist <-length(unique(panel_ke$country))
yearlist <- length(unique(panel_ke$year))

# Generate a summary of the complete dataset, inspecting missing values

summary(panel_ke)

#  Check Missing values. 35 % of missing values

anyNA(panel_ke)
table(is.na(panel_ke))
prop.table(table(is.na(panel_ke)))*100


# Check missing values per variable (absolute and percentage)

library(naniar)
miss_var_summary (panel_ke) %>% 
  print(n = 28)



# Missing visualizations

gg_miss_var(panel_ke[, c(4:30)])


# summarise for all economic variables per country, generating a data frame

my_summary_eco <- panel_ke %>% 
  group_by(country) %>% 
  summarise_at(.vars = vars(fdi, hdi, gdppc, inflation, trade, tariff, High_Exports, investment, domcredit),
               .funs = c(mean="mean"), na.rm =TRUE) 

my_summary_inst<-panel_ke %>% 
  group_by(country) %>% 
  summarise_at(.vars = vars(Govern_Effect, Voice_Account, Rule_of_Law, Polit_Stability, Cont_corruption, Reg_qual),
               .funs = c(mean="mean"), na.rm =TRUE) 


my_summary_educ <-panel_ke %>% 
  group_by(country) %>% 
  summarise_at(.vars = vars(primary, secondary, tertiary, exp_educ),
               .funs = c(mean ="mean"), na.rm = TRUE)

my_summary_ict <- panel_ke %>% 
  group_by (country) %>% 
  summarise_at(.vars = vars(articles, patents, mobile, telephone, trademark, internet),
               .funs = c(mean = "mean"), na.rm =TRUE)


# Visualization with ggpairs for education variables

educ_data <-panel_ke %>% 
  select(primary, secondary, tertiary, exp_educ)

ggpairs(educ_data, title= "Education variables")

economic_data<-panel_ke %>% 
  select(fdi, hdi, gdppc, inflation, trade, tariff, High_Exports, investment, domcredit)

ggpairs(economic_data, title = "Economic data")


inst_data<- panel_ke %>% 
  select(Govern_Effect, Voice_Account, Rule_of_Law, Polit_Stability, Cont_corruption, Reg_qual)

ggpairs(inst_data, title = "Institutional variables")

# Generate correlation matrix

m = cor(panel_ke[, c(4:30)], use = "pairwise.complete.obs")
par(mfrow = c(2,2))
corrplot(m[1:6, 1:6], method = "number", type = "upper", col = "black")
corrplot(m[7:13, 7:13],method = "number", type = "upper", col = "black" )
corrplot(m[14:21, 14:21], method = "number", type = "upper", col = "black")
corrplot(m[22:27, 22:27], method = "number", type = "upper", col = "black")



# Look at the time series for each country for the time period, for instance GDPPC
for (i in 1:length(countrylist)){
  currcty <- countrylist[i]
  filename <- paste("gdppc",currcty,".png",sep="")
  png(filename,width=800,height=600)
  plot(y=panel_ke$gdppc[panel_ke$country==currcty], x=panel_ke$year[panel_ke$country==currcty], type="l", ylab="GDPPC",xlab="YEAR",
       main = paste("country", currcty))
  dev.off()
}



