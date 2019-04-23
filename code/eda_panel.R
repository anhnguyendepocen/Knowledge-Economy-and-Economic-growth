library(tidyverse)  
library(naniar) # Missing-values package

# Load the dataset

panel_data_final <- read.table("../Knowledge-Economy-and-Economic-growth/data/panel_data_full2.csv", header = TRUE, sep = ",", fill = TRUE)
panel_data_final$X <- NULL

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

#  Check Missing values. 41 % of missing values

anyNA(panel_ke)
table(is.na(panel_ke))
prop.table(table(is.na(panel_ke)))*100


# Check missing values per variable (absolute and percentage)

library(naniar)
miss_var_summary (panel_ke) %>% 
  print(n = 26)

# Check missing values per year

# For time period 1960-1970

panel_ke %>% 
  group_by(year) %>% 
  filter(year<=1970) %>% 
  summarise_at(4:25, ~sum(!is.na(.))) %>% 
  print(n = 30)

# Period 1970-1980

panel_ke %>% 
  group_by(year) %>% 
  filter(year>1970 & year<=1980) %>% 
  summarise_at(4:25, ~sum(!is.na(.))) %>% 
  print(n = 30)

# Period 1980-1990
panel_ke %>% 
  group_by(year) %>% 
  filter(year>1980 & year<=1990) %>% 
  summarise_at(4:25, ~sum(!is.na(.))) %>% 
  print(n = 30)

# Period 1990-2000
panel_ke %>% 
  group_by(year) %>% 
  filter(year>1990 & year <=2000) %>% 
  summarise_at(4:25, ~sum(!is.na(.))) %>% 
  print(n = 30)




# Missing visualizations

gg_miss_var(panel_ke[, c(4:30)])


# low method to determine which countries are increasing/decreasing

panel_ke %>%
    group_by(country) %>% 
  summarise(m_hdi=mean(hdi, na.rm = T)) %>% 
  arrange(desc(m_hdi))

panel_ke %>%
  group_by(country) %>% 
  summarise(m_trade=mean(trade, na.rm = T)) %>% 
  arrange(desc(m_trade))


panel_ke %>% 
  group_by(country) %>% 
  summarise(m_internet = mean(internet, na.rm =TRUE)) %>% 
  arrange(desc(m_internet))

# Using ggplot

panel_ke %>% 
  select(Country, Year, hdi) %>% 
ggplot(aes(x = Year, y = hdi)) + geom_point() +
  ggtitle("Human Development Index") + facet_wrap(~Country)




countrylist<- unique(panel_ke$Country)
n<-length(countrylist)



# Look at the time series for each country, for instance GDPPC
for (i in 1:length(countrylist)){
  currcty <- countrylist[i]
  filename <- paste("gdppc",currcty,".png",sep="")
  png(filename,width=800,height=600)
  plot(y=panel_ke$gdppc[panel_ke$Country==currcty], x=panel_ke$Year[panel_ke$Country==currcty], type="l", ylab="GDPPC",xlab="YEAR",
       main = paste("Country", currcty))
  dev.off()
}



