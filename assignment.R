# loads the required libraries 
library("countrycode")
library("dplyr")
library("httr")
library("jsonlite")
library("knitr")
library("ggplot2")
library("tidyr")
library("mapdata")
library("plotly")

emissions_data = read.csv("data/WDI_emissions_Data.csv", stringsAsFactors = FALSE)
countries_data = read.csv("data/WDI_selected_Data.csv", stringsAsFactors = FALSE)

countries_gdp = read.csv("data/UNdata_Export_20180505_025626051.csv", stringsAsFactors = FALSE)

# Using only CO2 emissions (metric tons per capita) and CO2 emissions (kt) data
reqd_data <- emissions_data %>% filter(Series.Code == "EN.ATM.CO2E.KT" | Series.Code == "EN.ATM.CO2E.PC")

# Removing rows with NA values
reqd_data <- na.omit(reqd_data)

# Adding countries name from the other data source
countries_abbv <- countries_data %>% select(Country.Code, Country.Name) %>% unique()
reqd_data <- reqd_data %>% inner_join(countries_abbv, by = "Country.Code")

countries_gdp <- spread(countries_gdp, Year, Value) %>% select(-Item)

reqd_data <- reqd_data %>% inner_join(countries_gdp, by = c("Country.Name" = "Country.or.Area"))

# Cleaning column headers
reqd_data <- reqd_data %>%
  rename_at(vars(starts_with('YR')), funs(sub('YR', 'EM', .)))
reqd_data <- reqd_data %>% 
  rename_at(vars(starts_with('19')), funs(paste0('GDP', .))) 
reqd_data <- reqd_data %>% 
  rename_at(vars(starts_with('20')), funs(paste0('GDP', .))) 

# Adding Continent Names
reqd_data <- reqd_data %>% mutate(Continent_Name = countrycode(Country.Name, 'country.name', 'continent') )

# Data for the first visualization
carbon_gdp_2014 <- reqd_data %>% select(Country.Name, Continent_Name, Series.Code, EM2014, GDP2014) %>% spread(Series.Code, EM2014)

# Plotting Carbon Emissions vs GDP (2014)

ggplot1 <- ggplot(carbon_gdp_2014, aes(x = GDP2014, y = EN.ATM.CO2E.PC)) + geom_point(aes(size = EN.ATM.CO2E.KT, color = Continent_Name)) + labs(title = "Carbon Emissions vs GDP (2014)", x = "Gross Domestic Product", y = "Carbon Emissions Per Capita", size = "Total Emissions", color = "Continents")

# Plotting Carbon Emissions vs GDP (Continent Correlation)
ggplot2 <- ggplot(carbon_gdp_2014, aes(x = GDP2014, y = EN.ATM.CO2E.PC)) + geom_point(aes(size = EN.ATM.CO2E.KT)) + geom_smooth(method = "lm", se = FALSE, color = "red") + labs(title = "Carbon Emissions vs GDP (Continent Correlation)", x = "Gross Domestic Product", y = "Carbon Emissions Per Capita", size = "Total Emissions") + facet_grid(~Continent_Name)

# Prepping data for the Choropleth
map_df <- reqd_data %>% mutate(ISO = iso.alpha(Country.Name, n = 3)) %>% mutate(Emission.Factors = cut(Most_Recent, 5)) %>% select(ISO = Country.Code, Emission.Factors)
world <- map_data("world")
world <- world %>% mutate(ISO = iso.alpha(region, n = 3)) %>% left_join(map_df, by = "ISO")

# Plotting the Choropleth
ggplot3_emission.choropleth <- ggplot(data=world) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Emission.Factors)) +
  theme_bw() + labs(title = "Chloropleth Map", x = "Longitudes", y = "Latitudes", fill = "Carbon Emission Per Capita") + 
  coord_equal()
 
# Setting up data for interactive choropleth
df <- carbon_gdp_2014 %>% mutate(Code = iso.alpha(Country.Name, n = 3))

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

# plotting the choropleth
ggplot4_interactive_choropleth <- plot_geo(df) %>%
  add_trace(
    z = ~EN.ATM.CO2E.KT, color = ~EN.ATM.CO2E.KT, colors = 'Greens',
    text = ~Country.Name, locations = ~Code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Carbon Emission', ticksuffix = ' KT') %>%
  layout(
    title = 'Carbon Emissions <br> Source:<a href="(http://databank.worldbank.org/data/home.aspx">World Bank</a>',
    geo = g
  )

