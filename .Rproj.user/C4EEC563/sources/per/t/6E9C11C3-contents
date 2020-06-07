#Import the libraries to be used
#Tidyverse includes ggplot2
library(tidyverse)

#Import the datasets to be used
suicide_data <- read_csv("./data/suicide_data.csv")
happiness_data <- read_csv("./data/happiness_2015.csv")
consumption <- read.csv("./data/alcohol_consumption2015.csv")

#Cleaning and wrangling datasets which I am using

#Remove the variables with NA values and 0 suicides.
data <- suicide_data %>% filter(suicides_no !="NA" &  suicides_no!=0) 

#Removing HDI(Happiness Development Index) column since it's full of N/A values and later I will use Happiness 2015 dataset 
#for happiness ratings. Deleting suicides per 100k, because I had problems with them later, while
#plotting, so I was doing it manually together while groupping. Generation was taken off as well, it does not have
#much difference with age. Also GDP per year was taken off, since I used GDP per Capita instead.
data <- data[-c(7:10, 12)]

#Rename the columns for convenience and making them more visually appealing
colnames(data) <- c("Country", "Year", "Gender", "Age", "Suicides", "Population",  "GDPperCapita")

#Most of the data from 2016 countries is missing, so we just delete the year.
data <- data %>% filter(Year != 2016)

#Gathering countries to continents, which will be later used for suicides, GDP,happiness and alcohol consumption ratings
#We create a new variable named Continent and fill it with N/A values
data$Continent <- NA
#Europe
data$Continent[which(data$Country %in% c("Albania", "Austria", "Belgium", "Bosnia and Herzegovina", 
                                         "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
                                         "Denmark", "Estonia","Finland", "France", "Germany", "Greece", 
                                         "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", 
                                         "Luxembourg", "Malta", "Montenegro", "Netherlands", "Norway", 
                                         "Oman", "Poland", "Portugal", "Romania", "San Marino", "Serbia", 
                                         "Slovakia", "Slovenia", "Spain",
                                         "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom"))] <- "Europe"
#Asia
data$Continent[which(data$Country %in% c("Armenia", "Azerbaijan", "Bahrain", "Belarus", "Georgia", 
                                         "Israel", "Japan", "Kazakhstan", "Kuwait", "Kyrgyzstan", "Macau", 
                                         "Maldives", "Philippines", "Qatar", "Republic of Korea", 
                                         "Russian Federation", "Singapore", "Sri Lanka", "Thailand", "Turkmenistan",
                                         "United Arab Emirates", "Uzbekistan"))] <- "Asia"

#America (North and South together)
data$Continent[which(data$Country %in% c("Antigua and Barbuda", "Argentina", "Aruba", "Bahamas", "Barbados", 
                                         "Belize", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica",
                                         "Cuba", "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana", 
                                         "Jamaica", "Mexico", "Panama", "Paraguay", "Puerto Rico", "Saint Lucia",
                                         "Saint Vincent and Grenadines", "Suriname", "Trinidad and Tobago", 
                                         "United States", "Uruguay"))] <- "America"
#Africa
data$Continent[which(data$Country %in% c("Cabo Verde", "Mauritius", "Nicaragua", "Seychelles", "South Africa"))] <- "Africa"

#Oceania
data$Continent[which(data$Country %in% c("Australia", "Fiji", "Kiribati", "New Zealand"))] <- "Oceania"

#Now I am going to tidy the data for alcohol consumption
#Removing country codes and year columns, since all the data is only from 2015 and I will not be using Year as variable
consumption <- consumption[-c(2,3)]

#Renaming columns for convenience
colnames(consumption) <- c("Country", "LitresPerCapita")

#Rounding the Alcohol consumption column up to two decimals
consumption[-1] <- round(consumption[,-1], 2)

#Start mergin the datasets to be used as one. First I get the data from the main suicides datasets. I take only 2015 year data,
# country name, continent of the country and we create variable suicides_per_100k, rounded up to two decimals.
alcohol_data <- data %>% filter(Year == 2015) %>%
  select(Country, Continent, Suicides, Population) %>%
  group_by(Country, Continent) %>% summarise(suicides_per_100k = round((sum(Suicides) / sum(Population)) * 100000, 2))

#Create final alcohol consumption dataset by merging consumption dataset and main dataset which contains continents and suicide per 100k.
#Majority of the countries will be missing eventually, because they weren't common between datasets or didn't have values.
alcohol_data <- merge(alcohol_data, consumption, by="Country")

#Clean the happiness dataset so it only includes happiness ratings. Dataset is only from 2015 year as                                    
happiness_data <- happiness_data[-c(2,3,5:12)]

#Renaming variables for convenience
colnames(happiness_data) <- c("Country", "HappinessScore")

#Get the final dataset which will be used for happiness, alcohol consumption and suicide correlations.

alcohol_happiness <- merge(alcohol_data, happiness_data, by="Country")


#Global average rate over time
global_average <- (sum(as.numeric(data$Suicides)) / sum(as.numeric(data$Population))) * 100000


#START PLOTTING

#As for upcoming plots, I am grouping the data and counting records within each group, since I am
#plotting time series data

#Global suicides by year and suicides per 100k.
data %>% group_by(Year) %>% summarize(suicides_per_100k= sum(Suicides) / sum(Population) * 100000) %>%
  ggplot(aes(x = Year, y = suicides_per_100k)) + geom_line(colour = "#56B4E9", size = 1) + 
  geom_point(colour = "#000000", size = 2) + labs(title = "Global Suicides", subtitle ="1985-2015", x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) 

#Global suicides by continent by year and suicides per 100k
data %>% group_by(Year, Continent) %>% summarize(suicides_per_100k= sum(Suicides) / sum(Population) * 100000) %>%
  ggplot(aes(x = Year, y = suicides_per_100k, colour = Continent)) + 
  geom_line() + 
  geom_point() + 
  labs(title = "Suicide rates per continent",  subtitle="1985-2015",
       x = "Year", 
       y = "Suicides per 100k", 
       colour = "Continent") + 
  scale_x_continuous(breaks = seq(1985, 2015,2))

#Changes during the year by continents, to see if there is increase or decrease.
data %>% group_by(Year, Continent) %>% summarize(suicides_per_100k= sum(Suicides) / sum(Population) * 100000) %>%
  ggplot(aes(x = Year, y = suicides_per_100k, colour = Continent)) + 
  geom_point() + geom_smooth(method = "lm") + facet_wrap(~ Continent) + theme(legend.position = "none") + 
  labs(title="Changes during the years", subtitle="1985-2015", 
       x = "Year", 
       y = "Suicides per 100k") + scale_x_continuous(breaks = seq(1985, 2015,5)) +
      scale_y_continuous(breaks = seq(0, 25,5))


#Suicides by gender
data %>% group_by(Year, Gender) %>% summarize(suicides_per_100k= sum(Suicides) / sum(Population) * 100000) %>%
  ggplot(aes(x = Year, y = suicides_per_100k, group = Gender, colour= Gender)) + geom_line(size=1) + geom_point(size=3) + 
  labs(title = "Global Suicides by Gender", subtitle="1985 - 2015",   x = "Year", y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) 


#Suicides by gender, just with facet
data %>% group_by(Year, Gender) %>%
  summarize(suicide_per_100k = (sum(Suicides) / sum(Population)) * 100000) %>%
  ggplot(aes(x = Year, y = suicide_per_100k, colour = Gender)) + facet_grid(Gender~., scales = "free_y") + geom_line() + 
  geom_point() + labs(title = "Suicides by Gender", subtitle = "1985-2015",
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Gender")+
  scale_x_continuous(breaks = seq(1985, 2015, 2))


#Making age as ordinal variables as I will need them in Age Groups plot.
data$Age <- factor(data$Age, 
                   levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))


#Global suicides by Age Groups
data %>% group_by(Year, Age) %>% summarize(suicides_per_100k= sum(Suicides) / sum(Population) * 100000) %>%
  ggplot(aes(x = Year, y = suicides_per_100k, group = Age, colour= Age)) + geom_line(size=1) + geom_point(size=3) +
  geom_smooth(method = "lm")+
  labs(title = "Global Suicides by Age Groups", subtitle="1985 - 2015",   x = "Year", y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) 


#Now let's count how many suicides per 100k each country has averaging and put them in seperate dataset
country_data <-data %>%
  select(Country,Suicides, Population, ) %>%
  group_by(Country) %>% summarise(suicide_per_100k = round((sum(Suicides) / sum(Population)) * 100000, 2))


#Suicides by country per 100k
ggplot(country_data,aes(x = reorder(Country, suicide_per_100k), y = suicide_per_100k,fill = Country)) + 
  geom_bar(stat = 'identity', show.legend = F) + 
  coord_flip() +labs(title = 'Suicides by country',subtitle = '1985 - 2015',x = 'Country',
                     y = 'Suicides per 100k people')

#Suicides and GDP correlation by continents

data %>% group_by(Country, Continent) %>%
  summarize(suicides_per_100k = (sum(suicides_per_100k= sum(Suicides) / sum(Population))) * 100000, 
            GDPperCapita = mean(GDPperCapita)) %>% ggplot(aes(x = GDPperCapita, y = suicides_per_100k, col = Continent)) + 
  geom_point() + geom_smooth(method = "lm", aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 70000, 10000)) + 
  scale_y_continuous(breaks = seq(0, 35, 5)) +
  labs(title = "Ratio between GDP per Capita and Suicides",
       x = "GDP per Capinta($)", 
       y = "Suicides per 100k", 
       col = "Continent") 


#Take top 15 countries by suicide ratings
top15_suicides <- alcohol_happiness[alcohol_happiness$suicides_per_100k %in% tail(sort(alcohol_happiness$suicides_per_100k),15),]

#Take the top 15 countries by alcohol consumption
top15_drinking <- alcohol_happiness[alcohol_happiness$LitresPerCapita %in% tail(sort(alcohol_happiness$LitresPerCapita),15),]

#Highest suicide count having countries 2015
ggplot(top15_suicides,aes(x = reorder(Country, suicides_per_100k), y = suicides_per_100k,fill = Country)) + 
  geom_bar(stat = 'identity', show.legend = F) + 
  coord_flip() +labs(title = '15 highest suicide count having countries',subtitle = '2015',x = 'Country',
                     y = 'Suicides per 100k people')

#Highest alcohol consumption having countries 2015
ggplot(top15_drinking,aes(x = reorder(Country, LitresPerCapita), y = LitresPerCapita,fill = Country)) + 
  geom_bar(stat = 'identity', show.legend = F) + 
  coord_flip() +labs(title = '15 most consuming alcohol countries',subtitle = '2015',x = 'Country',
                     y = 'Litres per person')


#Alco consumption per country 2015       
ggplot(alcohol_happiness,aes(x = reorder(Country, LitresPerCapita), y = LitresPerCapita,fill = LitresPerCapita)) + 
         geom_bar(stat = 'identity', show.legend = F) + coord_flip() +labs(title = "Alchohol consumption per country",subtitle = '2015 year',x = 'Country',
                            y = 'Litres per Person')       
#Alco and suicides correlation between countries 2015 
ggplot(alcohol_happiness,aes(x = reorder(Country, suicides_per_100k), y = suicides_per_100k,fill = LitresPerCapita)) + 
         geom_bar(stat = 'identity', show.legend = T) + 
         coord_flip() +labs(title = 'Alcohol and Suicide correlation per country',subtitle = '2015',x = 'Country',
                            y = 'Suicides per 100k people')


#Alco and suicides correlation between countries 2015 
ggplot(alcohol_happiness, aes(x =LitresPerCapita , y = suicides_per_100k, col = Country)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", aes(group = 1)) + geom_text(aes(label=Country), size=2,color="black")+
  labs(title = "Correlation between alcohol consumption and Suicides per 100k", 
       subtitle = "Year2015",
       x = "Litres of alcohol per capita", 
       y = "Suicides per 100k", 
       col = "Country") + theme(legend.position="none")


#Happiest countries by happiness score
ggplot(alcohol_happiness,aes(x = reorder(Country, HappinessScore), y = HappinessScore,fill = HappinessScore)) + 
  geom_bar(stat = 'identity', show.legend = F) + coord_flip() +labs(title = "Highest happiness score having countries",subtitle = '2015 year',x = 'Country',
                                                                    y = 'Happiness Score') 

#Hapiness score and suicides correlation
ggplot(alcohol_happiness,aes(x = reorder(Country, suicides_per_100k), y = suicides_per_100k,fill = HappinessScore)) + 
  geom_bar(stat = 'identity', show.legend = T) + 
  coord_flip() +labs(title = 'Happiness and suicide correlation per country',subtitle = 'Year 2015',x = 'Country',
                     y = 'Suicides per 100k people')

#Happiness and suicides correlation between countries 2015 
ggplot(alcohol_happiness, aes(x =HappinessScore , y = suicides_per_100k, col = Country)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", aes(group = 1)) + geom_text(aes(label=Country), size=2,color="black")+
  labs(title = "Correlation between happiness and suicides per 100k", 
       subtitle = "Year 2015",
       x = "Happiness Score", 
       y = "Suicides per 100k", 
       col = "Country") + theme(legend.position="none")
