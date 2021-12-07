library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(RColorBrewer)

data <- read.csv(".\\Datasets\\crocwatch-crocodile-sightings-2010-2015.csv")

### DATA WRANGLING ###

# Renaming relevant variables
# Focusing on relevant variables
# Converting data types from char to numeric/date
# Removing null values
# Converting yes/no columns to true/false
# Removing data errors
data_cleaned <- data%>%rename(long=longitude, lat=latitude, date=Date.Sighted, 
                              species=Species.freshwater.Crocodylus.johnstoni...estuarine...Crocodylus.porsus., 
                              meters=size.of.largest.crocodile..m...estimate.,
                              exposed=all.parts.of.crocodile.exposed,
                              threat=Location.or.crocodile.behaviour.is.a.threat,
                              amount=number.of.crocodiles)%>%
  select(long, lat, date, species, meters, exposed, threat, amount)%>%
  mutate(long=as.numeric(long), lat=as.numeric(lat), meters=as.numeric(meters),amount=as.numeric(amount), date=as.Date(date, "%m/%d/%y"))%>%
  filter(species != "unknown", species != "no data", species != "No data", species != "Unknown", exposed != "n/a", exposed != "unknown", exposed != "no data", threat != "n/a")%>%
  drop_na()%>%
  mutate(exposed=ifelse(exposed == "yes" | exposed == "Yes", T, F), threat=ifelse(threat == "yes" | threat == "Yes", T, F))%>%
  filter(long < 154, lat > -30, amount > 0, meters < 7)

summary(data_cleaned)

### GRAPHS ###

# Stacked bar graph of threatening sightings based on crocodile species
ggplot(data=data_cleaned, mapping=aes(x=species, fill=threat)) + geom_bar(position = "fill") +
  labs(title="Percentage of Threatening Encounters in Relation to Crocodile Species", x="Species", y="Percentage", fill="Threat") +
  scale_fill_brewer(palette="Set2") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggsave(filename="./Graphs/Percentage of Threatening Encounters in Relation to Crocodile Species.png")

# Stacked bar graph of threatening sightings based on if fully exposed
ggplot(data=data_cleaned, mapping=aes(x=exposed, fill=threat)) + geom_bar(position = "fill") +
  labs(title="Percentage of Threatening Encounters by Exposure", x="Fully Exposed", y="Percentage", fill="Threat") +
  scale_fill_brewer(palette="Set2") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename="./Graphs/Percentage of Threatening Encounters by Exposure.png")

# Time series plot of amount of threats over time.
# References: https://stackoverflow.com/questions/14549433/count-rows-by-date, 
# https://kohske.wordpress.com/2010/12/27/faq-geom_line-doesnt-draw-lines/
# https://stackoverflow.com/questions/22058393/convert-a-numeric-month-to-a-month-abbreviation/22058435
time_data <- data_cleaned%>%select(date, threat)%>%filter(threat==T)%>%
  mutate(month=format(date, "%m"), year=format(date, "%Y"))%>%group_by(month, year)%>%
  summarize(threats=n())%>%mutate(month=as.numeric(month), month=factor(month.abb[month],levels=month.abb))
ggplot(data=time_data, aes(x=month, y=threats, group=1)) + geom_line(color="red") + geom_point() +
  labs(title="2020 Threats per Month", x="Month", y="Total Threats") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename="./Graphs/2020 Threats per Month.png")

# Box plot of species and their length in meters.
ggplot(data=data_cleaned, aes(x=species,y=meters, fill="dark green")) + geom_boxplot(show.legend=F) +
  labs(title="Distribution of Length by Species", x="Species", y="Length (meters)") +
  scale_fill_brewer(palette="Set2") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename="./Graphs/Distribution of Length by Species.png")

# Box plot of species and their length in meters, while also taking into account whether that species was a threat
ggplot(data=data_cleaned, aes(x=species,y=meters, fill=threat)) + geom_boxplot() +
  labs(title="Distribution of Length by Species and Threatening Encounters", x="Species", y="Length (meters)", fill="Threat") +
  scale_fill_brewer(palette="Set2") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename="./Graphs/Distribution of Length by Species and Threatening Encounters.png")

# Map of Australia. Reference: https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/
worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
australia_df <- worldmap%>%filter(name == 'Australia')
australia_cropped <- st_crop(australia_df, xmin = 138, xmax = 154, ymin = -30, ymax = 8)
australia_map <- ggplot() + geom_sf(data = australia_cropped) + theme_bw()

# Creating Heat Map. Reference: https://axelhodler.medium.com/creating-a-heat-map-from-coordinates-using-r-780db4901075/
# Couldn't figure out how to fix legends. Edited the photo of graph in paint to make the legends more visually appealing.
australia_map  +
  stat_density2d(data=data_cleaned,  aes(x=long, y=lat, fill=..level.., alpha=..level..), geom="polygon", show.legend=F) +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral"))) +
  labs(title="Distribution of Total Sightings Across Queensland", x="Longitude", y="Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename="./Graphs/Distribution of Total Sightings Across Queensland.png")

# Plots sightings on the map. Size is based on amount of crocodiles spotted 
australia_map +
  geom_point(data=data_cleaned,  aes(x=long, y=lat), color='dark green', size=data_cleaned$amount, alpha=.5) +
  labs(title="Map of Sightings and Amount of Crocodiles per Sighting", x="Longitude", y="Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename="./Graphs/Map of Sightings and Amount of Crocodiles per Sighting.png")

# Plots sightings on the map. Size is based on the amount of crocodiles spotted. 
# Color is based on whether it was threatening (red) or non-threatening(blue)
threat <- data_cleaned%>%filter(threat == T)
non_threat <- data_cleaned%>%filter(threat == F)
australia_map +
  geom_point(data=threat,  aes(x=long, y=lat), color='red', size=threat$amount, alpha=.5) +
  geom_point(data=non_threat,  aes(x=long, y=lat), color='dark blue', size=non_threat$amount, alpha=.5) +
  labs(title="Map of Threatening an Non-threatening Sightings", x="Longitude", y="Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename="./Graphs/Map of Threatening an Non-threatening Sightings.png")

### REGRESSION MODELS ###

# Linear Regression to predict length based on coordinates, species, and whether it is exposed or a threat
lm_data <- data_cleaned%>%select(-date, -amount)

library(MASS)

lm_model <- lm(meters~., lm_data)
summary(lm_model)

lm_AICmodel <- stepAIC(lm_model, direction="both",  trace=F)
summary(lm_AICmodel)

detach(package:MASS,unload=TRUE)

# Logistic Regression to predict whether a crocodile is a threat based on coordinates, species, whether 
# it is exposed, size, and the amount of crocodiles spotted.

glm_data <- data_cleaned%>%select(-date)%>%mutate(threat=ifelse(threat, 1, 0))

library(MASS)

glm_model <- glm(threat~., glm_data, family="binomial")
summary(glm_model)
exp(coef(glm_model))

glm_AICmodel <- stepAIC(glm_model, direction="both",  trace=F)
summary(glm_AICmodel)
exp(coef(glm_AICmodel))

detach(package:MASS,unload=TRUE)
