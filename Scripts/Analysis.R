library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tigris)
library(httr)
library(broom)
library(rgdal)

data <- read.csv(".\\Datasets\\2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")

### DATA CLEANING ###

data.cleaned <- data%>%select(X, Y)

### GRAPHS ###

# Data set for map of New York. Reference: https://rpubs.com/jhofman/nycmaps
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

# Focusing on Central Park, New York
central_park_map <- ggplot() + geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), color="black", fill="lightgray") +
  coord_map(xlim = c(-73.985, -73.945), ylim = c(40.765, 40.8))

# Creating Heat Map. Reference: https://axelhodler.medium.com/creating-a-heat-map-from-coordinates-using-r-780db4901075"
# Couldn't figure out how to fix legends. Edited the photo of graph to make the legends more visually appealing.
central_park_map +
  stat_density2d(data=data.cleaned,  aes(x=X, y=Y, fill=..level.., alpha=..level..), geom="polygon") +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
  
