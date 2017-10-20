data <- read.csv("/home/aaron/policedata/Seattle_Police_Department_911_Incident_Response.csv")
library(dplyr)

unique(data$Event.Clearance.Group)

bikedata <- data %>% filter(data$Event.Clearance.Group == "BIKE")

library(ggplot2)
library(ggmap)
head(bikedata, n = 1)

map_seattle <- get_map(location = c(lon = mean(bikedata$Longitude), lat = mean(bikedata$Latitude)), 
                       zoom = 13, 
                       maptype="terrain", 
                       scale = 2)
library(tidyr)
bikedata2 <- bikedata %>% separate(Event.Clearance.Date, c("DATE","TIME"), " ") %>% 
             separate(DATE,c("MM","DD","YYYY"),"/") %>% filter(YYYY >= 2015) %>% 
             droplevels()

write.csv(bikedata2, "/home/aaron/policedata/biketheft.csv", row.names = FALSE)

ggmap(map_seattle) + 
  geom_point(data=bikedata2[which(bikedata2$YYYY == 2015 & 
                                  bikedata2$MM == "07"),], 
              aes(x = Longitude, y = Latitude),
              fill = "red",
              alpha = 0.5, 
              size = 4,
              shape = 21) +
  scale_fill_brewer(palette = "Paired")

ggmap(map_seattle) + 
  geom_point(data=bikedata2[which(bikedata2$YYYY == 2015),], 
             aes(x = Longitude, y = Latitude, fill = MM),
             alpha = 0.5, 
             size = 4,
             shape = 21)
