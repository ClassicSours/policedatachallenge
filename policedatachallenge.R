library(dplyr)
library(ggplot2)
library(ggmap)
library(tidyr)


data <- read.csv("/home/aaron/policedata/Seattle_Police_Department_911_Incident_Response.csv")
bikedata <- data %>% filter(data$Event.Clearance.Group == "BIKE")
map_seattle <- get_map(location = c(lon = mean(bikedata$Longitude), lat = mean(bikedata$Latitude)), 
                       zoom = 11, 
                       maptype="terrain", 
                       scale = 2)

# head(bikedata, n = 1)

# bikedata2 <- bikedata %>% separate(Event.Clearance.Date, c("DATE","TIME"), " ") %>% 
#              separate(DATE,c("MM","DD","YYYY"),"/") %>% filter(YYYY >= 2015) %>% 
#              droplevels()

write.csv(bikedata2, "/home/aaron/policedata/biketheft.csv", row.names = FALSE)
bikedata <- read.csv("/home/aaron/policedata/biketheft.csv")
map_seattle <- get_map(location = c(lon = mean(bikedata$Longitude), lat = mean(bikedata$Latitude)), 
                       zoom = 13, 
                       maptype="terrain", 
                       scale = 2)

ggmap(map_seattle) + 
  geom_point(data=bikedata[which(bikedata2$YYYY == 2015 & 
                                  bikedata2$MM == "07"),], 
              aes(x = Longitude, y = Latitude),
              fill = "red",
              alpha = 0.5, 
              size = 4,
              shape = 21) +
  scale_fill_brewer(palette = "Paired")

ggmap(map_seattle) + 
  geom_point(data=bikedata[which(bikedata2$YYYY == 2015),], 
             aes(x = Longitude, y = Latitude, fill = MM),
             alpha = 0.5, 
             size = 4,
             shape = 21)

zoning <- read.csv("/home/aaron/policedata/census10.csv")

ggplot(zoning, aes(x = INTPTLON10, y = INTPTLAT10, fill = as.factor(TRACT))) +
  geom_polygon() +
  coord_map()

library(maptools)
kingcounty.shp <- maptools::readShapeSpatial("/home/aaron/policedata/WGS84/City_of_Seattle_Zoning.shp")

library(broom)
kingcounty <- broom::tidy(kingcounty.shp)

kingcounty.data <- kingcounty.shp@data
kingcounty.data$id <- "0"
for (i in c(1:nrow(kingcounty.data))) {
  kingcounty.data$id[i] <- kingcounty.shp@polygons[[i]]@ID
}

kingcounty.all <- left_join(kingcounty,kingcounty.data, by = c("id" = "id"))
kingcounty.all$ZONEING <- NA
kingcounty.all$ZONEING[grep(pattern = "downtown", ignore.case = TRUE, x = kingcounty.all$ZONELUT_DE )] <- "Downtown"
kingcounty.all$ZONEING[grep(pattern = "commercial", ignore.case = TRUE, x = kingcounty.all$ZONELUT_DE )] <- "Commercial"
kingcounty.all$ZONEING[grep(pattern = "industrial", ignore.case = TRUE, x = kingcounty.all$ZONELUT_DE )] <- "Industrial"
kingcounty.all$ZONEING[grep(pattern = "rise", ignore.case = TRUE, x = kingcounty.all$ZONELUT_DE )] <- "Rise"
kingcounty.all$ZONEING[grep(pattern = "neighborhood", ignore.case = TRUE, x = kingcounty.all$ZONELUT_DE )] <- "Neighborhood"
kingcounty.all$ZONEING[grep(pattern = "residential", ignore.case = TRUE, x = kingcounty.all$ZONELUT_DE )] <- "Residential"
kingcounty.all$ZONEING[grep(pattern = "mixed", ignore.case = TRUE, x = kingcounty.all$ZONELUT_DE )] <- "Mixed"

library(gridExtra)

a <- ggplot() + 
  geom_polygon(data = kingcounty.all, aes(x = long, y = lat, group = group, fill = ZONELUT_DE), color = "black") + 
  coord_map()

b <- ggplot() + 
  geom_polygon(data = kingcounty.all, aes(x = long, y = lat, group = group, fill = ZONEING), color = "black") + 
  coord_map() + 
  scale_fill_discrete(na.value = NA)

grid.arrange(a,b,ncol=2)

ggplot() + 
  geom_polygon(data = kingcounty, aes(x = long, y = lat, group = group, fill = as.factor(id)), color = "black") + 
  coord_map() + 
  theme(legend.position = "none")

ggplot() + 
  geom_polygon(data = kingcounty, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  geom_point(data=bikedata[which(bikedata$YYYY == 2015),],
             aes(x = Longitude, y = Latitude, fill = as.factor(MM)),
             alpha = 0.5,
             size = 1,
             shape = 21) +
  coord_map() + theme(legend.position = "none")

library(mgcv)

## ?in.out
kingcounty.bnd <- do.call(rbind, lapply(split(kingcounty.all, kingcounty.all$id), 
                      function(x) rbind(x,
                                        within(x[nrow(x),], {lat <- NA; long <- NA}))))[,c("lat","long","id","ZONEING")]

in.out(as.matrix(kingcounty.bnd[,c("lat","long")]), as.matrix(bikedata[,c("Latitude","Longitude")]))

coordinates(bikedata) <- ~ Longitude + Latitude
proj4string(bikedata) <- proj4string(bikedata)

coordinates(kingcounty.all) <- ~ long + lat
proj4string(kingcounty.all) <- proj4string(kingcounty.all)

over.table <- over(kingcounty.all,SpatialPoints(bikedata))
bikedata <- cbind(bikedata, over(kingcounty.all,bikedata))

