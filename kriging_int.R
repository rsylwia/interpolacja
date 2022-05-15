library(sf)
library(stars)
library(tmap)
library(geostatbook)
library(rgdal)
library(sf)
library(dismo)
library(gstat)
library(dismo)
library(fields)
library(pgirmess)
library(ggplot2)
library(raster)

bb_data <- read_sf(dsn = "D:/studia/GEOINFORMATYKA/dane/granica.shp")
pp_data <-read_sf(dsn = "D:/studia/GEOINFORMATYKA/magisterka/daen_21_03/magister/pm10_2020_poprawne.shp")

granica1 = read_sf(dsn = "granica.shp")
granica = st_as_sf(granica1)

punkty1 = read_sf(dsn = "D:/studia/GEOINFORMATYKA/magisterka/daen_21_03/magister/pm10_2020_poprawne.shp")
punkty = st_as_sf(punkty1)

punkty_pol = st_transform(punkty, 4326)
granica_pol = st_transform(granica, 4326)

st_bbox(punkty_pol)
punkty_bbox = st_bbox(granica_pol)
nowa_siatka2 = st_as_stars(punkty_bbox, 
                           dx = 0.01,
                           dy = 0.01)

nowa_siatka2 = st_set_crs(nowa_siatka2, "EPSG:4326")
nowa_siatka2 <- st_crop(nowa_siatka2, granica_pol)


vario = variogram(sred_stycz~ 1, locations = punkty1)
model = vgm(60, model = "Sph", range = 320000 , nugget = 80)
model
plot(vario, model = model)

sk <- krige(sred_stycz ~ 1, 
            locations = punkty_pol,
            newdata = nowa_siatka2,
            model = model,
            beta = '35')
