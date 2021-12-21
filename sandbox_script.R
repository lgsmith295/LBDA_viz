######
#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")

library(raster)
library(sf)
library(sp)
library(ncdf4)
library(tidyverse)
library(ggthemes)
library(gganimate)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(extrafont)
loadfonts(device = "win", quiet = TRUE)

states <- ne_states(country = c("United States of America", "mexico", "canada"),  returnclass = "sf")
class(states)

states <- states %>%
  filter(name != "Alaska",
         name != "Hawaii")

borders <- ne_countries(country = c("United States of America", "mexico", "canada"), returnclass = "sf")

NADA <- nc_open("data/nada_hd2_cl.nc")

NADA

pdsi <- ncvar_get(NADA, "pdsi") 
pdsi[pdsi == -99.999] <- NA

lat <- NADA$dim$lat$vals
lon <- NADA$dim$lon$vals
time <- NADA$dim$time$vals

years <- c(1845:1875) + 1




for(i in 1: length(years)){
  yr <- years[i]
  pdsi1 <- pdsi[yr, , ]
  rast <- raster(ncol = length(lon), nrow = length(lat),
                 crs = "+proj=utm +zone=48 +datum=WGS84",
                 xmn = min(lon), xmx = max(lon),
                 ymn = min(lat), ymx = max(lat),
                 vals = pdsi1[nrow(pdsi1):1, ])
  if(i == 1){
    stck <- stack(rast)
  } else {
    stck <- stack(stck, rast)
  }
}


pdsi_chk <- pdsi[1875, , ]
rast_chk <- raster(ncol = length(lon), nrow = length(lat),
                   crs = "+proj=utm +zone=48 +datum=WGS84",
                   xmn = min(lon), xmx = max(lon),
                   ymn = min(lat), ymx = max(lat),
                   vals = pdsi_chk[nrow(pdsi_chk):1, ])
 
all.equal(stck$layer, rast_chk, values = TRUE)



mydf <- purrr::map_dfr(
  as.list(stck), 
  ~setNames(as.data.frame(as(., "SpatialPixelsDataFrame")), c('PDSI', 'x', 'y')),
  .id = "year_id"
)

mydf$year_id <- as.integer(mydf$year_id)
index <- c(1:length(years))
key <- data.frame(cbind(years, index))

mydf <- left_join(mydf, key, by = c('year_id' = "index"))
mydf$title <- factor(ifelse(mydf$years < 1856 | mydf$years > 1865, " ", "Civil War Drought"))




g <- ggplot(data = mydf, aes(x = x, y = y, fill = PDSI))  +
  geom_tile() +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = states, fill = "transparent", color = "grey45", inherit.aes = FALSE) +
  geom_sf(data = borders, fill = "transparent", color = "grey33", inherit.aes = FALSE) +
  ggthemes::theme_map() +
  theme(legend.key.width = unit(.70,"cm"), legend.position = c(.10, .10))


g

gganim <- g +  transition_states(years, transition_length = 1, state_length = 0) +
  labs(title = 'Year: {closest_state}') +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  geom_text(aes(y = 35, label = title),
            x = -60, check_overlap = TRUE)


animate(gganim, fps = 10)


anim_save("Namerica_anim.gif", animation = last_animation())


###### Clipped to just the continental US #####
rm(list = ls(all.names = TRUE))

us_states <- ne_states(country = c("United States of America"),  returnclass = "sf")

us_states <- us_states %>%
  filter(name != "Alaska",
         name != "Hawaii")

NADA <- nc_open("data/nada_hd2_cl.nc")

pdsi <- ncvar_get(NADA, "pdsi") 
pdsi[pdsi == -99.999] <- NA

lat <- NADA$dim$lat$vals
lon <- NADA$dim$lon$vals
time <- NADA$dim$time$vals

years <- c(1850:1870)


for(i in 1: length(years)){
  yr <- years[i]
  pdsi1 <- pdsi[yr, , ]
  rast <- raster(ncol = length(lon), nrow = length(lat),
                 crs = "+proj=utm +zone=48 +datum=WGS84",
                 xmn = min(lon), xmx = max(lon),
                 ymn = min(lat), ymx = max(lat),
                 vals = pdsi1[nrow(pdsi1):1, ])
  if(i == 1){
    stck <- stack(rast)
  } else {
    stck <- stack(stck, rast)
  }
}



rast_us <- crop(stck, extent(us_states))
rast_us <- mask(rast_us, us_states)


USdf <- purrr::map_dfr(
  as.list(rast_us), 
  ~setNames(as.data.frame(as(., "SpatialPixelsDataFrame")), c('PDSI', 'x', 'y')),
  .id = "year_id"
)

USdf$year_id <- as.integer(USdf$year_id)
index <- c(1:length(years))
key <- data.frame(cbind(years, index))

USdf <- left_join(USdf, key, by = c('year_id' = "index"))
USdf$title <- factor(ifelse(USdf$years < 1856 | USdf$years > 1865, " ", "Civil War Drought 1856-1865"))
USdf$desc <- factor(ifelse(USdf$years < 1856 | USdf$years > 1865, " ", "Sustained La Niña conditions in Pacific sea surface temperatures caused widespread drought in the central and western United States. This contributed to the near extinction of the Plains Bison in the 19th century."))


g <- ggplot(data = USdf, aes(x = x, y = y, fill = PDSI))  +
  geom_tile() +
  scale_fill_viridis(direction = -1, option = "B") +
  geom_sf(data = us_states, fill = "transparent", color = "grey45", inherit.aes = FALSE) +
  ggthemes::theme_map() +
  theme(legend.key.width = unit(.50,"cm"), legend.position = c(.68, .25)) +
  scale_x_continuous(limits = c(-125, -50))+
  scale_y_continuous(limits = c(15, 50)) +
  labs(title = "Living Blended Drought Atlas 1850-1870") +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  geom_segment(aes(x = -64.5, y = 29.6, xend = -64.5, yend = 35.8),
               arrow = arrow(length = unit(0.2, "cm")), 
               lineend = "round", 
               linejoin = "round", 
               size = 1.2,
               color = "midnightblue") +
  geom_segment(aes(x = -64.5, y = 29.6, xend = -64.5, yend = 24),
               arrow = arrow(length = unit(0.2, "cm")), 
               lineend = "round", 
               linejoin = "round", 
               size = 1.2,
               color = "gold") +
  geom_text(aes(y = 36, x = -59.5, label = "wetter"), 
              size = 3,
              family = "Segoe Print",
              color = "midnightblue") +
  geom_text(aes(y = 24.5, x = -59.5, label = "drier"), 
              size = 3,
              family = "Segoe Print",
              color = "gold")
  

g

gganim2 <- g +  transition_states(years, transition_length = 1, state_length = 2) +
  labs(subtitle = 'Year: {closest_state}') +
  theme(plot.subtitle = element_text(size = 14, family = "serif")) +
  geom_text(aes(y = 24, label = title),
            x = -115, check_overlap = TRUE, size = 4, fontface = "bold") +
  geom_text(aes(y = 20.5, label = str_wrap(desc)),
            x = -128, check_overlap = TRUE, size = 4, lineheight = .8, hjust = 0)

animate(gganim2, duration = 35, fps = 10, end_pause = 10)

anim_save("USA_anim.gif", animation = last_animation())



# tbl_ce <- read.table("data/namerica_pdsi_ce.txt")  %>%
#   column_to_rownames(var = "V1") %>%
#   setNames(c(1:286))
# 
# 
# tbl_recon <- read.table("data/namerica_pdsi_recs.txt")  %>%
#   column_to_rownames(var = "V1") %>%
#   setNames(c(1:286))
