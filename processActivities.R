library(dplyr)
library(ggplot2)
library(leaflet)

myActivities = readRDS('myActivities.RDS')
# a = myActivities[[5]]

# Get the easy stuff for now
get = c("id", "name", "distance", "moving_time", "elapsed_time", "total_elevation_gain",
        "type", "start_date", "start_date_local", "start_latitude", "start_longitude",
        "has_heartrate", "average_heartrate", "max_heartrate", "elev_high", "elev_low",
        "workout_type")
stats = 
    lapply(myActivities, function(a) {
    
        # ID the filename for .gpx
        nums = gsub('[^0-9]', '', a$start_date)
        gpxFile = paste0(substr(nums, 1, 8), '-', substr(nums, 9, 14), '-', a$type, '.gpx')    

        got = structure(lapply(get, function(i) a[[i]]), names = get)
        got[sapply(got, is.null)] = NA
        got
        data.frame(got, gpx = gpxFile)
    })
stats = do.call(rbind, stats)
str(stats)
stats[nrow(stats), ]
max(as.Date(stats$start_date))
# stats$miles = stats$distance * 0.000621371
# stats$ft_gain = stats$total_elevation_gain * 3.28

# pal <- colorFactor(c("navy", "red"), domain = c("pirate", "ship"))

# leaflet(df) %>% addTiles() %>%
#   addCircleMarkers(
#     radius = ~ifelse(type == "ship", 6, 10),
#     color = ~pal(type),
#     stroke = FALSE, fillOpacity = 0.5
#   )

# colFx = colorFactor(
#   RColorBrewer::brewer.pal(length(unique(stats$type)), name = "Set3"),
#   stats$type
# )

tri = stats[stats$type %in% c("Swim", "Run", "Ride") & !is.na(stats$start_latitude), ]
tri$type = droplevels(tri$type)
triCols = RColorBrewer::brewer.pal(4, "Pastel1")[c(3, 4, 2)][as.integer(tri$type)]
triSize = ifelse(tri$type == "Ride", tri$distance / 2e4,
                 ifelse(tri$type == "Run", tri$distance / 5e3,
                        tri$distance / 5e2))

tri$start_latitude = tri$start_latitude + rnorm(nrow(tri), 0, .001) 
tri$start_longitude = tri$start_longitude + rnorm(nrow(tri), 0, .001) 

# This works! Save images for faster loading, maybe make new ones based on Strava's color scheme
# Images: https://github.com/pointhi/leaflet-color-markers
# And size to relative distance by sport!
ico = makeIcon(iconUrl = "https://camo.githubusercontent.com/70c53b19fb9ec32c09ff59b4aebe6bb8058dfb8b/68747470733a2f2f7261772e6769746875622e636f6d2f706f696e7468692f6c6561666c65742d636f6c6f722d6d61726b6572732f6d61737465722f696d672f6d61726b65722d69636f6e2d7265642e706e673f7261773d74727565",
               iconWidth = 25, iconHeight = 41,
               iconAnchorX = 13, iconAnchorY = 41,
               shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
               popupAnchorX = 0.01, popupAnchorY = -41
               )

# Size markers proportional to ironman distances, but with a minimum
setSF = function(x, ll = 8, ul = 26.2)  min(max(ll, x), ul)
metersToMiles = function(x) x / 1609.34
milesToMeters = function(x) x * 1609.34
scaleTo = c(Run = setSF(max(tri$distance[tri$type == "Run"]), milesToMeters(12), milesToMeters(26.2)),
            Ride = setSF(max(tri$distance[tri$type == "Ride"]), milesToMeters(50), milesToMeters(112)),
            Swim = setSF(max(tri$distance[tri$type == "Swim"]), 2500, 3800))
scaleFactor = unname((tri$distance / scaleTo[tri$type])^.8)
heights = 32 * scaleFactor
widths = 21 * scaleFactor

triIcons = icons(
  iconUrl = c("assets/icons/run.png", "assets/icons/bike.png", "assets/icons/swim.png")[tri$type],
  iconWidth = widths, iconHeight = heights,
  iconAnchorX = widths / 2, iconAnchorY = heights,
  # shadowUrl = "assets/icons/shadow2.png",
  # shadowWidth = floor(widths), shadowHeight = floor(heights),
  # shadowAnchorX = widths / 4, shadowAnchorY = heights,
  popupAnchorX = .01, popupAnchorY = - (heights + 1))

tri %>%
  leaflet() %>%
  # addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(lng = ~ start_longitude, lat = ~ start_latitude
             # , options = markerOptions(riseOn = TRUE, riseOffset = 10000)
             , popup = ~ paste(name, "--", round(distance * 6.21e-4, 1), "miles")
             , icon = triIcons
  )


# Race performance
runs = stats[stats$type == "Run", ]
races = runs[which(runs$workout_type == 1), ]

raceDistance = sort(c("5k" = 5e3, "10k" = 10e3, "25k" = 25e3, "50k" = 5e4, "100k" = 1e5,
                      milesToMeters(c("mile" = 1, "5 miler" = 5, "10 miler" = 10, 
                                      "half marathon" = 13.1, "marathon" = 26.2, 
                                      "50 miler" = 50, "100 miler" = 100))))

tagRaces = function(rDist, rCand, buffer) {
  cuts = unlist(lapply(sort(raceDistance), function(x) c(x - x * buffer, x + x * buffer)))
  if(!all.equal(cuts, sort(cuts))) stop("Overlapping intervals. Reduce the buffer.")
  dists = names(cuts)[cut(rDist, breaks = cuts, labels = FALSE, ordered_result = TRUE)]
  ifelse(sapply(dists, function(x) substr(x, nchar(x), nchar(x))) == "2", 
         NA, 
         sapply(dists, function(x) substr(x, 1, nchar(x) - 1)))
}
races$raceType = factor(tagRaces(races$distance, raceDistance, .05),
                        levels = names(raceDistance), ordered = TRUE)

prs = 
  filter(races, !is.na(raceType)) %>%
  group_by(raceType) %>%
  summarise(time = min(elapsed_time))
prs$distance = raceDistance[match(prs$raceType, names(raceDistance))]


races = 
  right_join(prs, races, by = "raceType") %>%
  filter(!is.na(raceType)) %>% 
  filter(raceType %in% names(table(raceType)[table(raceType) > 1]))

ggplot(races, aes(x = as.Date(start_date_local), y = elapsed_time / 60)) +
  geom_point(aes(color = time == elapsed_time), size = 2) +
  facet_wrap(~ raceType, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "firebrick"), name = "PR") +
  ylab("Time (minutes)") + xlab("Date")

prs$speed = prs$distance / prs$time
prs$pace = (prs$time / 60) / metersToMiles(prs$distance)

library(broom)
m = lm(speed ~ distance + I(distance^2), prs)
prs = broom::augment(m, prs)

speedToPace = function(x) metersToMiles(x)^-1 / 60

# Blue is lm, red is with distance^2
ggplot(prs, aes(x = distance, y = speed)) +
  geom_point() +
  geom_point(aes(y = .fitted), color = "red") +
  scale_x_continuous(name = "", breaks = prs$distance, labels = prs$raceType) +
  scale_y_continuous(name = "Pace",
                     labels = function(x) round(speedToPace(x), 1)) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle = -45, vjust = 1, hjust = 0)) +
  geom_smooth(method = "lm", se = FALSE)

# Cameron formula from http://www.runningforfitness.org/faq/rp
ab = function(dist) 13.49681 - 0.048865 * dist + 2.438936 / (dist * 0.7905)
timeHat = function(oTime, oDist, nDist) (oTime / oDist) * (ab(oDist) / ab(nDist)) * nDist
timeHat(1177, metersToMiles(5e3), metersToMiles(10e3))

# Only predict from distances run more than once:
predFrom = names(table(races$raceType)[table(races$raceType) > 1])
# Or just the four main distances; might restrict predicteds to this too:
predTo = predFrom = c("5k", "10k", "half marathon", "marathon")
preds = 
  lapply(which(prs$raceType %in% predTo), function(j) 
    sapply(which(prs$raceType %in% predFrom), function(i) 
      timeHat(prs$time[i], metersToMiles(prs$distance)[i], metersToMiles(prs$distance[j]))
    )) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  structure(names = paste0("predFrom:", predFrom)) %>%
  cbind(., raceType = predTo) %>%
  left_join(prs, ., by = "raceType")

filter(preds, raceType %in% predTo) %>%
  tidyr::gather(source, timeHat, grep("predFrom", names(preds))) %>% 
  mutate(source = factor(source, source),
         paceHat = speedToPace(distance / timeHat)) %>%
  ggplot(aes(x = distance, y = paceHat)) + 
  geom_line(aes(color = source)) + 
  geom_point(aes(y = pace)) +
  scale_x_continuous(name = "", breaks = prs$distance, labels = prs$raceType) +
  theme(axis.text.x  = element_text(angle = -45, vjust = 1, hjust = 0))