# Animating run data from Strava
# Coder: Rick Klein https://github.com/raklein

# Assembled thanks to these tutorials:
# https://github.com/fawda123/rStrava
# http://www3.econ.muni.cz/~137451/files/ggplot2.html
# https://padpadpadpad.github.io/post/animate-your-strava-activities-using-rstrava-and-gganimate/
# https://github.com/thomasp85/gganimate/wiki/Tracking-of-hurricanes-and-typhoons
# https://github.com/ropenscilabs/learngganimate/blob/master/shadow_wake.md

#install.packages('devtools')
#devtools::install_github('fawda123/rStrava')
#devtools::install_github("dkahle/ggmap")
#devtools::install_github('thomasp85/gganimate')
#install.packages("gifski") # may be necessary to use gganimate properly

library("rStrava")
library("tidyverse")
library("lubridate")
library("gganimate")
library("ggmap")
library("wesanderson") # color palette
library("Cairo") # better looking renders

# PRIVATE keys you'll have to input yourself -- DON'T SHARE THEM once you do!
# Should all be free, except google maps may cost a few pennies
# Strava API: https://developers.strava.com/
# and Google Maps API: https://developers.google.com/places/web-service/get-api-key
app_name <- 'XXXXXXXXXXX' # name of app on Strava
app_client_id  <- 'XXXXX' # an integer, assigned by Strava
myid <- "XXXXXXXX" # another integer you can find on your Strava profile
app_secret <- 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' # an alphanumeric secret, assigned by Strava
goog_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" # from your own Google API account

# I'm going to call these from a file I store locally, for privacy
# This line shouldn't run for anyone else, but you can also store your keys here
source("./data_keys/keys.R")

# First create the strava authentication token
# A browser should pop-up asking you to login to Strava and approve
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret))

# Get basic athlete info
myinfo <- get_athlete(stoken, id = myid)
head(myinfo)

# Register your google key
register_google(goog_key)

# Get list of all activities on this profile 
my_acts <- get_activity_list(stoken)
my_acts

# Get data for all activities 
act_data <- compile_activities(my_acts)

# Pull detail of a single run, in this case the most recent.
strms_data <- get_activity_streams(my_acts, stoken, acts = 1)

# Note to self: you have a couple runs from Lyon (id == 2411726291 and id == 275734838)
# Keep this in mind or it will mess up your latitude/longitude for maps.

# Calculate coordinates (latitude/longitude) to center map on
lat_center <- median(strms_data$lat)
lng_center <- median(strms_data$lng)

# Download map based on center of lat/long
# NOTE: Due to Google API changes you may need to enable billing
# (see https://github.com/dkahle/ggmap)
# However it should be free, or cheap
# (see https://developers.google.com/maps/documentation/maps-static/usage-and-billing)
# At writing, a call to the Static Maps API costs 0.002 USD, and this script
# makes ~2 calls from start to finish. So, you'd have to run it 250 times to incur a $1 cost.
# (HOWEVER -- this also means you should keep your Google Maps API key safe)

map <- get_map(location = c(lon = lng_center, lat = lat_center),
               zoom = 14, # may have to adjust this
               source = 'google',
               maptype = 'terrain')

# Plot that map
ggmap(map)

# Use ggplot to draw our run on top
singlerun_plot <- ggmap(map) +
  geom_path(data=strms_data, aes(x = lng, y = lat, color = altitude))

singlerun_plot

# Animate the lines based on time in the run
singlerun_anim <- singlerun_plot + transition_reveal(time)

# Generate the animation, specifying some optional arguments for quality
gganimate::animate(singlerun_anim, type="cairo", width = 640, height = 640)

# Save the animation if you want
# This works by saving the last rendered animation in "plots"
anim_save("singlerun_anim.gif")

### Now let's plot and animate all runs at the same time
# Fetch data
strms_data_all <- get_activity_streams(my_acts, stoken)

# Calculate coordinates (latitude/longitude) to center map on using all runs
lat_center <- median(strms_data_all$lat)
lng_center <- median(strms_data_all$lng)

# Download map using new center
map <- get_map(location = c(lon = lng_center, lat = lat_center), 
               zoom = 14, # may have to adjust this
               source = 'google', 
               maptype = 'terrain')

# Plot
allruns_plot <- ggmap(map) +
  geom_path(data=strms_data_all, aes(x = lng, y = lat, color = as.factor(id))) +
  theme(legend.position = "none") 

allruns_plot

# Add code to animate that
allruns_anim <- allruns_plot + transition_reveal(time)

# Render
gganimate::animate(allruns_anim, type="cairo", width = 640, height = 640)

# Save
anim_save("allruns_anim.gif")

# Problem: Sometimes I stop my Strava run and start a new one to time myself on certain legs
# (e.g., record my run to the bastille separately from my run back from the bastille)
# This makes the runs start from everywhere, actually they all start at my place

# Tidy the data to correctly lump these separate "runs" together
alldata <- inner_join(strms_data_all, act_data, by = 'id') # Merge in meta data
alldata$start_date <- as_datetime(alldata$start_date) # convert str to date
alldata$timestamp <- alldata$start_date + alldata$time # calculate timestamp
alldata <- arrange(alldata, timestamp) # order by timestamp

alldata <- alldata %>% 
  group_by(date(start_date)) %>% # group the df based on the day of the run
  mutate(time_cumulative = timestamp - min(start_date)) # time since run started

alldata$time_cumulative <- as.numeric(alldata$time_cumulative) # change class

# for visual inspection it's sometimes easier to view only every nth row
tmp <- alldata[seq(1, nrow(alldata), 60),] 
tmp <- select(tmp, id, time, timestamp, start_date, time_cumulative)

# A couple runs look like they lost GPS or something: ~36, ~25

# Reanimate with new time variable and some aesthetic improvements
allruns_anim2 <- ggmap(map) +
  geom_path(data=alldata,
            aes(x = lng, y = lat, group = id, color = as.factor(date(start_date))),
            size = 1.25,
            alpha = 0.5) +
  theme(legend.position = "none") +
  # gganimate below
  transition_reveal(time_cumulative)

# Render
gganimate::animate(allruns_anim2, type="cairo", width = 640, height = 640)

# Save
anim_save("allruns_anim2.gif")

# # If you want, you can crop the figure by adding something like this:
### COMMENTED OUT: This code gets screwed up by the runs from Lyon --
# specifically the min() and max() calls. But, you could work around that.
# allruns_anim2_cropped <- ggmap(map) +
#   geom_path(data=alldata,
#             aes(x = lng, y = lat, group = id, color = as.factor(date(start_date))),
#             size = 1.25,
#             alpha = 0.5) +
#   theme(legend.position = "none") +
#   scale_x_continuous(limits = c(min(alldata$lng), max(alldata$lng)), expand = c(0, 0)) +
#   scale_y_continuous(limits = c(min(alldata$lat), max(alldata$lat)), expand = c(0, 0)) +
#   # gganimate below
#   transition_reveal(time_cumulative)
# 
# # Render
# gganimate::animate(allruns_anim2_cropped, type="cairo", width = 640, height = 640)
# 
# # Save
# anim_save("allruns_anim2_cropped.gif")

# Problem: Can't see the return trips easily. 
# Plot with trailing shadow instead?

# New color palette too, let's borrow from Wes Anderson
# Need to specify the number of colors needed, so this just counts the unique 
# start dates.
nruns <- length(unique(date(alldata$start_date)))

# Specify color pallette from the wesanderson package
pal <- wes_palette("Zissou1", n = nruns, type = "continuous")

################################
## Final, beautiful runs plot
################################
# So, now we're working with the color palette from The Life Aquatic with Steve Zissou (2004)
# and animating all our runs at once as little 'slug-like' objects. The bluer the 
# run the older it is, with red runs being most recent.

allruns_anim3 <- ggmap(map) +
  geom_point(data=alldata,
            aes(x = lng, y = lat, group = as.factor(date(start_date)), color = as.factor(date(start_date))),
            size = 5,
            alpha = 1.0) +
  theme(legend.position = "none") +
  scale_color_manual(values = pal) +
  # gganimate below
  transition_reveal(time_cumulative) +
  shadow_wake(wake_length = .05)

# Add some settings to the render to make it higher quality
gganimate::animate(allruns_anim3, detail = 5, type = "cairo", fps = 20, nframes = 400, width = 640, height = 640) #highqual

# Save
anim_save("allruns_anim3.gif")

# Finally, Because most of the image is static, you could massively reduce size
# with little loss in quality by uploading it e.g. here: https://ezgif.com/optimize 
# and selecting 'Optimization method:  Optimize Transparency'