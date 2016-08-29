library(httr)
source("secretAuth.R")  # Auth token assigned to auth

# First time only:
if(FALSE) {
    # start before I started logging. 
    latest = as.integer(as.POSIXct('2009-01-01', "%Y-%m-%d", tz = "UTC"))
    # Create list to store activities
    myActivities = list()
} else {
    myActivities = readRDS('myActivities.RDS')
    latest = max(as.integer(as.POSIXct(sapply(myActivities, '[[', 'start_date'), "%Y-%m-%dT%H:%M:%S", tz = "UTC")))
}
# Loop until no more activities to get
while(TRUE) {
    # get activities, newer than the former newest
    newActivities = GET('https://www.strava.com/api/v3/athletes/4386652/activities', 
                        add_headers(Authorization = auth), 
                        query = list(per_page = '200', after = latest)) %>%
        content(as = 'parsed')
    # If nothing came back, we're done
    if(length(newActivities) == 0)
        break
    # otherwise, add the new ones to the list
    myActivities = c(myActivities, newActivities)
    # and update the newest activity on record
    latest = max(as.integer(as.POSIXct(sapply(myActivities, '[[', 'start_date'), "%Y-%m-%dT%H:%M:%S", tz = "UTC")))
    # and print a progress message
    cat('\nGot', length(myActivities), 'activities ranging from', 
        paste(range(sapply(myActivities, '[[', 'start_date')), collapse = " to "))
}

# Check:
# tmp = data.frame(date = as.Date(sapply(myActivities, '[[', 'start_date')) )
# ggplot(tmp, aes(date)) + geom_density(adjust = .01)
saveRDS(myActivities, 'myActivities.RDS')
