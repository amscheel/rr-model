### A script for intermediate data wrangling steps I did to clean up/create the
# eventual data sets


## DATA FOR COMPETITION

data_competition_m1 <- readRDS(here("data", "simdata_settings3.RData"))
data_competition_m23468 <- readRDS(here("data", "simdata_settings3_3.RData"))
data_competition_m16 <- readRDS(here("data", "simdata_settings3_4.RData"))

# data_competition_m248 contains the conditions m == 3 and m == 6, which
# we don't need any more (focussing only on 1, 2, 4, 8, 16). 
# Delete rows with m == 3 and m == 6:
data_competition_m248 <- data_competition_m23468[data_competition_m23468$generation_duration!=3 & data_competition_m23468$generation_duration!=6,]

# Now bind the dataframes with m = 1, m = c(2, 4, 8) and m = 16 together
data_competition <- rbind(data_competition_m1, data_competition_m248)
data_competition <- rbind(data_competition, data_competition_m16)

# Save the new data file as the main data file for competition
saveRDS(data_competition, "data_competition.RData")

# data_competition also contains all the information we'll use to study
# the effect of m alone (i.e., with zero competition --> relative_top_n == 1)
# We'll create a new df with only those results and save it
data_comp <- readRDS(here("data", "data_competition.RData"))
data_m <- data_comp[data_comp$relative_top_n==1,]
saveRDS(data_m, "data_m.RData")


