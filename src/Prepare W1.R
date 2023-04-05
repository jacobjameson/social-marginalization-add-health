#-------------------------------------------------------------------------
# AUTHOR:             Jacob Jameson
# PURPOSE:            Construct and Prepare Wave 1
#-------------------------------------------------------------------------
#
# load packages ----------------------------------------------------------
libs <- c("tidyverse", "haven", 'scales')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

# Data paths ------------------------------------------------------------
data_path <- '~/Add Health Data'
inhome_path <-  paste0(data_path, '/Wave I In Home Interview Data')
weights_path <-  paste0(data_path, '/Wave I In-Home Weights')

# Load the wave 1 data and wave 1 weights -------------------------------
allwave.1 <- read_xpt(paste0(inhome_path, '/allwave1.xpt'))
homeweights.1 <- read_xpt(paste0(weights_path, '/Homewt1.xpt'))

# Merge wave 1 data with the weights ------------------------------------
wave.1 <- merge(allwave.1, homeweights.1, by='AID')

# Rename variables lowercase --------------------------------------------
names(wave.1) <- tolower(names(wave.1))

# Remove data no longer using -------------------------------------------
rm(allwave.1, homeweights.1)

# There are 20,745 observations in the wave I in-home survey

#-------------------------------------------------------------------------
# Create variables that will be used in the analysis
#
#   - bmi: 
#   - race: 
#-------------------------------------------------------------------------
#

# construct bmi variable -------------------------------------------------

wave.1 <- wave.1 %>%
  mutate(height.meters = ifelse(
    (h1gh59a == 4 | h1gh59a == 5 | h1gh59a == 6) &
    (h1gh59b >= 0 & h1gh59b <= 11),
    (h1gh59a*12 + h1gh59b)*0.0254, NA),
         weight.kg = ifelse(h1gh60 >= 50 & h1gh60 <= 450, 
                            h1gh60*0.453592, NA),
         bmi = weight.kg/(height.meters^2))

# determine age at time of survey ---------------------------------------

wave.1$h1gi1m <- ifelse(wave.1$h1gi1m == 96, NA, wave.1$h1gi1m)
wave.1$h1gi1y <- ifelse(wave.1$h1gi1m == 96, NA, wave.1$h1gi1y + 1900)

wave.1$bday <- as.Date(paste0(wave.1$h1gi1y, '-', wave.1$h1gi1m, '-1'))

wave.1$survey.date <- as.Date(paste0(wave.1$iyear + 1900, '-', 
                                     wave.1$imonth, '-', wave.1$iday))

wave.1$age.m <- as.numeric(difftime(wave.1$survey.date, 
                                    wave.1$bday, units = "days")) / 30.44

wave.1$age.m  <- round(wave.1$age.m) + 0.5

# Overweight was defined as a body mass index (BMI) 
# (calculated as weight in kilograms divided by the square of height in meters) 
# greater than the 95th percentile for age and sex

# https://www.cdc.gov/growthcharts/html_charts/bmiagerev.htm#males

bmi.table.f <- readxl::read_xlsx('src/data/BMI Tables.xlsx',
                                 sheet = "Females, 2-20 years") %>%
  rename(age.m = `Age (in months)`) %>%
  mutate(bio_sex = 2)

bmi.table.m <- readxl::read_xlsx('src/data/BMI Tables.xlsx',
                                 sheet = "Males, 2-20 years") %>%
  rename(age.m = `Age (in months)`) %>%
  mutate(bio_sex = 1)

bmi.table <- rbind(bmi.table.f, bmi.table.m)

wave.1 <- merge(wave.1, bmi.table, by =c('bio_sex', 'age.m'), all.x=T)

# determine overweight and obese ------------------------------------------

wave.1 <- wave.1 %>%
  mutate(overweight = case_when(
                      bmi >= `90th Percentile BMI Value` ~ 1,
                      is.na(bmi) == T ~ NA,
                      TRUE ~ 0),
         obese = case_when(
                      bmi >= `95th Percentile BMI Value` ~ 1,
                      is.na(bmi) == T ~ NA,
                      TRUE ~ 0))


# construct race variable -------------------------------------------------

wave.1 <- wave.1 %>% 
  mutate(race = case_when(
    h1gi4 == 1 ~ "Hispanic",
    h1gi6a == 1 & (h1gi6b != 1 & h1gi6c != 1 & h1gi6d != 1 & h1gi6e != 1) ~ 
      "Non Hispanic White",
    h1gi6b == 1 & (h1gi6a != 1 & h1gi6c != 1 & h1gi6d != 1 & h1gi6e != 1) ~ 
      "Non Hispanic Black/Af-Am",
    h1gi6c == 1 & (h1gi6a != 1 & h1gi6b != 1 & h1gi6d != 1 & h1gi6e != 1) ~ 
      "Multiple Races or Other",
    h1gi6d == 1 & (h1gi6a != 1 & h1gi6b != 1 & h1gi6c != 1 & h1gi6e != 1)  ~ 
      "Non Hispanic Asian/Pacific Islander",
    h1gi6e == 1 ~ 
      "Multiple Races or Other",
    TRUE ~ 
      "Multiple Races or Other"
  ))

wave.1$race <- factor(wave.1$race,
                      levels = c("Non Hispanic White", "Non Hispanic Black/Af-Am",
                                 "Non Hispanic Asian/Pacific Islander",
                                 "Hispanic",
                                 "Multiple Races or Other"),
                      labels = c("Non Hispanic White", "Non Hispanic Black/Af-Am",
                                 "Non Hispanic Asian/Pacific Islander",
                                 "Hispanic",
                                 "Multiple Races or Other"))

