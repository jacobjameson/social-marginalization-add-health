#-------------------------------------------------------------------------
# AUTHOR:             Jacob Jameson
# PURPOSE:            Construct Wave 1 Networks
#-------------------------------------------------------------------------
#
# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "haven", 'scales', 'reshape', 'igraph',
          'centiserve', 'labelled')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

# Data paths ------------------------------------------------------------
data_path <- '~/Add Health Data'
inschool_path <-  paste0(data_path, 
                         '/Wave I In-School Questionnaire Data')

# Link in-school and in-home IDs ----------------------------------------
inschool <- read_xpt(paste0(inschool_path, '/Inschool.xpt'))
inschool <- inschool[,c('AID', 'SQID', 'SSCHLCDE')]

inschool <- inschool %>%
  filter(SQID != '', AID != '')

friend.df <- read_xpt(paste0(data_path,
                             '/Wave I In-School Friendship Nominations/sfriend.xpt'))

friend.df <- friend.df %>%
  filter(SQID != '999999')

friend.df <- merge(friend.df, inschool, by='SQID')
friend.df[] <- lapply(friend.df, as.character)

# Rename variables lowercase ------------------------------------------------
names(friend.df) <- tolower(names(friend.df))

# Clear environment --------------------------------------------------------
rm(list=setdiff(ls(), 'friend.df')) # 85,627 observations


# Clear environment --------------------------------------------------------

id.replace <- c('77777777', '99999999', '88888888', '99959995')

friend.df <- friend.df %>%
  mutate_at(vars(starts_with("mf"), 
                 starts_with("ff")), 
            ~ ifelse(. %in% id.replace, NA, .))

# How many friend nominations does each person receive? --------------------
aid_friends <- friend.df %>%
  pivot_longer(cols = starts_with("mf"), 
               names_to = "col", 
               values_to = "aid_val") %>%
  bind_rows(friend.df %>%
              pivot_longer(cols = starts_with("ff"),
                           names_to = "col", 
                           values_to = "aid_val")) %>%
  filter(!is.na(aid)) %>%
  count(aid_val) %>% 
  select(aid = aid_val, friend_noms = n)


# How many best friend nominations does each person receive?  --------------
aid_bff <- friend.df %>%
  pivot_longer(cols = starts_with("mf1"), 
               names_to = "col", 
               values_to = "aid_val") %>%
  bind_rows(friend.df %>%
              pivot_longer(cols = starts_with("ff1"),
                           names_to = "col", 
                           values_to = "aid_val")) %>%
  filter(!is.na(aid)) %>%
  count(aid_val) %>% 
  select(aid = aid_val, bff_noms = n)


# Best friend reciprocity ---------------------------------------------------



