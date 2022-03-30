pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)
linelist_raw <- import("./data/raw/linelist_raw.xlsx")

head(linelist_raw,50)

out1 <- skimr::skim(linelist_raw)

names(linelist_raw)

# pipe the raw dataset through the function clean_names(), assign result as "linelist"  
linelist <- linelist_raw %>% 
  janitor::clean_names()

# see the new column names
names(linelist)

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome)

linelist_raw %>% 
  select(# NEW name             # OLD name
    date_infection       = `infection date`,    # rename and KEEP ONLY these columns
    date_hospitalisation = `hosp date`)

#ERROR
linelist_raw <- openxlsx::readWorkbook("linelist_raw.xlsx", fillMergedCells = TRUE)

names(linelist)

# linelist dataset is piped through select() command, and names() prints just the column names
linelist %>% 
  select(case_id, date_onset, date_hospitalisation, fever) %>% 
  names()  # display the column names

# move date_onset and date_hospitalisation to beginning
linelist %>% 
  select(date_onset, date_hospitalisation, everything()) %>% 
  names()

# select columns that are class Numeric
linelist %>% 
  select(where(is.numeric)) %>% 
  names()

# select columns containing certain characters
linelist %>% 
  select(contains("date")) %>% 
  names()

# searched for multiple character matches
linelist %>% 
  select(matches("onset|hosp|fev")) %>%   # note the OR symbol "|"
  names()

linelist %>% 
  select(any_of(c("date_onset", "village_origin", "village_detection", "village_residence", "village_travel"))) %>% 
  names()

linelist %>% 
  select(-c(date_onset, fever:vomit)) %>% # remove date_onset and all columns from fever to vomit
  names()

linelist$date_onset <- NULL   # deletes column with base R syntax

# Create a new linelist with id and age-related columns
linelist_age <- select(linelist, case_id, contains("age"))

# display the column names
names(linelist_age)

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  #####################################################

# remove column
select(-c(row_num, merged_header, x28))

linelist <- linelist %>% 
  distinct()

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  #####################################################

# de-duplicate
distinct()

linelist <- linelist %>% 
  mutate(new_col = 10)

linelist <- linelist %>% 
  mutate(bmi = wt_kg / (ht_cm/100)^2)

new_col_demo <- linelist %>%                       
  mutate(
    new_var_dup    = case_id,             # new column = duplicate/copy another existing column
    new_var_static = 7,                   # new column = all values the same
    new_var_static = new_var_static + 5,  # you can overwrite a column, and it can be a calculation using other variables
    new_var_paste  = stringr::str_glue("{hospital} on ({date_hospitalisation})") # new column = pasting together values from other columns
  ) %>% 
  select(case_id, hospital, date_hospitalisation, contains("new"))        # show only new columns, for demonstration purposes

# HIDDEN FROM READER
# removes new demo columns created above
# linelist <- linelist %>% 
#   select(-contains("new_var"))

class(linelist$age)

class(linelist$date_onset)

linelist <- linelist %>% 
  mutate(age = as.numeric(age))

# age normalized to mean of ALL rows
linelist %>% 
  mutate(age_norm = age / mean(age, na.rm=T))

# age normalized to mean of hospital group
linelist %>% 
  group_by(hospital) %>% 
  mutate(age_norm = age / mean(age, na.rm=T))

linelist <- linelist %>% 
  mutate(across(.cols = c(temp, ht_cm, wt_kg), .fns = as.character))

#to change all columns to character class
linelist <- linelist %>% 
  mutate(across(.cols = everything(), .fns = as.character))

#to change all columns to character class
linelist <- linelist %>% 
  mutate(across(.cols = contains("date"), .fns = as.character))

linelist <- linelist %>% 
  mutate(across(.cols = where(is.POSIXct), .fns = as.Date))

sum(c(2,4,15,10))     # returns only one number

cumsum(c(2,4,15,10))  # returns the cumulative sum at each step

cumulative_case_counts <- linelist %>%  # begin with case linelist
  count(date_onset) %>%                 # count of rows per day, as column 'n'   
  mutate(cumulative_cases = cumsum(n))  # new column, of the cumulative sum at each row

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  ###################################################
# add new column
mutate(bmi = wt_kg / (ht_cm/100)^2) %>% 
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) 

table(linelist$hospital, useNA = "always")  # print table of all unique values, including missing

linelist <- linelist %>% 
  mutate(hospital = recode(hospital,
                           # for reference: OLD = NEW
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  ))

table(linelist$hospital, useNA = "always")

# Example: change gender of one specific observation to "Female" 
linelist <- linelist %>% 
  mutate(gender = replace(gender, case_id == "2195", "Female"))

linelist$gender[linelist$case_id == "2195"] <- "Female"

linelist <- linelist %>% 
  mutate(source_known = ifelse(!is.na(source), "known", "unknown"))

# Create a date of death column, which is NA if patient has not died.
linelist <- linelist %>% 
  mutate(date_death = if_else(outcome == "Death", date_outcome, NA_real_))

linelist <- linelist %>% 
  mutate(age_years = case_when(
    age_unit == "years"  ~ age,       # if age is given in years
    age_unit == "months" ~ age/12,    # if age is given in months
    is.na(age_unit)      ~ age,       # if age unit is missing, assume years
    TRUE                 ~ NA_real_)) # any other circumstance, assign missing

linelist <- linelist %>% 
  mutate(hospital = replace_na(hospital, "Missing"))

linelist %>% 
  mutate(hospital = fct_explicit_na(hospital))

# Convert temperatures above 40 to NA 
linelist <- linelist %>% 
  mutate(temp = replace(temp, temp > 40, NA))

# Convert onset dates earlier than 1 Jan 2000 to missing
linelist <- linelist %>% 
  mutate(date_onset = replace(date_onset, date_onset > as.Date("2000-01-01"), NA))

#check the class of the linelist variable age
class(linelist$age_years)

# examine the distribution
hist(linelist$age_years)

summary(linelist$age_years, na.rm=T)

# Simple example
################
pacman::p_load(epikit)                    # load package

linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(             # create new column
      age_years,                            # numeric column to make groups from
      breakers = c(0, 5, 10, 15, 20,        # break points
                   30, 40, 50, 60, 70)))

# show table
table(linelist$age_cat, useNA = "always")

# Include upper ends for the same categories
############################################
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      breakers = c(0, 6, 11, 16, 21, 31, 41, 51, 61, 71)))

# show table
table(linelist$age_cat, useNA = "always")

# With ceiling set to TRUE
##########################
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70),
      ceiling = TRUE)) # 70 is ceiling, all above become NA

# show table
table(linelist$age_cat, useNA = "always")

linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      lower = 0,
      upper = 100,
      by = 10))

# show table
table(linelist$age_cat, useNA = "always")

# Create new variable, by cutting the numeric age variable
# lower break is excluded but upper break is included in each category
linelist <- linelist %>% 
  mutate(
    age_cat = cut(
      age_years,
      breaks = c(0, 5, 10, 15, 20,
                 30, 50, 70, 100),
      include.lowest = TRUE         # include 0 in lowest group
    ))

# tabulate the number of observations per group
table(linelist$age_cat, useNA = "always")

linelist <- linelist %>% 
  
  # cut() creates age_cat, automatically of class Factor      
  mutate(age_cat = cut(
    age_years,
    breaks = c(0, 5, 10, 15, 20, 30, 50, 70, 100),          
    right = FALSE,
    include.lowest = TRUE,        
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-49", "50-69", "70-100")),
    
    # make missing values explicit
    age_cat = fct_explicit_na(
      age_cat,
      na_level = "Missing age")  # you can specify the label
  )    

# table to view counts
table(linelist$age_cat, useNA = "always")

quantile(linelist$age_years,               # specify numeric vector to work on
         probs = c(0, .25, .50, .75, .90, .95),   # specify the percentiles you want
         na.rm = TRUE)                            # ignore missing values 

linelist %>%                                # begin with linelist
  mutate(deciles = cut(age_years,           # create new column decile as cut() on column age_years
                       breaks = quantile(                      # define cut breaks using quantile()
                         age_years,                               # operate on age_years
                         probs = seq(0, 1, by = 0.1),             # 0.0 to 1.0 by 0.1
                         na.rm = TRUE),                           # ignore missing values
                       include.lowest = TRUE)) %>%             # for cut() include age 0
  janitor::tabyl(deciles)                   # pipe to table to display

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  
  # add column
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>%     
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) %>% 
  
  # add column: delay to hospitalisation
  mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>% 
  
  # clean values of hospital column
  mutate(hospital = recode(hospital,
                           # OLD = NEW
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  )) %>% 
  
  mutate(hospital = replace_na(hospital, "Missing")) %>% 
  
  # create age_years column (from age and age_unit)
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age,
    TRUE ~ NA_real_)) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  ###################################################   
mutate(
  # age categories: custom
  age_cat = epikit::age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 50, 70)),
  
  # age categories: 0 to 85 by 5s
  age_cat5 = epikit::age_categories(age_years, breakers = seq(0, 85, 5)))

linelist <- linelist %>% 
  filter(gender == "f")   # keep only rows where gender is equal to "f"

linelist %>% 
  drop_na(case_id, age_years)  # drop rows with missing values for case_id or age_years

# View first 100 rows
linelist %>% head(100)     # or use tail() to see the n last rows

# Show row 5 only
linelist %>% filter(row_number() == 5)

# View rows 2 through 20, and three specific columns
linelist %>% filter(row_number() %in% 2:20) %>% select(date_onset, outcome, age)

hist(linelist$date_onset, breaks = 50)

table(Hospital  = linelist$hospital,                     # hospital name
      YearOnset = lubridate::year(linelist$date_onset),  # year of date_onset
      useNA     = "always")                              # show missing values

linelist <- linelist %>% 
  # keep rows where onset is after 1 June 2013 OR where onset is missing and it was a hospital OTHER than Hospital A or B
  filter(date_onset > as.Date("2013-06-01") | (is.na(date_onset) & !hospital %in% c("Hospital A", "Hospital B")))

nrow(linelist)

table(Hospital  = linelist$hospital,                     # hospital name
      YearOnset = lubridate::year(linelist$date_onset),  # year of date_onset
      useNA     = "always")                              # show missing values

# dataframe <- filter(dataframe, condition(s) for rows to keep)

linelist <- filter(linelist, !is.na(case_id))

View(linelist)

linelist %>%
  rowwise() %>%
  mutate(num_symptoms = sum(c(fever, chills, cough, aches, vomit) == "yes")) %>% 
  ungroup() %>% 
  select(fever, chills, cough, aches, vomit, num_symptoms) # for display

linelist %>%
  rowwise() %>%
  mutate(num_NA_dates = sum(is.na(c_across(contains("date"))))) %>% 
  ungroup() %>% 
  select(num_NA_dates, contains("date")) # for display

linelist %>%
  rowwise() %>%
  mutate(latest_date = max(c_across(contains("date")), na.rm=T)) %>% 
  ungroup() %>% 
  select(latest_date, contains("date"))  # for display

linelist %>% 
  arrange(hospital, desc(date_onset))
