library(dplyr)
library(here)
library(tidyverse)
library(lubridate)
set_here()

# Read in Normals Data
## UPDATE NORMALS PRECIP & TEMP 1

normals_precip_1 <- read.csv('normals/precip_normals_1.csv')
normals_precip_2 <- read.csv('normals/precip_normals_2.csv')
normals_precip_3 <- read.csv('normals/precip_normals_3.csv')
normals_precip_4 <- read.csv('normals/precip_normals_4.csv')
normals_precip_5 <- read.csv('normals/precip_normals_5.csv')

normals_temp_1 <- read.csv('normals/temp_normals_1.csv')
normals_temp_2 <- read.csv('normals/temp_normals_2.csv')
normals_temp_3 <- read.csv('normals/temp_normals_3.csv')
normals_temp_4 <- read.csv('normals/temp_normals_4.csv')
normals_temp_5 <- read.csv('normals/temp_normals_5.csv')

# Read in Days Data


daily_precip_1 <- read.csv('daily_data/precip_day_1.csv')
daily_precip_2 <- read.csv('daily_data/precip_day_2.csv')
daily_precip_3 <- read.csv('daily_data/precip_day_3.csv')
daily_precip_4 <- read.csv('daily_data/precip_day_4.csv')
daily_precip_5 <- read.csv('daily_data/precip_day_5.csv')

daily_temp_1 <- read.csv('daily_data/temp_day_1.csv')
daily_temp_2 <- read.csv('daily_data/temp_day_2.csv')
daily_temp_3 <- read.csv('daily_data/temp_day_3.csv')
daily_temp_4 <- read.csv('daily_data/temp_day_4.csv')
daily_temp_5 <- read.csv('daily_data/temp_day_5.csv')


#Read in frost dates
frost_dates_raw <- read.csv('frost_dates.csv')

# ------------------
# Create functions 

# 1. smooth normals Data

## Running average so that each day is the average of the two days before, the day, and two days after

smooth_normals <- function(normals_dataset){
  normals_dataset <- normals_dataset[,-1] #remove index row from CSV
  zip1 <- normals_dataset$zip1 #get zip codes
  smooth <- data.frame(zip1)
  
  for (i in 4:365){
  new_col_index <- i - 2 
  day <- i - 1
  
  a <- i-2
  b <- i-1
  c <- i
  d <- i+1
  e <- i+2
  
  df <- normals_dataset %>% select(all_of(c(a,b,c,d,e)))
  means <- rowMeans(df)
  smooth[,new_col_index] <- means
  colnames(smooth)[new_col_index] <- day
  }
  
  return(smooth)
  }

#2. subtract daily from normals data

subtract <- function(daily, smoothed_normals) {
  zip1 <- smoothed_normals$zip1 #get zip codes
  subtracted_vals <- data.frame(zip1)
  
  for (i in 3:length(smoothed_normals)){
    day <- as.character(i)
    #print(day)
    merged <- left_join(daily %>% select(zip1, day), 
                        smoothed_normals %>% select(zip1, day), 
                        by = "zip1") 
    
    merged$diff <- merged[,2] - merged[,3]
    
    subtracted_vals <- left_join(subtracted_vals, 
                                 merged %>% select(zip1, diff),
                                 by = "zip1")
    
    subtracted_vals <- subtracted_vals %>% rename_with(~ day, .cols = "diff")
  }
  return(subtracted_vals)
}

#3. filter to desired frost dates

#note this is inclusive of frost dates

filter_frost <- function(df){
  for (i in 1:nrow(df)){
    zip1 <- df$zip1[i] #get zip code
    #print(zip1)
    
    last_frost <- frost_dates %>% filter(zip_code == zip1) %>% select(last_spring_frost_doy) %>% as.numeric() #get last frost
    first_frost <- frost_dates %>% filter(zip_code == zip1) %>% select(first_fall_frost_doy) %>% as.numeric() #get first frost
    
    if (!is.na(last_frost)){
      
      if (last_frost < first_frost){
        df[i, -c(1, which(names(df) %in% as.character(last_frost:first_frost)))] <- NA
      }
      
      if(last_frost > first_frost){
        df[i, which(names(df) %in% as.character(first_frost:last_frost))] <- NA
        
      }
      
    }
  }
  return(df)
}

#4. sum of squares


sum_of_squares <- function(diffs, variance){
  for (j in 1:length(diffs)){
    diff <- data.frame(diffs[[j]])
    
    #print(diff)
    for(i in 1:nrow(diff)){
      row <- as.numeric(diff[i,2:ncol(diff)]) #extract the row
      ss <- sum(row^2, na.rm = TRUE) #calculate sum of square deviations
      year <- j+1
      variance[i,year] <- ss
    }
  }
  return(variance)
}





# ------------------
# Apply function to smooth normals data

smoothed_normals_precip_1 <- smooth_normals(normals_precip_1) 
smoothed_normals_precip_2 <- smooth_normals(normals_precip_2)
smoothed_normals_precip_3 <- smooth_normals(normals_precip_3)
smoothed_normals_precip_4 <- smooth_normals(normals_precip_4)
smoothed_normals_precip_5 <- smooth_normals(normals_precip_5)

smoothed_normals_temp_1 <- smooth_normals(normals_temp_1) 
smoothed_normals_temp_2 <- smooth_normals(normals_temp_2)
smoothed_normals_temp_3 <- smooth_normals(normals_temp_3)
smoothed_normals_temp_4 <- smooth_normals(normals_temp_4)
smoothed_normals_temp_5 <- smooth_normals(normals_temp_5)

#------------------
#Rename dailies columns

#PRECIP

daily_precip_1_renamed <- daily_precip_1 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_precip_1) - 2)), .cols = -1)

daily_precip_2_renamed <- daily_precip_2 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_precip_2) - 2)), .cols = -1)

daily_precip_3_renamed <- daily_precip_3 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_precip_3) - 2)), .cols = -1)

daily_precip_4_renamed <- daily_precip_4 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_precip_4) - 2)), .cols = -1)

daily_precip_5_renamed <- daily_precip_5 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_precip_5) - 2)), .cols = -1)

#TEMP

daily_temp_1_renamed <- daily_temp_1 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_temp_1) - 2)), .cols = -1)

daily_temp_2_renamed <- daily_temp_2 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_temp_2) - 2)), .cols = -1)

daily_temp_3_renamed <- daily_temp_3 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_temp_3) - 2)), .cols = -1)

daily_temp_4_renamed <- daily_temp_4 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_temp_4) - 2)), .cols = -1)

daily_temp_5_renamed <- daily_temp_5 %>% 
  select(-X) %>% 
  rename_with(~ as.character(1:(ncol(daily_temp_5) - 2)), .cols = -1)


# ------------------

# Subtract day from calculated normals data
diff_temp_1 <- subtract(daily_temp_1_renamed, smoothed_normals_temp_1) 
diff_temp_2 <- subtract(daily_temp_2_renamed, smoothed_normals_temp_2)
diff_temp_3 <- subtract(daily_temp_3_renamed, smoothed_normals_temp_3)
diff_temp_4 <- subtract(daily_temp_4_renamed, smoothed_normals_temp_4)
diff_temp_5 <- subtract(daily_temp_5_renamed, smoothed_normals_temp_5)

diff_precip_1 <- subtract(daily_precip_1_renamed, smoothed_normals_precip_1)
diff_precip_2 <- subtract(daily_precip_2_renamed, smoothed_normals_precip_2)
diff_precip_3 <- subtract(daily_precip_3_renamed, smoothed_normals_precip_3)
diff_precip_4 <- subtract(daily_precip_4_renamed, smoothed_normals_precip_4)
diff_precip_5 <- subtract(daily_precip_5_renamed, smoothed_normals_precip_5)

# -----------------
# Filter to frost dates

# Converting to date format
frost_dates$last_spring_frost <- as.Date(frost_dates_raw$last_spring_frost, "%d-%b")
frost_dates$first_fall_frost <- as.Date(frost_dates_raw$first_fall_frost, "%d-%b")

# Converting to day of year
frost_dates$last_spring_frost_doy <- yday(frost_dates$last_spring_frost)
frost_dates$first_fall_frost_doy <- yday(frost_dates$first_fall_frost)

# Inspect & Edit - STILL NEED TO DO THIS AS OF 3/31
# 92590 in CA - first frost is January 3. Switch to Dec 31?
# 97624 in Klamath, OR - add frost dates manually


#TEMP
filtered_diff_temp_1 <- filter_frost(diff_temp_1) 
filtered_diff_temp_2 <- filter_frost(diff_temp_2) 
filtered_diff_temp_3 <- filter_frost(diff_temp_3) 
filtered_diff_temp_4 <- filter_frost(diff_temp_4) 
filtered_diff_temp_5 <- filter_frost(diff_temp_5) 

#PRECIP
filtered_diff_precip_1 <- filter_frost(diff_precip_1) 
filtered_diff_precip_2 <- filter_frost(diff_precip_2) 
filtered_diff_precip_3 <- filter_frost(diff_precip_3) 
filtered_diff_precip_4 <- filter_frost(diff_precip_4) 
filtered_diff_precip_5 <- filter_frost(diff_precip_5) 
 

# -----------------
# Calculate sum of squares

temp_diffs <- list(filtered_diff_temp_1,filtered_diff_temp_2,filtered_diff_temp_3,filtered_diff_temp_4,filtered_diff_temp_5)
precip_diffs <- list(filtered_diff_precip_1,filtered_diff_precip_2,filtered_diff_precip_3,filtered_diff_precip_4,filtered_diff_precip_5)

zips <- diff_precip_1$zip1
temp_variance <- as.data.frame(zips)
temp_variance <- sum_of_squares(temp_diffs, temp_variance)

precip_variance <- as.data.frame(zips)
precip_variance <- sum_of_squares(precip_diffs, precip_variance)

