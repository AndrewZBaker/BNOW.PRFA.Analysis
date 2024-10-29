#Prairie Falcon Nest Camera photo data Cleaning, Transformation, and Visualization 
#from the nest scrape at Grand View Sand Cliffs in Spring of 2024,
#With a focus on Incubation and PRFA-BNOW interactions

library(tidyverse)
library(usethis)
#lubridate is an included package in the tidyverse

#calling up .csv files from Access Database ####
eggs_df <- read.csv("~/BNOW.PRFA.Analysis/csv Files/Eggs-BNOW.PRFA.csv")
incubation_df <- read.csv("~/BNOW.PRFA.Analysis/csv Files/Incubating-BNOW.PRFA.csv")
interaction_df <- read.csv("~/BNOW.PRFA.Analysis/csv Files/Interactions-BNOW.PRFA.csv")

#Tidying Date/Time Data ####

  #Beginning with eggs dataframe, expanding date and time info while keeping only relevant variables
eggs_timefixed_df <- eggs_df |> 
  mutate(Date = mdy_hm(Date),
          #Formatting date info using lubridate
         month = month(Date),
         day = day(Date),
         time = format(Date, "%H:%M")) |> 
          #Creating columns. This time format is character, not numeric. Fixes for this are brought up later as needed
  select(-Date, -Comments, -Recorder)
          #Removing unnecessary columns

#Incubation and Interactions will take a bit more work. I'm writing a function to automate the process a bit
timefix <- function(data, DateVar) {
  data |> 
    mutate(
      !!sym(DateVar) := mdy_hm(!!sym(DateVar)),
      !!paste0(DateVar, ".month") := month(!!sym(DateVar)),
      !!paste0(DateVar, ".day") := day(!!sym(DateVar)),
      !!paste0(DateVar, ".time") := format(!!sym(DateVar), "%H:%M"),
      !!paste0(DateVar, ".time") := as.numeric(format(!!sym(DateVar), "%H")) * 60 + as.numeric(format(!!sym(DateVar), "%M"))) |>
    #DateVar.time is time in minutes past 0:00. This will be used for calculating daily times. 1439 minutes represents 23:59  
    select(-!!sym(DateVar))
}
#The !! and := operators are completely new to me, but my research into coding an intense function led me to using them.
  #!! and sym() together allows the DateVar (Date Variable, or column names) to be inserted easier into the function workings
  #:= allows the argument assignment to work in this function. := is better suited for creating or modifying columns with mutate() 

incubation_timefixed_df <- incubation_df |> 
  timefix("Start.Min") |> 
  timefix("Start.Max") |> 
  timefix("End.Min") |> 
  timefix("End.Max") |> 
  #I would have loved to use a map() function to apply timefix() across all columns in one line, but I'm still learning/troubleshooting that
  select(-Recorder, -Comments)

#Repeat for interaction data using the timefix() variable

interaction_timefix_df <- interaction_df |> 
  timefix("Start.Time") |> 
  timefix("End.Time") |> 
  select(-comments, -Recorder)


#Tidying/Transforming daily egg incubation time ####

  #The plan is to convert observations into incubation time per day and facet wrap based on min/max times
      
    #I've spent a lot of time deliberating exactly how to do this and I decided on making two separate dataframes to tally daily time:
      #One gathers observations that go across midnight and separates daily time into two counts, while the other has observations that
      #occur on the same day, and simply gets a single daily count.
      #I'll combine the two dataframes at the end, then tally daily minute counts for all days.

incubation_AcrossMidnight_df <- incubation_timefixed_df |> 
  filter(Start.Min.day != End.Min.day, Start.Max.day != End.Max.day) |> 
  #Selecting observations that started and ended on different days
  mutate(DailyMin1 = (1439 - Start.Min.time), 
         DailyMax1 = (1439 - Start.Max.time),
         #1439 is 23:59 in 24-hour time. We're getting a tally for day 1 here
         DailyMin2 = (End.Min.time),
         DailyMax2 = (End.Max.time)
         )
         #Since midnight is 0 minutes in this format, any observation enduring past midnight will be simply the end minutes reported 
  
        #DailyMin1 and DailyMax1 show how many minutes make up an observation. 
        #DailyMin2 and DailyMax2 show how many minutes past midnight the observation continued for. 
        #These will later be added to the subsequent day when tallying daily incubation, so that we get a true daily count.

#Once again but with a VERY original naming scheme for the rest of the data
incubation_NotAcrossMidnight_df <- incubation_timefixed_df |> 
  filter(Start.Min.day == End.Min.day, Start.Max.day == End.Max.day) |>
  mutate(DailyMin1 = (End.Min.time - Start.Min.time), 
         DailyMax1 = (End.Max.time - Start.Max.time),
         DailyMin2 = 0,
         DailyMax2 = 0)


#Now to combine datasets using rbind()
incubation_dailytime_df <- rbind(incubation_AcrossMidnight_df, incubation_NotAcrossMidnight_df)

#This new dataframe can use some tidying, since we will only use one date per observation.
incubation_dailytime_tidy_df <- incubation_dailytime_df |> 
  mutate(Month = Start.Max.month, Day = Start.Max.day) |> 
  select(1,2,5,8,11,14,15,16,17,18,19,20) |> 
  group_by(Day) |> 
  mutate(Day.ID = ifelse(Day > 14, Day - 24, Day + 6))
  #Assigning a Day ID April 25 being Day 1. There may have been many other ways to do this more effectively, but this is my convoluted way.
    #The change of months made this a bit challenging.


incubation_dailytime_tidy_df <- incubation_dailytime_tidy_df |> 
  group_by(Day.ID) |> 
  summarize(
    Date = paste(Month,"/",Day),
    DailyMin = sum(DailyMin1, na.rm = TRUE) + lag(sum(DailyMin2), default = 0),
    DailyMax = sum(DailyMax1, na.rm = TRUE) + lag(sum(DailyMax2), default = 0)
            ) |> 
  #lag() allows DailyMin2 and DailyMax2 to be applied to the following day's sum, as intended.
  #default = 0 allows zeros to fill blank spaces
  distinct()

#The last step is to pivot_longer() so we can easily plot Daily min and max values

incubation_dailytime_longer_df <- incubation_dailytime_tidy_df |> 
  pivot_longer(cols = c(DailyMin, DailyMax),
               names_to = "Type",
               values_to = "Minutes")
                #We went from 20 observations (one per day) each with a daily min and daily max
                #to 40 observations so each day has an observation of a daily min and a daily max.

#Visualizing Daily Incubation Time ####

Daily_Incubation_Combined_Plot <- incubation_dailytime_longer_df |> 
  ggplot(aes(x = Day.ID, 
             y = Minutes, 
             fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = seq(0, 20, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1400, by = 60)) +
    #Changing the amount of tickmarks on each axis to something more appropriate
    geom_smooth(method = "lm", 
                alpha = 0.3, 
                color = "black",
                linewidth = 0.5) +
    #Adding a regression line
    labs(x = "Days Incubating Eggs", 
         y = "Minutes per Day Incubating Eggs") +
    theme_classic()

ggsave("Daily Incubation Combined Plot.png", 
       plot = Daily_Incubation_Combined_Plot,
       units = "cm",
       width = 18,
       height = 12,
       dpi = 300)
#Saving plot as a .png image

#While this plot was meaningful, it seems a bit difficult to read.
#Instead, let's facet_wrap() the daily times so that the two times can be read separately.
Daily_Incubation_Faceted_Plot <- incubation_dailytime_longer_df |> 
  ggplot(aes(x = Day.ID, 
             y = Minutes, 
             fill = Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Type) +
  scale_x_continuous(breaks = seq(0, 20, by = 4)) +
  scale_y_continuous(breaks = seq(0, 1400, by = 60)) +
  #Changing the amount of tickmarks on each axis to something more appropriate
  geom_smooth(method = "lm", 
              alpha = 0.2, 
              color = "black",
              linewidth = 0.5) +
  #Adding a regression line
  labs(x = "Days Incubating Eggs", 
       y = "Minutes per Day Incubating Eggs") +
  theme_classic()
  
ggsave("Daily Incubation Faceted Plot.png", 
       plot = Daily_Incubation_Faceted_Plot,
       units = "cm",
       width = 18,
       height = 12,
       dpi = 600)
#Saving plot as a .png image

# Tidying/Transforming interaction data ####

#First let's make a df removing all interactions except with Barn Owls
interaction_withBNOW_df <- interaction_timefix_df |> 
  filter(BNOW.Count != 0) |> 
  select(-1, -5, -17, -18) |> 
  #Removing unnecessary columns: Observation ID, other species, Month/Day ended
  mutate(across(everything(), ~ replace_na(., 0)))
  #Converting NAs to zero

#Inspecting this new df, observations 10-14 lack Prairie Falcons.
#This means that all ranked interactions will be done by Barn Owls in these observations,
#while 1-9 may belong to either species.

#To visualize this, I'm going to create two plots:
    #1. A frequency table of each type of interaction for each night
    #2. A graph plotting PRFA/BNOW aggression values against time
    #Bonus. Other species interacting with Prairie Falcons, which were mice and a magpie


#Transforming df for graph 1
    #We need to combine like dates and convert time into minutes
    #Fortunately, none of these observations span across 0:00, so we won't have to repeat the last fiasco
interaction_withBNOW_df <- interaction_withBNOW_df |> 
  mutate(Total.Time = (End.Time.time - Start.Time.time)) |> 
  mutate(date = make_date(year = 2024, month = Start.Time.month, day = Start.Time.day))
 
#Creating a temporary df to create date IDs to easily combine observations from the same day
placeholder_DayID_df <- interaction_withBNOW_df |> 
  distinct(date) |> 
  arrange(date) |> 
  mutate(DayID = row_number()) |> 
  select(date, DayID)

#Adding IDs to the main df
interaction_withBNOW_DayIDs_df <- interaction_withBNOW_df |> 
  left_join(placeholder_DayID_df, by = "date")

#I missed one observation that did span across midnight.
#Since we can't separate interactions into two new ones, I'm not altering it.
#If needed, I'll edit the value to what it should be using the same method as I did for incubation
  #Code to begin that process, to eventually rbind() the new row
    #interaction_acrossMidnight_df <-  interaction_withBNOW_DayIDs_df[11, ]
        #This pulls all of row 11 and saves it as a new df to alter
  
interaction_withBNOW_DayIDs_df[11,16] <- 488
#Changing the original value from -951 to 488 minutes : the actual time from 9:39 PM to 5:48 AM in minutes


#We need to pivot_longer()
interaction_withBNOW_longer_df <- interaction_withBNOW_DayIDs_df |> 
  pivot_longer(
        cols = "asleep" : "other",
        names_to = "Behavior",
        values_to = "Rank",
        values_drop_na = TRUE
               ) |> 
  filter(Rank != 0)
  #Removing all ranks of 0 since it signifies that behavior didn't happen in the interaction
  
# Plotting Daily Prairie Falcon - Barn Owl interactions

interaction_withBNOW_longer_df |> 
  ggplot(aes(x = Behavior)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ date)

#This plot is far from perfect. The scales need to be worked on to be visible and a color system needs to be added
#I didn't spend too much time into perfecting this, because I wasn't sure if it was needed. 

#Things to do from here ####
    #Plot of all aggressive behavior from PRFAs including BNOWs and no other visible species
        #This can assume that BNOWs are the interacted species outside of frame

    #Plot of PRFA-BNOW aggression over time
        #Possibly add a log scale to put ranks of behaviors (and weights of aggression per behavior?) on a single axis
            #I'll need some guidance with this, maybe also some published resources on how to go about this


