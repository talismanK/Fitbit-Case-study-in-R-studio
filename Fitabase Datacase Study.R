Merge1 <- merge(dailyactivity_merged, dailycalories_merged, by = c("Id", "Calories"))
#the code above merges the tables dailyactivit and dailycalories into one table

Merge2 <- merge(dailyintensities_merged, dailyintensities_merged,
                by = c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance"))
#after merging dailyintensities and daily

merge_daily <- merge(Merge1, Merge2, by = c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance")) %>%
  select(-ActivityDay) %>% rename(Date = ActivityDate)

