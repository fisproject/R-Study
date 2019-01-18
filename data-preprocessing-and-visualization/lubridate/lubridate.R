library(dplyr)
library(tibble)
library(lubridate)

df <- tibble(dt = c("2018-01-01T01:00:00",
                    "2018-04-10T12:30:20",
                    "2018-08-01T14:50:30",
                    "2018-10-10T14:50:30"))

df_ct <- df %>%
  mutate(dt_ct = as.POSIXct(dt, format = "%Y-%m-%dT%H:%M:%S")) %>%
  mutate(
    year = year(dt_ct),
    month = month(dt_ct),
    days = days_in_month(dt_ct),
    wday = wday(dt_ct),
    weekdays = weekdays(dt_ct),
    hour = hour(dt_ct),
    minute = minute(dt_ct),
    second = second(dt_ct),
    dt_str = format(dt_ct, "%Y-%m-%d %H:%M:%S"))

df_season <- df_ct %>%
  mutate(
    season = factor(
      case_when(
        month >= 3 & month < 6 ~ "spring",
        month >= 6 & month < 9 ~ "summer",
        month >= 9 & month < 12 ~ "autumn",
        TRUE ~ "winter"),
      levels = c("spring", "summer", "autumn", "winter")))

airport <- tibble(arrive_str = c("2018-01-01T01:00:00",
                                 "2018-01-10 12:30:20",
                                 "2018/01/20 08:00:20"),
                  leave_str = c("2018-01-01T03:05:00",
                                "2018-01-11 12:30:20",
                                "2018/01/20 11:00:20")) %>%
    mutate(arrive = ymd_hms(arrive_str, tz = "Asia/Tokyo"),
           leave = ymd_hms(leave_str, tz = "Asia/Tokyo")) %>%
    mutate(iv = interval(arrive, leave)) %>%
    mutate(diff_weeks = iv / dweeks(1),
           diff_days = iv/ ddays(1),
           diff_hours = iv / dhours(1),
           diff_minutes = iv / dminutes(1))