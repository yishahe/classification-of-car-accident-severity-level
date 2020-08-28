---
title: "graph_model"
author: "Yisha He"
date: "3/28/2020"
output: 
  html_document: 
    keep_md: yes
---



## Data Preprocessing
Read in data:


```r
options(warn = -1)
df <- read_csv("US_accident.csv", col_types = cols(.default = col_character())) %>% 
  type_convert() %>%
  mutate(TMC = factor(TMC), Severity = factor(Severity), Year = factor(Year), Weekday = factor(Weekday)) %>%
  mutate_if(is.logical, factor) %>%
  mutate_if(is.character, factor)
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   TMC = col_character(),
##   Start_Time = col_datetime(format = ""),
##   End_Time = col_datetime(format = ""),
##   Street = col_character(),
##   Side = col_character(),
##   City = col_character(),
##   County = col_character(),
##   State = col_character(),
##   Zipcode = col_character(),
##   Country = col_character(),
##   Weather_Condition = col_character(),
##   Sunrise_Sunset = col_character(),
##   Civil_Twilight = col_character(),
##   Nautical_Twilight = col_character(),
##   Astronomical_Twilight = col_character(),
##   Month = col_character(),
##   Weekday = col_character()
## )
```

```
## See spec(...) for full column specifications.
```


Accident duration is very skewed, so logarithms are taken


```r
df$Junction <- as.factor(df$Junction)
df$Traffic_Signal <- as.factor(df$Traffic_Signal)
df$Month <- factor(df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                        "Jun", "Jul", "Aug", "Sep", "Oct",
                                        "Nov", "Dec"))
##Check distribution of duration
hist(df$Duration)
```

![](graph_model_files/figure-html/hist-1.png)<!-- -->

```r
##Take Logarithms of Duration
df$Duration <- log(df$Duration)
##filter out Weather Condition NA.
df <- df %>% filter(!(is.na(Weather_Condition)))
```
There are too many weather conditions recorded. Weather conditions with fewer than 20 occurances are deleted and similar weather conditions are merged.

```r
##Drop uncommon weather condition 
df %>% count(Weather_Condition) %>% filter(n < 20) %>% select(Weather_Condition, n)
```

```
## # A tibble: 18 x 2
##    Weather_Condition          n
##    <fct>                  <int>
##  1 Blowing Snow               5
##  2 Drizzle / Windy            1
##  3 Drizzle and Fog            2
##  4 Heavy Drizzle             19
##  5 Heavy Rain / Windy         1
##  6 Heavy Snow                16
##  7 Heavy T-Storm / Windy      6
##  8 Ice Pellets                5
##  9 Light Fog                  1
## 10 Light Freezing Drizzle     9
## 11 Light Ice Pellets         11
## 12 N/A Precipitation          3
## 13 Rain / Windy               5
## 14 Squalls                    3
## 15 T-Storm / Windy            4
## 16 Thunder / Windy            2
## 17 Wintry Mix                16
## 18 Wintry Mix / Windy         3
```

```r
drop_weather <- df %>% count(Weather_Condition) %>% filter(n < 20) %>% select(Weather_Condition)
drop_weather <- drop_weather$Weather_Condition %>% unlist()
df <- df %>% 
  filter(!(Weather_Condition %in% drop_weather)) %>% 
  mutate(Weather_Condition = factor(Weather_Condition))

###Merge some weather conditions
df$Weather_Condition <- as.character(df$Weather_Condition)
df$Weather_Condition[df$Weather_Condition %in% c("Cloudy", "Cloudy / Windy", "Mostly Cloudy", "Mostly Cloudy / Windy", "Partly Cloudy", "Partly Cloudy / Windy", "Scattered Clouds")]<-c("Cloudy/Windy")
df$Weather_Condition[df$Weather_Condition %in% c("Thunder", "Thunderstorm", "Thunderstorms and Rain", "Thunder in the Vicinity")]<- c("Thunder")
df$Weather_Condition[df$Weather_Condition %in% c("Haze", "Fog", "Patches of Fog", "Mist", "Shallow Fog", "Light Freezing Fog")]<- c("Haze/Mist/Fog")
df$Weather_Condition[df$Weather_Condition %in% c("Clear", "Fair", "Fair / Windy")]<-c("Clear/Fair")
df$Weather_Condition[df$Weather_Condition %in% c("Light Freezing Rain", "Light Thunderstorms and Rain","Light Rain", "Light Rain / Windy", "Light Drizzle", "Light Rain with Thunder", "Drizzle")]<- c("Rain")                
df$Weather_Condition[df$Weather_Condition %in% c("Heavy T-Storm", "Heavy Thunderstorms and Rain")]<- c("T-Storm")
df$Weather_Condition <- as.factor(df$Weather_Condition)
table(df$Weather_Condition)
```

```
## 
##    Clear/Fair  Cloudy/Windy Haze/Mist/Fog    Heavy Rain    Light Snow 
##         27438         26752           868           533           561 
##      Overcast          Rain          Snow       T-Storm       Thunder 
##         11749          6386           103           150           279
```

Monday to Friday share very similar patterns so 'Weekday' is changed to dummy variables where weekdays to 1, weekends to 0.

```r
df$Weekday <- as.character(df$Weekday)
df$Weekday[df$Weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri")]<- c("1")
df$Weekday[df$Weekday %in% c("Sat", "Sun")]<- c("0")
df$Weekday <- as.factor(df$Weekday)
```

