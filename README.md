# Developing-data
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, echo = FALSE, 
                      message = FALSE, warning = FALSE)
```

## Introduction

- This project was created as part of the Developing Data Products course of the Coursera [Data Science Specialisation](https://www.coursera.org/specializations/jhu-data-science).

- The goal of the project is to create a web page presentation using R Markdown that features a plot created with Plotly, and to host the resulting web page on either GitHub Pages, RPubs, or NeoCities.

- The interactive plot on the next slide represents the number of road accidents in Great Britain from 2005 to 2015, grouped by severity (slight, serious, or fatal).

    + A Loess smoother line has been added to highlight the overall evolution of the number of accidents.


```{r prerequisites}
rm(list=ls())
library(plotly)
library(data.table)
library(tidyr)
library(lubridate)
library(zoo)
```

```{r load_data, results='hide'}
# The source data sets are not included in this repository.
# To reproduce this presentation, you will first need to download the two
# following zipped data sets:
# - All STATS19 data (accident, casualties and vehicle tables) for 2005 to
#   2014", from
#   https://data.gov.uk/dataset/road-accidents-safety-data/resource/8ecee6ac-33fd-4f5b-8973-e900cc65d24a)
# - Road Safety - Accidents 2015, from
#   https://data.gov.uk/dataset/road-accidents-safety-data/resource/ceb00cff-443d-4d43-b17a-ee13437e9564)
# Then extract the `Accidents0514.csv` and `Accidents_2015.csv` files from
# the zip files in a subdirectory named `data`.
# read data for 2005-2014 and 2015 as data tables and keep only severity and
# date columns
accidents0514 <- fread("data/Accidents0514.csv", header = TRUE, 
                       sep = ",")
accidents0514 <- accidents0514 %>%
  select(Accident_Severity, Date)
accidents15 <- fread("data/Accidents_2015.csv", header = TRUE, 
                     sep = ",")
accidents15 <- accidents15 %>%
  select(Accident_Severity, Date)
# concatenate data tables and free up environment
accidents <- rbind(accidents0514, accidents15)
rm(list = c("accidents0514", "accidents15"))
```

```{r process_data}
# convert severity to factor and add labels
accidents$Accident_Severity <- 
  factor(accidents$Accident_Severity, 
         levels = 1:3, labels = c("Fatal", "Serious", "Slight"))
# convert date strings to Date objects
accidents$Date <- dmy(accidents$Date)
# group data by date and severity, get count, one row per date
accident_count <- accidents %>% 
  group_by(Date, Accident_Severity) %>%
  summarise(count = n()) %>%
  spread(key = Accident_Severity, value = count) %>% 
  as.data.frame()
# create a smoother for each severity to visualise general trends
loess_slight <- loess(Slight ~ as.numeric(Date), 
                      data = accident_count)
loess_serious <- loess(Serious ~ as.numeric(Date), 
                       data = accident_count)
loess_fatal <- loess(Fatal ~ as.numeric(Date), 
                     data = accident_count)
```

## Road accidents in GB (2005-2015)

```{r plot}
# plot data
plot_ly(accident_count) %>%
  add_trace(x = ~Date, y = ~Slight, type="scatter", mode = "markers", 
            name = "slight", legendgroup = "slight", 
            marker = list(color = "#52A9BD")) %>%
  add_trace(x = ~Date, y = ~Serious, type="scatter", mode = "markers",
            name = "serious", legendgroup = "serious", 
            marker = list(color = "#FFF16B")) %>%
  add_trace(x = ~Date, y = ~Fatal, type="scatter", mode = "markers",
            name = "fatal", legendgroup = "fatal", 
            marker = list(color = "#F5677D")) %>%
  add_trace(x = as.Date(loess_slight$x), y = fitted(loess_slight),
            type="scatter", mode = "lines",
            line = list(color = '#1A7A90'), 
            name = "slight Loess smoother", legendgroup = "slight", 
            hoverinfo = 'none', showlegend = FALSE) %>%
  add_trace(x = as.Date(loess_serious$x), y = fitted(loess_serious),
            type="scatter", mode = "lines",
            line = list(color = '#E9D625'),
            name = "serious Loess smoother", legendgroup = "serious",
            hoverinfo = 'none', showlegend = FALSE) %>%
  add_lines(x = as.Date(loess_fatal$x), y = fitted(loess_fatal),
            type="scatter", mode = "lines",
            line = list(color = '#DC2340'),
            name = "fatal Loess smoother", legendgroup = "fatal",
            hoverinfo = 'none', showlegend = FALSE) %>%
  layout(
    xaxis = list(title = "date"),
    yaxis = list(title = "number of accidents")
  )
