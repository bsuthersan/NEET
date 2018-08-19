##Read in the data

library(readxl)
library(tidyverse)
library(gganimate)
mydata <- read_xls("~/Downloads/neets-borough-region.xls", sheet=3)

##Rename/reshape the data properly
mydata <- mydata[1:11, ]
mydata[1,1] <- "Region"
colnames(mydata) <- mydata[1, ]
mydata <- mydata[2:11, ]

##Reshape - wide to long
#Change Q1:4 to dates

mydata <- mydata %>%
  reshape2:: melt(id.vars="Region") %>%
  mutate(Year = str_sub(variable, 1, 4)) %>%
  mutate(Date = case_when(
    str_detect(variable, "Q1") ~ paste0("01/01/",Year),
    str_detect(variable, "Q2")  ~ paste0("01/04/",Year),
    str_detect(variable, "Q3") ~ paste0("01/07/",Year),
    str_detect(variable,"Q4") ~ paste0("01/10/",Year),
    TRUE ~ "0")) %>%
  select(-Year, -variable) %>%
  mutate(Date = as.Date(Date, format="%d/%m/%Y")) %>%
  mutate(value = as.numeric(value)) %>%
  filter(Region=="England") %>%
  mutate(value = value/1000)

ggplot(mydata, aes(x=Date, y=value, group=1)) +
  geom_line(color="steelblue") +
  theme_minimal() +
  xlab("") +
  ylab("No. (000s)") +
  labs(caption = "Source: Department for Education")

