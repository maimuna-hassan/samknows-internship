
# Loading libraries and data ----------------------------------------------

library(dplyr)
library(tidyverse)

broadband_performance <- readr::read_csv("data/broadband_performance.csv")




# Cleaning data -----------------------------------------------------------

broadband_performance <- rename(broadband_performance, Original_tech = Technology)
broadband_performance <- rename(broadband_performance, temp_package_download = `PACKAGE (download by upload)`)

summarised_broadband_performance <- broadband_performance %>% mutate(
  Technology = case_when(
    Original_tech == "SOGEA" ~ "FTTC",
    TRUE ~ Original_tech
  ),
  package_download = case_when(
    temp_package_download == 36 ~ 38,
    TRUE ~ temp_package_download
  )
) %>% 
  filter(Technology %in% c("FTTC"),
         ISP %in% c("Vodafone", "EE", "Sky", "TalkTalk", "BT", "Plusnet", "Virgin")) %>% 
  group_by(
    ISP,
    Technology,
    package_download,
    Region
  ) %>% 
  summarise(
    ISP,
    `Technology`,
    package_download,
    `Download - 24 (ave)`,
    `Upload - 24 (ave)`,
    `Region`
  )

summarised_broadband_performance %>% 
  group_by(ISP) %>% 
  summarise(count = n())

# Saving summarized table locally -----------------------------------------

summarised_broadband_performance<- rename(summarised_broadband_performance, average_download_speed = `Download - 24 (ave)`)
summarised_broadband_performance<- rename(summarised_broadband_performance, average_upload_speed = `Upload - 24 (ave)`)

readr::write_csv(summarised_broadband_performance, "data/summarised_broadband_performance.csv")

