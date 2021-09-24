# Loading libraries and data ----------------------------------------------

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(gghighlight)

summarised_broadband_performance <- readr::read_csv("data/summarised_broadband_performance.csv")



# Average best ISP --------------------------------------------------------

top3_38 <- summarised_broadband_performance %>% 
  group_by(ISP, package_download) %>% 
  filter(package_download %in% c(38)) %>% 
  summarise(`Average Download Speed (Mbps)` = mean(average_download_speed)) %>% 
  arrange(desc(`Average Download Speed (Mbps)`)) %>% 
  head(n = 3)

summarised_broadband_performance %>% 
  group_by(ISP, package_download) %>% 
  filter(package_download %in% c(38)) %>% 
  summarise(`Average Download Speed (Mbps)` = mean(average_download_speed)) %>% 
  arrange(desc(`Average Download Speed (Mbps)`)) %>%
  mutate(
    top_ISP = ifelse(ISP %in% c("TalkTalk", "EE", "Vodafone"), TRUE, FALSE)
  ) %>% 
  ggplot(mapping = aes(x = ISP, y = `Average Download Speed (Mbps)`)) +
  geom_col(aes(fill = top_ISP)) +
  scale_fill_manual(values = c('grey', 'darkgreen'))



top3_76 <- summarised_broadband_performance %>% 
  group_by(ISP, package_download) %>% 
  filter(package_download %in% c(76)) %>% 
  summarise(`Average Download Speed (Mbps)` = mean(average_download_speed)) %>% 
  arrange(desc(`Average Download Speed (Mbps)`)) %>% 
  head(n = 3)

summarised_broadband_performance %>% 
  group_by(ISP, package_download) %>% 
  filter(package_download %in% c(76)) %>% 
  summarise(`Average Download Speed (Mbps)` = mean(average_download_speed)) %>% 
  arrange(desc(`Average Download Speed (Mbps)`)) %>%
  mutate(
    top_ISP = ifelse(ISP %in% c("BT", "TalkTalk", "Sky"), TRUE, FALSE)
  ) %>% 
  ggplot(mapping = aes(x = ISP, y = `Average Download Speed (Mbps)`)) +
  geom_col(aes(fill = top_ISP)) +
  scale_fill_manual(values = c('grey', 'darkgreen'))


# Discrepancy -------------------------------------------------------------

summarised_broadband_performance<- summarised_broadband_performance %>% mutate(
  discrepancy_download = ((package_download - average_download_speed)/package_download) * 100,
  significant = discrepancy_download >= 10
)



# Download Speed discrepancy ----------------------------------------------

get_download_disrepancy <- function(a_tibble, package_plan) {
   a_tibble %>%
    filter(package_download == package_plan) %>%
    ggplot(mapping = aes(x = discrepancy_download, fill = significant)) +
    scale_fill_manual(values = c(`TRUE` = "red", `FALSE` = "grey")) +
    geom_histogram(binwidth = 4) +
    facet_wrap(~ ISP, nrow = 3)
}

get_download_disrepancy(summarised_broadband_performance, 38)
get_download_disrepancy(summarised_broadband_performance, 76)



# Saving summarized table locally -----------------------------------------

readr::write_csv(summarised_broadband_performance, "data/summarised_broadband_performance.csv")


