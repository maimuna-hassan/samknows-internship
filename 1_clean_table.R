# Load libraries and data ----------------------------------------------

library(tidyverse)
library(janitor)

ofcom_published_data <- read_csv("data/broadband_performance.csv")

# Clean data -----------------------------------------------------------

cleaned_names <- ofcom_published_data %>%
  select(-contains("weight")) %>% # we will look at unweighted average; weighting methodology isn't clearly published
  select(-c(DL24hr:iPlayerPeak)) %>% # these columns have test count per device; not useful for us
  janitor::clean_names() %>%
  rename_with(
    \(x) {
      x %>%
        str_replace_all("ave", "avg") %>%
        str_replace_all("dns_rate", "dns_lookup") %>%
        str_replace_all("i_player", "iplayer") %>%
        str_replace_all("you_tube", "youtube") %>%
        str_replace_all("24hr", "24h") %>%
        str_replace_all("_24_", "_24h_") %>%
        str_replace_all("([:graph:]+[:alpha:])24h([:graph:]*)", "\\1_24h\\2")
      ## ^---- The last replacement step makes sure that there's always an underscore before '24h'.
    }
  ) %>%
  rename(package_download_speed = package_download_by_upload) %>%
  mutate(full_package_name = str_squish(full_package_name)) %>%
  select(unit_id, isp, technology, package_download_speed, package_upload_speed, full_package_name, market_class, country, region, geography, names(.))

# Sift for FTTC --------------------------------------------------------

## FTTC = Fibre to the Cabinet

## FTTC is the most common technology in the UK, with the widest
##   regional distribution within this dataset; let's analyse
##   the performance of FTTC packages geographically and across
##   ISPs.

fttc_data <- cleaned_names %>%
  filter(
    technology %in% c("FTTC", "SOGEA"), # SOGEA is essentially the same as FTTC, just no landline
    isp != "Kcom", # the sample is very overweight for Kcom because they are a monopoly provider in one part of the country
    isp %in% c("BT", "EE", "Plusnet", "Sky", "TalkTalk", "Vodafone"), # only look at main ISPs
    package_download_speed %in% c(36, 38, 76)
  ) %>%
  mutate(
    technology = "FTTC", # treat FTTC and SOGEA as the same
    package_download = case_when(
      package_download_speed == 36 ~ 38, # the 36 and 38 plans are marketed differently, but they're based on the same Openreach plan
      TRUE ~ package_download_speed
    )
  )

# Summarized FTTC data -------------------------------------------------

limits_of_confidence_interval_of_mean <- function(sample_mean, sample_sd, sample_size, confidence_level = 0.95) {
  confidence_level <- round(confidence_level, 2)
  confidence_level_string <- as.character(as.integer(confidence_level * 100))
  
  ci_width <- stats::qt(1 - (1 - confidence_level) / 2, sample_size - 1) * sample_sd / sqrt(sample_size)
  
  ci_limits <- list(
    sample_mean - ci_width,
    sample_mean + ci_width
  )
  
  names(ci_limits) <- c(
    paste0("ci", confidence_level_string, "_lower"),
    paste0("ci", confidence_level_string, "_upper")
  )
  
  return(ci_limits)
}

summarise_fttc_data <- function(grouped_df) {
  grouped_df %>%
    summarise(
      sample_size = n(),
      avg_download_speed = mean(download_24h_avg),
      sd_download_speed = sd(download_24h_avg),
      avg_upload_speed = mean(upload_24h_avg),
      sd_upload_speed = sd(upload_24h_avg),
      .groups = "drop"
    ) %>%
    mutate(
      ci_of_avg_download_speed = pmap(list(avg_download_speed, sd_download_speed, sample_size), limits_of_confidence_interval_of_mean),
      ci_of_avg_upload_speed = pmap(list(avg_upload_speed, sd_upload_speed, sample_size), limits_of_confidence_interval_of_mean)
    ) %>%
    unnest_wider(ci_of_avg_download_speed) %>%
    rename(
      avg_download_speed_ci95_lower = ci95_lower,
      avg_download_speed_ci95_upper = ci95_upper
    ) %>%
    unnest_wider(ci_of_avg_upload_speed) %>%
    rename(
      avg_upload_speed_ci95_lower = ci95_lower,
      avg_upload_speed_ci95_upper = ci95_upper
    ) %>%
    janitor::adorn_rounding(1)
}

by_isp_package <- fttc_data %>%
  group_by(technology, package_download_speed, isp) %>%
  summarise_fttc_data()

by_package_region <- fttc_data %>%
  group_by(technology, package_download_speed, region) %>%
  summarise_fttc_data()

# Save summarized table ------------------------------------------------

if(!dir.exists("output")) dir.create("output")

write_csv(fttc_data, "output/fttc_data.csv")
write_csv(by_isp_package, "output/by_isp_package.csv")
write_csv(by_package_region, "output/by_package_region.csv")
