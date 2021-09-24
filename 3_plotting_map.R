# Loading libraries and data ----------------------------------------------

library(dplyr)
library(tidyverse)
library(ggplot2)

library(sf)
library(raster)
library(spData)
library(spDataLarge)

library(tmap) # for static and interactive maps
library(tmaptools)
library(leaflet) # for interactive maps
library(gridExtra)

summarised_broadband_performance <- readr::read_csv("data/summarised_broadband_performance.csv")

region_map <- st_read("mapping_data/NUTS_Level_1_(January_2018)_Boundaries (2)/NUTS_Level_1_(January_2018)_Boundaries.shp")
#changing region column to common name
region_map<- rename(region_map, Region_Name = nuts118nm)






# Population by region by ISP by plan -------------------------------------

pop_by_region_plan <- summarised_broadband_performance %>% 
  group_by(Region, package_download) %>% 
  summarise(pop_by_region_plan = n())

summarised_broadband_performance <- left_join(
  summarised_broadband_performance, 
  pop_by_region_plan, 
  by = c("Region", "package_download")
)





# Smaller population with significant discrepancy -------------------------

pop_discrepancy_by_region_ISP_plan <- summarised_broadband_performance %>%
  group_by(Region, ISP, package_download, significant) %>% 
  summarise(pop_discrepancy_by_region_ISP_plan = n())

summarised_broadband_performance <- left_join(
  summarised_broadband_performance,
  pop_discrepancy_by_region_ISP_plan,
  by = c("Region", "ISP", "package_download", "significant")
)



# Function to compare best performing regions for each ISP (tmap) --------

ISP_regional_performance <- function(a_tibble, ISP_name, package_plan) {
  tibble1 <- a_tibble %>%
    filter(ISP == ISP_name,
           package_download == package_plan
    ) %>% 
    dplyr::select(package_download,
           Region,
           discrepancy_download,
           significant,
           pop_by_region_plan,
           pop_discrepancy_by_region_ISP_plan
           )%>% 
    mutate(
      Region_Name = case_when(
        Region == "North West" ~ "North West (England)",
        Region == "North East" ~ "North East (England)",
        Region == "South West" ~ "South West (England)",
        Region == "South East" ~ "South East (England)",
        Region == "East Midlands" ~ "East Midlands (England)",
        Region == "West Midlands" ~ "West Midlands (England)",
        Region == "East" ~ "East of England",
        Region == "Yorkshire & Humberside" ~ "Yorkshire and The Humber",
        TRUE ~ Region
      )
    ) %>% 
    group_by(Region_Name) %>% 
    mutate(`Average Discrepancy (%)` = mean(discrepancy_download)) %>% 
    distinct(
      Region_Name,
      `Average Discrepancy (%)`
    )
  
  tibble2 <- inner_join(region_map, tibble1)
  
  ##tmap version
  
  tm_shape(tibble2) +
    tm_polygons("Average Discrepancy (%)", id = "Region_Name", palette = "Reds") +
    tmap_mode("view")
  
  ##ggplot version
  
  # ggplot(tibble2) +
  #   geom_sf(aes(fill = `Average Discrepancy (%)`), colour = "black", size = 0.01) +
  #   scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  #   theme(
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor = element_blank(),
  #     panel.background = element_blank()
  #   )
}

ISP_regional_performance(summarised_broadband_performance, "BT", 76)

m1 <- ISP_regional_performance(summarised_broadband_performance, "BT", 76)
m2 <- ISP_regional_performance(summarised_broadband_performance, "BT", 38)

tmap_arrange(m1, m2, ncol = 2)

#grid.arrange(m1, m2, nrow = 1)





# Worst performing regions ------------------------------------------------



pop_discrepancy_by_region_plan <- summarised_broadband_performance %>% 
  group_by(Region, package_download, significant) %>% 
  summarise(pop_discrepancy_by_region_plan = n())
  
summarised_broadband_performance <- left_join(
  summarised_broadband_performance,
  pop_discrepancy_by_region_plan,
  by = c("Region", "package_download", "significant")
)

worst_performing_region <- function(a_tibble, package_plan) {
  tibble1 <- a_tibble %>%
    filter(package_download == package_plan,
           significant == TRUE) %>% 
    dplyr::select(package_download,
                  Region,
                  discrepancy_download,
                  significant,
                  pop_by_region_plan,
                  pop_discrepancy_by_region_plan
    )%>% 
    mutate(
      Region_Name = case_when(
        Region == "North West" ~ "North West (England)",
        Region == "North East" ~ "North East (England)",
        Region == "South West" ~ "South West (England)",
        Region == "South East" ~ "South East (England)",
        Region == "East Midlands" ~ "East Midlands (England)",
        Region == "West Midlands" ~ "West Midlands (England)",
        Region == "East" ~ "East of England",
        Region == "Yorkshire & Humberside" ~ "Yorkshire and The Humber",
        TRUE ~ Region
      )
    ) %>% 
    group_by(Region_Name) %>% 
    mutate(`Average Discrepancy (%)` = mean(discrepancy_download)) %>% 
    distinct(
      Region_Name,
      `Average Discrepancy (%)`
    )
  
  tibble2 <- inner_join(region_map, tibble1)
  
  ##tmap version
  
  tm_shape(tibble2) +
    tm_polygons("Average Discrepancy (%)", id = "Region_Name", palette = "Reds") +
    tmap_mode("view")
  
  ##ggplot version
  
  # ggplot(tibble2) +
  #   geom_sf(aes(fill = `Average Discrepancy (%)`), colour = "black", size = 0.01) +
  #   scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  #   theme(
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor = element_blank(),
  #     panel.background = element_blank()
  #   )
}



worst_performing_region(summarised_broadband_performance, 38)






# Which ISP contributes the most to worst performing regions --------------

comparison_worst_performing_region <- function(a_tibble, ISP_name, package_plan) {
  tibble1 <- a_tibble %>%
    filter(package_download == package_plan,
           ISP == ISP_name,
           significant == TRUE) %>% 
    dplyr::select(package_download,
                  Region,
                  discrepancy_download,
                  significant,
                  pop_by_region_plan,
                  pop_discrepancy_by_region_plan
    )%>% 
    mutate(
      Region_Name = case_when(
        Region == "North West" ~ "North West (England)",
        Region == "North East" ~ "North East (England)",
        Region == "South West" ~ "South West (England)",
        Region == "South East" ~ "South East (England)",
        Region == "East Midlands" ~ "East Midlands (England)",
        Region == "West Midlands" ~ "West Midlands (England)",
        Region == "East" ~ "East of England",
        Region == "Yorkshire & Humberside" ~ "Yorkshire and The Humber",
        TRUE ~ Region
      )
    ) %>% 
    group_by(Region_Name) %>% 
    mutate(`Average Discrepancy (%)` = mean(discrepancy_download)) %>% 
    distinct(
      Region_Name,
      `Average Discrepancy (%)`
    )
  
  tibble2 <- inner_join(region_map, tibble1)
  
  ##tmap version
  
  # tm_shape(tibble2) +
  #   tm_polygons("Average Discrepancy (%)", id = "Region_Name", palette = "Reds") +
  #   tmap_mode("view")
  
  ##ggplot version
  
  ggplot(tibble2) +
    geom_sf(aes(fill = `Average Discrepancy (%)`), colour = "black", size = 0.01) +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    )
}

comparison_worst_performing_region(summarised_broadband_performance, "TalkTalk", 38)

m1 <- comparison_worst_performing_region(summarised_broadband_performance, "TalkTalk", 38)
m2 <- comparison_worst_performing_region(summarised_broadband_performance, "EE", 38)
m3 <- comparison_worst_performing_region(summarised_broadband_performance, "BT", 38)
m4 <- comparison_worst_performing_region(summarised_broadband_performance, "Plusnet", 38)
m5 <- comparison_worst_performing_region(summarised_broadband_performance, "Sky", 38)
m6 <- comparison_worst_performing_region(summarised_broadband_performance, "Vodafone", 38)

grid.arrange(m1, m2, m3, m4, m5, m6, nrow = 2)
