##################################################################################################################

## Data source: https://open.canada.ca/data/dataset/49edb1d7-5cb4-4fa7-897c-515d1aad5da3/resource/42300f81-a00a-4f0f-9ba9-842bb0e1971e/download/cihr_grants_award_202021.csv

####################################################################################################################
# Required packages

library(tidyverse)
library(data.table)
library(here)
library(stringi)
library(janitor)
library(ggbump)
library(ggtext)
library(BBmisc)
library(hablar)
library(feather) 
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)

####----------------------------------------------------------------------------------------------------------------

# Data Preparation

cihr_province <- cihr |>
  group_by(province) |>
  summarise(total = sum(total_amount)) |>
  filter(province != "Unknown/Inconnu") |>
  mutate(province = str_replace(province, "North West Territories", "Northwest Territories"))

# Combine Canadian map coordinates and total finding data to it 

can_sf <- rnaturalearthdata::states50 |>
  st_as_sf() |>
  filter(admin == "Canada") |>
  mutate(name = stri_trans_general(name, id = "Latin-ASCII")) |>
  left_join(cihr_province, by = c("name" = "province"))

rank <- st_geometry(can_sf) |>
  st_point_on_surface() |>
  st_coordinates() |>
  as_tibble() |>
  bind_cols(tibble(fine_total = normalize(rank(can_sf$total), range = c(40,80), method = "range"),
                   province = can_sf$name,
                   xend = -20,
                   x_axis_start = xend + 10,
                   fine_total_x = normalize(can_sf$total, range = c(first(x_axis_start), 40), method = "range"),
                   val_txt = paste0(format((can_sf$total)/1000000, digits = 2, nsmall = 2)),
                   val_txt2 = if_else(province == "Ontario", paste0(val_txt, " million canadian dollars"), val_txt)))

# Plot

st_text <- "Funding data from the Canadian Institutes of Health Research (CIHR) between 2007 and 2021 indicates that researchers in <span style='color:#F6C33AFF'>**Ontario**</span> and <span style='color:#E3DB38FF'>**Quebec**</span> overwhelmingly secured grants for biomedical and healthcare research."

province_funding <- ggplot() +
  geom_sf(data = can_sf, 
          linewidth = 0.3, 
          fill = "transparent", 
          color = "#828282") +
  # Sigmoid from country to start of barchart
  geom_sigmoid(data = rank, 
               aes(x = X, y = Y, xend = x_axis_start - 0.2, yend = fine_total, group = province, color = fine_total), 
               alpha = 0.7, 
               smooth = 10, 
               size = 1.5) +
  # Line from xstart to value
  geom_segment(data = rank, 
               aes(x = x_axis_start, y = fine_total, xend = fine_total_x + 2, yend = fine_total, color = fine_total), 
               alpha = 0.7, 
               linewidth = 1.5, 
               lineend = "round") +
  # Y axis - black line
  geom_segment(data = rank, 
               aes(x = x_axis_start, y = 38, xend = x_axis_start, yend = 82), 
               alpha = 0.7, 
               linewidth = 1.3, 
               color = "black") +
  # dot on centroid of province in map
  geom_point(data = rank, 
             aes(x = X, y = Y, color = fine_total), 
             size = 2) +
  # Province text
  geom_text(data = rank, aes(x = x_axis_start, y = fine_total, label = province, color = fine_total), 
            hjust = 1, 
            size = 4, 
            nudge_y = 1) +
  # Value text
  geom_text(data = rank, aes(x = fine_total_x + 3, y = fine_total, label = val_txt2, color = fine_total), 
            hjust = 0, 
            size = 3.5, 
            nudge_x = 0.5) +
  coord_sf(clip = "off") +
  scale_fill_viridis_c(option = "turbo", begin = 0.1, end = 0.65) +
  scale_color_viridis_c(option = "turbo", begin = 0.1, end = 0.65) +
  theme_void() +
  labs(title = "Which Canadian Provinces are Hotbeds of Health Research?",
       subtitle = st_text,
       caption = "Source: Open Data Canada - Canadian Institutes of Health Research • Graphic: Sejal Davla, PhD") + 
  theme(plot.margin = margin(0.5, 5, 0.5, 0.5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "gray80", 
                                  size = 24, 
                                  family = "Helvetica", 
                                  face = "bold"),
        plot.subtitle = element_markdown(color = "gray80", 
                                     size = 10),
        plot.caption = element_text(color = "gray60", 
                                    size = 10,
                                    hjust = 0.65,
                                    family = "Roboto",
                                    vjust = 10)) 
  
ggsave("province_funding.png", width = 14, height = 7, dpi = 300)













