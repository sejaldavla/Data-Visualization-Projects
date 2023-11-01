# Load packages ----------------------------------------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(scales)
library(geofacet)
library(camcorder)
library(ggtext)

# Load data ----------------------------------------------------------------------------------------------------------

path <- "/EN_ODP-TR-Study-IS_PT_study_level_sign.xlsx"
provincewise <- read_excel(path = path,
                           .name_repair = "unique")
str(provincewise)

# Clean data ----------------------------------------------------------------------------------------------------------

province <- provincewise |>
  select(c(1,2, 19, 36, 53, 70, 87, 104, 121, 138))

colnames(province) <- province[3,]
head(province, n = 10)  
tail(province, n = 15)

province <- province[-c(1:4,67:81), ]

colnames(province) <- c("PR", "Level", "2015","2016","2017","2018","2019","2020","2021","2022")
  
# Add states column for data transformation
States <- c(rep("Newfoundland and Labrador", 5),
            rep("Prince Edward Island", 5),
            rep("Nova Scotia", 5),
            rep("New Brunswick", 5),
            rep("Quebec", 5),
            rep("Ontario", 5),
            rep("Manitoba", 5),
            rep("Saskatchewan", 5),
            rep("Alberta", 5),
            rep("British Columbia", 5),
            rep("Yukon", 5),
            rep("Northwest Territories", 3),
            rep("Nunavut", 4))

province1 <- province |>
  mutate(States = States) |>
  select(-PR) |>
  mutate(Level = replace_na(Level, "Total"))

# Some categories are missing in the original data.
missing_df <- data.frame(Level = c("Other Studies","Education level not stated","Education level not stated"),
                         Y2015 = rep(0,3),
                         Y2016 = rep(0,3),
                         Y2017 = rep(0,3),
                         Y2018 = rep(0,3),
                         Y2019 = rep(0,3),
                         Y2020 = rep(0,3),
                         Y2021 = rep(0,3),
                         Y2022 = rep(0,3),
                         States = c("Northwest Territories", "Northwest Territories", "Nunavut")) |>
  rename("2015" = "Y2015",
         "2016" = "Y2016",
         "2017" = "Y2017",
         "2018" = "Y2018",
         "2019" = "Y2019",
         "2020" = "Y2020",
         "2021" = "Y2021",
         "2022" = "Y2022") |>
  mutate_all(as.character)


# Add missing values to the province data
province1 <- province1 |>
  bind_rows(missing_df)

write.csv(province1, "province1.csv")

# Prepare data for visualization ---------------------------------------------------------------------------------------------

# Pivot to have all the year values in the same column - For rows with total values

Province_total <- province1 |>
  filter(Level == "Total") |>
  pivot_longer(cols = c(2:9),
               names_to = "Year",
               values_to = "Enrolment") |>
  select(-Level) |>
  mutate(Enrolment = str_replace(Enrolment, "--","3")) |>
  mutate(Enrolment = as.numeric(str_remove(Enrolment, ",")))

Province_pivot <- province1 |>
  pivot_longer(cols = c(2:9),
               names_to = "Year",
               values_to = "Enrolment") |>
  mutate(Enrolment = str_replace(Enrolment, "--","3")) |>
  mutate(Enrolment = as.numeric(str_remove(Enrolment, ",")))

## Data visualization ---------------------------------------------------------------------------------------------------------------------------------------

# Start recording 

gg_record(
  dir = file.path("recording"),
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)


# Create a color palette and assign it to a value

col_palette <- c("#808080","#4FFBDF","#00C2A8","#008B74", "#845EC2")
names(col_palette) <- levels(factor(Province_pivot$Level))
bg_col <- "#FAFAFA"


st_text <- "<span style='color:#845EC2'>**Total**</span> international student enrolment is increasing across all provinces at both <span style='color:#008B74'>**secondary**</span> and <span style='color:#00C2A8'>**post secondary**</span> levels"
caption_text <- "Source: Open Data Canada - Immigration and Citizenship Canada • Graphic: Sejal Davla, PhD"


provincial_distribution <- Province_pivot |>
  filter(Level %in% c("Total", "Post Secondary", "Secondary or less")) |>
  ggplot() +
  geom_line(aes(x = Year, y = Enrolment, group = Level, color = Level),
            linewidth = 0.7) +
  scale_y_continuous(labels = label_comma()) +
  facet_geo(~States, grid = "ca_prov_grid1", scales = "free_y") +
  scale_color_manual(values = col_palette) +
  coord_cartesian(expand = FALSE, 
                  clip = "off") +
  labs(title = "Growth of International Students in Canadian Provinces and Territories",
       subtitle = st_text,
       caption = caption_text,
       y = NULL) +
  theme_minimal(base_family = "roboto",
                base_size = 12) +
  theme(plot.title.position = "plot",
        plot.background = element_rect(fill = bg_col),
        plot.title = element_textbox_simple(color = "#D9544D",
                                            size = 30,
                                            margin = margin(10,10,10,0)),
        plot.subtitle = element_markdown(size = 20),
        plot.caption = element_text(color = "#D9544D", 
                                    hjust = 0.5, 
                                    size = 12,
                                    margin = margin(10,0,0,0)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, 
                                   angle = 45,
                                   margin = margin(10,0,0,0)),
        axis.ticks.length.x = NULL,
        panel.spacing = unit(1.5, "cm"),
        strip.text = element_text(size = 10,
                                  face = "bold",
                                  color = "black",
                                  vjust = 0),
        plot.margin = margin(20,20,20,20)
  )

ggsave("Provincial_distribution.png", width = 20, height = 10, units = "in", dpi = 300)







