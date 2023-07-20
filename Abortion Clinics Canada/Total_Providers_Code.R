# Required packages

library(dplyr)
library(ggplot2)
library(hrbrthemes)

## Create a data table from Action Canada poster and population data from Stat Canada
Geography <- c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador",
               "Northwest Territories","Nova Scotia","Nunavut","Ontario","Prince Edward Island",
               "Quebec","Saskatchewan","Yukon")
Total_Providers <- c(5,24,4,5,4,1,11,2,38,1,49,3,1)
Medical_Abortion_Providers <- c(3,17,3,3,1,1,9,2,22,1,21,2,1)
Surgical_Abortion_Providers <- c(4,18,4,4,2,1,4,2,31,1,44,3,1)
Rural_Providers <- c(0,1,0,0,1,0,9,1,4,0,6,0,0)
Urban_Providers <- c(5,23,4,5,3,1,2,1,34,1,43,3,1)
Hospitals_Providing_Abortion_Care <- c(2,10,3,3,2,1,4,1,20,1,18,2,0)
Clinics_Providing_Abortion_Care <- c(3,14,1,2,1,0,7,1,18,0,31,1,1)
Crisis_Pregnancy_Centres <- c(21,26,7,7,0,0,6,0,77,2,14,5,0)

Abortion_Services <- data.frame(Geography,Total_Providers,Medical_Abortion_Providers,
                                Surgical_Abortion_Providers,Rural_Providers,Urban_Providers,
                                Hospitals_Providing_Abortion_Care,
                                Clinics_Providing_Abortion_Care,Crisis_Pregnancy_Centres)

Population <- read.csv("Canadian_population_by_province.csv", stringsAsFactors = FALSE)

New_Population <- select(Population, -c(2,3,4,5)) %>% 
  rename(population = Q1.2022) %>% 
  arrange(Geography) %>% 
  slice(-c(1,4))

Demographic <- New_Population %>% 
  mutate(new_col = c(2210682,2635832,692498,398132,263601,22137,506184,19312,7499595,82912,4300448,585844,21099)) %>% 
  rename(female_population = new_col)

#merge population and abortion clinic data
Abortion_data <- Demographic %>% 
  full_join(Abortion_Services, by = "Geography")

Abortion_data

# Create a bar graph of total providers 
AD <- Abortion_data %>%
  filter(!is.na(Total_Providers)) %>%
  arrange(Total_Providers) %>%
  mutate(Geography=factor(Geography, Geography)) %>%
  ggplot(aes(x=Geography, y=Total_Providers)) +
  geom_segment(aes(x=Geography ,xend=Geography, y=0, yend=Total_Providers), 
               linewidth = 4, color="#969696", alpha = 0.5) +
  geom_point(size=7, color="#cc3340", alpha = 0.8, stroke = 0.5) +
  geom_text(aes(label = Total_Providers), size = 4, color = 'white') +
  coord_flip(expand = FALSE, clip = "off") + 
  labs(x = NULL,
       y = NULL,
       title = "Abortion Service Providers Across Canadian Provinces",
       caption = "Data source: Action Canada \nBy Sejal Davla, PhD") +
  hrbrthemes::theme_ipsum() +
  theme(text = element_text(size = 12, family = "Helvetica"), 
        plot.title = element_text(color = "#65a5c7", vjust = 5),
        axis.text.y = element_text(color="#8067B7", size=14),
        axis.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = '#282828'),
        plot.caption = element_text(color = "white", 
                                    size = 12, 
                                    family = 'Arial'))
AD

ggsave("Abortion Data.png", plot = AD, 
       path = "/Users/sejaldavla/Desktop/Data_Portfolio/Aborsion Clinic Data Canada",
       height = 7, width = 10, units = "in", bg = "white")

Sys.info()
