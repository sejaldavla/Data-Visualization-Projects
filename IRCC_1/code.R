##### ----------------------------------------------------------------------------------------------------------------------------------------------------

## Data source: https://open.canada.ca/data/en/dataset/90115b00-f9b8-49e8-afa3-b4cff8facaee/resource/3897ef92-a491-4bab-b9c0-eb94c8b173ad

library(readxl)
library(tidyverse)
library(patchwork)
library(scales)
library(geofacet)
library(camcorder)
library(ggtext)
library(showtext)

## Data import and cleaning --------------------------------------------------------------------------------------------------------------------
ircc <- read_excel("/Users/sejaldavla/Desktop/Data_Portfolio/Immigration Canada/EN_ODP_annual-TR-Study-IS_CITZ_year_end.xlsx")
colnames(ircc) <- ircc[2,] # Change column names

#Remove rows with non-data information
dim(ircc)
slice_head(ircc, n = 10)
slice_tail(ircc, n = 10)
ircc <- ircc[-c(1,2,228:233), ]

# Arrange data for visualization and analysis
ircc_long <- ircc %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Enrolment") |>
  rename("Country" = "Country of Citizenship") |>
  mutate(Year = as.numeric(Year)) %>%
  mutate(Enrolment = str_replace(Enrolment, "--","3")) |> # In the dataset values between 0 and 5 are shown as “--“. Change it to mean value (2.5) rounded to 3.
  mutate(Enrolment = as.numeric(str_remove(Enrolment,","))) 
sum(is.na(ircc_long)) # No NA values in the dataframe

write.csv(ircc_long,"ircc_long.csv")

##### -----------------------------------------------------------------------------------------------------------------------------------------------------------------

## Data summary and visualization -------------------------------------------------------------------------------------------------------------------------------------

# Total enrolment by Year
ircc_year <- ircc_long |>
  filter(Country != "Total unique persons") |> # There is a total summary data per year in the original datafarme. remove it. 
  group_by(Year) |>
  summarize(Total = sum(Enrolment)) 

# Top 10 enrolments each year countrywise
ircc_top <- ircc_long |>
  filter(Country != "Total unique persons") |>
  group_by(Year) |>
  top_n(10, Enrolment) |>
  arrange(desc(Enrolment), .by_group = TRUE) |>
  mutate(Country = str_replace(Country, "China, People's Republic of", "China")) |>
  mutate(Country = str_replace(Country, "Korea, Republic of", "South Korea")) |>
  mutate(Country = str_replace(Country, "United Kingdom and Overseas Territories", "United Kingdom")) |>
  mutate(Country = str_replace(Country, "United States of America", "USA"))

## Plots ----------------------------------------------------------------------------------------------------------------------------

# Total recruitment
ircc_year |>
  ggplot(aes(x = Year, y = Total)) +
  geom_col() +
  geom_line() +
  scale_y_continuous(labels = label_comma()) +
  theme_classic()

## Circular Plot -------------------------------------------------------------------------------------------------------------------------------------

# Fonts
font_add_google(name = "Alegreya Sans SC")
font_add_google(name = "Lilita One")
font_add_google(name = "Lekton")
showtext_auto()

# create a function for circular bar graph

plot_circular <- function(year) {
  #Define color palette
  cp <- c('#00429d', '#3e67ae', '#618fbf', '#85b7ce', '#b1dfdb', '#ffcab9', '#fd9291', '#e75d6f', '#c52a52', '#93003a')
  # Assign names to color palette
  names(cp) <- ircc_top |>
    filter(Year == year) |>
    arrange(desc(Enrolment)) |>
    pull(Country) 
  # Plot data
  ircc_top |>
    filter(Year == year) |>
    group_by(Year) |>
    ggplot(aes(x = fct_reorder(Country, Enrolment), 
               y = Enrolment, 
               fill = Country)) +
    geom_bar(stat = "identity",
             width = 0.7) +
    coord_polar(theta = "y",
                start = 0,
                clip = "off") +
    geom_richtext(aes(y = 0, label = Country),
                  fill = NA,
                  hjust = 1,
                  vjust = 0.5,
                  family = "Alegreya Sans SC",
                  label.color = NA) +
    scale_y_continuous(limits = c(0, 375000),
                       labels = label_comma()) +
    scale_fill_manual(values = cp) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = bg_col,
                                         color = NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(0,0,0,0), "mm"))
}

p1 <- plot_circular(2022) +
  geom_richtext(aes(y = Enrolment, 
                    label = scales::comma(Enrolment)),
                fill = NA,
                family = "Alegreya Sans SC",
                angle = c(55, 259, 327, 333, 337, 340, 345, 350, 355, 0),
                hjust = 0,
                label.color = NA) +
  annotate(geom = "text",
           label = " 2022",
           x = -3, 
           y = 0,
           color = "#845EC2",
           size = 10,
           family = "Lilita One") +
  annotate(geom = "text",
           label = "Total Incoming students - 807,279",
           x = -5, 
           y = 0,
           color = "black",
           size = 5,
           family = "Lekton")
p1

ggsave("p1.png", width = 3, height = 3, units = "in", dpi = 300)


p2 <- plot_circular(2021) +
  geom_richtext(aes(y = Enrolment, 
                    label = scales::comma(Enrolment)),
                fill = NA,
                family = "Alegreya Sans SC",
                angle = c(157, 253, 330, 333, 337, 340, 345, 350, 355, 355),
                hjust = 0,
                label.color = NA) +
  annotate(geom = "text",
           label = " 2021",
           x = -3, 
           y = 0,
           color = "#845EC2",
           size = 10,
           family = "Lilita One") +
  annotate(geom = "text",
           label = "Total Incoming students - 617,238",
           x = -5, 
           y = 0,
           color = "black",
           size = 5,
           family = "Lekton")
p2

ggsave("p2.png", width = 3, height = 3, units = "in", dpi = 300)


p3 <- plot_circular(2020) +
  geom_richtext(aes(y = Enrolment, 
                    label = scales::comma(Enrolment)),
                fill = NA,
                family = "Alegreya Sans SC",
                angle = c(175, 245, 330, 333, 337, 340, 345, 350, 355, 0),
                hjust = 0,
                label.color = NA) +
  annotate(geom = "text",
           label = " 2020",
           x = -3, 
           y = 0,
           color = "#845EC2",
           size = 10,
           family = "Lilita One") +
  annotate(geom = "text",
           label = "Total Incoming students - 527,405",
           x = -5, 
           y = 0,
           color = "black",
           size = 5,
           family = "Lekton")

p3

ggsave("p3.png", width = 3, height = 3, units = "in", dpi = 300)

p4 <- plot_circular(2019) +
  geom_richtext(aes(y = Enrolment, 
                    label = scales::comma(Enrolment)),
                fill = NA,
                family = "Alegreya Sans SC",
                angle = c(155, 220, 330, 333, 337, 340, 345, 350, 355, 355),
                hjust = 0,
                label.color = NA) +
  annotate(geom = "text",
           label = " 2019",
           x = -3, 
           y = 0,
           color = "#845EC2",
           size = 10,
           family = "Lilita One") +
  annotate(geom = "text",
           label = "Total Incoming students - 637,866",
           x = -5, 
           y = 0,
           color = "black",
           size = 5,
           family = "Lekton")

p4

ggsave("p4.png", width = 3, height = 3, units = "in", dpi = 300)

p5 <- plot_circular(2018) +
  geom_richtext(aes(y = Enrolment, 
                    label = scales::comma(Enrolment)),
                fill = NA,
                family = "Alegreya Sans SC",
                angle = c(177, 220, 330, 333, 337, 340, 345, 350, 355, 355),
                hjust = 0,
                label.color = NA) +
  annotate(geom = "text",
           label = " 2018",
           x = -3, 
           y = 0,
           color = "#845EC2",
           size = 10,
           family = "Lilita One") +
  annotate(geom = "text",
           label = "Total Incoming students - 566,943",
           x = -5, 
           y = 0,
           color = "black",
           size = 5,
           family = "Lekton")

p5

ggsave("p5.png", width = 3, height = 3, units = "in", dpi = 300)


p6 <- plot_circular(2017) +
  geom_richtext(aes(y = Enrolment, 
                    label = scales::comma(Enrolment)),
                fill = NA,
                family = "Alegreya Sans SC",
                angle = c(225, 230, 330, 333, 337, 340, 345, 350, 355, 355),
                hjust = 0,
                label.color = NA) +
  annotate(geom = "text",
           label = " 2017",
           x = -3, 
           y = 0,
           color = "#845EC2",
           size = 10,
           family = "Lilita One") +
  annotate(geom = "text",
           label = "Total Incoming students - 490,763",
           x = -5, 
           y = 0,
           color = "black",
           size = 5,
           family = "Lekton")
p6

ggsave("p6.png", width = 3, height = 3, units = "in", dpi = 300)


p7 <- plot_circular(2016) +
  geom_richtext(aes(y = Enrolment, 
                    label = scales::comma(Enrolment)),
                fill = NA,
                family = "Alegreya Sans SC",
                angle = c(225, 280, 330, 333, 337, 340, 345, 350, 355, 355),
                hjust = 0,
                label.color = NA) +
  annotate(geom = "text",
           label = " 2016",
           x = -3, 
           y = 0,
           color = "#845EC2",
           size = 10,
           family = "Lilita One") +
  annotate(geom = "text",
           label = "Total Incoming students - 410,588",
           x = -5, 
           y = 0,
           color = "black",
           size = 5,
           family = "Lekton")
p7

ggsave("p7.png", width = 3, height = 3, units = "in", dpi = 300)


p8 <- plot_circular(2015) +
  geom_richtext(aes(y = Enrolment, 
                    label = scales::comma(Enrolment)),
                fill = NA,
                family = "Alegreya Sans SC",
                angle = c(230, 300, 330, 333, 337, 340, 345, 350, 355, 355),
                hjust = 0,
                label.color = NA) +
  annotate(geom = "text",
           label = " 2015",
           x = -3, 
           y = 0,
           color = "#845EC2",
           size = 10,
           family = "Lilita One") +
  annotate(geom = "text",
           label = "Total Incoming students - 352,312",
           x = -5, 
           y = 0,
           color = "black",
           size = 5,
           family = "Lekton")
p8 

ggsave("p8.png", width = 3, height = 3, units = "in", dpi = 300)

L1_title <- "Where do Canada's International students come from?"
L1_subtitle <- "Ranking newly arrived international student population by top 10 nationalities each <span style = 'color:#845EC2'>year</span>"

L1 <- p8 + p7 + p6 + p5 + p4 + p3 + p2 + p1 +
  plot_layout(ncol = 4) +
  plot_annotation(title = L1_title,
                  subtitle = L1_subtitle,
                  caption = caption_text) &
  theme(plot.title.position = "plot",
        plot.background = element_rect(fill = bg_col),
        plot.title = element_text(family = "Lilita One",
                                  color = "#D9544D",
                                  size = 40,
                                  face = "bold",
                                  margin = margin(0,10,10,0)),
        plot.subtitle = element_markdown(size = 25),
        plot.caption = element_text(color = "#D9544D",
                                    hjust = 0.5,
                                    size = 15,
                                    margin = margin(5,0,0,0))) 

L1

ggsave("L1.png", width = 9.2, height = 4.6, units = "in", dpi = 300)





# Data source: https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
region <- read.csv("/Users/sejaldavla/Desktop/Data_Portfolio/Immigration Canada/all.csv")

regions <- region |>
  select(name, region, sub.region) |>
  rename("Country" = "name")

# Combine the data to include continent and subregion information
ircc_1 <- ircc_long |>
  left_join(regions, by = "Country")

b <- ircc_1 |>
  filter(is.na(region))

unique(b$Country)
  






#treemap

ircc_1 |>
  filter(Country != "Total unique persons") |>
  filter(Year == 2022 & Enrolment > 10) |>
  ggplot(aes(area = Enrolment, fill = Country, label = Country, subgroup = region)) +
  geom_treemap() +
  geom_treemap_text(color = "black", place = "centre") +
  theme(legend.position = "none") 


a <- ircc_long |>
  filter(Country == "Total unique persons") 

a |> 
  ggplot(aes(x = Year, y = Enrolment)) +
  geom_line() +
  scale_y_continuous(labels = label_comma())


