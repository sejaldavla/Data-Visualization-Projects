# required packages

library(tidyverse)
library(here)
library(janitor)
library(ggtext)
library(showtext)
library(ragg)
library(glue)

# data downloaded from https://www.gunviolencearchive.org/mass-shooting

# data wrangling

gva <- read_csv(here("gva.csv")) |>
  clean_names()

gva_total <- gva |>
  select(incident_date, victims_killed, victims_injured) |>
  mutate(incident_date = mdy(incident_date)) |>
  group_by(incident_date) |>
  summarize(killed = sum(victims_killed),
            injured = sum(victims_injured))

## make a calendar

# create dataframe
calender_df <- data.frame(date = seq(ymd('2021-01-01'), ymd('2023-12-31'), by = "1 day"))

gva_pooled <- gva_total |>
  group_by(incident_date) |>
  summarise(total = killed + injured) |>
  drop_na() 
  
calendar_pooled <- calender_df |>
  mutate(Year = year(date),
         Month = month(date, 
                       label = TRUE,
                       abbr = FALSE),
         Day = wday(date, 
                    label = TRUE, 
                    abbr = FALSE,
                    week_start = 1),
         mday = mday(date),
         Month_week = (5 + day(date) + 
                         wday(floor_date(date, 'month'), week_start = 1)) %/% 7,
         neg_week = Month_week * (-1)) |> 
  left_join(gva_pooled, by = c("date" = "incident_date")) |>
  mutate(range_all = case_when(total %in% 1:10 ~ "1-10",
                           total %in% 11:20 ~ "11-20",
                           total %in% 21:30 ~ "21-30",
                           total %in% 31:40 ~ "31-40",
                           total %in% 41:50 ~ "41-50",
                           total %in% 51:90 ~ "more than 50")) |>
  mutate(Month_French = case_when(Month == "January" ~ "janvier",
                                  Month == "February" ~ "février",
                                  Month == "March" ~ "mars",
                                  Month == "April" ~ "avril",
                                  Month == "May" ~ "mai",
                                  Month == "June" ~ "juin",
                                  Month == "July" ~ "juillet",
                                  Month == "August" ~ "août",
                                  Month == "September" ~ "septembre",
                                  Month == "October" ~ "octobre",
                                  Month == "November" ~ "novembre",
                                  Month == "December" ~ "décembre"))
# color assignment

pal_new <- c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#C900FF","#ffffff")

calendar_pooled$range_all <- factor(calendar_pooled$range_all,
                                 levels = c("1-10", "11-20","21-30","31-40","41-50","more than 50"))

names(pal_new) <- levels(factor(calendar_pooled$range_all))

# image

pic <- image_read(here("pic.png"))
print(pic)
image_scale(pic, 10)

# fonts
font_add_google(name = "Play")
font_add_google(name = "Bebas Neue")
font_add_google(name = "Space Grotesk")
showtext_auto()

# text

title_text <- "A very American problem   <img src='pic.png' width='40'/>"
subtitle_text <- glue("This calendar shows the **total number of people killed and injured everyday** due to gun violence 
                      in the United States of America in the years 2021, 2022, and 2023. 
                      Each year more than 650 people get killed and more than 2600 are injured.")
caption_text <- "Source: Gun Violence Archive • Graphic: Sejal Davla, PhD"


# plot

gva_pooled_plot <- calendar_pooled |>
  ggplot(aes(x = Day, y = neg_week)) +
  geom_tile(fill = "white",
            color = "white") +
  geom_point(data = drop_na(calendar_pooled),
             aes(color = range_all, fill = range_all),
             size = 2.5,
             alpha = 0.7) +
  geom_text(aes(label = mday),
            color = "gray70",
            size = 5,
            family = "Play",
            fontface = "bold") +
  facet_grid(Year ~ Month,
             switch = "y") +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text) +
  scale_color_manual(name = "",
                     values = c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#C900FF"),
                     labels = c("Between 1 and 10", "Between 11 and 20", "Between 21 and 30", "Between 31 and 40", "Between 41 and 50", "More than 50"),
                     guide = guide_legend(ncol = 6)) +
  scale_fill_manual(name = "",
                    values = c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#C900FF"),
                    labels = c("Between 1 and 10", "Between 11 and 20", "Between 21 and 30", "Between 31 and 40", "Between 41 and 50", "More than 50"),
                    guide = guide_legend(ncol = 6)) + 
  theme_void() +
  theme(plot.title = element_markdown(size = 50,
                                  family = "Bebas Neue"),
        plot.subtitle = element_textbox_simple(family = "Space Grotesk",
                                               color = "gray40",
                                               size = 18,
                                               lineheight = 0.4,
                                               hjust = 0,
                                               margin = margin(10, 0, 10, 0)),
        plot.caption = element_text(size = 15,
                                    color = "#1565C0",
                                    hjust = 0.5),
        strip.text = element_text(size = 20),
        strip.text.y = element_text(angle = 90),
        plot.background = element_rect(fill = "white"),
        legend.position = "top",
        legend.text = element_textbox(size = 16,
                                   family = "Space Grotesk",
                                   color = "gray40"),
        legend.key.width = unit(0.5, "cm"),
        plot.margin = margin(5,5,5,5)) 

ggsave("gva_pooled_plot.png", width = 10, height = 4, units = "in")

####---------------------------------------------------------------------------------------------------------------------------------------------------------

##French version

calendar_pooled$Month_French <- ordered(calendar_pooled$Month_French, 
                                        levels = c("janvier",
                                                   "février", 
                                                   "mars", 
                                                   "avril", 
                                                   "mai", 
                                                   "juin", 
                                                   "juillet", 
                                                   "août", 
                                                   "septembre", 
                                                   "octobre", 
                                                   "novembre", 
                                                   "décembre"))

# text

title_french <- "Un problème très américain   <img src='pic.png' width='40'/>"
subtitle_french <- glue("Ce calendrier montre le nombre total de personnes tuées et blessées chaque jour lors de fusillades aux États-Unis d'Amérique au cours des années 2021, 2022 et 2023.
                        Chaque année, plus de 650 personnes sont tuées et plus de 2600 sont blessées.")
caption_french <- "Source : Archives sur la violence armée • Graphique : Sejal Davla, PhD"


# plot

gva_pooled_plot_French <- calendar_pooled |>
  ggplot(aes(x = Day, y = neg_week)) +
  geom_tile(fill = "white",
            color = "white") +
  geom_point(data = drop_na(calendar_pooled),
             aes(color = range_all, fill = range_all),
             size = 2.5,
             alpha = 0.7) +
  geom_text(aes(label = mday),
            color = "gray70",
            size = 5,
            family = "Play",
            fontface = "bold") +
  facet_grid(Year ~ Month_French,
             switch = "y") +
  labs(title = title_french,
       subtitle = subtitle_french,
       caption = caption_french) +
  scale_color_manual(name = "",
                     values = c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#C900FF"),
                     labels = c("entre 1 et 10", "entre 11 et 20", "entre 21 et 30", "entre 31 et 40", "entre 41 et 50", "plus que 50"),
                     guide = guide_legend(ncol = 6)) +
  scale_fill_manual(name = "",
                    values = c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#C900FF"),
                    labels = c("entre 1 et 10", "entre 11 et 20", "entre 21 et 30", "entre 31 et 40", "entre 41 et 50", "plus que 50"),
                    guide = guide_legend(ncol = 6)) + 
  theme_void() +
  theme(plot.title = element_markdown(size = 50,
                                      family = "Bebas Neue"),
        plot.subtitle = element_textbox_simple(family = "Space Grotesk",
                                               color = "gray40",
                                               size = 18,
                                               lineheight = 0.4,
                                               hjust = 0,
                                               margin = margin(10, 0, 10, 0)),
        plot.caption = element_text(size = 15,
                                    color = "#1565C0",
                                    hjust = 0.5),
        strip.text = element_text(size = 20),
        strip.text.y = element_text(angle = 90),
        strip.clip = "off",
        plot.background = element_rect(fill = "white"),
        legend.position = "top",
        legend.text = element_textbox(size = 16,
                                      family = "Space Grotesk",
                                      color = "gray40"),
        legend.key.width = unit(0.5, "cm"),
        plot.margin = margin(5,5,5,5)) 

ggsave("gva_pooled_plot_french.png", width = 10, height = 4, units = "in")

