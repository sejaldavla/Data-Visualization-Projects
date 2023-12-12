# required packages

library(tidyverse)
library(here)
library(janitor)
library(ggtext)
library(showtext)
library(ragg)
library(glue)
library(patchwork)


# load data

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

calendar <- calender_df |>
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
  left_join(gva_total, by = c("date" = "incident_date")) |>
  mutate(range_killed = case_when(killed %in% 1:5 ~ "1-5",
                                  killed %in% 6:15 ~ "6-15",
                                  killed %in% 16:25 ~ "16-25",
                                  killed == 0 ~ "other",
                                  killed == NA ~ "other")) |>
  mutate(range_injured = case_when(injured %in% 1:5 ~ "1-5",
                                   injured %in% 6:15 ~ "6-15",
                                   injured %in% 16:25 ~ "16-25",
                                   injured %in% 26:35 ~ "26-35",
                                   injured %in% 36:45 ~ "36-45",
                                   injured %in% 46:80 ~ "more than 45",
                                   injured == 0 ~ "other",
                                   injured == NA ~ "other")) 

# fonts
font_add_google(name = "Play")
font_add_google(name = "Bebas Neue")
font_add_google(name = "Space Grotesk")
showtext_auto()


# calculate total killed and injured every year

tots <- calendar |>
  select(Year, killed, injured) |>
  drop_na() |>
  group_by(Year) |>
  summarise(ktotal = sum(killed),
            itotal = sum(injured))


###---------------------------------------------------------------------------------------------------------------------------------

# Create a viz with killed and injured people added together

gva_pooled <- calendar |>
  group_by(date) |>
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
  left_join(gva_pooled, by = c("date" = "date")) |>
  mutate(range_all = case_when(total %in% 1:10 ~ "1-10",
                           total %in% 11:20 ~ "11-20",
                           total %in% 21:30 ~ "21-30",
                           total %in% 31:40 ~ "31-40",
                           total %in% 41:50 ~ "41-50",
                           total %in% 51:90 ~ "more than 50")) 

# color assignment

pal_new <- c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#C900FF","#ffffff")

calendar_pooled$range_all <- factor(calendar_pooled$range_all,
                                 levels = c("1-10", "11-20","21-30","31-40","41-50","more than 50"))

names(pal_new) <- levels(factor(calendar_pooled$range_all))

# image

pic <- image_read(here("pic.png"))
print(pic)
image_scale(pic, 10)

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


### --------------------------------------------------------------------------------------------------------------------------------

# Separate plots for killed and injured by gun violence


# plot

pal1 <- c("#F9F871","#FFC55B","#FF3F00","#ffffff")

calendar$range_killed <- factor(calendar$range_killed,
                                levels = c("1-5", "6-15","16-25","other"))
names(pal1) <- levels(factor(!is.na(calendar$range_killed)))


pal <- c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#5566E8","#ffffff")

calendar$range_injured <- factor(calendar$range_injured,
                                 levels = c("1-5", "6-15","16-25","26-35","36-45","more than 45","other"))
names(pal) <- levels(factor(!is.na(calendar$range_injured)))



p1 <- calendar |>
  ggplot(aes(x = Day, y = neg_week)) +
  geom_tile(fill = "white",
            color = "white") +
  geom_point(data = drop_na(calendar),
             aes(color = range_killed, fill = range_killed),
             size = 2.5,
             alpha = 0.7,
             show.legend = FALSE) +
  geom_text(aes(label = mday),
            color = "gray70",
            size = 5,
            family = "Play",
            fontface = "bold") +
  facet_grid(Year ~ Month,
             switch = "y") +
  scale_color_manual(values = c("#F9F871","#FFC55B","#FF3F00","#ffffff"),
                     labels = c("Between 1 and 5", "Between 6 and 15", "Between 16 and 25", "other")) +
  scale_fill_manual(values =  c("#F9F871","#FFC55B","#FF3F00","#ffffff"),
                    labels = c("Between 1 and 5", "Between 6 and 15", "Between 16 and 25", "other")) +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text) +
  theme_void() +
  theme(plot.title = element_text(size = 50,
                                  family = "Bebas Neue"),
        plot.subtitle = element_textbox_simple(family = "Space Grotesk",
                                               size = 18,
                                               lineheight = 0.4,
                                               hjust = 0,
                                               margin = margin(10, 0, 10, 0)),
        strip.text = element_text(size = 20),
        legend.position = "none") 

ggsave("p1.png", width = 10, height = 4, units = "in")


# Injured by gun violence

p2 <- calendar |>
  ggplot(aes(x = Day, y = neg_week)) +
  geom_tile(fill = "white",
            color = "white") +
  geom_point(data = drop_na(calendar),
             aes(color = range_injured, fill = range_injured),
             size = 2.5,
             alpha = 0.8) +
  geom_text(aes(label = mday),
            color = "gray70",
            size = 5,
            family = "Play",
            fontface = "bold") +
  facet_grid(Year ~ Month,
             switch = "y") +
  scale_color_manual(name = "Number of people killed or injured in a shooting: ",
                     values = c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#5566E8","#ffffff"),
                     labels = c("Between 1 and 5", "Between 6 and 15", "Between 16 and 25", "Between 26 and 35", "Between 36 and 45", "more than 45", "other"),
                     guide = guide_legend(ncol = 6,
                                          label.hjust = 0.5)) +
  scale_fill_manual(name = "Number of people killed or injured in a shooting: ",
                    values = c("#F9F871","#FFC55B","#FF3F00","#E4021B","#D758CD","#5566E8","#ffffff"),
                    labels = c("Between 1 and 5", "Between 6 and 15", "Between 16 and 25", "Between 26 and 35", "Between 36 and 45", "more than 45", "other"),
                    guide = guide_legend(ncol = 6)) +
  labs(title = "Persons Injured") +
  theme_void() +
  theme(plot.title = element_text(size = 40,
                                  face = "bold.italic",
                                  hjust = 0.5) ,
        strip.text = element_text(size = 20),
        legend.position = "top")

ggsave("p2.png", width = 10, height = 4, units = "in")


gva_plot <- p1 / p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "top",
        legend.justification = "left") 

ggsave("gva_plot.png", width = 12, height = 8, units = "in")


