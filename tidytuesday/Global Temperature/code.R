library(readr)
library(tidyverse)
library(ggtext)
library(showtext)


global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')
zonann_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')


font_add_google(name = "Monoton", family = "Monoton")
font_add_google(name = "Cabin Sketch", family = "Cabin Sketch")
showtext_auto()

title_text <- "Is the planet getting hotter?"

caption_text <- "Data: GISS Surface Temperature Analysis, NASA \nBy Sejal Davla, PhD"

temp1 <- zonann_temps %>%
  ggplot(aes(x = Year, y = Glob, fill = Glob)) +
  geom_col() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       name = "Global Temperature (°C)",
                       limits = c(-0.5,1.02)) +
  labs(x = NULL,
       y = "",
       title = title_text,
       fill = "Glob",
       caption = caption_text) +
  scale_x_continuous(breaks = seq(1880, 2022, 20)) +
  scale_y_continuous(limits = c(-0.5, 1.2),
                     breaks = seq(-0.5, 1.2, 0.5))+
  coord_cartesian(expand = FALSE) +
  theme_classic(base_family = "sans", 
                base_size = 12) +
  theme(plot.title = element_text(family = "Cabin Sketch", 
                                  size = 80, 
                                  color = "white",
                                  vjust = 0.05,
                                  hjust = -0.1),
        plot.caption = element_text(size = 30,
                                    color = "#e1e1e1",
                                    vjust = -15,
                                    lineheight = 0.4),
        panel.background = element_rect(fill = "#252321"),
        plot.background = element_rect(fill = "#252321"),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = "white", 
                                   size = 35,
                                   vjust = -1.5),
        axis.text.y = element_blank(),                           
        legend.direction = "horizontal",
        legend.position = "None",
        legend.background = element_rect(fill = "#252321"),
        legend.text = element_text(color = "white", size = 40),
        legend.title = element_text(color = "white", size = 40),
        legend.text.align = 0.5,
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        plot.margin = margin(0, 0.5, 1.5, 0.5, unit = "cm")
        ) + 
  geom_hline(yintercept = -0.48, 
             linetype = 3, 
             color = "#9567E0", 
             alpha = 0.7, 
             linewidth = 1) +
  annotate(geom = "text", 
           x = 1950, 
           y = -0.45, 
           label = "-0.48°C", 
           size = 12, 
           color = "#f6f6f6") +
  geom_hline(yintercept = 0.5, 
             linetype = 3, 
             color = "#f2a62c", 
             alpha = 0.7, 
             linewidth = 1) +
  annotate(geom = "text", 
           x = 1950, 
           y = 0.53, 
           label = "0.50°C", 
           size = 12, 
           color = "#f6f6f6") +
  geom_hline(yintercept = 1.02, 
             linetype = 3, 
             color = "#f21d0a", 
             alpha = 0.7, 
             linewidth = 1) +
  annotate(geom = "text", 
           x = 1950, 
           y = 1.05, 
           label = "1.02°C", 
           size = 12, 
           color = "#f6f6f6")

ggsave("temp1.png", width = 6, height = 8, units = "in", dpi = 300)


