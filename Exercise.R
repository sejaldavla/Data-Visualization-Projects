g_oceania <- readr::read_csv("/Users/sejaldavla/Downloads/fundamentals-ggplot2-pearson-main/data/gapminder_oceania.csv")
g_oceania

g_oceania %>% 
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_path()

g_oceania %>% 
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_line()


g_oceania %>% 
  group_by(country) %>% 
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_path()

g_oceania %>% 
  group_by(country) %>% 
  ggplot(aes(x = year, y = gdpPercap), color = country) +
  geom_line()


g_oceania %>% 
  group_by(country) %>% 
  ggplot(aes(x = year, y = gdpPercap), color = country) +
  geom_path() +
  geom_point()

g_oceania %>% 
  group_by(country) %>% 
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_line(aes(color = country)) +
  geom_point(aes(color = country), shape = 23) +
  labs(
    x = "Year",
    y = "GDP per capita in Oceania countries"
  )



g_oceania %>% 
  group_by(country) %>% 
  ggplot(aes(x = year, y = gdpPercap)) +
  geom_line(aes(color = country)) +
  geom_point(aes(color = country, shape = country)) +
  labs(
    x = "Year",
    y = "GDP per capita in Oceania countries",
    color = NULL,
    shape = NULL,
    title = "Income Trends in Oceanic Countries"
  )


#######-------------------------------------------------------------------------------------------------

## Exercise 2

#######------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(readr)
library(gapminder)
library(forcats)


## Extract data for years 1987 and 2007
gm_1987_2007 <- gapminder %>%
  filter(year%in% c("1987","2007"))

gm_1987_2007


base_plot <- gm_1987_2007 %>%
  mutate(continent = fct_relevel(continent, c("Africa","Asia","Americas","Europe","Oceania"))) %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(size = 0.3, alpha = 0.7) +
  geom_point(aes(color = gdpPercap), size = 3, shape = 19, alpha = 0.5) +
  facet_wrap(~continent, ncol = 5) +
  labs(
    x = NULL,
    y = "Life expectancy at birth",
    title = "Life expectancy has risen in most countries within the last 20 years",
    caption = "Source:Gapminder project",
    subtitle = "While several African countries have experienced a decline in GDP, only three countries \noutside of Africa showed a downward trend."
  ) 
base_plot

Final_Plot <- base_plot +
  scale_color_continuous(type = "viridis", name = "GDP per capita", labels = c("0","$10,000","$20,000","$30,000","$40,000","50,000")) +
  scale_x_continuous(limits = c(1987, 2007),
                     breaks = seq(1987, 2007, by = 20),
                     guide = guide_axis(check.overlap = TRUE)) +
  scale_y_continuous(limits = c(35, 85),
                     breaks = seq(35, 85, by = 5)) + 
  coord_cartesian(
    expand = FALSE,
    clip = "off"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "grey50"),
    panel.grid.major.x =  element_blank(),
    panel.grid.major.y = element_line(color = "#D3D3D3"),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "#D3D3D3"),
    panel.spacing.x = unit(1.5, "lines"),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_blank()
  ) 
Final_Plot


ggsave("final_plot.png", Final_Plot, units = "cm", height = 15, width = 20, dpi = 600)




