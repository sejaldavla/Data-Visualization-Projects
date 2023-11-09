# Load packages ----------------------------------------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(scales)
library(geofacet)
library(camcorder)
library(ggtext)

# Data was obtained from the Open Data Canada website 

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







