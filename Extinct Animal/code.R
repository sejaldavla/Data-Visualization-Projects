### ----------------------------------------------------------------------------------------------------

## Data source: https://www.kaggle.com/datasets/junyaozhang/extinct-animals-database

### ----------------------------------------------------------------------------------------------------

# Required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

# Load data
rawdata <- read_excel("/Users/sejaldavla/Desktop/Data_Portfolio/Extinct Animal/extinct_animals.xlsx", sheet = 1)
rawdata

##### Data cleaning - Getting sense of data in each column
Extinct_Animals <- rawdata %>% 
  select(-c(2,4, 8:9))  #Removing the second, fourth, and last two columns with NA values
sum(is.na(Extinct_Animals)) # Check for NA values in the dataframe

str(Extinct_Animals)
unique(Extinct_Animals$`Extinction date`)

## There is text in the extinction date column. Checking different text types. 
with_text <- Extinct_Animals %>%
  filter(str_detect(`Extinction date`, "[a-zA-Z]")) %>%
  filter(str_detect(`Extinction date`, "s$"))
with_text

## Remove data for unknown or unverifiable timelines, such as holocene, prehistoric era or any uncertain timelines such as century

New <- Extinct_Animals %>%
  mutate(`Extinction date` = str_replace(`Extinction date`, "s$", "")) %>%
  filter(!str_detect(`Extinction date`, "[a-zA-Z]")) %>%
  mutate(`Extinction date` = str_replace(`Extinction date`,"\\?","")) %>%
  mutate(`Extinction date` = str_replace(`Extinction date`,"1864-1873","1870")) %>%
  mutate(`Extinction date` = str_trim(`Extinction date`, "both")) %>%
  mutate(`Extinction date` = as.numeric(`Extinction date`))

#### Data summary
A <- as.data.frame(New 
                   %>% filter(`Extinction date` >= 1500) %>%
                     count(Category))
A


#### Data visualization - Ridgeplot
library(forcats)
library(ggridges)
baseplot <- New %>% 
  filter(`Extinction date` >= 1500) %>%
  mutate(Category = fct_relevel(Category, c("Birds","Reptiles","Mammals","Amphibians","Fish"))) %>%
  ggplot(aes(`Extinction date`, y = Category, fill = Category)) +
  geom_density_ridges(color = "grey", alpha = 0.7, scale = 1.9) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Extinction Time (Year)",
    y = NULL,
    title = "Extinct animal species recoreded in recent times",
    caption = "Data source: Kaggle/Extinct Animals \nSejal Davla",
    subtitle = "Among the extinct animal species documented between 1500 - 2016, there is a rapid rise \nacross all groups in the post-industrial era"
  )
baseplot

plot <- baseplot +
  scale_x_continuous(limits = c(1500, 2020), breaks = seq(1500, 2020, 50)) +
  coord_cartesian(clip = "off") +
  geom_vline(xintercept = 1760, linetype = "dotted") +
  geom_vline(xintercept = 1840, linetype = "dotted") +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "blue") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", family = "Roboto", size = 25),
    plot.subtitle = element_text(size = 15, color = "grey5"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Arial", size = 10, color = "blue", face = "bold"),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x.bottom = element_line(color = "grey", linewidth = 2),
    panel.background = element_rect(fill = "#ccffff", colour = NULL),
    legend.position = "none"
  ) +
  annotate("text",
           x = 1525,
           y = c(0.75,1.75, 2.75, 3.75, 4.75),
           label = c("238 Birds","27 Reptiles","149 Mammals","32 Amhibians", "50 Fish"),
           size = 4,
           fontface = 3,
           hjust = 0) +
  annotate("text", 
           x = 1765, 
           y = 5.7, 
           label = "Rapid \nIndustrialization", 
           size = 3.75,
           fontface = 2, 
           color = "Red",
           hjust = 0)
plot

ggsave("plot.png", height = 15, width = 25, units = "cm", dpi = 600)



