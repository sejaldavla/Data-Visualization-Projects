library(tidyverse)
library(ggdist)
library(tidyquant)
library(ggtext)
library(extrafont)
library(showtext)
library(ragg)
library(rstatix)

# Load data
OE_1 <- read.csv("OE/20200702 AANAT OE 6h.csv")
OE_2 <- read.csv("OE/20200704 AANAT OE 6h.csv")
OE_3 <- read.csv("OE/20200708 AANAT OE 6h.csv")

# Select relevant columns
OE_1 <- OE_1 %>% 
  select(Genotype, RS)

OE_2 <- OE_2 %>% 
  select(Genotype, RS)

OE_3 <- OE_3 %>% 
  select(Genotype, RS) %>%
  mutate(RS = as.integer(RS))

# Combine 3 data frames
OE <- bind_rows(OE_1, OE_2, OE_3)
OE <- OE %>%
  drop_na()

# Organize data for statistical analysis and visualization 
OE1 <- OE %>%
  mutate(Sleep = as.numeric(RS)) %>%
  mutate(Group = case_when(
    Genotype == "Alrm-Gal4>Iso" ~ "Control 1",
    Genotype == "Iso>UAS-AANAT1" ~ "Control 2",
    Genotype == "Alrm-Gal4>UAS-AANAT1" ~ "Experiment",
    Genotype == "Iso>Uas-AANAT" ~ "Control 2",
    Genotype == "Alrm-Gal4>Uas-AANAT" ~ "Experiment"
  )) %>%
  arrange(Group) %>%
  mutate(Index = seq(1:length(Group))) %>%
  relocate(Index, Group) %>%
  select(-RS)
OE1

# Define the color palette 
pal <- c("#5EB8E2","#98232C","#3E96D9")

label1 <- "<span style = 'color:#686761'>Control 1<br>(*Gal4*)"
label2 <- "<span style = 'color:#686761'>Experiment<br>(*Gal4*>*UAS*)"
label3 <- "<span style = 'color:#686761'>Control 2<br>(*UAS*)"

caption_text <- "Data: Davla *et al*., eLife, 2020 DOI: 10.7554/eLife.53994<br>By Sejal Davla,PhD"

# Create a Raincloud plot

p <- OE1 %>%
  mutate(Group = fct_relevel(Group, "Control 1", "Experiment", "Control 2")) %>%
  ggplot(aes(Group, Sleep, fill = factor(Group))) +
  stat_halfeye(.width = 0,
               point_color = NA,
               alpha = 0.7,
               justification = -0.07,
               position = position_dodge()) +
  geom_boxplot(width = 0.1,
               outlier.color = NA,
               alpha = 0.8) +
  geom_dotsinterval(side = "left",
                    position = "dodgejust",
                    dotsize = 0.9,
                    justification = 1.07,
                    stroke = 0) +
  scale_fill_manual(values = pal) +
  coord_flip(ylim = c(-100, 350),
             clip = "off") +
  theme_tq() +
  theme(axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.caption = element_markdown(size = 7,
                                        color = "blue")) +
  labs(y = "Relative sleep (minutes)",
       caption = caption_text) +
  geom_richtext(x = 1.25,
                y = 315,
                label = label1,
                fill = NA,
                label.color = "#5EB8E2",
                inherit.aes = FALSE,
                hjust = 0,
                size = 3) +
  geom_richtext(x = 2.55,
                y = 310,
                label = label2,
                fill = NA,
                label.color = "#98232C",
                inherit.aes = FALSE,
                hjust = 0,
                size = 3) +
  geom_richtext(x = 3.35,
                y = 215,
                label = label3,
                fill = NA,
                label.color = "#3E96D9",
                inherit.aes = FALSE,
                hjust = 0,
                size = 3) +
  stat_summary(geom = "point",
               fun = "mean",
               color = "black",
               shape = 18,
               size = 4,
               alpha = 0.7, 
               stroke = 0.7) + 
  stat_summary(geom = "text",
               fun = "mean",
               aes(label = paste("mean =", round(after_stat(y), 2))),
               color = "#242424",
               size = 3,
               family = "Calibri",
               vjust = -1.2) +
  expand_limits(x = -0.2)


ggsave("p.tiff", width = 7, height = 4, units = "in", dpi = 300)  


# Summary statistics

ss <- OE1 %>%
  select(Group, Sleep) %>%
  group_by(Group) %>%
  summarise(count = n(),
            mean = mean(Sleep, na.rm = TRUE),
            SD = sd(Sleep, na.rm = TRUE))

# Prepare data to conduct normality test
ss1 <-OE1 %>%
  group_by(Group) %>%
  mutate(Index = 1:n()) %>%
  ungroup() %>%
  select(Group, Sleep) %>%
  add_row(Group = "Control 1", Sleep = NA) %>%
  pivot_wider(names_from = Group,
              values_from = Sleep) %>%
  unnest(everything())

# Shapiro-Wilk normality test
apply(ss1, 2, shapiro.test)

# Data in one control group does not have normal distributed (p = 0.003), therefore we will use non-parametric test

kwt <- kruskal.test(Sleep ~ factor(Group), data = OE1)
kwt # There is a significant difference between the groups

#Dunn's test for groupwise comparisons
d <- dunn_test(data = OE1,
               formula = Sleep ~ Group,
               p.adjust.method = "holm")
d