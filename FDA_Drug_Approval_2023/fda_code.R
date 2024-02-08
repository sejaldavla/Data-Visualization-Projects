# Required packages

devtools::install_github("doehm/ggbrick",
                         force = TRUE)


library(tidyverse)
library(rvest)
library(XML)
library(janitor)
library(ggbrick)
library(ggtext)
library(showtext)
library(ragg)
library(plotly)

####-------------------------------------------------------------------------------------------------------------------------------------------

# Get data
fda <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2023")
tables <- fda |> 
  html_table(fill = TRUE)

fda_2023 <- tables[[1]] |>
  clean_names()
    
disease_category <- c("Neurology",
                      "Endocrinology",
                      "Oncology",
                      "Oncology",
                      "Hematology",
                      "Genetic disorder",
                      "Nephrology",
                      "Neurology",
                      "Neurology",
                      "Neurology",
                      "Oncology",
                      "Infectious diseases",
                      "Genetic disorder",
                      "Neurology",
                      "Neurology",
                      "Obstetrics and Gynecology",
                      "Opthalmology",
                      "Oncology",
                      "Infectious diseases",
                      "Infectious diseases",
                      "Oncology",
                      "Cardiovascular",
                      "Oncology",
                      'Dermatology',
                      "Neurology",
                      "Endocrinology",
                      "Infectious diseases",
                      "Oncology",
                      "Infectious diseases",
                      "Neurology",
                      "Opthalmology",
                      "Oncology",
                      "Oncology",
                      "Genetic disorder",
                      "Genetic disorder",
                      "Oncology",
                      "Oncology",
                      "Neurology",
                      "Genetic disorder",
                      "Nephrology",
                      "Gastroenterology",
                      "Neurology",
                      "Dermatology",
                      "Genetic disorder",
                      "Gastroenterology",
                      "Oncology",
                      "Oncology",
                      "Nephrology",
                      "Oncology",
                      "Hematology",
                      "Oncology",
                      "Oncology",
                      "Nephrology",
                      "Dermatology",
                      "Neurology")

fda_2023 <- fda_2023 |>
  mutate(disease_category = disease_category) |>
  mutate(year = rep(2023, 55))

# year 2022

fda_1 <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2022")
tables_1 <- fda_1 |> 
  html_table(fill = TRUE)

fda_2022 <- tables_1[[1]] |>
  clean_names() |>
  mutate(year = rep(2022, 37)) |>
  arrange(no) |>
  mutate(disease_category = c("Neurology",
                              "Dermatology",
                              "Oncology",
                              "Opthalmology",
                              "Hematology",
                              "Hematology",
                              "Oncology",
                              "Neurology",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Cardiovascular",
                              "Gastroenterology",
                              "Endocrinology",
                              "Dermatology",
                              "Neurology",
                              "Genetic disorder",
                              "Dermatology",
                              "Neurology",
                              "Dermatology",
                              "Oncology",
                              "Nephrology",
                              "Neurology",
                              "Opthalmology",
                              "Neurology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Endocrinology",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Oncology",
                              "Respiratory",
                              "Neurology",
                              'Dermatology'))

# year 2021

fda_2 <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2021")
tables_2 <- fda_2 |> 
  html_table(fill = TRUE)

fda_2021 <- tables_2[[1]] |>
  clean_names() |>
  mutate(year = rep(2021, 51)) |>
  arrange(no) |>
  mutate(disease_category = c("Cardiology",
                              "Infectious diseases",
                              "Nephrology",
                              "Oncology",
                              "Oncology",
                              "Cardiovascular",
                              "Oncology",
                              "Genetic disorder",
                              "Genetic disorder",
                              "Oncology",
                              "Neurology",
                              "Oncology",
                              "Neurology",
                              "Endocrinology",
                              "Neurology",
                              "Obstetrics and Gynecology",
                              "Oncology",
                              "Oncology",
                              "Neurology",
                              "Nephrology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Neurology",
                              "Infectious diseases",
                              "Neurology",
                              "Oncology",
                              "Endocrinology",
                              "Infectious diseases",
                              "Immunology",
                              "Dermatology",
                              "Immunology",
                              "Genetic disorder",
                              "Genetic disorder",
                              "Nephrology",
                              "Endocrinology",
                              "Oncology",
                              "Oncology",
                              "Neurology",
                              "Genetic disorder",
                              "Immunology",
                              "Hematology",
                              "Endocrinology",
                              "Infectious diseases",
                              "Oncology",
                              "Immunology",
                              "Neurology",
                              "Cardiovascular",
                              "Dermatology"))


# year 2020

fda_3 <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2020")
tables_3 <- fda_3 |> 
  html_table(fill = TRUE)

fda_2020 <- tables_3[[1]] |>
  clean_names() |>
  mutate(year = rep(2020, 54)) |>
  mutate(no = as.numeric(no)) |>
  mutate(no = if_else(drug_name == "Uplizna", 23, no)) |>
  mutate(approval_date = if_else(drug_name == "Ebanga", "12/21/2020", approval_date)) |>
  mutate(fda_approved_use_on_approval_date = str_replace(fda_approved_use_on_approval_date, "12/21/2020", "To treat ebolaDrug Trials Snapshot")) |>
  arrange(no) |>
  select(-x) |>
  drop_na() 

fda_2020 <- fda_2020 |>
  mutate(disease_category = c("Oncology",
                              "Opthalmology",
                              "Oncology",
                              "Gastroenterology",
                              "Cardiovascular",
                              "Neurology",
                              "Surgical",
                              "Neurology",
                              "Oncology",
                              "Oncology",
                              "Neurology",
                              "Neurology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Neurology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Neurology",
                              "Neurology",
                              "Oncology",
                              "Genetic disorder",
                              "Neurology",
                              "Infectious diseases",
                              "Oncology",
                              "Infectious diseases",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Neurology",
                              "Neurology",
                              "Neurology",
                              "Neurology",
                              "Dermatology",
                              "Endocrinology",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Infectious diseases",
                              "Genetic disorder",
                              "Nephrology",
                              "Genetic disorder",
                              "Oncology",
                              "Oncology",
                              "Dermatology",
                              "Dermatology",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Urology"))

# year 2019

fda_4 <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2019")
tables_4 <- fda_4 |> 
  html_table(fill = TRUE)

fda_2019 <- tables_4[[1]] |>
  clean_names() |>
  mutate(year = rep(2019, 48)) |>
  mutate(drug_name = if_else(active_ingredient == "pretomanid", "pretomanid", drug_name)) |>
  mutate(drug_name = if_else(active_ingredient == "fluorodopa F 18", "fluorodopa F 18", drug_name)) |>
  arrange(no) 

fda_2019 <- fda_2019 |>
  mutate(disease_category = c("Neurology",
                              "Hematology",
                              "Infectious diseases",
                              "Neurology",
                              "Neurology",
                              "Neurology",
                              "Obstetrics and Gynecology",
                              "Oncology",
                              "Dermatology",
                              "Cardiovascular",
                              "Oncology",
                              "Oncology",
                              "Obstetrics and Gynecology",
                              "Oncology",
                              "Urology",
                              "Hematology",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Neurology",
                              "Oncology",
                              "Oncology",
                              "Immunology",
                              "Infectious diseases",
                              "Oncology",
                              "Neurology",
                              "Gastroenterology",
                              "Dermatology",
                              "Opthalmology",
                              "Genetic disorder",
                              "Neurology",
                              "Neurology",
                              "Genetic disorder",
                              "Obstetrics and Gynecology",
                              "Genetic disorder",
                              "Oncology",
                              "Urology",
                              "Genetic disorder",
                              "Genetic disorder",
                              "Neurology",
                              "Genetic disorder",
                              "Genetic disorder",
                              "Oncology",
                              "Surgical",
                              "Neurology",
                              "Neurology",
                              "Oncology",
                              "Neurology"))

# year 2018

fda_5 <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2018")
tables_5 <- fda_5 |> 
  html_table(fill = TRUE)

fda_2018 <- tables_5[[1]] |>
  clean_names() |>
  mutate(year = rep(2018, 59)) |>
  arrange(no) 

fda_2018 <- fda_2018 |>
  mutate(disease_category = c("Oncology",
                              "Infectious diseases",
                              "Genetic disorder",
                              "Oncology",
                              "Infectious diseases",
                              "Dermatology",
                              "Hematology",
                              "Genetic disorder",
                              "Oncology",
                              "Neurology",
                              "Neurology",
                              "Cardiovascular",
                              "Hematology",
                              "Genetic disorder",
                              "Immunology",
                              "Infectious diseases",
                              "Neurology",
                              "Infectious diseases",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Oncology",
                              "Infectious diseases",
                              "Obstetrics and Gynecology",
                              "Supplement",
                              "Hematology",
                              "Oncology",
                              "Neurology",
                              "Obstetrics and Gynecology",
                              "Genetic disorder",
                              "Genetic disorder",
                              "Opthalmology",
                              "Dermatology",
                              "Infectious diseases",
                              "Infectious diseases",
                              "Oncology",
                              "Neurology",
                              "Oncology",
                              "Neurology",
                              "Oncology",
                              "Oncology",
                              "Dermatology",
                              "Infectious diseases",
                              "Immunology",
                              "Neurology",
                              "Oncology",
                              "Infectious diseases",
                              "Oncology",
                              "Respiratory",
                              "Gastroenterology",
                              "Hematology",
                              "Oncology",
                              "Oncology",
                              "Immunology",
                              "Oncology",
                              "Gastroenterology",
                              "Oncology",
                              "Oncology",
                              "Urology"))
  

# year 2017

fda_6 <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2017")
tables_6 <- fda_6 |> 
  html_table(fill = TRUE)

fda_2017 <- tables_6[[1]] |>
  clean_names() |>
  mutate(year = rep(2017, 46)) |>
  arrange(no)

fda_2017 <- fda_2017 |>
  mutate(disease_category = c("Gastroenterology",
                              "Nephrology",
                              "Genetic disorder",
                              "Dermatology",
                              "Oncology",
                              "Oncology",
                              "Neurology",
                              "Oncology",
                              "Gastroenterology",
                              "Oncology",
                              "Dermatology",
                              "Neurology",
                              "Neurology",
                              "Neurology",
                              "Genetic disorder",
                              "Oncology",
                              "Oncology",
                              "Orthopedic",
                              "Oncology",
                              "Neurology",
                              "Immunology",
                              "Infectious diseases",
                              "Hematology",
                              "Dermatology",
                              "Oncology",
                              "Infectious diseases",
                              "Oncology",
                              "Infectious diseases",
                              "Oncology",
                              "Infectious diseases",
                              "Infectious diseases",
                              "Oncology",
                              "Infectious diseases",
                              "Oncology",
                              "Oncology",
                              "Opthalmology",
                              "Infectious diseases",
                              "Respiratory",
                              "Genetic disorder",
                              "Genetic disorder",
                              "Endocrinology",
                              "Infectious diseases",
                              "Opthalmology",
                              "Endocrinology",
                              "Diagnostics",
                              "Cardiovascular"))

# year 2016

fda_7 <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2016")
tables_7 <- fda_7 |> 
  html_table(fill = TRUE)

fda_2016 <- tables_7[[1]] |>
  clean_names() |>
  mutate(year = rep(2016, 22)) |>
  arrange(no)

fda_2016 <- fda_2016 |>
  mutate(disease_category = c("Infectious diseases",
                              "Neurology",
                              "Infectious diseases",
                              "Dermatology",
                              "Respiratory",
                              "Hepatology",
                              "Oncology",
                              "Neurology",
                              "Oncology",
                              "Neurology",
                              "Hepatology",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Opthalmology",
                              "Endocrinology",
                              "Genetic disorder",
                              "Oncology",
                              "Infectious diseases",
                              "Dermatology",
                              "Oncology",
                              "Neurology"))

# year 2015

fda_8 <- read_html("https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-2015")
tables_8 <- fda_8 |> 
  html_table(fill = TRUE)

fda_2015 <- tables_8[[1]] |>
  clean_names() |>
  mutate(year = rep(2015, 45)) |>
  arrange(no)

fda_2015 <- fda_2015 |>
  mutate(disease_category = c("Cardiovascular",
                              "Dermatology",
                              "Hepatology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Infectious diseases",
                              "Infectious diseases",
                              "Oncology",
                              "Genetic disorder",
                              "Cardiovascular",
                              "Surgical",
                              "Gastroenterology",
                              "Cardiovascular",
                              "Genetic disorder",
                              "Cardiovascular",
                              "Neurology",
                              "Cardiovascular",
                              "Oncology",
                              "Infectious diseases",
                              "Obstetrics and Gynecology",
                              "Cardiovascular",
                              "Oncology",
                              "Urology",
                              "Neurology",
                              "Oncology",
                              "Endocrinology",
                              "Neurology",
                              "Hematology",
                              "Hematology",
                              "Oncology",
                              "Genetic disorder",
                              "Respiratory",
                              "Infectious diseases",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Oncology",
                              "Genetic disorder",
                              "Oncology",
                              "Surgical",
                              "Pulmonary",
                              "Urology"))


# plot data

other_category <- c("Hematology",                
                    "Genetic disorder",          
                    "Nephrology",
                    "Infectious diseases",       
                    "Obstetrics and Gynecology",
                    "Opthalmology",             
                    "Cardiovascular",            
                    "Dermatology",               
                    "Gastroenterology",
                    "Respiratory",               
                    "Cardiology",                
                    "Immunology",               
                    "Surgical",                  
                    "Urology",                   
                    "Supplement",               
                    "Orthopedic",                
                    "Diagnostics",               
                    "Hepatology",               
                    "Pulmonary")

fda_df <- fda_2023 |>
  bind_rows(fda_2022) |>
  bind_rows(fda_2021) |>
  bind_rows(fda_2020, fda_2019, fda_2018, fda_2017, fda_2016, fda_2015) |>
  mutate(new_category = case_when(disease_category == "Oncology" ~ "Oncology",
                                  disease_category == "Neurology" ~ "Neurology",
                                  .default = "other"))


summary_df <- fda_df |>
  select(year, disease_category) |>
  group_by(year, disease_category) |>
  count() |>
  arrange(desc(n))


col_pal <- c("#788FCE", "#BD8184", "#E6956F", "#F2CC8F", "#A6BA96", "#C5E8E3", 
             "#F4F1DE", "#CDC3D4", "#A88AD2", "#60627C", "#E55381", "#B2F7EF",
             "#4DA167", "#F2B5D4", "#3C7A89", "#FCECC9", "white", "grey", "grey10")

drug_scale <- c("Oncology" = "#F2B5D4",
                "Neurology" = "#B2F7EF",
                "Endocrinology" = "#A88AD2",
                "Hematology" = "#A88AD2",
                "Genetic disorder" = "#A88AD2",          
                "Nephrology" = "#A88AD2", 
                "Infectious diseases" = "#A88AD2",
                "Obstetrics and Gynecology" = "#A88AD2",
                "Opthalmology" = "#A88AD2",
                "Cardiovascular" = "#A88AD2",            
                "Dermatology" = "#A88AD2",               
                "Gastroenterology" = "#A88AD2",
                "Respiratory" = "#A88AD2",               
                "Cardiology" = "#A88AD2",                
                "Immunology" = "#A88AD2",               
                "Surgical" = "#A88AD2",                  
                "Urology" = "#A88AD2",                   
                "Supplement" = "#A88AD2",
                "Orthopedic" = "#A88AD2",                
                "Diagnostics" = "#A88AD2",               
                "Hepatology" = "#A88AD2",               
                "Pulmonary" = "#A88AD2") 

drug_scale_new <- c("Oncology" = "#F2B5D4",
                "Neurology" = "#B2F7EF",
                "other" = "#F4F1DE")


# fonts
font_add_google(name = "Josefin Slab", family = "josefinslab")
showtext_auto()

title_text <- "The last 10 years in drug discovery belong to <span style='color:#B2F7EF'>**Neurology**</span> and <span style='color:#F2B5D4'>**Oncology**</span>"

subtitle_text <- "The US Food and Drug Administration (FDA) approves new therapies for all kinds of maladies. 
Of all the FDA drugs approved in the last ten years, **almost 40-50% of approved drugs each year target different types of cancer 
and neurological diseases.** While some drugs fall into multiple categories, the general trend in FDA approvals shows an explosion of 
oncology and neurology therapies. **Other drugs fall into 19 different categories,** including but not limited to hematology, 
infectious diseases, cardiovascular, endocrinology, as well as diagnostics and surgical tools."

caption_text <- "Source: US Food and Drug Administration (FDA) • Graphic: Sejal Davla, PhD"
                          

plot_data <- fda_df |>
  count(year, new_category) |>
  arrange() |>
  ggplot() +
  geom_brick(aes(year, n, fill = new_category),
             bricks_per_layer = 3) +
  scale_fill_manual(values = drug_scale_new,
                    labels = c("Neurology drugs", "Oncology drugs", "Drugs in all other categories")) +
  scale_y_continuous(limits = c(0, 63),
                     breaks = seq(0, 60, 9)) +
  scale_x_continuous(breaks = seq(2015, 2023, 1)) +
  coord_cartesian(expand = FALSE) +
  labs(title = title_text,
       subtitle = str_wrap(subtitle_text, 80),
       caption = caption_text,
       y = "number of new therapies approved by the FDA") +
  theme_minimal(base_family = "josefinslab") +
  theme(plot.title = element_markdown(size = 40,
                                      family = "josefinslab",
                                      margin = margin(0.3, 0, 0.3, 0, "cm")),
        plot.subtitle = element_textbox_simple(size = 20,
                                               lineheight = 0.6,
                                               margin = margin(0.3, 0, 0.3, 0, "cm")),
        plot.caption = element_text(size = 15, 
                                    hjust = 0.5,
                                    color = "#788FCE"),
        plot.background = element_rect(color = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey90",
                                          linewidth = 0.1),
        axis.text = element_text(size = 15,
                                 color = "grey40"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = "top",
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size = 15)) + 
  geom_curve(aes(x = 2016.9, 
                 y = 46, 
                 xend = 2016.3, 
                 yend = 50), 
             arrow = arrow(length = unit(0.02, "npc"), ends = "first", type ="closed"),
             colour = "grey40", linewidth = 0.1, curvature = 0.3, angle = 90) +
  geom_text(aes(x = 2016.1,
                y = 50,
                label = "each brick represents one drug"),
            family = "josefinslab",
            size = 5.5,
            hjust = 0.9,
            color = "grey60")

plot_data  

ggsave("plot_data.png", width = 7, height = 5, units = "in")


fda_df |>
  count(disease_category, year) |>
  arrange(desc(n)) |>
  ggplot(aes(year, disease_category, size = n)) +
  geom_point() +
  theme_minimal()


####-------------------------------------------------------------------------------------------------

library(waffle)

fda_df |>
  count(disease_category, year) |>
  ggplot(aes(fill = disease_category, values = n)) +
  geom_waffle(na.rm = TRUE,
              size = 0.5,
              color = "white",
              flip = TRUE) +
  facet_wrap(~ year) +
  scale_fill_viridis_d(option = "B") 



fda_df |>
  count(year, new_category) |>
  mutate(new_category = factor(new_category, levels = c("other", "Oncology", "Neurology"))) |>
  arrange()
  
