
## Required packages

library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud2)

## Text mining and data cleaning

cihr_df |>
  count(name_nom) |>
  arrange(desc(n))

# Remove regular words that form a sentence such as articles, prepositions, joins etc using stop_words dataframe 
keywords <- cihr_df |>
  unnest_tokens(word, keywords_mots_cles) |>
  anti_join(stop_words)

keywords_multi <- cihr_df |>
  unnest_ngrams(word, keywords_mots_cles, n = 2)

keywords_multi_summary <- keywords_multi |>
  count(word) |>
  arrange(desc(n))

keywords_multi_summary |> 
  filter(n > 100) |>
  wordcloud2()

keyword_summary <- keywords |>
  count(word) |>
  arrange(desc(n)) 

remove_words <- c("research", "health", "medical", "based", "analysis", "de", "19", "methods", "system", "study", "2", "1")

keyword_summary <- keyword_summary |>
  filter(!word %in% remove_words)

new_key <- keyword_summary |>
  bind_rows(keywords_multi_summary) |>
  arrange(desc(n)) |>
  drop_na() |>
  mutate(word = case_when(word == "covid" ~ "COVID-19",
                          word == "dna" ~ "DNA",
                          word == "rna" ~ "RNA",
                          word == "hiv" ~ "HIV",
                          word == "stem" ~ "stem cells",
                          word == "research hospital" ~ "hospital research",
                          word == "crispr" ~ "CRISPR",
                          word == "mri" ~ "MRI",
                          T ~ word)) 

remove_words2 <- c("cell", 
                   "care",
                   "cells", 
                   "protein", 
                   "imaging", 
                   "mental",
                   "biology",
                   "models",
                   "knowledge",
                   "gene",
                   "covid 19",
                   "services",
                   "learning",
                   "model",
                   "diseases",
                   "data",
                   "quality",
                   "function",
                   "disorder",
                   "public",
                   "interactions",
                   "other medical",
                   "life",
                   "magnetic",
                   "primary",
                   "control",
                   "animal",
                   "science",
                   "resonance",
                   "response",
                   "mixed",
                   "physical",
                   "implementation",
                   "type",
                   "controlled",
                   "type",
                   "expression",
                   "activity",
                   "mice",
                   "machine",
                   "quality of",
                   "receptors",
                   "proteins",
                   "studies",
                   "resonance imaging",
                   "controlled trial",
                   "and medicine",
                   "acute",
                   "cellular",
                   "design",
                   "medicines",
                   "of heart",
                   "engineering and",
                   "health other",
                   "single",
                   "and medicines",
                   "post",
                   "emgagement"
)

pal_col <- rep(c("#c42348","#d91e36","#da344d","#da344d", "#BF4F51", "#E23D28", "#FF7F7F","#CC3333", "#FF6347", "#E23D28"), 15)

cihr_kw <- new_key |>
  filter(!word %in% remove_words2) |>
  slice_head(n = 120) |>
  wordcloud2(color = pal_col,
             figPath = "/Data_Portfolio/CIHR/maple-leaf.png",
             fontFamily = "Roboto",
             minRotation = -pi/6,
             maxRotation = pi/6) 

cihr_kw

# Save the graph as HTML widget
library(webshot)
webshot::install_phantomjs()
library("htmlwidgets")

saveWidget(cihr_kw,
           "cihr_wordcloud.html", 
           selfcontained = F,
           title = t_text,
           background = "gray95") #save as HTML widget

webshot("cihr_wordcloud.html",
        "cihr_wc.png", 
        delay = 15, 
        vwidth = 500, 
        vheight = 500) #save as PNG
