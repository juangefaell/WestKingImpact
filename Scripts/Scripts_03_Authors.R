############ WestKingImpact - Authors ###############

# BASIC INFO ----

# Researcher in charge: Juan Gefaell
# Section aim: Which authors might have connected the ontogenetic niche to evolutionary biology?
# Last update: 2025-05-29

# SETUP SECTION ----

## 1.- Packages ----
library(tidyverse) # Basic plots and statistical analyses
library(quanteda) # To help classify contributions
library(ggrepel) # For labels in plot
library(cowplot) # Composite figures
library(grid) # Composite figures 

## 2.- Dataset loading ----
setwd("") # Set the directory of the dataset file
read.csv("Data_01_ForwardCitationChasing.csv")

## 3.- Data handling and overview ----
ForCit <- read.csv("Data_01_ForwardCitationChasing.csv") # Set a shortcut name
str(ForCit) # Check the structure of the data

## 4.- Themes for figures ----

theme_cd <- function(BaseSize = 24, BaseFamily = "sans") {
  theme_classic(base_size = BaseSize, base_family = BaseFamily) +
    theme(
      plot.title = element_blank(),
      axis.title = element_text(size = BaseSize + 2, colour = "black"),
      axis.text = element_text(size = BaseSize, colour = "black"),
      axis.ticks = element_line(linewidth = 0.5, colour = "black"),
      axis.ticks.length = unit(2.2, "mm"),
      legend.title = element_blank(),
      legend.text = element_text(size = BaseSize),
      legend.key.size = unit(4, "mm"),
      strip.background = element_blank(),
      strip.text = element_text(size = BaseSize + 1, colour = "black"),
      plot.margin = margin(1, 1, 1, 1, unit = "mm")
    )
}

# Theme for tags:
tags_cd <- function(p, tag, size = 22, x = 0, y = 1) {
  p +
    labs(tag = tag) +
    theme(
      plot.tag = element_text(size = size, face = "bold"),
      plot.tag.position = c(x, y)
    )
}

# ANALYSIS SECTION ----

## Main analyses ----

### 1.- Most citing authors ----

# Separate individual authors (more useful to identify key researchers):
Authors_Long <- ForCit %>% 
  separate_rows(Author, sep = ";") %>% 
  mutate(Author = str_trim(Author)) %>% 
  group_by(DocID) %>% 
  mutate(N_Authors = n()) %>% # Count the number of authors in each publication
  ungroup() %>% 
  mutate(Weight = 1 / N_Authors) # Weighed author contribution to each publication

# Clean author names (remove spaces):
Authors_Long <- Authors_Long %>% 
  mutate(Author = stringr::str_trim(Author))

# Count citing authors:
Author_Counts <- Authors_Long %>% 
  count(Author, name = "N_Citations") %>% 
  arrange(desc(N_Citations))
# OUTPUT: This does not account for collaboration between authors, so the weighted measure must be used

# Count fractionated citations to WK:
Author_Counts_Frac <- Authors_Long %>% 
  distinct(DocID, Author, .keep_all = TRUE) %>% 
  group_by(Author) %>% 
  summarise(
    Fractional_Citations = sum(Weight), # How much each author has cited W&K, weighted by their contribution to the citing paper
    N_Papers = n(),
    .groups = "drop") %>% 
  arrange(desc(Fractional_Citations)) %>% 
  print(n = 272)

# Author contributions to fields:
Author_Field_Counts <- ForCit %>%
  separate_rows(Author, sep = ";") %>%
  mutate(Author = str_trim(Author)) %>%
  distinct(DocID, Author, Field) %>%
  count(Author, Field, name = "N_Publications") %>%
  arrange(desc(N_Publications)) %>% 
  print(n = 272)

# Fractional (not to autoinflate):
Author_Field_Fractional <- ForCit %>%
  separate_rows(Author, sep = ";") %>%
  mutate(Author = str_trim(Author)) %>%
  distinct(DocID, Author, Field) %>% 
  group_by(DocID) %>%
  mutate(Weight = 1 / n()) %>%
  ungroup() %>%
  group_by(Author, Field) %>%
  summarise(
    Fractional_Publications = sum(Weight),
    .groups = "drop") %>%
  arrange(desc(Fractional_Publications)) %>% 
  print(n = 272)

### 2.- Bridge-building potential (citing intensity ~ interdisciplinary) ----

# Calculate interdisciplinarity at the field level: 
Author_Diversity <- ForCit %>%
  separate_rows(Author, sep = ";") %>%
  mutate(Author = str_trim(Author)) %>%
  distinct(DocID, Author, Field) %>%
  count(Author, Field) %>%
  group_by(Author) %>%
  mutate(P = n / sum(n)) %>%
  summarise(
    N_Fields = n(),
    Total_Papers = sum(n),
    Shannon = -sum(P * log(P)),
    .groups = "drop") %>%
  arrange(desc(Shannon)) %>% 
  print(n = 244)

# Separate authors and get the fields of their publications: 
Author_Field <- ForCit %>%
  separate_rows(Author, sep = ";") %>%
  mutate(Author = str_trim(Author)) %>%
  distinct(DocID, Author, Field) %>%
  group_by(DocID) %>%
  mutate(
    N_Authors = n(),
    Author_Weight = 1 / N_Authors) %>%
  ungroup()

# Obtain their productivity (number of papers that cite W&K), breadth (number of fields in which they have published), and interdisciplinarity (Shannon):
Author_Profile <- Author_Field %>%
  group_by(Author, Field) %>%
  summarise(
    Weighted_Papers_Field = sum(Author_Weight),
    .groups = "drop") %>%
  group_by(Author) %>%
  mutate(P = Weighted_Papers_Field / sum(Weighted_Papers_Field)) %>%
  summarise(
    Weighted_Total_Papers = sum(Weighted_Papers_Field),
    N_Fields = n(),
    Shannon = -sum(P * log(P)),
    Shannon_Norm = Shannon / log(4),
    .groups = "drop") %>%
  mutate(
    Bridge_Score = Weighted_Total_Papers * Shannon_Norm) %>%
  arrange(desc(Bridge_Score))

# Set characteristics of the authors you want to mention in the plot:
Top_Authors <- Author_Profile %>% 
  filter(Weighted_Total_Papers >= 4.5, Shannon_Norm >= 0.3) %>% 
  slice_max(Weighted_Total_Papers, n = 10)

# Set thresholds for the quadrants:
x_cut <- 6.5
y_cut <- 0.35

# Plot (Figure 6):
BridgeFields <- Author_Profile %>% 
  filter(Weighted_Total_Papers >= 1) %>% 
  ggplot(aes(x = Weighted_Total_Papers, y = Shannon_Norm)) +
  geom_jitter(width = 0.05, height = 0.01, size = 4,
              shape = 21, fill = "#cecdaeff", color = "black") +
  geom_text_repel(
    data = Top_Authors,
    aes(label = Author),
    size = 5) +
  geom_vline(xintercept = x_cut, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = y_cut, linetype = "dashed", linewidth = 0.5) +
  labs(title = "",
       x = "Fractional author contributions citing \n West & King (1987)",
       y = "Author's interdisciplinarity \n (Normalized Shannon index)") +
  theme_cd(18) +
  theme(plot.margin = margin(10, 15, 10, 10)) +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0", "0.25", "0.5", "0.75", "1")) +
  scale_x_continuous(breaks = c(2.5, 5, 7.5, 10, 12.5),
                     labels = c("2.5", "5", "7.5", "10", "12.5"))

# Save figure (Figure 6):
ggsave("BridgeFields.svg",
       BridgeFields,
       width = 170, height = 150, units = "mm")

## Supplementary analyses ----

### 1.- Bridge-building potential at the contribution level (semantic) ----

#### 1.1.- Identify keywords ----

# Select the rows that contain relevant text to rank paper's interdisciplinarity:
Texts_Cont <- paste(ForCit$Title,
                    ForCit$Abstract) # Abstract of the contribution

# Tokenize, lowercase, and preserve multi-word expressions:
Tokens <- tokens(Texts_Cont, 
                 remove_punct = TRUE) %>% # Remove punctuation symbols (here it's not that important to polish the text, as we only want to find keywords)
  tokens_tolower()

# Compound multi-word dictionary terms (later used in the dictionary):
Multiword_Terms <- phrase(c(
  "developmental psychology",
  "evolutionary psychology",
  "niche construction",
  "natural selection",
  "life history",
  "developmental system",
  "developmental systems",
  "history of science",
  "philosophy of science"))

# Account for compound words:
Tokens <- tokens_compound(
  Tokens,
  pattern = Multiword_Terms)

# Create a document-feature matrix from tokens:
DFM_Mat <- dfm(Tokens)

# Create the  dictionary of key terms from the main fields (no weight this time): 
Dictionary <- list(
  Psychology_Cont = c("psychol*" = 1, 
                      "cognit*" = 1, 
                      "developmental_psychology" = 1, 
                      "sensor*" = 1,
                      "auditi*" = 1,
                      "percep*" = 1, 
                      "attachment*" = 1,
                      "emoti*" = 1,
                      "evolutionary_psychology" = 1,
                      "social*" = 1,
                      "personali*" = 1,
                      "infan*" = 1,
                      "child*" = 1,
                      "learn*" = 1,
                      "conditioning" = 1),
  Biology_Cont = c("biol*" = 1, 
                   "ethol*" = 1, 
                   "ecolog*" = 1, 
                   "genotyp*" = 1, 
                   "plastic*" = 1,
                   "niche_construction" = 1,
                   "evolution*" = 1,
                   "adaptation*" = 1,
                   "natural_selection" = 1,
                   "fitness" = 1,
                   "phenotyp*" = 1,
                   "life_history" = 1,
                   "inherit*" = 1,
                   "heredit*" = 1),
  Philosophy_Cont = c("philosop*" = 1, 
                      "histor*" = 1, 
                      "pluralis*" = 1, 
                      "developmental_system*" = 1, 
                      "history_of_science" = 1,
                      "philosophy_of_science" = 1,
                      "explanat*" = 1,
                      "reduction*" = 1,
                      "concept*" = 1,
                      "epistem*" = 1,
                      "semant*" = 1,
                      "ontolog*" = 1,
                      "theoret*" = 1,
                      "framework*" = 1,
                      "assumption*" = 1),
  Medicine_Cont = c("medic*" = 1, 
                    "disorder*" = 1, 
                    "neurolog*" = 1, 
                    "psychiat*" = 1,
                    "clinical" = 1,
                    "diagnos*" = 1,
                    "disease*" = 1, 
                    "pathol*" = 1, 
                    "epidemiol*" = 1, 
                    "pharmacol*" = 1,
                    "therap*" = 1,
                    "anxi*" = 1,
                    "depress*" = 1,
                    "stress*" = 1,
                    "treatment*" = 1))

# Create a function to compute the scores:
weighted_score <- function(dfm_mat, terms_weights) {
  out <- rep(0, ndoc(dfm_mat))
  for (pattern in names(terms_weights)) {
    matched_terms <- featnames(dfm_mat)[
      grepl(glob2rx(pattern), featnames(dfm_mat))
    ]
    if (length(matched_terms) > 0) {
      counts <- rowSums(as.matrix(dfm_mat[, matched_terms]))
      out <- out + counts * terms_weights[[pattern]] # Set the weighted measurement (no real weight this time)
    }
  }
  out
}

# Apply to each discipline:
Scores_Cont <- tibble(
  DocID = seq_len(ndoc(DFM_Mat)),
  Psychology_Keywords_Cont = weighted_score(DFM_Mat, Dictionary$Psychology),
  Philosophy_Keywords_Cont = weighted_score(DFM_Mat, Dictionary$Philosophy),
  Biology_Keywords_Cont = weighted_score(DFM_Mat, Dictionary$Biology),
  Medicine_Keywords_Cont = weighted_score(DFM_Mat, Dictionary$Medicine))

# Assign high scores, and add ties and ambiguity control based on difference with second-most important field (NOTE: If there is a tie, the algorithm assigns automatically to the first category):
Scores_Cont <- Scores_Cont %>% 
  rowwise() %>% 
  mutate(
    Scores = list(c(
      Psychology  = unname(Psychology_Keywords_Cont),
      Philosophy  = unname(Philosophy_Keywords_Cont),
      Biology     = unname(Biology_Keywords_Cont),
      Medicine    = unname(Medicine_Keywords_Cont))),
    Max_Score = max(unlist(Scores), na.rm = TRUE),
    Second_Score = sort(unlist(Scores), decreasing = TRUE)[2],
    Difference = Max_Score - Second_Score,
    N_Max = sum(unlist(Scores) == Max_Score), # A variable with the number of fields where it gets the maximum score
    AutomatedAssignment = ifelse(
      Max_Score == 0, 
      "Other", # Get other if "Max_Score == 0"
      names(unlist(Scores))[which.max(unlist(Scores))]),
    Tie = as.integer(N_Max > 1), # Create a tie variable for those outputs with more than one N_Max field
    Low_Confidence = as.integer(Difference <= 1), # Create a low confidence variable for those where the difference between fields is equal or less than 1 
    AutomatedAssignment_Revision = case_when(
      Max_Score == 0 ~ "Other",
      Tie == 1 ~ "Ambiguous_Tie",
      Low_Confidence == 1 ~ "Ambiguous_Close",
      TRUE ~ AutomatedAssignment)) %>% 
  select(-Scores) %>% 
  ungroup()

# Ensure 'DocID' in Scores has the same format
Scores_Cont <- Scores_Cont %>% 
  mutate(DocID = as.integer(gsub("text", "", DocID)))

# Merge:
ForCit_Cont <- ForCit %>% 
  left_join(Scores_Cont, by = "DocID")

#### 1.2.- Calculate interdisciplinarity at the contribution level ----

# Do the calculation:
ContributionInterdisc <- ForCit_Cont %>% 
  rowwise() %>% 
  mutate(
    Total_Keywords_Cont = sum(c_across(c(Psychology_Keywords_Cont, Philosophy_Keywords_Cont, Biology_Keywords_Cont, Medicine_Keywords_Cont)),
                              na.rm = TRUE),
    P_Psychology = Psychology_Keywords_Cont / Total_Keywords_Cont, # Frequencies of words of each field in the entire semantic landscape  
    P_Philosophy = Philosophy_Keywords_Cont / Total_Keywords_Cont,
    P_Biology = Biology_Keywords_Cont / Total_Keywords_Cont,
    P_Medicine = Medicine_Keywords_Cont / Total_Keywords_Cont,
    Keyword_Shannon = ifelse(
      Total_Keywords_Cont == 0, 
      NA_real_,
      -sum(
        c(P_Psychology, P_Philosophy, P_Biology, P_Medicine) * # Shannon
          log(c(P_Psychology, P_Philosophy, P_Biology, P_Medicine)),
        na.rm = TRUE)),
    Keyword_Shannon_Norm = Keyword_Shannon / log(4)) %>%  # Normalized interdisciplinarity (0–1)
  ungroup()

# Expand authors into individual rows:
ContributionInterdisc_row <- ContributionInterdisc %>% 
  separate_rows(Author, sep = ";") %>% 
  mutate(Author = str_trim(Author)) %>% 
  distinct(DocID, Author, .keep_all = TRUE) %>% 
  group_by(DocID) %>% 
  mutate(
    N_Authors = n(),
    Weight = 1 / N_Authors) %>% 
  ungroup()

# Authors' average contribution-level interdisciplinarity:
Author_ContributionInterdisc <- ContributionInterdisc_row %>% 
  group_by(Author) %>% 
  summarise(
    N_Papers = n(),
    Weighted_N_Papers = sum(Weight),
    Mean_ContributionInterdisc = mean(Keyword_Shannon_Norm, na.rm = TRUE),
    Median_ContributionInterdisc = median(Keyword_Shannon_Norm, na.rm = TRUE),
    SD_ContributionInterdisc = sd(Keyword_Shannon_Norm, na.rm = TRUE),
    .groups = "drop") %>% 
  arrange(desc(Mean_ContributionInterdisc)) %>% 
  print(n = 244)

# Focus only on authors with more than 3 publications citing W&K:
AuthConInt <- Author_ContributionInterdisc %>% 
  filter(Weighted_N_Papers >= 1) %>% 
  arrange(desc(Mean_ContributionInterdisc)) %>% 
  print(n = 26)

# Set thresholds for the quadrants:
x_cut <- 7
y_cut <- 0.4

# Set characteristics of the authors you want to mention in the plot:
Top_Authors_Cont <- AuthConInt %>% 
  filter(Weighted_N_Papers >= 6, Mean_ContributionInterdisc >= 0.25) %>% 
  #filter(Weighted_N_Papers >= 2.5, Mean_ContributionInterdisc > 0.55) %>% 
  slice_max(Weighted_N_Papers, n = 9)

# Remove a couple of NAs:
AuthConInt <- AuthConInt %>%
  filter(is.finite(Mean_ContributionInterdisc))

# Plot:
BridgeSemantic <- AuthConInt %>% 
  ggplot(aes(x = Weighted_N_Papers, y = Mean_ContributionInterdisc)) +
  geom_jitter(width = 0.01, height = 0.01, size = 4, shape = 21, fill = "#cecdaeff", color = "black") +
  geom_text_repel(data = Top_Authors_Cont,
                  aes(label = Author), size = 5) +
  geom_vline(xintercept = x_cut, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = y_cut, linetype = "dashed", linewidth = 0.5) +
  labs(title = "",
       x = "Fractional author contributions citing \n West & King (1987)",
       y = "Author's mean semantic \n interdisciplinarity (normalized)") +
  theme_cd(18) +
  theme(plot.margin = margin(10, 15, 10, 10)) +
  scale_y_continuous(breaks = c("0" = 0, "0.25" = 0.25, "0.5" = 0.5, "0.75" = 0.75, "1" = 1)) # Remove the annoying leading 0s

# Save figure (Figure 6):
ggsave("BridgeSemantic.svg",
       BridgeSemantic,
       width = 170, height = 150, units = "mm")
