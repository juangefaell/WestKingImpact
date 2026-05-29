############ WestKingImpact - Fields ###############

# BASIC INFO ----

# Researcher in charge: Juan Gefaell
# Section aim: In what fields and topics has West and King's 1987 paper been more influential? How patterns of influence change over time? 
# Last update: 2025-05-27

# SETUP SECTION ----

## 1.- Packages ----
library(tidyverse) # Basic plots and statistical analyses
library(quanteda) # To help classify contributions
library(topicmodels) # To do topic modeling
library(stopwords) # To do topic modeling
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
      plot.margin = margin(1, 1, 1, 1, unit = "mm"))}

# Theme for tags:
tags_cd <- function(p, tag, size = 22, x = 0, y = 1) {
  p +
    labs(tag = tag) +
    theme(
      plot.tag = element_text(size = size, face = "bold"),
      plot.tag.position = c(x, y))}

# ANALYSIS SECTION ----

## Main analyses ----

### 1.- Influence in fields ----

# Subsection aim: Describe in which fields West and King (1987) has been more influential

#### 1.1.- Semi-automated field assignment (rule-based classification) ----

# Select the rows that contain relevant text to assign papers to fields:
Texts <- paste(ForCit$Title, # Title of the contribution
               ForCit$Abstract, # Abstract of the contribution
               ForCit$PublicationTitle, # Name of journal/book in which the contribution was published
               ForCit$Notes, # Personal notes upon reading the title and abstract (which makes the process somewhat supervised)
               sep = " ") 

# Tokenize, lowercase, and preserve multi-word expressions:
Tokens <- tokens(Texts, 
                 remove_punct = TRUE) %>% # Remove punctuation symbols (here it's not that important to polish the text, as we only want to find keywords)
  tokens_tolower()

# Compound multi-word dictionary terms (later used in the weighted dictionary):
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

# Create the weighted dictionary of key terms from the main fields: 
Dictionary <- list(
  Psychology = c("psychol*" = 2, # Grant this word more weight
                 "cognit*" = 2, # Grant this word more weight
                 "developmental_psychology" = 2, # Grant this word more weight
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
  Biology = c("biol*" = 2, # Grant this word more weight
              "ethol*" = 2, # Grant this word more weight
              "ecolog*" = 2, # Grant this word more weight
              "genotyp*" = 2, # Grant this word more weight
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
  Philosophy = c("philosop*" = 2, # Grant this word more weight
                 "histor*" = 2, # Grant this word more weight
                 "pluralis*" = 2, # Grant this word more weight (hardly other scholars than philosphers use the term "pluralism")
                 "developmental_system*" = 2, # Grant this word more weight (because most DST literature is philosophical)
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
  Medicine = c("medic*" = 2, # Grant this word more weight
               "disorder*" = 2, # Grant this word more weight
               "neurolog*" = 2, # Grant this word more weight
               "psychiat*" = 2, # Grant this word more weight
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

# Create a function to compute the weighted scores:
weighted_score <- function(dfm_mat, terms_weights) {
  out <- rep(0, ndoc(dfm_mat))
  for (pattern in names(terms_weights)) {
    matched_terms <- featnames(dfm_mat)[
      grepl(glob2rx(pattern), featnames(dfm_mat))
    ]
    if (length(matched_terms) > 0) {
      counts <- rowSums(as.matrix(dfm_mat[, matched_terms]))
      out <- out + counts * terms_weights[[pattern]] # Set the weighted measurement
    }
  }
  out
}

# Apply to each discipline:
Scores <- tibble(
  DocID = seq_len(ndoc(DFM_Mat)),
  Psychology_Keywords = weighted_score(DFM_Mat, Dictionary$Psychology),
  Philosophy_Keywords = weighted_score(DFM_Mat, Dictionary$Philosophy),
  Biology_Keywords = weighted_score(DFM_Mat, Dictionary$Biology),
  Medicine_Keywords = weighted_score(DFM_Mat, Dictionary$Medicine))

# Assign high scores, and add ties and ambiguity control based on difference with second-most important field (NOTE: If there is a tie, the algorithm assigns automatically to the first category):
Scores <- Scores %>% 
  rowwise() %>% 
  mutate(
    Scores = list(c(
      Psychology  = unname(Psychology_Keywords),
      Philosophy  = unname(Philosophy_Keywords),
      Biology     = unname(Biology_Keywords),
      Medicine    = unname(Medicine_Keywords)
    )),
    Max_Score = max(unlist(Scores), na.rm = TRUE),
    Second_Score = sort(unlist(Scores), decreasing = TRUE)[2],
    Difference = Max_Score - Second_Score,
    N_Max = sum(unlist(Scores) == Max_Score), # A variable with the number of fields where it gets the maximum score
    AutomatedAssignment = ifelse(
      Max_Score == 0, 
      "Other", # Get other if "Max_Score == 0"
      names(unlist(Scores))[which.max(unlist(Scores))]
    ),
    Tie = as.integer(N_Max > 1), # Create a tie variable for those outputs with more than one N_Max field
    Low_Confidence = as.integer(Difference <= 1), # Create a low confidence variable for those where the difference between fields is equal or less than 1 
    AutomatedAssignment_Revision = case_when(
      Max_Score == 0 ~ "Other",
      Tie == 1 ~ "Ambiguous_Tie",
      Low_Confidence == 1 ~ "Ambiguous_Close",
      TRUE ~ AutomatedAssignment
    )
  ) %>% 
  select(-Scores) %>% 
  ungroup()

# Create a 'DocID' variable in the original file to merge:
ForCit <- ForCit %>%
  mutate(DocID = row_number()) 

# Ensure 'DocID' in Scores has the same format
Scores <- Scores %>% 
  mutate(DocID = as.integer(gsub("text", "", DocID)))

# Merge:
ForCit_Auto <- ForCit %>% 
  left_join(Scores, by = "DocID")

# Check numbers within each category: 
table(ForCit_Auto$AutomatedAssignment_Revision) # (If you run this, note that the variable already exists; check with dataset 'ForCit')
# OUTPUT: There is uncertainty with many papers, but honestly I think it reflects reality in most cases (i.e., many publications are very interdisciplinary)

# Save to manually check, correct, and store:
# write.csv(ForCit_Auto, "WKI_DA_RD_01ForwardCitationChasing.csv") # NOTE: The variables are now incorporated in regular 'ForCit' if this is re-runned

# AFTER MANUALLY CORRECTING CONFLICTING CASES, see differences in assignment:
view(ForCit %>%
       select(DOI, AutomatedAssignment, Field, AutomatedAssignment_Revision, ReasonCorrection, ReasonCorrection_Standard) %>% 
       filter(AutomatedAssignment != Field))

# Now count the reasons behind reassignment or confirmation of automated assignments:
table(ForCit$ReasonCorrection_Standard)

#### 1.2.- Influence across fields ####

# Define colors for the main categories (Sanzo Wada-inspired):
Field_Colors <- c(
  "Psychology" = "#8895b6ff", 
  "Philosophy" = "#cecdaeff", 
  "Biology" = "#417246ff", 
  "Medicine" = "#9E2913") 

# Count per year:
Fields_TimeSeries <- ForCit %>% 
  filter(Field != "Other") %>% 
  filter(!is.na(PublicationYear)) %>% 
  count(PublicationYear, Field, name = "n")

# Cumulative:
Fields_Cumulative <- Fields_TimeSeries %>% 
  arrange(Field, PublicationYear) %>% 
  group_by(Field) %>% 
  mutate(cum_n = cumsum(n)) %>% 
  ungroup()

# Plot (Figure 3A):
Fields_Dynamic <- ggplot(Fields_Cumulative, aes(x = PublicationYear, y = cum_n, group = Field)) +
  geom_line(aes(color = Field), linewidth = 1.5) +
  geom_point(aes(fill = Field), size = 3, shape = 21, stroke = 1) +
  scale_fill_manual(values = Field_Colors) +
  scale_color_manual(values = Field_Colors) +
  labs(x = "Year", y = "Cumulative nº of publications \n citing West & King (1987)", color = "Field") +
  theme_cd(18) +
  theme(legend.position = c(0.25, 0.75))

Fields_Dynamic # Check the plot

### 2.- Influence in topics  ----

#### 2.1.- Psychology ----

# Select the field and the text used for topic modeling:
Psychology_TM <- ForCit %>% 
  filter(Field == "Psychology") %>% 
  mutate(
    TM_ID = as.character(DocID), # Create a column to merge correctly afterwards
    Topic_Text = paste(Title, Abstract, Notes, sep = " "))

# Create the corpus: 
Psychology_Corpus <- corpus(Psychology_TM, text_field = "Topic_Text",
                            docid_field = "TM_ID")

# Tokenize and clean:
Psychology_Tokens <- tokens(
  Psychology_Corpus,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_remove(c("also", "may", "one", "two", "using", "suggests", "study", "paper", "article", "research", 
                  "results", "provide", "role","can","different", "preferred", "method", "show", "result")) %>% 
  tokens_wordstem(language = "en")

# Remove very small stems: 
Psychology_Tokens <- tokens_select(Psychology_Tokens, min_nchar = 3)

# Recode the "develop*" tokens to merge them:
Psychology_Tokens <- Psychology_Tokens %>%
  tokens_replace(
    pattern = "develop*",
    replacement = "develop",
    valuetype = "glob")

# Create document-feature matrix (for topic modeling):
Psychology_DFMMat_quan <- dfm(Psychology_Tokens)

# Remove unfrequent words (they can bias topics):
Psychology_DFMMat_quan <- dfm_trim(
  Psychology_DFMMat_quan,
  min_docfreq = 3)

# Preserve document ids:
Psychology_DocIDs <- docnames(Psychology_DFMMat_quan)

# Convert to format suited to topic modeling:
Psychology_DFMMat_tm <- convert(Psychology_DFMMat_quan, to = "topicmodels")

# Perform topic modeling: 
Psychology_LDA <- LDA(Psychology_DFMMat_tm, 
                      k = 3,
                      method = "Gibbs",
                      control = list(
                        seed = 123, # Set seed for LDA
                        burnin = 1000, # Discard early unstable iterations of the process
                        iter = 2000, # Increase iterations to achieve stability in topic composition
                        thin = 100)) # Reduce autocorrelation

terms(Psychology_LDA, 10) # Change the number of words to display at will
# OUTPUT: 
# Topic 1: developmental ecology?
# Topic 2: Maternal effects?
# Topic 3: DST and nature vs. nurture?

# Get document-topic probabilities:
Psychology_Topic_Prob <- posterior(Psychology_LDA)$topics

# Assign the most probable topic to each citing publication:
Psychology_Topic_Assign <- as.data.frame(Psychology_Topic_Prob) %>% 
  setNames(paste0("Topic_", 1:ncol(.))) %>% 
  mutate(
    TM_ID = Psychology_DocIDs,
    Dominant_Topic = max.col(across(starts_with("Topic_"))),
    Dominant_Topic_Prob = apply(across(starts_with("Topic_")), 1, max))

# Add the topics assigned to the dataset with papers:
Psychology_TM_Topics <- Psychology_TM %>% 
  mutate(TM_ID = as.character(DocID)) %>% 
  left_join(Psychology_Topic_Assign, by = "TM_ID")

view(Psychology_TM_Topics) # View the result

# Safety check: 
length(Psychology_DocIDs)
nrow(Psychology_Topic_Prob)
# OUTPUT: They have the same n, so looks good

# Check how well topic modeling discriminates between topics for each publication:
Psychology_TM_Topics %>% 
  summarise(
    Mean_Discrimination = mean(Dominant_Topic_Prob),
    SD_Discrimination = sd(Dominant_Topic_Prob))

# Rename topics based on their key terms:
Topic_Labels_Psychology <- c("1" = "Developmental ecology" ,
                  "2" = "Maternal effects",
                  "3" = "DST and nature vs. nurture")

# Incorporate the renamed topics:
Psychology_TM_Topics <- Psychology_TM_Topics %>% 
  mutate(
    Topic_Label = Topic_Labels_Psychology[as.character(Dominant_Topic)])

# Prepare the dataset for the plot:
Psychology_Topic_Plot <- Psychology_TM_Topics %>% 
  count(Topic_Label, name = "n") %>% 
  group_by(Topic_Label) %>% 
  summarise(n = sum(n), .groups = "drop") 

# Plot: 
Psychology_Topic_Hist <- Psychology_Topic_Plot %>% 
  ggplot(aes(x = reorder(str_wrap(Topic_Label, width = 20), n), y = n)) +
  geom_col(color = "black", width = 0.8, fill = "#8895b6ff") +
  coord_flip() +
  labs(x = "", 
       y = "",
       title = "Psychology") +
  theme_cd(18) +
  theme(plot.title = element_text(size = 19, face = "bold", margin = margin(b = 20)),
        legend.position = "none") 

#### 2.2.- Biology ----

# Select the field and the text used for topic modeling:
Biology_TM <- ForCit %>% 
  filter(Field == "Biology") %>% 
  mutate(
    TM_ID = as.character(DocID), # Create a column to merge correctly afterwards
    Topic_Text = paste(Title, Abstract, Notes, sep = " "))

# Create the corpus: 
Biology_Corpus <- corpus(Biology_TM, text_field = "Topic_Text",
                         docid_field = "TM_ID")

# Tokenize and clean:
Biology_Tokens <- tokens(
  Biology_Corpus,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_remove(c("also", "may", "one", "two", "using", "suggests", "study", "paper", "article", "research", 
                  "results", "provide", "role","can","different", "preferred", "method", "show", "result")) %>% 
  tokens_wordstem(language = "en")

# Remove very small stems: 
Biology_Tokens <- tokens_select(Biology_Tokens, min_nchar = 3)

# Recode the "develop*" tokens to merge them:
Biology_Tokens <- Biology_Tokens %>%
  tokens_replace(
    pattern = "develop*",
    replacement = "develop",
    valuetype = "glob")

# Create document-feature matrix (for topic modeling):
Biology_DFMMat_quan <- dfm(Biology_Tokens)

# Remove unfrequent words (they can bias topics):
Biology_DFMMat_quan <- dfm_trim(
  Biology_DFMMat_quan,
  min_docfreq = 3)

# Preserve document ids:
Biology_DocIDs <- docnames(Biology_DFMMat_quan)

# Convert to format suited to topic modeling:
Biology_DFMMat_tm <- convert(Biology_DFMMat_quan, to = "topicmodels")

# Perform topic modeling: 
Biology_LDA <- LDA(Biology_DFMMat_tm, 
                   k = 2, # Two topics were selected because there are far less publications in biology
                   method = "Gibbs",
                   control = list(
                     seed = 123, # Set seed for LDA
                     burnin = 1000, # Discard early unstable iterations of the process
                     iter = 2000, # Increase iterations to achieve stability in topic composition
                     thin = 100)) # Reduce autocorrelation
                    
terms(Biology_LDA, 10) # Change the number of words to display at will
# OUTPUT: These are the keywords in the topics:
# Topic 1: Developmental approaches in evolution? 
# Topic 2: Developmental ethology?

# Get document-topic probabilities:
Biology_Topic_Prob <- posterior(Biology_LDA)$topics

# Assign the most probable topic to each citing publication:
Biology_Topic_Assign <- as.data.frame(Biology_Topic_Prob) %>% 
  setNames(paste0("Topic_", 1:ncol(.))) %>% 
  mutate(
    TM_ID = Biology_DocIDs,
    Dominant_Topic = max.col(across(starts_with("Topic_"))),
    Dominant_Topic_Prob = apply(across(starts_with("Topic_")), 1, max))

# Add the topics assigned to the dataset with papers:
Biology_TM_Topics <- Biology_TM %>% 
  mutate(TM_ID = as.character(DocID)) %>% 
  left_join(Biology_Topic_Assign, by = "TM_ID")

view(Biology_TM_Topics) # View the result

# Safety check: 
length(Biology_DocIDs)
nrow(Biology_Topic_Prob)
# OUTPUT: Looks good, again

# Check how well topic modeling discriminates between topics for each publication:
Biology_TM_Topics %>% 
  summarise(
    Mean_Discrimination = mean(Dominant_Topic_Prob),
    SD_Discrimination = sd(Dominant_Topic_Prob))

# Rename topics based on their key terms:
Topic_Labels_Biology <- c("1" = "Developmental approaches to evolution",
                  "2" = "Developmental ethology")

# Incorporate the renamed topics:
Biology_TM_Topics <- Biology_TM_Topics %>% 
  mutate(
    Topic_Label = Topic_Labels_Biology[as.character(Dominant_Topic)])

# Prepare the dataset for the plot:
Biology_Topic_Plot <- Biology_TM_Topics %>% 
  count(Topic_Label, name = "n") %>% 
  group_by(Topic_Label) %>% 
  summarise(n = sum(n), .groups = "drop") 

# Plot: 
Biology_Topic_Hist <- Biology_Topic_Plot %>% 
  ggplot(aes(x = reorder(str_wrap(Topic_Label, width = 20), n), y = n)) +
  geom_col(color = "black", width = 0.8, fill = "#417246ff") +
  coord_flip() +
  labs(x = "", 
       y = "",
       title = "Biology") +
  theme_cd(18) +
  theme(plot.title = element_text(size = 19, face = "bold", margin = margin(b = 20)),
        legend.position = "none") 

#### 2.3.- Philosophy ----

# Select the field and the text used for topic modeling:
Philosophy_TM <- ForCit %>% 
  filter(Field == "Philosophy") %>% 
  mutate(
    TM_ID = as.character(DocID), # Create a column to merge correctly afterwards
    Topic_Text = paste(Title, Abstract, Notes, sep = " "))

# Create the corpus: 
Philosophy_Corpus <- corpus(Philosophy_TM, text_field = "Topic_Text",
                            docid_field = "TM_ID")

# Tokenize and clean:
Philosophy_Tokens <- tokens(
  Philosophy_Corpus,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_remove(c("also", "may", "one", "two", "using", "suggests", "study", "paper", "article", "research", 
                  "results", "provide", "role","can","different", "preferred", "method", "show", "result")) %>% 
  tokens_wordstem(language = "en")

# Remove very small stems: 
Philosophy_Tokens <- tokens_select(Philosophy_Tokens, min_nchar = 3)

# Recode the "develop*" tokens to merge them:
Philosophy_Tokens <- Philosophy_Tokens %>%
  tokens_replace(
    pattern = "develop*",
    replacement = "develop",
    valuetype = "glob")

# Create document-feature matrix (for topic modeling):
Philosophy_DFMMat_quan <- dfm(Philosophy_Tokens)

# Remove unfrequent words (they can bias topics):
Philosophy_DFMMat_quan <- dfm_trim(
  Philosophy_DFMMat_quan,
  min_docfreq = 3)

# Preserve document ids:
Philosophy_DocIDs <- docnames(Philosophy_DFMMat_quan)

# Convert to format suited to topic modeling:
Philosophy_DFMMat_tm <- convert(Philosophy_DFMMat_quan, to = "topicmodels")

# Perform topic modeling: 
Philosophy_LDA <- LDA(Philosophy_DFMMat_tm, 
                      k = 2, # Two topics were selected because there are far less publications in philosophy
                      method = "Gibbs",
                      control = list(
                        seed = 123, # Set seed for LDA
                        burnin = 1000, # Discard early unstable iterations of the process
                        iter = 2000, # Increase iterations to achieve stability in topic composition
                        thin = 100)) # Reduce autocorrelation
                       
terms(Philosophy_LDA, 20)
# OUTPUT: These are the keywords in the topics:
# Topic 1: Developmental accounts and nature vs. nurture? 
# Topic 2: Niche construction in humans? 

# Get document-topic probabilities:
Philosophy_Topic_Prob <- posterior(Philosophy_LDA)$topics

# Assign the most probable topic to each citing publication:
Philosophy_Topic_Assign <- as.data.frame(Philosophy_Topic_Prob) %>% 
  setNames(paste0("Topic_", 1:ncol(.))) %>% 
  mutate(
    TM_ID = Philosophy_DocIDs,
    Dominant_Topic = max.col(across(starts_with("Topic_"))),
    Dominant_Topic_Prob = apply(across(starts_with("Topic_")), 1, max))

# Add the topics assigned to the dataset with papers:
Philosophy_TM_Topics <- Philosophy_TM %>% 
  mutate(TM_ID = as.character(DocID)) %>% 
  left_join(Philosophy_Topic_Assign, by = "TM_ID")

view(Philosophy_TM_Topics) # View the result

# Safety check: 
length(Philosophy_DocIDs)
nrow(Philosophy_Topic_Prob)
# OUTPUT: Looks good, once again

# Check how well topic modeling discriminates between topics for each publication:
Philosophy_TM_Topics %>% 
  summarise(
    Mean_Discrimination = mean(Dominant_Topic_Prob),
    SD_Discrimination = sd(Dominant_Topic_Prob))

# Rename topics based on their key terms:
Topic_Labels_Philosophy <- c("1" = "DST and developmental accounts of nature vs. nurture",
                  "2" = "Niche construction in humans")

# Incorporate the renamed topics:
Philosophy_TM_Topics <- Philosophy_TM_Topics %>% 
  mutate(
    Topic_Label = Topic_Labels_Philosophy[as.character(Dominant_Topic)])

# Prepare the dataset for the plot:
Philosophy_Topic_Plot <- Philosophy_TM_Topics %>% 
  count(Topic_Label, name = "n") %>% 
  group_by(Topic_Label) %>% 
  summarise(n = sum(n), .groups = "drop") 

# Plot: 
Philosophy_Topic_Hist <- Philosophy_Topic_Plot %>% 
  ggplot(aes(x = reorder(str_wrap(Topic_Label, width = 20), n), y = n)) +
  geom_col(color = "black", width = 0.8, fill = "#cecdaeff") +
  coord_flip() +
  labs(x = "", 
       y = "Publications citing \n West & King (1987)",
       title = "Philosophy") +
  theme_cd(18) +
  theme(plot.title = element_text(size = 19, face = "bold", margin = margin(b = 20)),
        legend.position = "none") 

#### 2.4.- Medicine ----

# Select the field and the text used for topic modeling:
Medicine_TM <- ForCit %>% 
  filter(Field == "Medicine") %>% 
  mutate(
    TM_ID = as.character(DocID), # Create a column to merge correctly afterwards
    Topic_Text = paste(Title, Abstract, Notes, sep = " "))

# Create the corpus: 
Medicine_Corpus <- corpus(Medicine_TM, text_field = "Topic_Text",
                          docid_field = "TM_ID")

# Tokenize and clean:
Medicine_Tokens <- tokens(
  Medicine_Corpus,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_remove(c("also", "may", "one", "two", "using", "suggests", "study", "paper", "article", "research", 
                  "results", "provide", "role","can","different", "preferred", "method", "show", "result")) %>% 
  tokens_wordstem(language = "en")

# Remove very small stems: 
Medicine_Tokens <- tokens_select(Medicine_Tokens, min_nchar = 3)

# Recode the "develop*" tokens to merge them:
Medicine_Tokens <- Medicine_Tokens %>%
  tokens_replace(
    pattern = "develop*",
    replacement = "develop",
    valuetype = "glob")

# Create document-feature matrix (for topic modeling):
Medicine_DFMMat_quan <- dfm(Medicine_Tokens)

# Remove unfrequent words (they can bias topics):
Medicine_DFMMat_quan <- dfm_trim(
  Medicine_DFMMat_quan,
  min_docfreq = 3)

# Preserve document ids:
Medicine_DocIDs <- docnames(Medicine_DFMMat_quan)

# Convert to format suited to topic modeling:
Medicine_DFMMat_tm <- convert(Medicine_DFMMat_quan, to = "topicmodels")

# Perform topic modeling: 
Medicine_LDA <- LDA(Medicine_DFMMat_tm, 
                    k = 2, # Two topics were selected because there are far less publications in medicine
                    method = "Gibbs",
                    control = list(
                      seed = 123, # Set seed for LDA
                      burnin = 1000, # Discard early unstable iterations of the process
                      iter = 2000, # Increase iterations to achieve stability in topic composition
                      thin = 100)) # Reduce autocorrelation
                     
terms(Medicine_LDA, 10)
# OUTPUT: These are the keywords in the topics:
# Topic 1: Developmental origins of psychiatric disorders?
# Topic 2: Maternal effects, with a more psychopathological touch?

# Get document-topic probabilities:
Medicine_Topic_Prob <- posterior(Medicine_LDA)$topics

# Assign the most probable topic to each citing publication:
Medicine_Topic_Assign <- as.data.frame(Medicine_Topic_Prob) %>% 
  setNames(paste0("Topic_", 1:ncol(.))) %>% 
  mutate(
    TM_ID = Medicine_DocIDs,
    Dominant_Topic = max.col(across(starts_with("Topic_"))),
    Dominant_Topic_Prob = apply(across(starts_with("Topic_")), 1, max))

# Add the topics assigned to the dataset with papers:
Medicine_TM_Topics <- Medicine_TM %>% 
  mutate(TM_ID = as.character(DocID)) %>% 
  left_join(Medicine_Topic_Assign, by = "TM_ID")

view(Medicine_TM_Topics) # View the result

# Safety check: 
length(Medicine_DocIDs)
nrow(Medicine_Topic_Prob)
# OUTPUT: Yep, looks good

# Check how well topic modeling discriminates between topics for each publication:
Medicine_TM_Topics %>% 
  summarise(
    Mean_Discrimination = mean(Dominant_Topic_Prob),
    SD_Discrimination = sd(Dominant_Topic_Prob))

# Rename topics based on their key terms:
Topic_Labels_Medicine <- c("1" = "Developmental origins of psychiatric disorders",
                  "2" = "Maternal effects (psychopathol.)")

# Incorporate the renamed topics:
Medicine_TM_Topics <- Medicine_TM_Topics %>% 
  mutate(
    Topic_Label = Topic_Labels_Medicine[as.character(Dominant_Topic)])

# Prepare the dataset for the plot:
Medicine_Topic_Plot <- Medicine_TM_Topics %>% 
  count(Topic_Label, name = "n") %>% 
  group_by(Topic_Label) %>% 
  summarise(n = sum(n), .groups = "drop") 

# Plot: 
Medicine_Topic_Hist <- Medicine_Topic_Plot %>% 
  ggplot(aes(x = reorder(str_wrap(Topic_Label, width = 20), n), y = n)) +
  geom_col(color = "black", width = 0.8, fill = "#9E2913") +
  coord_flip() +
  labs(x = "", 
       y = "Publications citing \n West & King (1987)",
       title = "Medicine") +
  theme_cd(18) +
  theme(plot.title = element_text(size = 19, face = "bold", margin = margin(b = 20)),
        legend.position = "none") +
  scale_y_continuous(breaks = c("0" = 0, "2" = 2, "4" = 4, "6" = 6, "8" = 8, "10" = 10, "12" = 12, "14" = 14))

### 3.- Impact of philosophers on biology discussions  (Figure 4) ----

# Divide the dataset into periods before and after philosophers discussed it:
Biology_Topic_Plot <- Biology_TM_Topics %>% 
  mutate(
    Topic_Label = Topic_Labels_Biology[as.character(Dominant_Topic)],
    Period = case_when(
      PublicationYear <= 2004 ~ "First citation-2004",
      TRUE ~ "2005-present"), 
    Period = factor(Period, levels = c("First citation-2004", "2005-present"))) %>% 
  count(Period, Topic_Label, name = "n") 

# Plot (NOTE: To get the correct labels, you have to re-run the Topic_Label code from 1.3.2.- Biology):
Philosophy_Influence <- Biology_Topic_Plot %>% 
  ggplot(aes(x = Period, y = n, group = Topic_Label)) +
  geom_line(aes(linetype = Topic_Label), linewidth = 1.5, color = "black") +
  geom_point(fill = "#417246ff", size = 5, shape = 21, stroke = 1.2, color = "black") +
  geom_text(data = filter(Biology_Topic_Plot, Period == "2005-present"),
            aes(label = str_wrap(Topic_Label, width = 20)), 
            hjust = -0.15, size = 5, lineheight = 0.9) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.3))) +
  coord_cartesian(clip = "off") +
  labs(x = "", 
       y = "Publications citing \nWest & King (1987)",
       title = "Biology publications before and\n after philosophers picked it up") +
  theme_cd(18) +
  theme(legend.position = "none",
        plot.title = element_text(size = 17, face = "bold", margin = margin(b = 20)),
        plot.margin = margin(10, 100, 1, 10))

Philosophy_Influence # View the plot

# Save figure (Figure 3):
ggsave("Figure_PhilosophyInfluence.svg",
       Philosophy_Influence,
       width = 150, height = 150, units = "mm")

### 4.- Composite figure (Figure 2)  ----

# Set margins:
Fields_Dynamic <- Fields_Dynamic + theme(plot.margin = margin(1, 0.5, 1, 1, "mm"))

Psychology_Topic_Hist <- Psychology_Topic_Hist  + theme(plot.margin = margin(1, 3, 1, 0.5, "mm"))
Biology_Topic_Hist <- Biology_Topic_Hist + theme(plot.margin = margin(1, 1, 1, 2, "mm"))
Philosophy_Topic_Hist <- Philosophy_Topic_Hist + theme(plot.margin = margin(1, 3, 1, 0.5, "mm"))
Medicine_Topic_Hist <- Medicine_Topic_Hist + theme(plot.margin = margin(1, 1, 1, 2, "mm"))

# Add tags:
Fields_Dynamic_t <- tags_cd(Fields_Dynamic, "A", size = 22, x = 0.02, y = 0.98)

Psychology_Topic_Hist_t <- tags_cd(Psychology_Topic_Hist, "B", size = 22, x = 0.02, y = 0.96)
Biology_Topic_Hist_t <- tags_cd(Biology_Topic_Hist, "C", size = 22, x = 0.02, y = 0.96)
Philosophy_Topic_Hist_t <- tags_cd(Philosophy_Topic_Hist, "D", size = 22, x = 0.02, y = 0.98)
Medicine_Topic_Hist_t <- tags_cd(Medicine_Topic_Hist, "E", size = 22, x = 0.02, y = 0.98)

# Topics-left column:
Topic_Hist_l <- plot_grid(
  Psychology_Topic_Hist_t, Philosophy_Topic_Hist_t,
  ncol = 1,
  align = "v",  
  axis = "tb",
  rel_heights = c(0.9, 0.9))

# Topics-right column:
Topic_Hist_r <- plot_grid(
  Biology_Topic_Hist_t, Medicine_Topic_Hist_t,
  ncol = 1,
  align = "v",
  axis = "tb",
  rel_heights = c(0.9, 0.9))

# Topics-full figure:
Topic_Hist <- plot_grid(
  Topic_Hist_l, Topic_Hist_r,
  ncol = 2,
  align = "h",
  axis = "lr",
  rel_widths = c(0.9, 0.9, 0.9))

Topic_Hist <- Topic_Hist + theme(plot.margin = margin(1, 2, 1, 1, "mm")) # Set a bit of extra margin to the right

# Figure with Fields and Topics (Figure 3)
FigureInfluence <- plot_grid(
  Fields_Dynamic_t, Topic_Hist,
  ncol = 2,
  align = "v",
  axis = "tb",
  rel_widths = c(0.5, 0.9))

# Save in .svg to export to InkScape:
ggsave("FigureFieldsTopics.svg",
       FigureInfluence,
       width = 459, height = 171, units = "mm")
 
## Supplementary analyses ----

### 1.- Figure of total influences by field (Figure S1) ----

# Group Field categories that do not surpass the threshold:
ForCit_FieldGrouped <- ForCit %>% 
  group_by(Field) %>% # Group by Field
  mutate(N_Field = n()) %>% # Create a new variable for "n" in each field
  ungroup() %>% # Ungroup
  select(-N_Field)

# Define colors for the main categories (Sanzo Wada-inspired):
Field_Colors <- c(
  "Psychology" = "#8895b6ff", 
  "Philosophy" = "#cecdaeff", 
  "Biology" = "#417246ff", 
  "Medicine" = "#9E2913") 

# Plot of the most relevant categories:
Fields_Static <- ForCit_FieldGrouped %>%
  filter(Field != "Other") %>% 
  ggplot(aes(x = fct_infreq(Field), fill = Field)) +  
  geom_bar(color = "black", position = "dodge") +
  scale_fill_manual(values = Field_Colors) +
  labs(x = "", 
       y = "Publications citing \n West & King (1987)") +
  guides(fill = "none") +
  theme_cd(18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

Fields_Static # View the plot

