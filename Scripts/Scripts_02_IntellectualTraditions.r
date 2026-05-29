############ WestKingImpact - Intellectual traditions ###############

# BASIC INFO ----

# Researcher in charge: Juan Gefaell
# Section aim: What are the works that are most frequently co-cited alongside West and King (1987)? What is the conceptual landscape around this work?
# Last update: 2026-05-28

# SETUP SECTION ----

## 1.- Packages ----
library(tidyverse) # Basic plots and statistical analyses
library(stringr) # To normalize text
library(stringi) # To manipulate text in general
library(ggrepel) # For adding repelling text tags in plots
library(igraph) # For Network analyses
library(widyr) # To calculate pairwise counts (co-citations across papers)
library(ggraph) # To construct fancy network plots
library(cowplot) # Composite figures
library(grid) # Composite figures 

## 2.- Dataset loading ----
setwd("") # Set the directory of the dataset file
read.csv("WKI_DA_RD_02CoCitationPatterns.csv") 

## 3.- Data handling and overview ----
CoCit <- read.csv("WKI_DA_RD_02CoCitationPatterns.csv") # Set a shortcut name
str(CoCit) # Check the structure of the data

## 4.- Themes for figures ----

# Theme for figures:
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
      plot.margin = margin(3, 3, 3, 3, unit = "mm")
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

### 1.- Data curation ----

#### 1.1.- Functions to normalize text and DOIs ----

# Specific aim: These functions normalize the text variables that will subsequently help us curate the data.

#///

# Previous step (normalizing helpers):
norm_doi <- function(x) { # Function to normalize DOIs
  x %>% 
    str_to_lower() %>%  # Convert to lowercase
    str_replace_all("^https?://(dx\\.)?doi\\.org/", "") %>%  # Remove htpps and stuff
    str_replace_all("^doi:", "") %>%  
    str_squish() %>%  # Remove white spaces
    na_if("") # Convert missing info to NAs
}

norm_txt <- function(x) { # Function to normalize text
  x %>% 
    str_to_lower() %>% # Convert to lowercase
    stringi::stri_trans_general("Latin-ASCII") %>% # Remove accents
    str_replace_all("[[:punct:]]+", " ") %>% 
    str_squish() # Remove white spaces
}

#### 1.2.- Checking duplicates ----

# Specific aim: Search for duplicates, and if any, remove them.
# NOTE: This was already done in a previous step ('ParameterAcquisition'), so this chunk is only to corroborate that duplicates within 'Citing_' works have been successfully removed.

#/// 

# Create the reference identity key to assess duplicates:
CoCit <- CoCit %>% 
  mutate(
    DOI_Norm = norm_doi(DOI), # Curated DOI
    Author_Norm = norm_txt(Author), # Curated author names
    Title_Norm = norm_txt(Title), # Curated titles
    Identifier_Provisional = case_when( # Prefer DOI, if not, then author, year, and title
    !is.na(DOI_Norm) ~ paste0("DOI:", DOI_Norm),
    TRUE ~ paste("REF:", Author_Norm, " | ", PublicationYear, " | ", Title_Norm)))

# Look for duplicates within citing papers:
Duplicate_Check <- CoCit %>% 
  count(Citing_Title, Identifier_Provisional, name = "n") %>% 
  filter(n > 1) %>% 
  arrange(desc(n), Citing_Title)
# OUTPUT: No duplicates (i.e., The duplicates were successfully removed in the 'ParameterAcquisition' step)

#### 1.3.- Merge book editions----

# Specific aim: Search for different editions of the same book, or instances where it has been named slightly differently, so that they are computed together. Otherwise this can affect co-citation patterns.

#/// 

# Add in the table the potential candidate books that have more than one edition or naming (inferred from manual inspection of the dataset and data analysis):
Edition_Merges <- tribble(
  ~Match_Author,                         ~Match_Title,                                      ~Unified_Key,                                ~Unified_Label,
  "jablonka.*lamb|lamb.*jablonka",       "evolution.*four.*dimensions",                     "BOOK:JABLONKA_LAMB_E4D",                 "Jablonka & Lamb | 2005 | Evolution in Four Dimensions",
  "michel.*moore|moore.*michel",         "developmental.*psychobiology",                    "BOOK:MICHEL_MOORE_DEV_PSYCHOBIO",        "Michel & Moore | 1995 | Developmental Psychobiology",
  "west.*eberhard|eberhard.*west",       "developmental.*plasticity.*evolution",            "BOOK:WEST_EBERHARD_DEV_PLASTICITY",      "West-Eberhard | 2003 | Developmental Plasticity and Evolution", # Just in case...
  "keller",                              "century.*gene",                                  "BOOK:KELLER_CENTURY_GENE",               "Keller | 2000 | The Century of the Gene", # Just in case...
  "griffiths.*stotz|stotz.*griffiths",   "genetics.*philosophy",                            "BOOK:GRIFFITHS_STOTZ_GEN_PHIL",          "Griffiths & Stotz | 2013 | Genetics and Philosophy",
  "robert",                              "embryology.*epigenesis.*evolution",               "BOOK:ROBERT_EVO_DEVO",                   "Robert | 2004 | Embryology, Epigenesis and Evolution"
)

# Merge editions of same book:
CoCit <- CoCit %>% 
  rowwise() %>% # Inspect at a row level
  mutate(
    Key_Edition = { # Create a variable for potential matches
      hit <- Edition_Merges %>% # Look for matches in the table at the Author_Norm and Title_Norm levels
        filter(
          str_detect(Author_Norm, Match_Author), # Search for 'Match_Author' (generic) in 'Author_Norm'
          str_detect(Title_Norm, Match_Title) # Searc for 'Match_Title' (generic) in 'Title_Norm'
        )
      if (nrow(hit) > 0) hit$Unified_Key[1] else NA_character_ # If it matches, substitute it for the 'Unified_Key' in the table above
    },
    Label_Edition = { # Same as above, but for labels (more legible label than 'Unified_Key')
      hit <- Edition_Merges %>% 
        filter(
          str_detect(Author_Norm, Match_Author),
          str_detect(Title_Norm, Match_Title)
        )
      if (nrow(hit) > 0) hit$Unified_Label[1] else NA_character_
    }
  ) %>% 
  ungroup() %>% # Exit the rowwise() mode
  mutate(
    Identifier = if_else( # If 'Key_Edition' has the edition-merged 'Unified_Key', keep it; if not, return the previous Identifier_Provisional
      !is.na(Key_Edition),
      Key_Edition,
      Identifier_Provisional),
    
    Label_Provisional = if_else( # Same for the label
      !is.na(Label_Edition),
      Label_Edition,
      paste(Author, PublicationYear, Title, sep = " | ")))
  
# Check merged references:
view(CoCit %>% 
  filter(!is.na(Key_Edition)) %>% 
  select(
    Citing_Title,
    Author,
    PublicationYear,
    Title,
    DOI, 
    Identifier_Provisional,
    Identifier,
    Label_Provisional
  ) %>% 
  arrange(Identifier, PublicationYear))
# OUTPUT: No problem with West-Eberhard or Keller, but great that it unifies the rest of citations
# NOTE: The code chunk above considers two citations of a "Precis" of E4D as a citation of the book itself. I will leave it untouched since it can count as a citation to E4D in practice...

#### 1.4.- Truncated reference lists ----

# Check whether there are truncated reference lists (due to extraction)
CoCit %>% 
  group_by(Citing_DocID, Citing_Title) %>% 
  summarise(n_refs = n(), .groups = "drop") %>% 
  arrange(n_refs)
# Citing_DocID 165: Only 6 out of 14 references
# Citing_DocID 25: It only has 7, but features 8 in the dataset
# Citing_DocID 54: Only 9 out of 30
# Citing_DocID 136: Only 13 out of 15
# ...
# OUTPUT: There are some truncated references (this is quite normal due to indexing problems), but it doesn't affect citations of W&K (see above)

# Identify the West & King (1987) reference in the citing papers:
WestKing <- CoCit %>% 
  filter(PublicationYear == 1987) %>% 
  filter(str_detect(norm_txt(Author), "\\bwest\\b") | str_detect(norm_txt(Author), "\\bking\\b")) %>% 
  filter(str_detect(norm_txt(Title), "ontogenetic|niche")) %>% 
  distinct(Author, PublicationYear, Title, DOI, Identifier_Provisional, Identifier, Label_Provisional)

# Obtain the West & King (1987) key:
Key_WestKing <- WestKing %>% 
  distinct(Identifier) %>% 
  pull(Identifier)

# Check that only one West & King (1987) was found:
length(Key_WestKing)
# OUTPUT: Yes, good.

# Confirm that all the 186 references have West & King (1987) in its list:
WestKing_Ref <- CoCit %>%
  group_by(Citing_DocID, Citing_Title, Citing_Author, Citing_PublicationYear) %>%
  summarise(
    n_refs = n(),
    has_WestKing = any(Identifier %in% Key_WestKing),
    .groups = "drop")

WestKing_Ref %>% # Check if there are "FALSE" cases in "has_WestKing":
  filter(has_WestKing == FALSE)
# OUTPUT: 0 rows, so all references cite W&K (1987)

#### 1.5.- Basic descriptive data and works with long reference lists----

# Count references per citing paper:
Citing_DocID_Refs <- CoCit %>% 
  distinct(Citing_DocID, Identifier) %>% 
  count(Citing_DocID, name = "N_Refs")

# Compute average and spread:
Refs_Summary <- Citing_DocID_Refs %>% 
  summarise(
    Mean = mean(N_Refs),
    SD = sd(N_Refs),
    Median = median(N_Refs),
    Min = min(N_Refs),
    Max = max(N_Refs),
    IQR = IQR(N_Refs))
# OUTPUT: There is at least one outlier (see 'Max = 882')...

# Plot: 
ggplot(Citing_DocID_Refs, aes(x = N_Refs)) +
  geom_histogram(binwidth = 15, fill = "grey70", color = "black") +
  labs(x = "Number of references per paper", y = "Count") +
  theme_cd(18)
# OUTPUT: This confirms that there are two outliers with >750 references...

# Identifying the outliers:
Q1 <- quantile(Citing_DocID_Refs$N_Refs, 0.25) # Calculate the first quartile
Q3 <- quantile(Citing_DocID_Refs$N_Refs, 0.75) # Calculate the third quartile
IQR_Out <- IQR(Citing_DocID_Refs$N_Refs) # Calculate the interquartile range

Upper_Out <- Q3 + 1.5 * IQR_Out # Upper outliers

Refs_Outliers <- Citing_DocID_Refs %>% 
  filter(N_Refs > Upper_Out) %>% 
  arrange(desc(N_Refs))
# OUTPUT: Numbers 112 and 135 have more than 800 cited references... Suspicious

# Inspect the outliers:
view(CoCit %>% 
  filter(Citing_DocID %in% 112)) # Change between '135' and '112' to identify which they are
# OUTPUT #1: Nº 135 seems completely legit. This is a long literature review. I haven't counted references, but the >800 number seems reasonable from inspecting the article and the 'References' section.
# OUTPUT #2: Nº 112 is a Behavioral and Brain Sciences article, which includes a long review along its replies. References are computed for the review + replies, which accounts for their high number. W&K is cited in the main article. It's also legit.

### 2.- Co-citation counts ----

# Aim: Identify which works are more frequently cited alongside West & King (1987)

#///

# Dataset for bar plot:
CoCit_Count <- CoCit %>% 
  filter(!Identifier %in% Key_WestKing) %>% # Exclude W&K
  distinct(Citing_DocID, Identifier, Label_Provisional) %>% # 1 point per citing paper
  count(Identifier, Label_Provisional, name = "Weight") %>% # Create the "Weight" variable
  arrange(desc(Weight)) 

# Create custom labels to display in the bar plot:
Label_Custom_Long <- c(
  "DOI:10.1086/399858" = "Lehrman (1953) A critique of Konrad Lorenz's theory...",
  "REF: oyama susan  |  1986  |  the ontogeny of information developmental systems and evolution" = "Oyama (1986) The Ontogeny of Information...",
  "DOI:10.2307/2940982" = "Griffiths & Gray (1994) Developmental systems and...",
  "BOOK:JABLONKA_LAMB_E4D" = "Jablonka & Lamb (2005) Evolution in Four Dimensions",
  "REF: oyama susan griffiths paul e gray russell d  |  2001  |  cycles of contingency developmental systems and evolution" = "Oyama & al. (2001) Cycles of Contingency...",
  "DOI:10.1146/annurev.neuro.24.1.1161" = "Meaney (2001) Maternal care, gene expression, and...",
  "REF: gottlieb gilbert  |  1997  |  synthesizing nature nurture prenatal roots of instinctive behavior" = "Gottlieb (1997) Synthesizing Nature and Nurture...",
  "DOI:10.1037/0012-1649.27.1.4" = "Gottlieb (1991) Experiential canalization of behavioral...",
  "BOOK:WEST_EBERHARD_DEV_PLASTICITY" = "West-Eberhard (2003) Developmental Plasticity and Evolution",
  "BOOK:MICHEL_MOORE_DEV_PSYCHOBIO" = "Michel & Moore (1995) Developmental Psychobiology...",
  "DOI:10.1016/0273-2297(87)90011-6" = "Johnston (1987) The persistence of dichotomies in...",
  "BOOK:ROBERT_EVO_DEVO" = "Robert (2004) Embryology, Epigenesis, and Evolution...",
  "DOI:10.1037/0033-295x.105.4.792-802" = "Gottlieb (1998) Normally occurring environmental and...",
  "REF: lehrman daniel s  |  1970  |  semantic and conceptual issues in the nature nurture problem" = "Lehrman (1970) Semantic and conceptual issues...",
  "DOI:10.1038/nn1276" = "Weaver & al. (2004) Epigenetic programming by maternal...",
  "BOOK:GRIFFITHS_STOTZ_GEN_PHIL" = "Griffiths & Stotz (2013) Genetics and Philosophy",
  "DOI:10.1215/9780822380658" = "Oyama (2000) Evolution’s Eye: A Systems View...",
  "DOI:10.1126/science.277.5332.1659" = "Liu & al. (1997) Maternal care, hippocampal glucocorticoid...",
  "DOI:10.1016/0273-2297(90)90019-z" = "Lickliter & Berry (1990) The phylogeny fallacy...",
  "DOI:10.1037/0033-295x.109.1.26" = "Johnston & Edwards (2002) Genes, interactions, and...",
  "DOI:10.1126/science.286.5442.1155" = "Francis & al. (1999) Nongenomic transmission across..."
)

# Incorporate the custom labels into the dataset: 
CoCit_Count <- CoCit_Count %>% 
  mutate(
    Label_Custom = recode(
      Identifier,
      !!!Label_Custom_Long,
      .default = Label_Provisional))

#### 2.1.- Co-citation counts figure----

# Plot:
CoCitation_Figure <- CoCit_Count %>% 
  arrange(desc(Weight)) %>% 
  slice_head(n = 10) %>% # Display only the 10 most co-cited works (change at will)
  ggplot(aes(x = reorder(Label_Custom, Weight), y = Weight)) +
  geom_col(color = "black", fill = "#cecdaeff", linewidth = 0.7, width = 0.7) +
  #geom_text(aes(label = Weight), hjust = -0.2, size = 3) + # See the number if you want
  coord_flip() +
  labs(x = NULL, y = "Co-citation counts", title = "Works most frequently co-cited \n alongside West & King (1987)") +
  theme_cd(18) +
  theme(axis.text.y = element_text(color = "black", margin = margin(r = 4), size = 16),
        plot.title = element_text(size = 19, face = "bold", margin = margin(b = 20)))

# Save figure (Figure 4):
ggsave("Figure_CoCit.svg",
       CoCitation_Figure,
       width = 300, height = 150, units = "mm")

### 3.- Co-citation network ----

# Aim: To identify meaningful connections among the works that are cited alongside W&K (1987)

#///

#### 3.1.- Function to create short labels----
make_short_label <- function(label) {
  
  parts <- str_split_fixed(label, "\\s*\\|\\s*", 3)
  
  author <- parts[, 1]
  year   <- parts[, 2]
  
  authors <- str_split(author, "\\s*;\\s*|\\s*&\\s*")
  
  map2_chr(authors, year, function(a, y) {
    
    a <- str_squish(a)
    a <- a[a != ""]
    
    last_names <- str_extract(a, "^[^,]+")
    last_names <- str_squish(last_names)
    
    label_author <- case_when(
      length(last_names) == 0 ~ "Unknown",
      length(last_names) == 1 ~ last_names[1],
      length(last_names) == 2 ~ paste(last_names[1], "&", last_names[2]),
      length(last_names) > 2  ~ paste(last_names[1], "et al.")
    )
    
    paste0(label_author, " (", y, ")")
  })
}

# Create one short label per work:
Short_Labels <- CoCit %>% 
  distinct(Identifier, Label_Provisional) %>% 
  mutate(Label_Short = make_short_label(Label_Provisional)) %>% 
  distinct(Identifier, .keep_all = TRUE) %>% 
  select(Identifier, Label_Short)

#### 3.2.- Network construction ----

# NOTE: "Top_N" and "Min_Weight" affect network structure significantly (bear that in mind!)

# Select the most frequently co-cited works with W&K:
Top_N <- 100 # We chose this because it incorporates Laland et al. (2015) and it is of an interpretable size (otherwise, the network is a mess and the works it includes are largely conceptually irrelevant)
# NOTE: It can be changed at will to see how the network changes. With not very substantial changes, the network stays roughly equivalent.

CoCit_Count_Top <- CoCit_Count %>% # Inspect which ones these are and create an object
  arrange(desc(Weight)) %>% 
  slice_head(n = Top_N) %>% 
  pull(Identifier) 

# Set the weight threshold (meaning only papers that are cited together in at least 3 papers citing W&K, 1987):
Min_Weight <- 3 # It can be changed to more or less. By setting '3' as threshold, we ensure co-citations are somewhat consistent, rather than circumnstantial
# NOTE: Same as with Top_N.

# Create dataset for the network with Top_N references:
CoCit_Network <- CoCit %>% 
  filter(!Identifier %in% Key_WestKing) %>% # Removing West & King (1987)
  filter(Identifier %in% CoCit_Count_Top) %>% # Select only those mostly cited with West & King (1987)
  distinct(Citing_DocID, Identifier) # Remove duplicates (there shouldn't be, but just in case)

# Construct edges as pairwise co-citation counts:
Network_NoWK_Edges <- CoCit_Network %>% 
  pairwise_count(item = Identifier, feature = Citing_DocID, sort = TRUE, upper = FALSE) %>% 
  rename(from = item1, to = item2, Weight = n)

# Filter weak edges (based on Weight):
Network_NoWK_Edges <- Network_NoWK_Edges %>% 
  filter(Weight >= Min_Weight) # NOTE the threshold set above

# Create node labels:
Network_NoWK_Node_Labels <- Short_Labels

# Construct nodes:
Network_NoWK_Nodes <- tibble(
  name = unique(c(Network_NoWK_Edges$from, Network_NoWK_Edges$to))) %>% 
  left_join(Network_NoWK_Node_Labels, by = c("name" = "Identifier")) %>% 
  distinct(name, .keep_all = TRUE)

# Build graph:
Network_NoWK_Graph <- graph_from_data_frame(
  d = Network_NoWK_Edges,
  vertices = Network_NoWK_Nodes,
  directed = FALSE)

# Calculate node strength:
V(Network_NoWK_Graph)$Strength <- strength(
  Network_NoWK_Graph,
  weights = E(Network_NoWK_Graph)$Weight)


#### 3.3.- CNM (Fast greedy) optimization algorithm ----

# Run FG clustering:
Network_NoWK_FGClusters <- cluster_fast_greedy(
  Network_NoWK_Graph,
  weights = E(Network_NoWK_Graph)$Weight)

# Inspect modularity:
modularity(Network_NoWK_FGClusters)
# OUTPUT: 0.1775023

# Length:
length(Network_NoWK_FGClusters)
# OUTPUT: 3 clusters or communities

# Store FG cluster membership:
V(Network_NoWK_Graph)$Network_NoWK_FGClusters <- as.factor(
  membership(Network_NoWK_FGClusters))

# Define colors for clusters (FG):
Cluster_Colors_FG <- c(
  "2" = "#417246ff",
  "1" = "#8895b6ff",
  "3" = "#cecdaeff",
  "4" = "#9E2913") # The fourth one is not used here

# Plot the network (Figure 5):
CNM_Network <- ggraph(Network_NoWK_Graph, layout = "stress") +
  geom_edge_link(aes(width = Weight), alpha = 0.1) +
  geom_node_point(aes(fill = Network_NoWK_FGClusters, size = Strength), shape = 21, color = "black", alpha = 0.9) +
  geom_node_text(aes(label = Label_Short), repel = TRUE, size = 2.5) +
  scale_edge_width(range = c(0.5, 1)) +
  scale_size(range = c(1, 7)) +
  scale_fill_manual(values = Cluster_Colors_FG, name = "CNM communities") +
  theme_void()

# Save in .svg to export to InkScape:
ggsave("CNMNetwork.svg",
       CNM_Network,
       width = 200, height = 150, units = "mm")

# Create DOI lookup table:
DOI_Lookup <- CoCit %>% 
  group_by(Identifier) %>% 
  summarise(
    DOI = first(na.omit(DOI)),
    .groups = "drop")

# Network data:
Network_NoWK_Data <- tibble(
  Identifier = V(Network_NoWK_Graph)$name,
  Work = V(Network_NoWK_Graph)$Label_Short,
  FGCluster = V(Network_NoWK_Graph)$Network_NoWK_FGClusters,
  Strength = V(Network_NoWK_Graph)$Strength) %>% 
  left_join(DOI_Lookup, by = "Identifier") %>% 
  arrange(FGCluster, desc(Strength)) 


## Supplementary analyses ----

#### 1.- Basic network parameters ----

# NOTE: Run section 3 of Main analysis for this chunk to work

# Number of nodes and edges:
n_nodes <- vcount(Network_NoWK_Graph) 
n_edges <- ecount(Network_NoWK_Graph) # Bear in mind that this depends on Min_Weight

# Average degree (<k>):
K_Average <- 2 * ecount(Network_NoWK_Graph) / vcount(Network_NoWK_Graph)
# OUTPUT: Papers co-cited with West & King (1987) are co-cited between themselves with a mean of X other different papers (parameter influenced by Min_Weight; not taking into account their strength)

# Spread around <k> (CV):
K_SD <- sd(degree(Network_NoWK_Graph))
K_SD / K_Average
# OUTPUT: Substantial spread

# Average strength (weighted degree):
S_Average <- mean(strength(Network_NoWK_Graph, weights = E(Network_NoWK_Graph)$Weight))
# OUTPUT: The average intensity of the previous <k> co-citation relationships is X (on average, each paper has in X total co-citation instances with the papers with which it connects)

# Spread around strength
S_SD <- sd(strength(Network_NoWK_Graph, weights = E(Network_NoWK_Graph)$Weight))
S_SD / S_Average
# OUTPUT: More spread than with <k>

# Degree distribution:
Deg <- degree(Network_NoWK_Graph) # Node degrees

Deg_Dist <- tibble(k = Deg) %>% # Distribution
  count(k) %>% 
  mutate(p_k = n / sum(n))

Deg_Figure <- ggplot(Deg_Dist, aes(x = k, y = p_k)) + # Plot
  geom_point(size = 3, shape = 21, fill = "#cecdaeff") +
  #scale_x_log10() +
  #scale_y_log10() +
  labs(x = expression("Degree (" * italic(k) * ")"), y = expression("p(" * italic(k) * ")")) +
  theme_cd(18)
# OUTPUTS: Departure from the standard shape is because low co-citing works have been removed

# Correlation between strength and degrees:
Str <- strength(Network_NoWK_Graph, weights = E(Network_NoWK_Graph)$Weight) # Get the strength of the nodes

cor.test(Deg, Str) # Perform the correlation

DegCor_Figure <- tibble(Deg = Deg, Str = Str) %>% # Visualize the correlation
  ggplot(aes(x = Deg, y = Str)) +
  geom_point(fill = "#cecdaeff", shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(x = expression("Degree (" * italic(k)* ")"), y = "Strength") +
  theme_cd(18)
# OUTPUT: 0.9789087, which means that works that are connected to many papers (Degree) are also strongly connected to them (Strength)

# Composite figure (Figure S2):
Deg_Figure <- tags_cd(Deg_Figure, "A", size = 22, x = 0.02, y = 0.98) # Add tags
DegCor_Figure <- tags_cd(DegCor_Figure, "B", size = 22, x = 0.02, y = 0.98)

Fig_Deg <- plot_grid(Deg_Figure, DegCor_Figure, ncol = 2, align = "h", axis = "tb",
                     rel_heights = c(1.0, 0.8, 0.8)) # Here it goes

# Save it:
ggsave("NetworkDegree.svg",
       Fig_Deg,
       width = 220, height =100, units = "mm")

#### 2.- CNM network: Per-cluster modularity contributions (for Table S3) ----

# NOTE: Run section 3 of Main analysis for this chunk to work

# Global modularity:
modularity(Network_NoWK_FGClusters)
# OUTPUT: 0.1775023 (weak modularity)

# Extract the membership:
FGCluster_Membership <- membership(Network_NoWK_FGClusters) 

# Get the total number of links (weighted):
TotalWeight <- sum(E(Network_NoWK_Graph)$Weight)

# Establish cluster ids:
FGCluster_IDs <- sort(unique(FGCluster_Membership))

FGCluster_Modularity <- lapply(FGCluster_IDs, function(cl){
  
  # Nodes in cluster:
  Nodes_cl <- names(FGCluster_Membership[FGCluster_Membership == cl])
  
  # Subgraph:
  Sub_cl <- induced_subgraph(Network_NoWK_Graph, vids = Nodes_cl)
  
  # Internal links/weights:
  Lc <- sum(E(Sub_cl)$Weight)
  
  # Sum of strengths (weighted degrees):
  kc <- sum(strength(Network_NoWK_Graph,
                     vids = Nodes_cl,
                     weights = E(Network_NoWK_Graph)$Weight))
  
  # Modularity contribution (from Barabási, 2016, formula 9.11, p. 20; NOTE that there are two formulas for community modularity):
  Mc <- (Lc / TotalWeight) - (kc / (2 * TotalWeight))^2
  
  tibble(
    FGCluster = cl,
    Lc = Lc,
    kc = kc,
    Mc = Mc,
    N_Nodes = length(Nodes_cl)
  )
  
}) %>% bind_rows()

# See results:
FGCluster_Modularity
# OUTPUT: 1 and 2 are the most important.

# Check if total modularity can be obtained from per-cluster modularity:
sum(FGCluster_Modularity$Mc)
# OUTPUT: Yes, good.

#### 3.- CNM network: Betweenness centrality (to identify potential bridges) ----

# Get betweenness centrality:
V(Network_NoWK_Graph)$Betweenness <- betweenness(
  Network_NoWK_Graph,
  directed = FALSE,
  weights = 1 / E(Network_NoWK_Graph)$Weight, # "1 / Weight" to confer less conceptual distance to stronger co-citations (by default, the function gives weight more distance)
  normalized = TRUE)

# Bridging works:
Bridge_Works <- tibble(
  Work = V(Network_NoWK_Graph)$Label_Short,
  Betweenness = V(Network_NoWK_Graph)$Betweenness,
  Strength = V(Network_NoWK_Graph)$Strength,
  FGCluster = V(Network_NoWK_Graph)$Network_NoWK_FGClusters) %>% # Change between community detection methods
  arrange(desc(Betweenness))

# Set thresholds for the quadrants:
x_cut <- 300
y_cut <- 0.15

# Relationship between strength and betweenness (Figure S3):
Betwenness <- ggplot(Bridge_Works, aes(x = Strength, y = Betweenness, fill = FGCluster)) +
  geom_point(size = 3.5, shape = 21) +
  geom_text_repel(aes(label = Work), size = 3.5) +
  geom_vline(xintercept = x_cut, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = y_cut, linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = Cluster_Colors_FG, name = "CNM community") + # Change this line if you switch between community detection methods
  theme_cd(18) +
  theme(legend.title = element_text(size = 16))

# Save figure:
ggsave("Betwenness.svg",
       Betwenness,
       width = 210, height = 140, units = "mm")

#### 4.- Louvain optimization algorithm ----

# NOTE: Run section 3 of Main analysis for this chunk to work

# Louvain clustering (agglomerative):
Network_NoWK_LouvainClusters <- cluster_louvain(
  Network_NoWK_Graph,
  weights = E(Network_NoWK_Graph)$Weight)

# Store Louvain cluster membership as vertex attribute:
V(Network_NoWK_Graph)$LouvainCluster <- as.factor(membership(Network_NoWK_LouvainClusters))

# Define colors for clusters:
Cluster_Colors <- c(
  "1" = "#417246ff",
  "3" = "#8895b6ff",
  "2" = "#cecdaeff",
  "4" = "#9E2913")

# Plot:
Louvain_Network <- ggraph(Network_NoWK_Graph, layout = "stress") +
  geom_edge_link(aes(width = Weight), alpha = 0.1) +
  geom_node_point(aes(fill = LouvainCluster, size = Strength), alpha = 0.9, color = "black", shape = 21) +
  geom_node_text(aes(label = Label_Short), repel = TRUE, size = 2.5) +
  scale_edge_width(range = c(0.5, 1.5)) +
  scale_size(range = c(1, 7)) +
  scale_fill_manual(values = Cluster_Colors, name = "Louvain community") +
  theme_void()

# Save in .svg to export to InkScape:
ggsave("LouvainNetwork.svg",
       Louvain_Network,
       width = 200, height = 150, units = "mm")

# Network data:
Network_NoWK_Data <- data.frame(
  Work = V(Network_NoWK_Graph)$Label_Short,
  LouvainCluster = V(Network_NoWK_Graph)$LouvainCluster,
  Strength = V(Network_NoWK_Graph)$Strength) %>%
  arrange(LouvainCluster, desc(Strength))

##### 4.1.- Per-cluster modularity contributions ----

# Global modularity:
modularity(Network_NoWK_LouvainClusters)
# OUTPUT: X (weak modularity)

# Extract the membership:
LouvainCluster_Membership <- membership(Network_NoWK_LouvainClusters) 

# Get the total number of links (weighted):
TotalWeight <- sum(E(Network_NoWK_Graph)$Weight)

# Establish cluster ids:
LouvainCluster_IDs <- sort(unique(LouvainCluster_Membership))

LouvainCluster_Modularity <- lapply(LouvainCluster_IDs, function(cl){
  
  # Nodes in cluster:
  Nodes_cl <- names(LouvainCluster_Membership[LouvainCluster_Membership == cl])
  
  # Subgraph:
  Sub_cl <- induced_subgraph(Network_NoWK_Graph, vids = Nodes_cl)
  
  # Internal links/weights:
  Lc <- sum(E(Sub_cl)$Weight)
  
  # Sum of strengths (weighted degrees):
  kc <- sum(strength(Network_NoWK_Graph,
                     vids = Nodes_cl,
                     weights = E(Network_NoWK_Graph)$Weight))
  
  # Modularity contribution (from Barabási, 2016, formula 9.11, p. 20; NOTE that there are two formulas for community modularity):
  Mc <- (Lc / TotalWeight) - (kc / (2 * TotalWeight))^2
  
  tibble(
    LouvainCluster = cl,
    Lc = Lc,
    kc = kc,
    Mc = Mc,
    N_Nodes = length(Nodes_cl)
  )
  
}) %>% bind_rows()

# See results:
LouvainCluster_Modularity
# OUTPUT: Roughly equivalent to CNM...

# Check if total modularity can be obtained from per-cluster modularity:
sum(LouvainCluster_Modularity$Mc)
# OUTPUT: Yes, good.
