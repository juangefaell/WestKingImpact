# README file for datasets and scripts of `WestKingImpact` project 

* This document provides information about the data files and scripts used in `WestKingImpact` data analysis, corresponding to the paper *Philosophers of science facilitated intellectual exchange between developmental approaches in ethology and evolutionary biology*. 

## Outline of contents

````
├──── *README.md
├──── Data
│ ├── Data_01_ForwardCitationChasing.csv
│ └─── Data_02_CoCitationPatterns.csv
└──── Scripts
  ├── Scripts_01_Fields.R
  ├── Scripts_02_IntellectualTraditions.R
  └── Scripts_03_Authors.R
````

## `Data` folder

* This folder contains the raw datasets used in the different scripts. For each of the datasets, a description of the variables and values they can take is provided. 

### Data\_01_ForwardCitationChasing

* This dataset contains the references extracted with forward citation chasing. It corresponds to the curated list of works that cite West & King (1987).

#### Variables

##### Basic Variables 

* These variables were either obtained directly from `citationchaser` or manually added afterwards (see supporting information, section on `Fields` for a description)
	
	-**DocID**: Identifier number for each of the documents.

	-**DOI**: Digital object identifier of the output, as retrieved from `citationchaser`.
	
	-**ItemType**: What kind of contribution the output is. It can take the following values:
		
		-JournalArticle: A paper.
		-Commentary: A commentary on a paper.
		-Book: An entire book.
		-BookChapter: A section or chapter of a book.
		-BookReview: A book review.
		-Dissertation: A doctoral or undergraduate dissertation.
		
	-**PublicationYear**: Year when the output was published.
	
	-**Author**: The list of authors as they are retrieved from `citationchaser`.
	
	-**Title**: Title of the output.
	
	-**PublicationTitle**: Title of the journal or book where the output was published.
	
	-**Abstract**: Abstract of the output.

	-**Notes**: Personal notes about the contribution that can help make sense of the field it belongs and the topics it covers.
	
##### Variables generated during analyses

* This list of variables were not in the original dataset, but were created during the data analysis (see the scripts):

	-**Psychology_Keywords**: Weighted psychology stem keywords in the document, as inferred from a corpus comprised of the abstract (variable `Abstract`), title (`Title`), journal title (`PublicationTitle`), and personal notes (`Notes`). 
	
	-**Philosophy_Keywords**: Weighted philosophy stem keywords in the document, as inferred from a corpus comprised of the abstract (variable `Abstract`), title (`Title`), journal title (`PublicationTitle`), and personal notes (`Notes`). 	
	-**Biology_Keywords**: Weighted biology stem keywords in the document, as inferred from a corpus comprised of the abstract (variable `Abstract`), title (`Title`), journal title (`PublicationTitle`), and personal notes (`Notes`). 	
	-**Medicine_Keywords**: Weighted medicine stem keywords in the document, as inferred from a corpus comprised of the abstract (variable `Abstract`), title (`Title`), journal title (`PublicationTitle`), and personal notes (`Notes`). 
	
	-**Max_Score**: Maximum number of keywords the work gets from a field.
	
	-**Second_Score**: Second maximum number of keywords the work gets from a different field than that of `Max_Score`. 
	
	-**Difference**: Difference between `Max_Score` and `Second_Score`. 
	
	-**N_Max**: Number of fields with the maximum number of keywords in a work.
	
	-**AutomatedAssignment**: Field with to which the keywords in `Max_Score` belong. If there is more than one, it automatically assigns based on alphabetic order.
	
	-**Tie**: Dichotomous variable such that if `N_Max` ≥ 1, gets '1' (`TRUE`), and if it is 1, gets '0' (`FALSE`). 
	
	-**Low_Confidence**: Dichotomous variable such that if `Difference` = 1, gets gets '≤ 1' (`TRUE`), and if it is > 1, gets '0' (`FALSE`). 
	
	-**AutomatedAssignment_Revision**: Field assignment marking tie and ambiguous assignments. If `Tie` and `Low_Confidence` get '0', it returns the value of `AutomatedAssignment`. If `Tie` gets '1', it returns `Ambiguous_Tie`. If `Low_Confidence` gets '1', it returns `Ambiguous_Close` (i.e., the assignment in `AutomatedAssignment` is ambiguous because there is more than one field that gets a high number of keywords).
	
	-**Field**: Final field assigned to the paper. Each paper that yielded `Ambiguous_Tie` or `Ambiguous_Close`in the `AutomatedAssignment_Revised` variable, one of the coauthors (JG) took an assignment decision based on reassessment of the authors, abstract, title, and personal notes. Some non-ambiguous contributions were also reassigned, as the semi-automated protocol failed to assign them correctly. 
	
	-**ReasonCorrection**: If the field of a work was reassigned (i.e., if the fields of `AutomaticAssignment_Revised` and `Field` are different), the reason why it was changed. 
	
	-**ReasonCorrection_Standard**: Standard categories of reasons for reassignment, to be able to count them. 

### Data\_02_CoCitationPatterns

* This dataset was created for the co-citation analyses. It was created from merging the reference lists of all works citing West & King (1987) (curated). The script to generate this dataset is available upon request.  

* As for the variables, it contains the basic variables of the ForwardCitationChasing dataset, which in this case have been renamed so as to have the `Citing_` prefix before their original names (e.g., `Citing_ItemType`, `Citing_Author`, and so on). These variables refer to the source that cites West and King (1987) and whose references are the object of analysis in this dataset. The renaming was done so that the variable names of the newly added references are not confused with those of the citing paper. 

* The remaining variables are (for each of the works in the reference lists; i.e., rows):
	
	-`DocID`, `ItemType`, `PublicationYear`, `Author`, `Title`,  `PublicationTitle`, `DOI`, and `Abstract`, which have the same definition as in the ForwardCitationChasing dataset, but applied to the reference list of the works that cite West and King (1987). 
	
	-**Publisher**: Publishing house of the journal or the book. It was conserved because it can inform about the discipline to which the article belongs (because it is not feasible to do such assignation by hand as in ForwardCitationChasing due to the length of the dataset; >20,000 rows). It was nonetheless not used in the end. 

## `Scripts` folder

* This folder contains the scripts used for the data analyses. Scripts normally use one specific dataset. They are structured based on the main questions addressed in the manuscript, as well as in the same order as the main narrative. 

* All scripts have the same basic structure:

		-BASIC INFO: Explains the aim, the researcher in charge, and the date of last update.
		-SETUP SECTION: Loads the packages and datasets. It handles the data and sets themes for figures.
		-ANALYSIS SECTION: Features the code for each of the analyses. It is divided between `Main analyses`(Analyses in the main manuscript) and `Supplementary analyses`(for supplementary material). 
		
### Scripts\_01_Fields

* This script tries to map the impact of West & King (1987) in the literature, and identify how it arrived in evolutionary biology.

* In the `Main analyses` section, it includes:
	* **1.-** Influence in fields (**Section 1**): This includes creating the dictionary-based assignment (1.1), and the cummulative impact plot (1.2).
	* **2.-** Influence in topics (**Section 2**): This includes the topic modeling for the different disciplines, along with their plots.
	* **3.-** Impact of philosophers on biology discussions (Figure 4) (**Section 3**): This includes the plot measuring how philosophical discussions of West & King (1987) might have influenced evolutionary biology discussions. 
	* **4.-** Composite figure (Figure 2) (**Section 4**): This includes what the name says. 

### Scripts\_02_IntellectualTraditions

* This script aims at identifying intellectual traditions using co-citation analyses.

* In the `Main analyses` section, it includes:
	* **1.-** Data curation (**Section 1**): Curation procedure of the co-citation dataset.
	* **2.-** Co-citation counts (**Section 2**): This includes the bar plot with the most important co-cited works.
	* **3.-** Co-citation network (**Section 3**): This includes the co-citation network analysis, with its basic parameters and the CNM community detection. 

### Scripts\_03_Authors

* This script aims at identifying particular authors (specially within DST) that could have facilitated the transfer of the ontogenetic niche concept to evolutionary biology.

* In the `Main analyses` section, it includes:
	* **1.-** Main citing authors (**Section 1**): Tables identifying the authors that have cited West & King (1987) more (including weighted citation)
	* **2.-** Bridge-building potential (**Section 2**): This includes the plot of interdisciplinarity and citing intensity of authors.
