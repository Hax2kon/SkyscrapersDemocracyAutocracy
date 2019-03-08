# SkyscrapersDemocracyAutocracy

Replication material for

Gjerløw, Haakon and Carl Henrik Knutsen (2019) "Leaders, private interests, and socially wasteful projects: Skyscrapers in democracies and autocracies"
Political Research Quarterly (Forthcoming)

The replication material is organized in folders as follows:

*Production_of_Skyscraper_data*: Contains material to organize all skyscrapers at www.skyscrapercenter.com into a data.frame
*Produce_Country_Year_Formats*: Structures the skyscrapers into as country-year format, and combines this with data from www.v-dem.net (the V-Dem data must be donwloaded separately)
*Data*: For those who don't bother re-creating the data set, her are the .rdata-files necessary to reproduce the different items in the article. Notice that these datasets differ somewhat from the ones produced by "Produce_Country_Year_Formats", mainly because we have removed all variables that are never used in the paper or its online appendix.
*Analyses*: These scritps reproduce the different items in the paper, as well as all items in the online appendix.

Below follows a description of the content of the different folders:


## 	Production_of_skyscraper_data-folder
Steps to produce data on Skyscrapers:
You will need to specify your folder-path in the scripts at each step.

1. Run wget-script (Requires Linux)
2. Run "Rep_Produce_Skyscraperdata.R". This script uses "Rep_ScrapeFunction.R". This extracts the various information from the building
3. Run Rep_Clean_Skyscraperdata.R . This script cleans the data extracted.
4. Run Rep_Produce_Buildinglevel.R . This script merges the skyscraperdata with the V-Dem data set, and creates a few variables. The result is the building-level dataset.

CountryRecodeFile.csv is a file with the different country codes, made to make merging more convenient.


## 	Produce_Country_Year_Formats-folder
These scripts creates the same country-year data, but with different inclusion-criteria for the skyscrapers (used for robustness tests in the appendix)

- *Produce_country_year_notower.r*: This creates the main data files used in the majority of analyses.
- *Produce_country_year_notower_125limit.r*: Buildings >125 meters are included, as compared to the >150 meter threshold in the main data.
- *Produce_notower_exclude_demolished.r*: Excludes buildings that have been demolished.
- *Produce_country_year_includetower.r*: Towers are included as well.


## 	Data-folder
- *buildings.rdata*: This is the dataset with buildings as the unit.
- *main_dataframe.rdata*: This is the main dataset. Towers are excluded and buildings must be >150 meters.
- *countryyear_125mLimit.rdata*: The data with >125m threshold.
- *countryyear_InclTowers.rdata*: The data including towers
- *countryyear_ExclDemolished.rdata*: The data with demolished buildings excluded.


## 		Analyses-folder
- *rep_Figure1*: Reproduce Figure 1
- *rep_Figure3*: Reproduce Figure 3
- *rep_Table1*: Reproduce Table 1
- *rep_Table2*: Reproduce Table 2
- *rep_Table3*: Reproduce Table 3

- *rep_Appendix.r*: This scripts contains the code to reproduce all items in the appendix that does not use the building-level data
- *rep_Appendix_BuildingModels.r*: This scripts contains the code to reproduce all items in the appendix that use the building-level data

