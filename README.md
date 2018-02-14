# An analysis of bird responses to urbanization within the contigous United States

Due to the size of the files, this repository is only for a portion of the overall analysis. It mirrors a local repository on my machine, for a portion of clustering community bird data in collaboration with @mitchest

There are three main analyses in this project:

**ANALYSIS 1:** Investigating pattern of exotic and native species response to urbanization

This analysis is done off this repo:
![alt text](https://github.com/coreytcallaghan/United-States-Urban-Birds-Patterns/blob/master/Figures%20for%20paper/Figure%202/Figure_2.png)

Figure 2. (A) Mean predicted species richness (± standard error) response to landcover classes for both exotic (left) and native (right) species. Responses are predicted from generalized additive models and represent species richness observed on a given checklist. (B) Parameter estimates and 95% confidence intervals for the associated landcover classes for exotic species richness (left) and native species richness (right). See appendix 3 for model details. Note the different scales on the y-axes.

![alt text](https://github.com/coreytcallaghan/United-States-Urban-Birds-Patterns/blob/master/Figures%20for%20paper/Figure%203/Figure_3.png)

Figure 3. (A) Mean predicted effective species diversity (± standard error) response to landcover classes for both exotic (left) and native (right) species. Responses are predicted from generalized additive models and represent effective species diversity observed on a given checklist. (B) Parameter estimates and 95% confidence intervals for the associated landcover classes for exotic effective species diversity (left) and native effective species diversity (right). See appendix 3 for model details. Note the different scales on the y-axes.


![alt text](https://github.com/coreytcallaghan/United-States-Urban-Birds-Patterns/blob/master/Figures%20for%20paper/Figure%204/Figure_4.png)
Figure 4. (A) Mean predicted species abundance (± standard error) response to landcover classes for both exotic (left) and native (right) species. Responses are predicted from generalized additive models and represent the total species abundance observed on a given checklist. (B) Parameter estimates and 95% confidence intervals for the associated landcover classes for exotic species abundance (left) and native species abundance (right). See appendix 3 for model details. Note the different scales on the y-axes.


**ANALYSIS 2:** Investigating the relationship among bird communities and urbanization patterns. Are there a significant difference among landcovers in the number of urban bird communities that belong. Determine why urban green is highest, possibly because it attracts natural green and urban communities


**ANALYSIS 3:** Investigating habitat heterogeneity within urban areas. Is the heterogeneity higher within urban areas and this may explain why the highest biodiversity metrics occur within urban green areas, as opposed to natural green areas.


# Workflow of the overall project


## Step 1: Download ALL eBird sampling dataset

### Step 1.1: Subset to USA complete checklists only (18,790,965)
### Step 1.2: Subset to checklists which are >5 minutes and <240 minutes
### Step 1.3: Subset to checklists which are either stationary, travelling, or exhaustive protocols
### Step 1.4: Subset to exclude checklists which travelled greater than 10 kilometers
### Step 1.5: Subset to checklists which are between January 1st 2010 - November 31st 2017
This leaves 7,396,020 checklists which are to be used in the analysis! 


## Step 2: Download eBird basic dataset January 1st 2010 - November 2017

### Step 2.1: Convert the eBird basic dataset to a sqlite database to access it


## Step 3: Assign each checklist (SAMPLING_EVENT_IDENTIFIER) to various spatial datasets

### Step 3.1: Assign each checklist to a BCR (already done in downloaded eBird data)
### Step 3.2: Assign each checklist to an urban city
### Step 3.3: Assign each checklist to urban/non-urban
### Step 3.4: Assign each checklist to a NLCD 2011 landcover class


##### Directions for step 3 
###### Step 4.1.1: pull in txt file containing Final_sampling_data
###### Step 4.1.2: display x/y values
###### Step 4.1.3: use 'extract values to points' tool with the EVENTS data (do not export to shapefile!!)
###### Step 4.1.4: use 'join' tool to join the extracted features from the previous steps with the 2014 urban city census layer
###### Step 4.1.4.1: used intersect and search radius of 25 meters
###### Step 4.1.4.2: removed all unneccesary columns
###### Step 4.1.5: export the table as a txt file
###### Step 4.1.6: fix the column names in R
###### Step 4.1.7: make one column that is urban(belongs to a city) or non-urban (doesn't belong to a city)
###### Step 4.1.8: rename the NLCD values to the associated BCRs
This is saved as eBird_spatial_joins.txt and then after cleaning in R (making_final_spatial_data_eBird_checklists.R) is saved as "eBird_sampling_spatial_data.RData"


## Step 4: Extract all unique species seen in the lower 48 from eBird sqldb
This is saved as "unique_species_merged_with_clements.csv"


## Step 5: Use Clements 2017 checklist to assign the taxonomic information to each species


## Step 6: Assign each unique species either native or exotic
Note that House Finch I put as native for the entire US
This is saved as "Species_classifications.csv and .xlsx"


## Step 7: Extract all the data from the sqldb for each BCR and save it as a .RData file
These are saved in "Data/eBird data/eBird_BCR_split_data" folder. 29 files 


## Step 8: Create an Appendix 1 file - using unique species seen in each BCR and the species classification csv
### Step 8.1: Clean the file up and delete a couple of duplicates


## Step 9: Make a dataframe for Analysis 1. 
### Step 9.1: Load each .RData file in a loop.
### Step 9.2: Create richness per checklist df
### Step 9.3: Combine species and issf into one count
### Step 9.4: Remove any lists that put at least one 'X'
### Step 9.5: Creat diversity/abundance dataframe
### Step 9.6: Write these dataframes out as temp data saves
### Step 9.7: Read in all the temp data saves and combine into one dataframe for ANALYSIS 1
Apparently a small number of checklists weren't assigned an urban or nonurban layer when doing the spatial joins. These remained as "Green Area" as opposed to Urban or Natural Green Area in this step, I filtered these out, before saving the data for analysis 1



