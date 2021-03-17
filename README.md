# Bachelors-final-project
This repository is dedicated to all the work developed during the final semester of my undergraduate project and it is my Bachelor's project required for my Bachelor's degree. My bachelor thesis is published at the UPC knowledge repository and can be accessed here: https://upcommons.upc.edu/handle/2117/120691

## Brief overview
This goal of this project is to find optimal discriminating parameters using non-linear machine learning methods (specifically support vector machine and random forest) for microbial source tracking in fecally contaminated water to reduce time and cost overhead for the microbiologists from data consisting of 10,000 samples discriminated by 45 features. 
In this 45 features, the first 30 predictors are chemical and microbial indicators such as human viruses, host-specific bacteria, host mitochondrial DNA, host-specific bacteriophages and artificial sweeteners such as saccharin. The next 15 variables are the ratios calculated from these indicators (viz. the first 30 variables). 

For each machine learning method, two analyses was done using the aforementioned data but different features: one with all features (45 predictors) and the other with singles only excluding ratios (i.e., the first 30). And for each analysis, classification was done on two-class setting and four-class settings. (complete details can be referred from the thesis). 

## Directory structure
Each directory here focuses on a particular machine learning method. In each directory, there will be 2 classes and 4 classes directories, indicating data setting considered are 2 classes and 4 classes respectively. In each of these directories, there will be 'all features' and 'singles only' directory, indicating the features considered for the analysis are all 45 features and singles only (first 30 features) respectively.
