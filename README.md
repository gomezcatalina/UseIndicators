#UseIndicators

# Summary of decisions/changes to read, update and analyze indicator data sets
# Fall 2016
# Bundy, Cook, Gomez


## Section I. Raw data sets (shelfsetq, esswsssetq, nafosetq, stratsetq)
When the data are first extracted some indicators have NAs and some have zeros. However, an NA does not always mean “no available information” and zero does not always mean zero. In the script used to extract the indicators, when merging tables across all years, if some of the tables have missing years they get filled with NA for that missing year. The following points outline decisions and changes that were made to NAs or zeros to make them reflect the actual situation (script used is here: https://github.com/gomezcatalina/ExtractIndicators/blob/master/R/combineIndicators.R)

a.	For biomass based indicators (RV survey), NA’s were considered real NA’s since even if biomass of species “x” was not captured in the survey, species “x” is still likely to present, just not detected or perhaps not recorded. If we treated them as zeros, this would have an impact on trends, whereas treating them as NAs does not. The following indicators had NAs that were maintained as NAs: BiomassTL3','BiomassTL4','Biomass','BiomassClupeids','BiomassFinfish','BiomassFlatfish','BiomassForage','BiomassGadoids','BiomassGroundfish','BiomassSkates','BTGLargeBenthivore','BTGMediumBenthivore','BTGPiscivore','BTGPlanktivore','BTGZoopiscivore', 'BPelagicToDemersal' 
(see also section II below)

b.	Some landings indicators (.L) had NAs (when merging tables across all years, if some of the tables have missing years they get filled with NA for that missing year). All landings indicators (.L) with missing years should be zero, not NA. Thus, NAs were replaced with zeros. This was applied to the following indicators at the following spatial scales: LClupeids.L; LForageFish.L; LInvertebrates.L; LLargePelagic.L; LSkates.L. shelfsetq, esswsssetq, nafosetq.
This change was done using combineIndicators.r script (via filterNA2Zero.r function).
Note: In the latest extraction of indicators (Gomez, September 28 2016) landings up to 2015 were not extracted (even though years for all landings scripts are updated up to 2015). Consequently, for now, NA’s for landings indicators in 2015 will remain as NA’s.  

c.	Some fishing pressure (.L) indicators had NA’s. If landings indicators (see item b) equal to zero, then NAs of Fishing Pressure were set to zero as well. This was applied to the following indicators at the following spatial scales: FPClupeids.L; InverseFPClupeids.L; FPForageFish.L; InverseFPForageFish.L; FPInvertebrates.L; InverseFPInvertebrates.L;  InverseFPSkates.L. shelfsetq, esswsssetq, nafosetq.
The remaining NA’s in FP will remain as NA’s because there are landings, but not estimate of biomass, which means there is a value for FP, its just not available

d.	Two indicators (RV survey) have zeros (at the strata level mostly): LargeFishIndicator & LargeSpeciesIndicator. Since these are based on biomass, these zeros will be changed to NAs using combineIndicators.r script, following the same logic for the biomass indicators. 
(see also section II below)

## Section II. Raw data sets modified (dealing with NAs)

Measurability and missing data: we calculated the proportion of NAs for each indicator and individual large scale and strata scale. Indicators that have more than 25% of NAs at a given scale (red fonts highlighted below for the large scale and strata scale) will not be further considered in the redundancy analysis (filtered out of the csv files) as they have more than one quarter of the time series missing, thus challenging the criterion of measurability. 

Three indicators were removed from all strata (BInvertebratesToDemersal, BiomassInvertebrates, BiomassTL2 ) and eleven indicators were removed from some strata.

Imputation (replacement) of missing values: For the remaining NAs in the indicator time-series, we replaced indicator values to avoid NAs. Avoiding NA’s is required to conduct the hierarchical cluster analysis (HCA). If NA’s are not replaced, HCA removes entire rows (years) in the data set (i.e. HCA omits all the other data for the year of the missing value, therefore excluding the whole year from the analysis).
To replace NA’s we used the non-parametric missing value imputation via Random Forest (RF) [function missForest ()].This imputation uses the relationship between all rows of indicators to fill in the NA for that specific row, which should lead to an 'average' response for that NA value given the information from all other indicators. 

For example, to fill NA’s in BiomassClupeids at the NAFO scale, missForest takes the relationships between BiomassClupeids to all other indicators across all NAFO areas (i.e. 4VS,4W, 4X,4VN in data set nafosetq.csv), and then predicts BiomassClupeids using all the indicators for the row (year) where BiomassClupeids is NA.

One of the advantages of RF imputations is that it constitutes a regression classification scheme that is scale invariant, so the scale of the predictors (indicators) does not matter. Essentially, it uses the relationship between all rows of indicators (year) to fill in the NA for that specific row (year), which should lead to an 'average' response for that NA value given the information from all other indicators. 

1.	Imputation at the shelf scale

a.	Read input data (shelfsetq.csv) and filtered out  three indicators (removed three columns) that had more than 25% NA’s (Table 1):
BInvertebrateToDemersal, BiomassInvertebrates, FPInvertebrates.L 
b.	Performed imputation using the dataset shelfsetq.csv
c.	Saved new filtered csv file (used in redundancy analysis): data/shelfsetq_filtered&interpolated.csv
2.	Imputation ESS and WSS  

a.	Read input data (esswsssetq.csv) and filtered out  three indicators (removed three columns) that had more than 25% NA’s (Table 1):
BInvertebrateToDemersal, BiomassInvertebrates, FPInvertebrates.L 
b.	Performed imputation using the dataset esswsssetq.csv
c.	Saved new filtered csv file (used in redundancy analysis): data/esswsssetq_filtered&interpolated.csv 
3.	Imputation for NAFO areas 

a.	Read input data (nafosetq.csv) and filtered out  three indicators that consistently had more than 25% NA’s at the NAFO scale (Table 1):
BInvertebrateToDemersal, BiomassInvertebrates, FPInvertebrates.L 
b.	Performed imputation using the dataset nafosetq.csv 

c.	As per Table 1, we filtered out three additional indicators with > 25% NA’s: BiomassClupeids and CCPlanktivores for 4VS & BiomassTL2 and CCPlanktivore for 4VN 

d.	Saved new filtered csv files (used in redundancy analysis): data/nafo4VSsetq_filtered&interpolated.csv data/nafo4VNsetq_filtered&interpolated.csv, data/nafo4Wsetq_filtered&interpolated.csv, data/nafo4Xsetq_filtered&interpolated.csv
Note: the interpolated data for 4X and WSS are different because WSS interpolation is performed using the dataset esswsssetq.csv while 4X interpolation is performed using the dataset nafosetq.csv.This differences will be important when interpreting results of the redundancy analyses.)
4.	Imputation at the strata scale
a.	Read input data (stratasetq.csv) and filtered out  three indicators that had more than 25% NA’s consistently for all individual strata (Table 2): BInvertebrateToDemersal, BiomassInvertebrates, BiomassTL2
b.	Performed imputation using the dataset stratasetq.csv

c.	As per Table 2, we filtered out additional indicators with > 25% NA’s. Indicators were filtered individually depending on the strata ID. 

d.	Saved new filtered csv files for each individual strata scale (used in redundancy analysis): stratasetq440_filt&int.csv, stratasetq441_filt&int.csv etc

Data products used for this section are: 

Scripts used: Read&interpolateData.R 
https://github.com/gomezcatalina/UseIndicators/blob/master/code/Read%26InterpolateData.R
Input and output data: 
https://www.dropbox.com/sh/c6j3jecyahzeng4/AAB3JXqIdJD9e0JlPqD6oNO7a?dl=0

## Section III. Redundancy analysis

1.	Hierarchical Cluster Analysis (HCA)
Scripts used: HCA_largeScales.R & HCA_strata.R
https://github.com/gomezcatalina/UseIndicators/tree/master/code
Data sets used: output/data/…setq_filtered&interpolated_s.csv
https://www.dropbox.com/sh/c6j3jecyahzeng4/AAB3JXqIdJD9e0JlPqD6oNO7a?dl=0

Data output: output/analysis/HCA 
https://www.dropbox.com/sh/7b7kyukxxy7jbq2/AADGNJoYM4DKFbxA62b_zF_ha?dl=0
Note: This includes a spreadsheet for the strata and large spatial scales summarizing cluster memberships

2.	 Spearman correlation results for all scales
Script used: CorrelationAnalysis.R
https://github.com/gomezcatalina/UseIndicators/blob/master/code/CorrelationAnalysis.R
Data sets used: output/data/…setq_filtered&interpolated_s.csv (same as HCA)
Data output: output/analysis/SpearCorr
https://www.dropbox.com/sh/fjhhl3o4jeqo6km/AAADyVa1d7Ld0g3LAt2ffjhoa?dl=0
Note: SpearCorrResults_summary.xlsx has the results for all scales 

## Section IV. Plot clusters of indicators and singletons
Scripts used: Read&InterpolateData.R (sections II, IV, V)
https://github.com/gomezcatalina/UseIndicators/blob/master/code/Read%26InterpolateData.R 
Data sets used: output/data/…setq_filtered&interpolated_s.csv  
Data output: output/figures/clusteres&singletons/...pdf https://www.dropbox.com/sh/co27e9n4o317h53/AACDK8K_UOP5cvJcKyrFP7pEa?dl=0


