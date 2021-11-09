* Angrist, Caldwell & Hall
* Uber vs. Taxi
* Note: the authors thank Anran Li for research assistance
* See the attached Readme for more information
********************************************************************************
* THESE GLOBALS NEED TO BE CHANGED BEFORE THE FILE WILL RUN ON A NEW COMPUTER
global `c(username)'_code	"/Users/sydneec/Documents/Github/Uber/"
global root					"/Users/sydneec/Dropbox (Personal)/Research/1-Active/Uber-Private/"

********************************************************************************
cd "${`c(username)'_code}"
do globals.do		// set remaining directories & set other globals
********************************************************************************
* Clean Files
	// Clean the eligible sample
	do clean_eligibles.do
	
	// Clean the labor supply data
	do clean_laborsupply.do

* Generate the one "intermediate" data file we use
	// Create the intermediate data set that is used in the opt-in analysis
	do create_participation.do
	
********************************************************************************
* TABLES
	* all are formatted in Formatted-Tables.xlsx
	* individual raw files are in tables_raw_deck.xls -- a different sheet for each table
* Table 1: Boston Uber Drivers
	do Table1.do
		
// Table 2: Earnings Accelerator Opt-In Week Parameters & Take-Up is a design table and is not produced by Stata
	
* Table 3: Who Opts In?
	do Table3.do
		
// Table 4: Earnings Accelerator Taxi Parameters and Take-Up is a design table and is not produced by Stata
	
* Table 5: Estimated ISE's (+Table A7)
	do Table5_FW.do	// First 3 columns -- ALSO PRODUCES ROBUSTNESS TABLE A7 col. 1-3
	do Table5_Taxi.do 	// Second 3 columns -- ALSO PRODUCES ROBUSTNESS TABLE A7 col. 1-3 & Columns 1-2 of Table A10
	
* Table 6: Gains and Losses from Taxi
	do Table6.do
	
* Table 7: Modeling Taxi-Take Up
	do Table7.do
	
* Table 8: Compensating Variation
	do Table8.do	// also produces robustness table A9
	
* APPENDIX TABLES
	// A1: Experimental Timeline -- is a text table
	
	* A2: Covariate Balance for Wave 1 and Wave 2
	do TableA2.do
	
	* A3-A4: Covariate Balance for Taxi 1 and Taxi 2
	do TableA3-A4.do
		
	* A5-A6: Participation 2SLS
	do TableA5-A6_FW.do
	do TableA5-A6_Taxi.do
	
	// A7: Estimates w/o covariates is created by the code for Table 5.
	
	// A8: Taxi  Take-Up by Subgroup is created by the code for Table 7
	
	// A9: Compensating Variation with UI is created by the code for Table 8.
	
	* A10:  No Lyft and low-Lyft Uber ISE's
	do TableA10.do
********************************************************************************
* FIGURES

// Figures 1-6 are screenshots or diagrams

* Figure 7: Participation Effects on LS
	do Figure7_fw.do
	do Figure7_taxi.do
	
* Figure 8: Taxi Under Subscription
	do Figure8.do
	
* Figure 9: Comparing Empirical and Theoretical Participation Quantiles
	do Figure9.do
	
* APPENDIX FIGURES
	// A1 & A2 are screenshots from the experiment
	
	* A3: Distribution  Treatment Effects
	do FigureA3.do

********************************************************************************
* 
do TextCallouts.do
