* Last 3 columns of Tables 5 & A7 + Columns 1 and 2 of Table A10
******************************************************************************

// date to use for when on taxi
local date_taxi1	0919
local date_taxi2	1017

// date to use for lagged earnings and hours for taxi
local lag_taxi1	0912
local lag_taxi2	1010

local covariates ${covariates_stack}

******************************************************************************
* Create the taxi stack
use "${`c(username)'_data}/laborsupply", clear
// Only have data for individuals who opted in to free week
keep if optin

	// stack
	rename hours_worked_0* hours_worked_*
	rename gross_total_0* gross_total_*
	rename total_trip_payout_0* total_trip_payout_*
	reshape long hours_worked_ gross_total_ total_trip_payout_, i(driver_uuid) j(date)
	rename *_ *
	
	bys driver_uuid (date): gen week = _n
	encode driver_uuid, gen(id)
	xtset id week
	
	// keep taxi weeks only
	gen taxiweek = inlist(date, 919, 1017)
	gen taxi1 = (date==919)
	gen taxi2 = (date==1017)
	
	drop if taxi2 & missing(taxi2_control)
	
	// LHS is log hours
	gen log_hours = log(hours_worked)
	gen log_earnings = log(gross_total)
	
	// RHS is wages
	gen avg_hourly = log(total_trip_payout / hours_worked)
	
	// Lags for covariates
	gen lag_log = L.log_earnings
	
	// keep only taxi weeks
	keep if taxiweek
	
	// Taxi treatment variables
	gen taxi_treatment1 = (taxi1_treatment1 & taxi1==1)
		label var taxi_treatment1 "0 Fee"
	gen taxi_treatment2 = (taxi1_treatment2 & taxi1==1)
		label var taxi_treatment2 ".125 Bonus"
	gen taxi_treatment3 = (taxi2_treatment1 & taxi2==1)
		label var taxi_treatment3 "0 Fee"
	gen taxi_treatment4 = (taxi2_treatment2 & taxi2==1)
		label var taxi_treatment4 "Half Fee"
	
	gen taxi1_treatment = !taxi1_control if taxi1
	replace taxi1_treatment =0 if missing(taxi1_treatment)
	
	gen taxi2_treatment = !taxi2_control if taxi2
	replace taxi2_treatment = 0 if missing(taxi2_treatment)
	
	// generate overid
	gen treatmenttype = 1 if taxi1 & taxi1_treatment1
	replace treatmenttype = 2 if taxi1 & taxi1_treatment2
	replace treatmenttype = 3 if taxi2 & taxi2_treatment1
	replace treatmenttype = 4 if taxi2 & taxi2_treatment2
	egen overid = group(treatmenttype high commission)
	tab overid, gen(overid_)	// there are 16!
	
	foreach v of varlist overid_* {
		replace `v' = 0 if missing(`v')
	}
	
	gen taxi_treatment = taxi1_treatment + taxi2_treatment

******************************************************************************
	keep log_hours avg_hourly ${covariates_stack} taxi1 taxi2 taxi_treatment* ///
		driver_uuid strata high low all taxi1_treatment taxi2_treatment ///
		optin driver_uuid taxi?_control taxi?_optin ///
		taxi_treatment female male exp inexp vehicle_year week  overid*
******************************************************************************
* Version with Covariates for the main text

matrix results = J(10, 4, .)
local i = 1

foreach bw in all high low {

// OLS
regress log_hours avg_hourly i.strata  ${covariates_stack} i.week ///
	if !missing(log_hours) & !missing(avg_hourly) & `bw', vce(cluster driver_uuid)
	
	matrix results[1, `i'] = _b[avg_hourly]
	matrix results[2, `i'] = _se[avg_hourly]

// 2sls
ivregress 2sls log_hours (avg_hourly = taxi_treatment) ${covariates_stack} i.strata  i.week ///
	if `bw' , vce(cluster driver_uuid)
	matrix results[5, `i'] = _b[avg_hourly]
	matrix results[6, `i'] = _se[avg_hourly]

// 2sls overid
ivregress 2sls log_hours (avg_hourly = overid_*) ${covariates_stack} i.strata i.week ///
	if `bw' , vce(cluster driver_uuid)
	matrix results[7, `i'] = _b[avg_hourly]
	matrix results[8, `i'] = _se[avg_hourly]
	
reg avg_hourly taxi_treatment i.strata ${covariates_stack} i.week ///
	if !missing(log_hours) & !missing(avg_hourly) & `bw' & e(sample)==1, vce(cluster driver_uuid)
	matrix results[3, `i'] = _b[taxi_treatment]
	matrix results[4, `i'] = _se[taxi_treatment]
	
	// number of drivers & number of observations
	matrix results[10, `i'] = e(N)
	unique driver_uuid if e(sample)==1
	matrix results[9, `i'] = r(sum)

local i = `i' +1
} // end loop over bandwidth

preserve
clear
svmat results
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("Table5_Taxi") sheetreplace	
restore

******************************************************************************
* Version without Covariates for the appendix

matrix results = J(10, 4, .)
local i = 1

foreach bw in all high low {

// OLS
regress log_hours avg_hourly i.strata  i.week ///
	if !missing(log_hours) & !missing(avg_hourly) & `bw', vce(cluster driver_uuid)
	
	matrix results[1, `i'] = _b[avg_hourly]
	matrix results[2, `i'] = _se[avg_hourly]

// 2sls
ivregress 2sls log_hours (avg_hourly = taxi_treatment)  i.strata  i.week ///
	if `bw' , vce(cluster driver_uuid)
	matrix results[5, `i'] = _b[avg_hourly]
	matrix results[6, `i'] = _se[avg_hourly]

// 2sls overid
ivregress 2sls log_hours (avg_hourly = overid_*)  i.strata i.week ///
	if `bw' , vce(cluster driver_uuid)
	matrix results[7, `i'] = _b[avg_hourly]
	matrix results[8, `i'] = _se[avg_hourly]
	
reg avg_hourly taxi_treatment i.strata  i.week ///
	if !missing(log_hours) & !missing(avg_hourly) & `bw' & e(sample)==1, vce(cluster driver_uuid)
	matrix results[3, `i'] = _b[taxi_treatment]
	matrix results[4, `i'] = _se[taxi_treatment]
	
	// number of drivers & number of observations
	matrix results[10, `i'] = e(N)
	unique driver_uuid if e(sample)==1
	matrix results[9, `i'] = r(sum)

local i = `i' +1
} // end loop over bandwidth
	
preserve
clear
svmat results
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA7_Taxi") sheetreplace	
restore


******************************************************************************
* Columns 1 and 2 of Table A10

matrix results = J(8, 2, .)
local i = 1
cap gen mid = (vehicle_year<=2010)

foreach bw in old mid {

// OLS
regress log_hours avg_hourly i.strata i.week ///
	if !missing(log_hours) & !missing(avg_hourly) & `bw', vce(cluster driver_uuid)
	
	matrix results[5, `i'] = _b[avg_hourly]
	matrix results[6, `i'] = _se[avg_hourly]

// 2sls overid
ivregress 2sls log_hours (avg_hourly = taxi_treatment)  i.strata i.week if `bw' , vce(cluster driver_uuid)
	matrix results[3, `i'] = _b[avg_hourly]
	matrix results[4, `i'] = _se[avg_hourly]
	
	matrix results[8, `i'] = e(N)
	unique driver_uuid if e(sample)==1
	matrix results[7, `i'] = r(sum)
	
reg avg_hourly taxi_treatment i.strata i.week if !missing(log_hours) & !missing(avg_hourly) & `bw' & e(sample)==1, vce(cluster driver_uuid)
	matrix results[1, `i'] = _b[taxi_treatment]
	matrix results[2, `i'] = _se[taxi_treatment]

local i = `i' +1
} // end loop over bandwidth
	
preserve
clear
svmat results
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA10_Col12") sheetreplace	
restore
