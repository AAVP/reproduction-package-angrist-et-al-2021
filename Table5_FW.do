* First 3 columns of Table 5 & First 3 columns of Table A7
******************************************************************************

local covariates ${covariates_stack}

******************************************************************************
* Create the stack
	// need the old data for wave 1 & wave 2
	use "${`c(username)'_data}/laborsupply", clear
	assert _N==1600
	
	// stack
	rename hours_worked_0* hours_worked_*
	rename gross_total_0* gross_total_*
	rename total_trip_payout_0* total_trip_payout_*
	reshape long hours_worked_ gross_total_ total_trip_payout_, i(driver_uuid) j(date)
	rename *_ *
	
	bys driver_uuid (date): gen week = _n
	encode driver_uuid, gen(id)
	xtset id week
	
	// generate indicators for live weeks
	gen fw = inlist(date, 829, 905)	// week 1 is 8/29, week 2 is 9/05
	gen fw1 = (date==829)
	gen fw2 = (date==905)
	
	// LHS is log hours
	gen log_hours = log(hours_worked)
	gen log_earnings = log(gross_total)	// gross totals are used as a covariate
	
	// RHS is wages
	gen avg_hourly = log(total_trip_payout / hours_worked)
	
	// Lags for covariates
	gen lag_log = L.log_earnings if fw1
	replace lag_log = L2.log_earnings if fw2	// because 1 lag was a live week
	
	// Sample: keep live weeks only
	keep if fw
	
	// Treatment indicators
	replace wave1 = 0 if fw2		// treated in wave 1 if calendar time = wave 1 and offered
	replace wave2 = 0 if fw1		// treated in wave 2 if calendar time = wave 2 and offered
	
	// overid treatment indicators
	foreach bw in high low {
	foreach c in 20 25 {
		gen wave1_`bw'_`c' = wave1 * (commission==`c') * `bw'
		gen wave2_`bw'_`c' = wave2 * (commission==`c') * `bw'
	}
	}
	
	// keep only the relevant variables
	keep log_hours avg_hourly ${covariates_stack} wave1 wave2  ///
		driver_uuid strata_eligible high low all fw* id ///
		optin high* low* vehicle_year week wave1_* wave2_*
gen treated = wave1 + wave2

******************************************************************************
******************************************************************************
// WITH COVARIATES
matrix results = J(10, 4, .)
local i = 1

foreach bw in all high low {
// OLS
regress log_hours avg_hourly i.strata_eligible `covariates' i.week ///
	if !missing(log_hours) & !missing(avg_hourly) & `bw', vce(cluster driver_uuid)
	matrix results[1, `i'] = _b[avg_hourly]
	matrix results[2, `i'] = _se[avg_hourly]

// 2SLS
ivregress 2sls log_hours (avg_hourly = treated) `covariates' i.strata_eligible i.week ///
	if `bw', vce(cluster driver_uuid)
	matrix results[5, `i'] = _b[avg_hourly]
	matrix results[6, `i'] = _se[avg_hourly]

// 2SLS Over-id
ivregress 2sls log_hours (avg_hourly = wave1_* wave2_*) `covariates' i.strata_eligible i.week ///
	if `bw', vce(cluster driver_uuid)
	matrix results[7, `i'] = _b[avg_hourly]
	matrix results[8, `i'] = _se[avg_hourly]
	
// first stage 
reg avg_hourly treated i.strata_eligible i.week `covariates' ///
	if e(sample)==1 & !missing(log_hours) & !missing(avg_hourly) & `bw', vce(cluster driver_uuid)
	matrix results[3, `i'] = _b[treated]
	matrix results[4, `i'] = _se[treated]
	
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
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("Table5_FW") sheetreplace	
restore
******************************************************************************
******************************************************************************
* Results without covariates

matrix results = J(10, 4, .)
local i = 1

foreach bw in all high low {
// OLS
regress log_hours avg_hourly i.strata_eligible i.week ///
	if !missing(log_hours) & !missing(avg_hourly) & `bw', vce(cluster driver_uuid)
	matrix results[1, `i'] = _b[avg_hourly]
	matrix results[2, `i'] = _se[avg_hourly]

// 2SLS
ivregress 2sls log_hours (avg_hourly = treated) i.strata_eligible i.week ///
	if `bw', vce(cluster driver_uuid)
	matrix results[5, `i'] = _b[avg_hourly]
	matrix results[6, `i'] = _se[avg_hourly]

// 2SLS Over-id
ivregress 2sls log_hours (avg_hourly = wave1_* wave2_*) i.strata_eligible i.week ///
	if `bw', vce(cluster driver_uuid)
	matrix results[7, `i'] = _b[avg_hourly]
	matrix results[8, `i'] = _se[avg_hourly]
	
// first stage 
reg avg_hourly treated i.strata_eligible i.week ///
	if e(sample)==1 & !missing(log_hours) & !missing(avg_hourly) & `bw', vce(cluster driver_uuid)
	matrix results[3, `i'] = _b[treated]
	matrix results[4, `i'] = _se[treated]
	
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
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA7_FW") sheetreplace	
restore
