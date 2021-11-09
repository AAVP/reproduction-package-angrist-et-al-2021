******************************************************************************
// Column 5
	// need the old data for wave 1 & wave 2
	use "${`c(username)'_data}/laborsupply", clear
	assert _N==1600
	
	// strategy uses data from Wave 1 of opt-in week & the week prior to opt-in week (8/22 and 8/29)
	// pool wave 2 drivers + wave1 drivers who use an old car
		gen insample = 1 if old==1
		replace insample=1 if wave1==0
		keep if insample==1
		
	// stack the two weeks
	expand 2 
	bys driver_uuid: gen id = _n
	
	// first set of data is 8/29 (this is the treatment week); second is 08/22 (opt-in week)
	gen log_earnings = log(gross_total_0829) if id==1
	replace log_earnings = log(gross_total_0822) if id==2
	gen log_wages = log(total_trip_payout_0829/hours_worked_0829) if id==1
	replace log_wages = log(total_trip_payout_0822/hours_worked_0822) if id==2
	
	// hours
	gen log_hours = log(hours_worked_0829) if id==1
	replace log_hours = log(hours_worked_0822) if id==2
	
	// treatment indicator for DID
	gen live_old = (id==1)*old			// in paper this is di * live
	gen live = (id==1)					// live in paper
	gen twentyfive = (commission==25)	

// Column 5
matrix results = J(8, 1, .)

// first stage
reg log_wage live_old live old high twentyfive if !missing(log_hours), vce(cluster driver_uuid)
	// here twentyfive, high and live are the bandwidth, commission, and time dummies
	matrix results[1, 1] = _b[live_old]
	matrix results[2,1] = _se[live_old]
	
ivregress 2sls log_hours (log_wage = live_old) live old high twentyfive, vce(cluster driver_uuid)
	matrix results[3, 1] = _b[log_wage]
	matrix results[4, 1] = _se[log_wage]
	
// OLS
reg log_hours log_wage live old high twentyfive, vce(cluster driver_uuid)
	matrix results[5, 1] = _b[log_wage]
	matrix results[6, 1] = _se[log_wage]
	local row = `row' + 2
		
	matrix results[8, 1] = e(N)
	unique driver_uuid if e(sample)==1
	matrix results[7, 1] = r(sum)

clear
svmat results
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA10_Col5") sheetreplace	


******************************************************************************
// Columns 1-4 
use * if optin==1 using "${`c(username)'_data}/laborsupply", clear
	gen mid = (vehicle_year<=2010)
	
	* Sample in column 1 is 2003+ and sample in column 2 is 2010+.
	keep if old | mid

	// reshape long
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

	// drop the guy who left after taxi 1
	drop if taxi2 & missing(taxi2_control)
	
	// LHS is log hours
	gen log_hours = log(hours_worked)
	gen log_earnings = log(gross_total)
	
	// RHS is wages
	gen avg_hourly = log(total_trip_payout / hours_worked)
	
	// keep only taxi weeks
	keep if taxiweek
	replace taxiweek = taxi1 + 2*taxi2
	
	// generate overid
	gen treatmenttype = 1 if taxi1 & taxi1_treatment1
	replace treatmenttype = 2 if taxi1 & taxi1_treatment2
	replace treatmenttype = 3 if taxi2 & taxi2_treatment1
	replace treatmenttype = 4 if taxi2 & taxi2_treatment2
	
	// gen just-id
	replace taxi1_treatment1 = 0 if taxi2
	replace taxi1_treatment2 = 0 if taxi2
	replace taxi2_treatment1 = 0 if taxi1
	replace taxi2_treatment2 = 0 if taxi1
	gen taxi_treatment = taxi1_treatment1 + taxi1_treatment2 + taxi2_treatment1 + taxi2_treatment2
	
	keep driver_uuid log_hours avg_hourly taxi* high commission treatmenttype old mid
	
	tempfile taxi
	save `taxi'
	
* add in the old car people who were treated in wave 1
use "${`c(username)'_data}/laborsupply", clear
	gen mid = (vehicle_year<=2010)
	keep if old | mid
	append using "${`c(username)'_data}/oldcar"	// add in the 74 drivers
	count
	replace wave1 = 0 if missing(wave1)
	
	gen log_hours = log(hours_worked_0829)
	gen avg_hourly = log(total_trip_payout_0829 / hours_worked_0829)
	gen lag_log_hours = log(hours_worked_0822)
	
	keep driver_uuid old wave1 log_hours avg_hourly high commission old mid
	
append using `taxi'

	// replace things with 0's
	foreach x in wave1 taxi_treatment taxi1_treatment1 taxi1_treatment2 taxi2_treatment1 taxi2_treatment2 {
		replace `x' = 0 if missing(`x')
	}
	rename taxiweek week
	replace week = 3 if missing(week)

	replace treatmenttype = 5 if wave1 & missing(treatmenttype)

	// generate overid
	egen overid = group(treatmenttype high commission)
	tab overid, gen(overid_)	// there are 16!
	
	foreach v of varlist overid_* {
		replace `v' = 0 if missing(`v')
	}
	
	cap drop strata
	egen strata = group(old commission high)
	
gen treated = taxi_treatment + wave1
matrix results = J(8, 2, .)
local i = 1

foreach bw in old mid {

// OLS
regress log_hours avg_hourly i.strata i.week if !missing(log_hours) & !missing(avg_hourly) & `bw', vce(cluster driver_uuid)
	matrix results[5, `i'] = _b[avg_hourly]
	matrix results[6, `i'] = _se[avg_hourly]

// 2sls justid
ivregress 2sls log_hours (avg_hourly = treated)  i.strata i.week if `bw' , vce(cluster driver_uuid)
	matrix results[3, `i'] = _b[avg_hourly]
	matrix results[4, `i'] = _se[avg_hourly]
	
	matrix results[8, `i'] = e(N)
	unique driver_uuid if e(sample)==1
	matrix results[7, `i'] = r(sum)
	
reg avg_hourly treated i.strata i.week if !missing(log_hours) & !missing(avg_hourly) & `bw' & e(sample)==1, vce(cluster driver_uuid)
	matrix results[1, `i'] = _b[treated]
	matrix results[2, `i'] = _se[treated]

local i = `i' +1
} // end loop over bandwidth
	
preserve
clear
svmat results
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA10_Col34") sheetreplace	
restore
