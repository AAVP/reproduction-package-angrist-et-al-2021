// Purpose: Looks at the distribution of covariates in our sample and in Boston as a total
********************************************************************************
* Preliminaries
cap log close

clear all
set more off

* Load data
use "${`c(username)'_data}/laborsupply", clear
	keep driver_uuid
	tempfile drivers
	save `drivers', replace
use "${`c(username)'_data}/eligible", clear

merge 1:1 driver_uuid using `drivers' , assert(master match)
	gen RCT = (_merge ==3)
	count if RCT
	assert r(N)==1600

* Generate variable for new car
gen verynew = (vehicle_year>=2011) & !missing(vehicle_year)

gen all = 1
********************************************************************************
// Covariates and table parameters

#delimit ;
local samples
	all
	eligible
	RCT
	;
local covariates
	female
	age
	hours_4
	avg_hours_4
	avg_earnings_4
	avg_farebox_4
	months_since_signup
	vehicle_solutions
	old
	verynew
	commission
	;
local replace_0
	hours_1
	earnings_1
	avg_hours_4
	avg_earnings_4
	farebox_1
	avg_farebox_4
;
#delimit cr

gen high = (hours_bucket=="high")
gen new_20 = commission==20 & car=="new"
gen new_25 = commission==25 & car=="new"
gen old = car=="old"
egen strata = group(high new_20 new_25 old)

* Generate variables
gen farebox_1 = (hours_1 * earnings_1) / (1-commission/100)
gen avg_farebox_4 = (avg_hours_4 * avg_earnings_4) / (1-commission/100)


* Clean variables
foreach var of local covariates {
	destring `var', replace ignore("NA")
}

foreach var of local replace_0 {
	replace `var' = 0 if missing(`var')
}

local n_cols = 4

********************************************************************************
* Make table

// Set up blank matrices
matrix results = J(1, `n_cols', 0)
matrix N = J(1, `n_cols', 0)

// loop over covariates which define rows
foreach cov of local covariates {
	matrix row = J(2, `n_cols', 0)
	
	// loop over samples which define columns
	local col = 1
	foreach s of local samples {
	
	sum `cov' if `s'
		matrix row[1, `col'] = r(mean)
		matrix row[2, `col'] = r(sd)
		matrix N[1, `col'] = r(N)
	local col = `col' + 1
	
	} // end loop over samples
	
	reg `cov' RCT i.strata if eligible, r
	matrix row[1, `col'] = _b[RCT]
	matrix row[2, `col'] = _se[RCT]
	matrix N[1, `col'] = e(N)
	
	// append row to base matrix
	matrix results = results \ row
} // end loop over covariates

matrix results = results \ N	
clear
svmat results
matlist results
drop if _n == 1
export excel using "${`c(username)'_tables}/tables_raw_deck.xls", sheet("Table1") sheetreplace

