* Right half of Tables A5 & A6
*****************************************************************************

use * if optin==1 using "${`c(username)'_data}/laborsupply", clear
	// only included in the Taxi phase if opted in to free week
	assert _N==1031

******************************************************************************
local outcomes_T5 have_earnings log_hours log
local outcomes_T6 completed_trips num_days hourly_farebox prop_surged avg_rating prop_rated 
local full_outcomes `outcomes_T5' `outcomes_T6'

******************************************************************************	
* Create stack: 2 observationsn per driver (one for each of the two free weeks)
expand 2
bys driver_uuid: gen id = _n
******************************************************************************	

// date to use for lagged earnings and hours for taxi
local lag_taxi1	0912
local lag_taxi2	1010
	
* Create labor supply outcomes that are not in laborsupply.dta
foreach t in 0919 1017 0912 1010 {
	gen have_earnings_`t' = (gross_total_`t' > 0)
		label var have_earnings_`t' "Positive Earnings in Week beginning `t'"
		
	* Outcome for Table A6: hourly earnings
	gen hourly_farebox_`t' = gross_total_`t' / hours_worked_`t'
		label var hourly_farebox_`t' "Avg Hourly Farebox Week Beginning `t'"
}

foreach o of local full_outcomes {
	gen `o' = `o'_0919 if id==1
	replace `o' = `o'_1017 if id ==2
}

// remove the observation corresponding to person who moved out of Boston between taxi 1 & 2
	drop if id==2 & missing(taxi2_control)
******************************************************************************
* Exogenous instrument: indicators for each type of taxi offer
	// make sure to recode to 0 in the off-weeks
replace taxi1_treatment1 = 0 if id==2
replace taxi1_treatment2 = 0 if id==2
replace taxi2_treatment1 = 0 if id==1
replace taxi2_treatment2 = 0 if id==1
cap drop treated
gen treated = taxi1_treatment1 + taxi1_treatment2 + taxi2_treatment1 + taxi2_treatment2
******************************************************************************
* Endogenous variable: take taxi
gen taxi_optin = taxi1_optin if id==1
replace taxi_optin = taxi2_optin if id==2

******************************************************************************
// Set up lag log 
gen lag_log = log_0912 if id==1
replace lag_log = log_1010 if id==2

******************************************************************************
// Replace if missing
replace avg_rating = . if avg_rating==0 | completed_trips==0
replace prop_surged = . if completed_trips==0
replace prop_rated = . if avg_rating==. | completed_trips ==0

******************************************************************************
* Table A5: other outcomes

// Panel A: strata
matrix results = J(1, 6, .)
foreach o of local outcomes_T5 {
matrix row = J(3, 6, .)
local col = 1

foreach bw in all high low {
	// control mean
	sum `o' if `bw' & treated==0
	matrix row[1, `col'] = r(mean)
	local col = `col' + 1
	
	// treatment effect
	ivregress 2sls `o' (taxi_optin = taxi1_treatment1 taxi1_treatment2 taxi2_treatment1 taxi2_treatment2) ///
			i.id i.strata ///
			if `bw' , vce(cluster driver_uuid)
	matrix row[1, `col'] = _b[taxi_optin]
	matrix row[2, `col'] = _se[taxi_optin]
	matrix row[3, `col'] = e(N)

	local col = `col' + 1
}
matrix results = results \ row
}

preserve
clear
svmat results
drop if _n==1
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA5_TaxiA") sheetreplace	
restore

// Panel B: strata + covariates
matrix results = J(1, 6, .)
foreach o of local outcomes_T5 {
matrix row = J(3, 6, .)
local col = 1

foreach bw in all high low {
	// control mean
	sum `o' if `bw' & treated==0
	matrix row[1, `col'] = r(mean)
	local col = `col' + 1
	
	// treatment effect
	ivregress 2sls `o' (taxi_optin = taxi1_treatment1 taxi1_treatment2 taxi2_treatment1 taxi2_treatment2) ///
			i.id i.strata ${covariates_stack} ///
			if `bw' , vce(cluster driver_uuid)
	matrix row[1, `col'] = _b[taxi_optin]
	matrix row[2, `col'] = _se[taxi_optin]
	matrix row[3, `col'] = e(N)

	local col = `col' + 1
}
matrix results = results \ row
}
preserve
clear
svmat results
drop if _n==1
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA5_TaxiB") sheetreplace	
restore

******************************************************************************
* Table A6: other outcomes

matrix results = J(1, 6, .)
foreach o of local outcomes_T6 {
matrix row = J(3, 6, .)
local col = 1

foreach bw in all high low  {
	// control mean
	sum `o' if `bw' & treated==0
	matrix row[1, `col'] = r(mean)
	local col = `col' + 1
	
	// treatment effect
	ivregress 2sls `o' (taxi_optin = taxi1_treatment1 taxi1_treatment2 taxi2_treatment1 taxi2_treatment2) ///
			i.id i.strata if `bw', vce(cluster driver_uuid)
	matrix row[1, `col'] = _b[taxi_optin]
	matrix row[2, `col'] = _se[taxi_optin]
	matrix row[3, `col'] = e(N)

	local col = `col' + 1
}
matrix results = results \ row
}

// export matrix to spreadsheet
clear
svmat results
drop if _n==1
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA6_Taxi") sheetreplace	
