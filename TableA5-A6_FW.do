* Tables A5 & A6 -- participation 2SLS for regular & other outcomes
******************************************************************************
use "${`c(username)'_data}/laborsupply", clear
assert _N==1600

******************************************************************************
local outcomes_T5 have_earnings log_hours log
local outcomes_T6 completed_trips num_days hourly_farebox prop_surged avg_rating prop_rated 
local full_outcomes `outcomes_T5' `outcomes_T6'

******************************************************************************	
* Create stack: 2 observationsn per driver (one for each of the two free weeks)
expand 2
bys driver_uuid: gen id = _n

* Create labor supply outcomes that are not in laborsupply.dta
foreach t in 0905 0829 {
	gen have_earnings_`t' = (gross_total_`t' > 0)
		label var have_earnings_`t' "Positive Earnings in Week beginning `t'"
		
	* Outcome for Table A6: hourly earnings
	gen hourly_farebox_`t' = gross_total_`t' / hours_worked_`t'
		label var hourly_farebox_`t' "Avg Hourly Farebox Week Beginning `t'"
}
******************************************************************************
* Endogenous variable is an indicator for whether they received the 
	// treatment in a given week (offered and accept)
gen took_treatment = optin * wave1 if id==1
replace took_treatment = optin * (1-wave1) if id==2

* Use 2 instruments: indicators for treatment in a given week
replace wave1 = 0 if id==2	// indicator for week 1
replace wave2 = 0 if id==1	// indicator for week 2

* Outcomes are for 08/29 in week 1, and 09/05 in week 2
foreach o of local full_outcomes {
	gen `o' = `o'_0829 if id==1
	replace `o' = `o'_0905 if id ==2
}

// Replace if missing
replace avg_rating = . if avg_rating==0 | completed_trips==0 // needed to fill in stars 1-5
replace prop_surged = . if completed_trips==0
replace prop_rated = . if avg_rating==. | completed_trips ==0

cap drop treated
gen treated = wave1 + wave2
******************************************************************************
* Table A5

* Panel A: Strata Only
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
	ivregress 2sls `o' (took_treatment = wave1 wave2) ///
			i.id i.strata_eligible if `bw', vce(cluster driver_uuid)
	matrix row[1, `col'] = _b[took_treatment]
	matrix row[2, `col'] = _se[took_treatment]
	matrix row[3, `col'] = e(N)

	local col = `col' + 1
}
matrix results = results \ row
}

// export matrix to spreadsheet
preserve
clear
svmat results
drop if _n==1
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA5_FWA") sheetreplace	
restore

* Panel B: Strata + Covariates
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
	ivregress 2sls `o' (took_treatment = wave1 wave2) ///
			i.id i.strata_eligible ${covariates_freeweek} if `bw', vce(cluster driver_uuid)
	matrix row[1, `col'] = _b[took_treatment]
	matrix row[2, `col'] = _se[took_treatment]
	matrix row[3, `col'] = e(N)

	local col = `col' + 1
}
matrix results = results \ row
}

// export matrix to spreadsheet
preserve
clear
svmat results
drop if _n==1
matlist results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA5_FWB") sheetreplace	
restore

******************************************************************************
// Table A6: other outcomes
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
	ivregress 2sls `o' (took_treatment = wave1 wave2) ///
			i.id i.strata_eligible if `bw', vce(cluster driver_uuid)
	matrix row[1, `col'] = _b[took_treatment]
	matrix row[2, `col'] = _se[took_treatment]
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
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA6_FW") sheetreplace	
