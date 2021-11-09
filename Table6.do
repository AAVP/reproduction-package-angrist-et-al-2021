* Table 6: calculate benefits for drivers
******************************************************************************
use "${`c(username)'_data}/laborsupply", clear

// Taxi lease costs
local cost_high_1	110
local cost_low_1	45
local cost_high_2	165
local cost_low_2	75

cap drop fees*
gen commission_scaled = commission/100

******************************************************************************

// create adjusted labor supply, h1 = h0(1+epsilon * % change wage)
foreach x in 0912 0919 {
gen gross_total_`x'_adj = gross_total_`x' if taxi1_control == 1
// taxi 1 treatment was 0 fee (just 0 commission)
replace gross_total_`x'_adj = gross_total_`x'*(1+1.2*commission_scaled/(1-commission_scaled)) if taxi1_treatment1==1
// taxi 2 treatment was -.125 fee
replace gross_total_`x'_adj = gross_total_`x'*(1+1.2*(commission_scaled+.125)/(1-commission_scaled)) if taxi1_treatment2==1
}
******************************************************************************

// calculate 
foreach x in 0912 0912_adj 0919 0919_adj {

	gen fees_`x' = gross_total_`x'*commission_scaled									// how much they would have saved in fees
	replace fees_`x'  = gross_total_`x'*(commission_scaled+.125) if taxi1_treatment2==1 // same for taxi 2-- bonus
	replace fees_`x' = . if taxi1_control==1 											// control guys couldn't opt in 
	replace fees_`x' = fees_`x' + l1 													// take out lease costs (which are negative)
	
	gen benefit_`x' = (fees_`x' > 0) if !missing(fees_`x') 								// benefit if came out ahead
}

******************************************************************************

// create adjusted labor supply
foreach x in 1010 1017 {
gen gross_total_`x'_adj = gross_total_`x' if taxi2_control == 1
replace gross_total_`x'_adj = gross_total_`x'*(1+1.2*commission_scaled/(1-commission_scaled)) if taxi2_treatment1==1
replace gross_total_`x'_adj = gross_total_`x'*(1+1.2*0.5*commission_scaled/(1-commission_scaled)) if taxi2_treatment2==1
}
******************************************************************************

// calculate 
foreach x in 1010 1010_adj 1017 1017_adj{

	gen fees_`x' = gross_total_`x'*commission_scaled
	replace fees_`x'  = fees_`x'/2 if taxi2_treatment2==1
	replace fees_`x' = . if taxi2_control==1
	replace fees_`x' = fees_`x' + l2
	
	gen benefit_`x' = (fees_`x' > 0) if !missing(fees_`x')
}

******************************************************************************

// Taxi 1
foreach x in 0912 0919 {
	sum taxi1_optin if benefit_`x'==1 & !taxi1_control
	assert !missing(hours_worked_`x')
	gen drive_`x' = (hours_worked_`x'>0)	// positive hours
}

foreach x in 1010 1017 {
	sum taxi2_optin if benefit_`x'==1 & !taxi2_control
	assert !missing(hours_worked_`x')
	gen drive_`x' = (hours_worked_`x'>0)	// positive hours
}
keep benefit* taxi?_optin taxi?_control driver_uuid fees* drive_*


// Rename variables
foreach x in benefit fees drive {
	rename `x'_0912 `x'_exante_1	// taxi 1: based on opt in week
	rename `x'_0919 `x'_expost_1	// taxi 1: based on live week
	rename `x'_1010 `x'_exante_2	// taxi 2: based on opt in week
	rename `x'_1017 `x'_expost_2	// taxi 2: based on live week
}

foreach x in fees benefit {
	rename `x'_0912_adj `x'_adj_exante_1
	rename `x'_1010_adj `x'_adj_exante_2
	rename `x'_0919_adj `x'_adj_expost_1
	rename `x'_1017_adj `x'_adj_expost_2
}

rename taxi?_optin optin_?
rename taxi?_control control_?

// Reshape long
keep *_adj_* benefit_exante_* benefit_expost_* optin_* control_* fees_* driver_uuid drive_exante_* drive_expost_*
reshape long fees_adj_exante_ fees_adj_expost_ benefit_adj_exante_ benefit_adj_expost_ drive_exante_ drive_expost_ benefit_exante_ benefit_expost_ optin_ control_ fees_exante_ fees_expost_, i(driver_uuid) j(week)

// Drop control drivers who never had a choice to make
keep if !control_

// How many opted in if they benefitted ex ante or ex post
sum optin_ if benefit_exante_
sum optin_ if benefit_expost_

sum benefit_expost_ if optin_
sum fees_expost_ if !benefit_expost_ & optin_

******************************************************************************

gen drive_adj_exante_ = drive_exante_
gen drive_adj_expost_ = drive_expost_

matrix results = J(8, 4, .)
local i = 1

foreach x in adj_exante expost adj_expost {
forval optin=1(-1)0 {
	
	if ("`x'"=="expost" & `optin'==0) | ("`x'"=="adj_expost" & `optin'==1) {
		continue
	}
** ALL DRIVERS

	// mean benefit
	sum fees_`x'_ if optin==`optin'
	matrix results[1, `i'] = r(mean)
	
	// percent that benefitted
	sum benefit_`x'_ if optin==`optin'
	matrix results[2, `i'] = r(mean)
	
	count if optin==`optin'
	matrix results[3, `i'] = r(N)

	
** DROVE
	// mean benefit conditional on *actually driving* during live week
	sum fees_`x'_ if optin==`optin' & drive_expost_==1
	matrix results[4, `i'] = r(mean)
	
	// percent that benefitted
	sum benefit_`x'_ if optin==`optin' & drive_expost_==1
	matrix results[5, `i'] = r(mean)
	
	count if optin==`optin' & drive_expost_==1
	matrix results[6, `i'] = r(N)
	
	local i = `i' + 1

}
}
matlist results

clear
svmat results
export excel using "${`c(username)'_tables}/tables_raw_deck.xls", sheet(Table6) sheetreplace
