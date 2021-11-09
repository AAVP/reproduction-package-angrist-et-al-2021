* table 3: looks at the characteristics of those that opt in to free week
********************************************************************************
* Load data
use "${`c(username)'_data}/laborsupply", clear

* Generate variables
gen farebox_1 = (hours_1 * earnings_1) / (1-commission/100)
gen avg_farebox_4 = (avg_hours_4 * avg_earnings_4) / (1-commission/100)
********************************************************************************
* Table parameters and covariates
#delimit ;
local covariates
	female
	age
	commission
	vehicle_solutions
	vehicle_year
	months_since_signup
	hours_worked_0822
	gross_total_0822
	avg_hours_4
	avg_earnings_4
	avg_farebox_4
	;
#delimit cr

// table layout parameters
local n_stats 	= 2

********************************************************************************
* Make tables
// initialize top row and sample size matrix
matrix results = J(26, 6, 0)
matrix N = J(1, 6, 0)
matrix stats = J(`n_stats', 6, 0)

local col = 1

foreach bw in all high low { 	// Loop over high and low bandwidth
		// loop over covariates
		local row = 1
		foreach cov of local covariates {
			di in red "looking at `cov'"
			// Look at it overall
			
			sum `cov' if `bw' & optin==0	// didn't opt in to free week
			matrix results[`row', `col'] = r(mean)
			matrix results[`row'+1, `col'] = r(sd)
			matrix N[1, `col'] = r(N) 	
			
			//  test whether significant
			regress `cov' optin if `bw', vce(robust) 
			
			// Save coefficient and standard error from optin variable
			matrix results[`row', `col'+1] = _b[optin]
			matrix results[`row'+1, `col'+1] = _se[optin]

			// increment row
			local row = `row' + 2
			
		} // end covariate loop
	
		count if `bw' & !optin
		matrix N[1, `col'] = r(N)
		count if `bw'
		matrix N[1, `col'+1] = r(N)
		
		local col = `col' + 2
}		
// append stats to means
preserve
matrix results = results \ N	
clear
svmat results
matlist results
export excel using "${`c(username)'_tables}/tables_raw_deck.xls", sheet("Table3") sheetreplace
restore
