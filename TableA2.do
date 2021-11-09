* Covariate Balance for Wave 1 and Wave 2
************************************************************************************************************************************
use "${`c(username)'_data}/laborsupply", clear

* List of covariates to present in balance table
local covariates female hours_1 avg_hours_4 earnings_1 avg_earnings_4 months_since_signup vehicle_solutions

// initialize top row and sample size matrix
matrix results = J(1, 2, 0)
matrix N = J(1, 2, 0)

// loop over covariates
foreach cov of local covariates {
	di in red "looking at `cov'"
			
	// initialize blank row
	matrix row = J(2,2, 0)
	
	// covariates should never be missing
	assert !missing(`cov')

	// get unadjusted covariate mean for treated and control
	sum `cov' if wave1==1
	matrix row[1, 1] = r(mean)
		
	//  test whether significant, controls for strata dummies
	regress `cov' wave2 i.strata_eligible, vce(robust) 
				
	// Save coefficient and standard error from "type" variable
	matrix row[1, 2] = _b[wave2]
	matrix row[2, 2] = _se[wave2]
			 
	// append row to base matrix
	matrix results = results \ row
} // end covariate loop
	
	matrix stats = J(2, 2, 0)
	
// joint F test and sample size
mvreg `covariates' = wave2 i.strata_eligible
noisily test wave2
	
// save significance tests
local allCov_F = r(p)
matrix stats[1, 2] = r(F)
matrix stats[2, 2] = r(p)
	
count if wave1
matrix N[1, 1] = r(N)
count
matrix N[1, 2] = r(N)


// append stats to means
matrix results = results \ stats \ N	
clear
svmat results
matlist results
drop if _n == 1 // drop the blank initialization row

************************************************************************************************************************************
* export and save results
export excel using "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA2") sheetreplace
************************************************************************************************************************************
