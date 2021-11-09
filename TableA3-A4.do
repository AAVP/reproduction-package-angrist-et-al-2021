* Appendix Tables A3 & A4: covariate balance for taxi 1 and taxi 2
********************************************************************************

use * if optin==1 using "${`c(username)'_data}/laborsupply", clear
	// only individuals who accepted free week were included in this phase
	count
	assert _N==1031

********************************************************************************
#delimit ;
/* Balance characteristics for Taxi 1 */
local covariates_1
	female
	hours_4
	avg_hours_4
	earnings_4
	avg_earnings_4
	months_since_signup
	vehicle_solutions
	gross_total_0822
	hours_worked_0822
	old
	verynew
	;
/* Balance characteristics for Taxi 2 */
local covariates_2
	female
	hours_4
	avg_hours_4
	earnings_4
	avg_earnings_4
	months_since_signup
	vehicle_solutions
	gross_total_1003
	hours_worked_1003
	old
	verynew
	taxi1_treated
	;
#delimit cr
********************************************************************************
local date_taxi1	0822		// a non-treatment week we include for taxi 1
	// note: we had to use a long lag because of the short time window between
	// taxi 1 and free week 2
local date_taxi2	1003		// the week before taxi 2
gen verynew = vehicle_year>=2011

gen taxi1_treated = 1 - taxi1_control // not in the taxi 1 control group
********************************************************************************

// table layout parameters
local n_cols 	= 5
local n_stats 	= 2
label define groups 0 "control" 1 "t1" 2 "t2"

* Loop over the two weeks of taxi to create the balance tables
forval taxi = 1/2 {

	cap drop treatment1 treatment2 group
	
	* generate indicators for each of the two taxi treatments in a given week
	gen treatment1 = (taxi`taxi'_treatment1 ==1)
	gen treatment2 = (taxi`taxi'_treatment2 ==1)
	
	* generate an indicator for what group someone is in
		// use this to streamline subsetting below
	gen group = 0 if taxi`taxi'_control==1
	replace group = 1 if treatment1==1
	replace group = 2 if treatment2==1
		label values group groups
		
	// initialize top row and sample size matrix
	matrix results = J(1, `n_cols', 0)
	matrix N = J(1, `n_cols', 0)
	
	// loop over the week-specific covariate list
		// lists are identical except that farebox & hours weeks differ
	foreach cov of local covariates_`taxi' {
		di in red "looking at `cov'"

		// initialize blank row
		matrix row = J(2, `n_cols', 0)
		
		// make sure the covariate is never missing
		assert !missing(`cov')

		// get unadjusted covariate mean for control & 2 treatment groups
		local col = 1
		forval z=0(1)2 {
			sum `cov' if group==`z'
			matrix row[1,`col'] = r(mean)
			matrix row[2,`col'] = r(sd)
			matrix N[1, `col'] = r(N)
			local col = `col' + 1
		}
		
		// for each of the two treatments:
		forval t=1/2 {
			
			//  test whether covariate significantly different from control
			regress `cov' treatment`t' i.strata if (group==0 | group==`t'), vce(robust) 
				
			// Save coefficient and standard error from treatment variable
			matrix row[1, `col'+`t'-1] = _b[treatment`t']
			matrix row[2, `col'+`t'-1] = _se[treatment`t']
			test treatment`t'
			local p = r(p)

		} 
				
		// append row to base matrix
		matrix results = results \ row
		} // end covariate loop
	
	matrix stats = J(`n_stats', `n_cols', 0)
	
	* F statistic for test using all covariates
	forvalues t=1/2 {
		// joint F test and sample size
		mvreg `covariates_`taxi'' = treatment`t' i.strata if (group==0 | group==`t')
		noisily test treatment`t'
	
		// save significance tests
		local allCov_F = r(p)
		matrix stats[1, `col'+`t'-1] = r(F)
		matrix stats[2, `col'+`t'-1] = r(p)
		count if (group==0 | group==`t')
		matrix N[1, `col'+`t'-1] = r(N)
		
	}
	
	// append stats to means
	preserve
		matrix results = results \ stats \ N	
		clear
		svmat results
		matlist results
		drop if _n == 1
	
	local tablenum =2+`taxi'
	export excel using "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA`tablenum'") sheetreplace
	restore
	
} // end loop over bandwidths

