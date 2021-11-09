* Figure 9
*******************************************************************************

// statistical parameters
set seed 16
local significance	.05
local ise			${ise}
local breps			500

// graph parameters
local s_color	eltblue
local ci_color	gs8
local fit_color	eltblue
local ff_color	black
local title_size	small
local xtitle	"Log Opt-In Earnings"
local ytitle	"Control Earnings Quantile"
local axis_size		small

*******************************************************************************

use * if sample_2==1 using "${`c(username)'_data_generated}/generated_participation", clear
	// use the optin week data
	assert _N==2061	// should have 2 weeks of data

keep if positive // keep only those who drive

// find the treatment group opt-in rates
gen optin_1 = .
gen optin_2 = .
gen q_1 = . 
gen q_2 = .
gen size_1 = .
gen size_2 = .

	forval week=1/2 {				// loop over weeks
	foreach commission in 20 25 {	// loop over commissions -- determine treatment
	foreach bw in high low {		// loop over bandwidth -- determines treatment
	forval treatment=1/2 {			// loop over treatment groups

		// calculate the number of people in a treatment, bandwidth, commission, week group that opted in
		sum participate if taxi`week'_treatment`treatment'==1 & `bw' & commission==`commission' & taxi`week'==1
		local pct = 100 - round(r(mean)*100)
		replace optin_`treatment' = `pct' if `bw' & commission==`commission' & taxi`week'==1

		// find the quantile of the log earnings distribution (adjusted based on ISE) this corresponds to
		_pctile log_earnings_adj if taxi`week' & `bw' & commission==`commission', p(`pct')
		replace q_`treatment' = r(r1) if commission==`commission' & taxi`week'==1 & `bw'
		
		// save the sample size
		count if commission==`commission' & taxi`week'==1 & `bw' & taxi`week'_treatment`treatment'==1
		replace size_`treatment' = r(N) if commission==`commission' & taxi`week'==1 & `bw'
	}	// end loop over treatments
	}	// end loop over bandwidths
	}	// end loop over commissions
	}	// end loop over weeks
	
*******************************************************************************
// find the relevant quantiles to look for
preserve
	keep optin_1 optin_2
	gen id = _n
	reshape long optin_, i(id) j(week)
	drop if missing(optin)
	duplicates drop
	levelsof optin, local(quantiles)
	di in red "`quantiles'"
restore

*******************************************************************************
// quantile regression to get the levels
keep if control
sqreg log_earnings d_1 d_2 d_3 d_4 d_5 d_6 d_7, quantiles(`quantiles') reps(`breps')

gen prediction_1 = 0
gen prediction_2 = 0
gen se_1 = 0
gen se_2 = 0

// save the results
foreach q of local quantiles {
	cap di _b[q`q':_cons]
	if _rc!=0 {
		local p = `q'*10
	}
	else {
		local p = `q'
	}
	
	replace prediction_1 = _b[q`p':_cons] if optin_1 == `q'
	replace se_1 = _se[q`p':_cons] if optin_1 == `q'
	
	replace prediction_2 = _b[q`p':_cons] if optin_2 == `q'
	replace se_2 = _se[q`p':_cons] if optin_2==`q'
	
	forval i=1/7 {
		lincom _b[q`p':d_`i'] + _b[q`p':_cons]
		replace prediction_1 = r(estimate) if d_`i'==1 & optin_1 == `q'
		replace se_1 = r(se) if d_`i'==1 & optin_1==`q'
		
		replace prediction_2 = r(estimate) if d_`i'==1 & optin_2 == `q'
		replace se_2 = r(se) if d_`i'==1 & optin_2==`q'
	}

}

keep high commission prediction_1 prediction_2 elastic_1 elastic_2 optin_1 ///
	optin_2 q_1 q_2 se_1 se_2 taxi1 taxi2 L_t_1 L_t_2 size_?
duplicates drop
reshape long prediction_ elastic_ optin_ q_ se_ L_t_ size_, i(high commission taxi1 taxi2) j(treatment)
rename *_ *

*******************************************************************************
// GRAPHS
gen lower_ci = prediction - invttail(size, `significance'/2)*se
gen upper_ci = prediction + invttail(size, `significance'/2)*se
gen log_L_t = log(L_t)

// get the coefficients for displaying on the graph
regress prediction log_L_t [fw=size], r
	
	local b 		=round(100*_b[log_L_t]/100, .01)
	local int 		=round(_b[_cons], .01)
	
* Graph q versus theta
#delimit;
graph twoway
	(scatter prediction log_L_t, color(`s_color')) 
	(rcap lower_ci upper_ci log_L_t, color(`ci_color'))
	(lfit prediction log_L_t [fw=size], lcolor(`fit_color'))
	(line log_L_t log_L_t, lcolor(`ff_color')) /* forty five degree line */
	,
	xtitle(`xtitle', size(`axis_size')) xlabel(4.5(.5)6.5, labsize(small)) ylabel(4.5(.5)6.5, labsize(small))
	ytitle(`ytitle', size(`axis_size'))
	legend(order(1 "Estimated Quantiles" 2 "95% Confidence Interval" 3 "Regression Fit" 4 "45 Degree Line") size(small) rows(2))
	graphregion(color(white)) bgcolor(white)
	text(5.4 6 "Slope: `b'", size(small))
	text(5.1 6 "Intercept: `int'", size(small))
	;
	gr export "${`c(username)'_graphs}/quantile.pdf", as(pdf) replace;
#delimit cr
