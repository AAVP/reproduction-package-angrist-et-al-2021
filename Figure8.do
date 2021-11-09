* Figure 8
// use data & restrict sample
use * if optin_week==1 using "${`c(username)'_data_generated}/generated_participation", clear

* keep people who actually worked
keep if !missing(log_earnings)

*************************************************************************

// find the treatment group opt-in rates
forval i = 1/2 {
	gen optin_`i' = .
	gen quantile_`i' = .
	gen count_`i' = .
}

* calculate the opt-in rates  for each of the taxi treatments
forval week=1/2 {				// loop over weeks
foreach commission in 20 25 {	// loop over commissions -- determine treatment
foreach bw in high low {		// loop over bandwidth -- determines treatment
forval treatment=1/2 {			// loop over treatment groups

	sum taxi`week'_optin if taxi`week'==1 & ///
		taxi`week'_treatment`treatment'==1 & ///
		`bw' & commission==`commission'
		
	* opt-in rate (multiplied by 100)
	local pct = r(mean)*100
	replace optin_`treatment' = `pct' if `bw' & commission==`commission' & taxi`week'==1
	
	* number of people in that treatment group & week
	count if taxi`week'_treatment`treatment'==1 & `bw' & commission==`commission' & taxi`week'==1
	replace count_`treatment' = r(N) if taxi`week'_treatment`treatment'==1 & `bw' & commission==`commission' & taxi`week'==1
	
	* find the break-even associated with that treatment
	cap drop above 
	sum L_t_`treatment' if `bw' & commission==`commission' & taxi`week'==1 & taxi`week'_control==1
		
	* calculate the fraction of people who would have opted-in using the treatment group's opt-in week earnings
	gen above = (log_earnings >= log(L_t_`treatment'*(1+.5*${ise}*t_`treatment'/(1-commission/100))^-1)) * 100
	replace above = 0 if missing(log_earnings)
	sum above if `bw' & commission==`commission' & taxi`week'==1 & taxi`week'_treatment`treatment'==1
	replace quantile_`treatment' = r(mean) if `bw' & commission==`commission' & taxi`week'==1

}	// end loop over treatments
}	// end loop over bandwidths
}	// end loop over commissions
}	// end loop over weeks
	
* Change the dataset so there is now one observation for each group
keep optin_? quantile_? L_t_? high commission count_?
gen id = _n
duplicates drop
reshape long optin_ quantile_ L_t_ count_, i(high commission id) j(treatment)
rename *_ *
drop id
duplicates drop

*************************************************************************
* regress opt-in rates on quantiles, weighting by the number of people in each group
reg optin quantile [fw=count]
*************************************************************************

// Graph of predicted versus observed opt-in rates
graph twoway (scatter optin quantile if commission==20 & high==1, msymbol(circle) color(cranberry)) ///
	(scatter optin quantile if commission==25 & high==1, msymbol(circle) color(navy)) ///
	(scatter optin quantile if commission==20 & high==0, msymbol(diamond) color(cranberry)) ///
	(scatter optin quantile if commission==25 & high==0, msymbol(diamond) color(navy)) ///
	(function y=x, range(50 90) color(black) lpattern(dash)) ///
	(lfit optin quantile [fw=count], range(50 90) color(ebblue)), ///
	legend(order(5 "Predicted = Observed" 6 "Linear Fit"))	xtitle("Predicted [q{sub:0}(L,t)]") ///
	ytitle("Observed [p(L,t)]") ///
	graphregion(color(white)) bgcolor(white) ///
	xlabel(50(10)90) ylabel(20(10)90)
gr export "${`c(username)'_graphs}/optin.pdf", as(pdf) replace
