* 2SLS graphs for Free weeks
******************************************************************************

local covariates ${covariates_stack}
local significance .05

******************************************************************************
* Create the taxi stack
use "${`c(username)'_data}/laborsupply", clear
keep driver_uuid female vehicle_solutions months_since_signup old  ///
	gross_total_* hours_* taxi?_treatment? wave1 optin new* old high strata_eligible

rename (gross_total_0* hours_worked_0*) (gross_total_* hours_worked_*)
reshape long gross_total_ hours_worked_, i(driver_uuid) j(date)
rename *_ *

// generate the correct date
bysort driver_uuid (date): gen week = _n
tostring date, replace
replace date = "0" + date if strlen(date)==3
replace date = substr(date, 1, 2) + "/" + substr(date, 3, 2)

// set panel structure
encode driver_uuid, gen(id)
xtset id week

// indicator for weeks there was a treatment
gen treatmentweek = inlist(date, "08/29", "09/05", "09/26", "10/17")

// generate 1 lag of log earnings (from a non-treatment week)
gen log_earnings = log(gross_total)
gen log_hours = log(hours_worked)
gen lag_log = L.log_earnings
replace lag_log = L2.log_earnings if L.treatmentweek==1
replace lag_log = L3.log_earnings if L2.treatmentweek==1
replace lag_log = L4.log_earnings if L3.treatmentweek==1
replace lag_log = 1 if week==1		// start of the data

// make sure treatment is only turned on in the relevant weeks
gen have_earnings = gross_total>0

// make sure have correct endogenous variable
replace optin = optin * wave1 if week==1 | week==2
replace optin = optin * (1-wave1) if week==3 | week == 4

// gen treated
gen treated = wave1 if week==1 | week==2
replace treated = 1-wave1 if week==3 | week ==4
******************************************************************************
******************************************************************************
* Save treatment effects
matrix hours = J(6, 5, .)
matrix earnings = J(6, 5, .)
matrix particip = J(6, 5, .)
local col = 1
keep if week<=4

levelsof week, local(weeks)

local row = 1
foreach week of local weeks {

// hours  
  ivregress 2sls log_hours  (optin= treated) i.strata_eligible `covariates'  ///
	if week==`week', r
		matrix hours[`row', `col'] = _b[optin]
		matrix hours[`row', `col'+1] = _se[optin]
		matrix hours[`row', `col'+2] = e(N)

// earnings
  ivregress 2sls log_earnings (optin= treated) i.strata_eligible `covariates'  ///
	if week==`week', r
		matrix earnings[`row', `col'] = _b[optin]
		matrix earnings[`row', `col'+1] = _se[optin]
		matrix earnings[`row', `col'+2] = e(N)

// participation
  ivregress 2sls have_earnings (optin= treated) i.strata_eligible `covariates'  ///
	if week==`week', r
		matrix particip[`row', `col'] = _b[optin]
		matrix particip[`row', `col'+1] = _se[optin]
		matrix particip[`row', `col'+2] = e(N)
	
	local row = `row' + 1
} // end loop over weeks


******************************************************************************
* Combine into one dataset
clear
foreach x in hours earnings particip {
	clear
	svmat `x'
	
	rename (`x'1 `x'2 `x'3) ///
		(b se N)
	
	gen ci_lower = b - invttail(N, `significance'/2)*se
	gen ci_upper = b + invttail(N, `significance'/2)*se

	drop if missing(b)
	foreach y in b se N ci_lower ci_upper {
		rename `y' `y'_`x'
	}

	gen week = _n
	tempfile `x'
	save ``x'', replace
}

* merge in the results from all of the outcomes
use `hours', clear
merge 1:1 week using `particip', nogen
merge 1:1 week using `earnings', nogen

* create specific week variables so that it is staggered
gen week_particip = week + .1
gen week_earnings = week + .05
gen zero  = 0
******************************************************************************

* Create graphs
#delimit;
	graph twoway (line b_hours week if week<=2, lcolor(navy)) (rcap ci_lower_hours ci_upper_hours week if week<=2, lcolor(gs2)) 
	 (line b_earnings week_earnings if week<=2, lcolor(ebblue)) (rcap ci_lower_hours ci_upper_earnings week_earnings if week<=2, lcolor(gs7)) 
	(line b_particip week_particip if week<=2, lcolor(eltblue)) (rcap ci_lower_particip ci_upper_particip week_particip if week<=2, lcolor(gs13)) 

	(line b_hours week if week==3 | week==2, lcolor(navy) lpattern(dash)) (rcap ci_lower_hours ci_upper_hours week if week>2, lcolor(gs2)) 
	 (line b_earnings week_earnings if week==3 | week==2, lcolor(ebblue) lpattern(dash)) (rcap ci_lower_hours ci_upper_earnings week_earnings if week>2, lcolor(gs7)) 
	(line b_particip week_particip if week==3 | week==2, lcolor(eltblue) lpattern(dash)) (rcap ci_lower_particip ci_upper_particip week_particip if week>2, lcolor(gs13)) 

	(line b_hours week if week>=3, lcolor(navy) ) (rcap ci_lower_hours ci_upper_hours week if week>2, lcolor(gs2)) 
	 (line b_earnings week_earnings if week>=3, lcolor(ebblue) ) (rcap ci_lower_hours ci_upper_earnings week_earnings if week>2, lcolor(gs7)) 
	(line b_particip week_particip if week>=3, lcolor(eltblue) ) (rcap ci_lower_particip ci_upper_particip week_particip if week>2, lcolor(gs13)) 

	
	,
		xlabel(1 "Aug 22"
		2 `" "Aug 29" "Wave 1" "Treated" "'
		3 `" "Sept 5" "Wave 2"  "Treated"  "' 
		4 "Sept 11" )
	yline(0, lcolor(red) lwidth(vthin))
	graphregion(color(white)) bgcolor(white)
	xtitle("") ytitle("Treatment Effect")
	legend(order(1 "Hours" 3 "Farebox" 5 "Active") rows(1))
;
#delimit cr
gr export "${`c(username)'_graphs}/tsls_byweek_fw.pdf", as(pdf) replace
