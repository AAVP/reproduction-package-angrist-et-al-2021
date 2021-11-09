* 2SLS graphs for Taxi weeks
******************************************************************************

local covariates ${covariates_stack}
local significance .05

******************************************************************************
* Create the taxi stack
use * if optin==1 using "${`c(username)'_data}/laborsupply", clear
keep driver_uuid female vehicle_solutions months_since_signup old  ///
	gross_total_* hours_* taxi?_control optin new* old high exp inexp taxi?_optin strata

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
gen treatmentweek = inlist(date, "08/29", "09/05", "09/19", "10/17")

// event time for each of the treatments
gen taxi1_eventtime = week - 5
gen taxi2_eventtime = week - 9

// outcomes
gen log_earnings = log(gross_total)
gen log_hours = log(hours_worked)
gen lag_log = L.log_earnings
replace lag_log = L2.log_earnings if L.treatmentweek==1
replace lag_log = L3.log_earnings if L2.treatmentweek==1
replace lag_log = L4.log_earnings if L3.treatmentweek==1
replace lag_log = 1 if week==1		// start of the data

// make sure treatment is only turned on in the relevant weeks
gen taxi1 = (taxi1_eventtime>=-2 & taxi1_eventtime <=2)
gen taxi2 = (taxi2_eventtime>=-2 & taxi2_eventtime <=2)

gen taxi1_treatment = 1 - taxi1_control
gen taxi2_treatment = 1 - taxi2_control

gen have_earnings = gross_total>0

******************************************************************************
******************************************************************************
* Save treatment effects
matrix hours = J(6, 6, .)
matrix earnings = J(6, 6, .)
matrix particip = J(6, 6, .)
local col = 1
forval taxi=1/2 {
local row = 1

forval week=-2/2 {

// hours  
ivregress 2sls log_hours  (taxi`taxi'_optin= taxi`taxi'_treatment) i.strata `covariates'  ///
	if taxi`taxi' & taxi`taxi'_eventtime==`week', r
		matrix hours[`row', `col'] = _b[taxi`taxi'_optin]
		matrix hours[`row', `col'+1] = _se[taxi`taxi'_optin]
		matrix hours[`row', `col'+2] = e(N)

// earnings
ivregress 2sls log_earnings (taxi`taxi'_optin= taxi`taxi'_treatment) i.strata `covariates'  ///
	if taxi`taxi' & taxi`taxi'_eventtime==`week', r
		matrix earnings[`row', `col'] = _b[taxi`taxi'_optin]
		matrix earnings[`row', `col'+1] = _se[taxi`taxi'_optin]
		matrix earnings[`row', `col'+2] = e(N)

// participation
ivregress 2sls have_earnings (taxi`taxi'_optin= taxi`taxi'_treatment) i.strata `covariates'  ///
	if taxi`taxi' & taxi`taxi'_eventtime==`week', r
	
		matrix particip[`row', `col'] = _b[taxi`taxi'_optin]
		matrix particip[`row', `col'+1] = _se[taxi`taxi'_optin]
		matrix particip[`row', `col'+2] = e(N)
	
	local row = `row' + 1
} // end loop over weeks
local col = `col' + 3
} // end loop over taxi weeks

******************************************************************************
* Combine into one dataset
clear
foreach x in hours earnings particip {
	clear
	svmat `x'
	
	rename (`x'1 `x'2 `x'3 `x'4 `x'5 `x'6) ///
		(b_taxi1 se_taxi1 N_taxi1 b_taxi2 se_taxi2 N_taxi2)
	drop if missing(b_taxi1)
	gen week = _n - 3
	reshape long b_taxi se_taxi N_taxi, i(week) j(group)
	rename *_taxi *
	
	gen ci_lower = b - invttail(N, `significance'/2)*se
	gen ci_upper = b + invttail(N, `significance'/2)*se

	foreach y in b se N ci_lower ci_upper {
		rename `y' `y'_`x'
	}

	tempfile `x'
	save ``x'', replace
}

use `hours', clear
merge 1:1 week group using `particip', nogen
merge 1:1 week group using `earnings', nogen

gen week_particip = week + .1
gen week_earnings = week + .05
gen zero = 0
******************************************************************************

* Create graphs
forval g=1/2 {
	#delimit;
	graph twoway (line b_hours week if group==`g', lcolor(navy)) (rcap ci_lower_hours ci_upper_hours week if group==`g', lcolor(gs2)) 
	(line b_earnings week_earnings if group==`g', lcolor(ebblue)) (rcap ci_lower_hours ci_upper_earnings week_earnings if group==`g', lcolor(gs7))
	(line b_particip week_particip if group==`g', lcolor(eltblue)) (rcap ci_lower_particip ci_upper_particip week_particip if group==`g', lcolor(gs13)) 
	
	,
	graphregion(color(white)) bgcolor(white)
	xtitle("Relative to treatment week; Taxi `g'") ytitle("Treatment Effect")
	legend(order(1 "Hours" 3 "Farebox" 5 "Active") size(small) rows(1))
	name(taxi`g', replace)
	yline(0, lcolor(red) lwidth(vthin))
	ylabel(-.4(.2).7, nogrid) xlabel(-2(1)2)
	
;
#delimit cr

gr export "${`c(username)'_graphs}/tsls_byweek_taxi_`g'.pdf", as(pdf) replace
}

#delimit ;
grc1leg taxi1 taxi2,
 xcommon
	graphregion(color(white))
	rows(2)

;
gr export "${`c(username)'_graphs}/tsls_byweek_taxi_combined.pdf", as(pdf) replace;
#delimit cr

