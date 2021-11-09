*  Table 8: CV (includes code for callouts in the table note)
*******************************************************************************
#delimit ;
local taxrates
	.15
	.2
	.25
	.5
	;
local leases
	50
	100
	150
	200
	400
	600
	800
	;
local wage 22; 	/* mean wage 	*/
local ise 1.2; 	/* 2SLS estimate*/
local k 1.4;	/* kappa */
#delimit cr

*******************************************************************************

* Calculate CV using the eligible population
use "${`c(username)'_data}/eligible", clear
	* earnings_ variables in this dataset mean HOURLY earnings that week
	* hours_ variables in this dataset mean hours worked that week

* keep relevant variables and reshape to long format (one obs per driver-week)
keep driver_uuid hours_? earnings_? commission
reshape long hours_ earnings_, i(driver_uuid commission) j(week)
rename *_ *

* set the time series
encode driver_uuid, gen(id)
xtset id week
tsfill

// check that strongly balanced (id was used to fill, so driver_uuid would be missing)
assert !missing(driver_uuid)

// replace earnings and hours with 0's as necessary
replace earnings = 0 if missing(earnings)
replace hours = 0 if missing(hours)

// generate mean farebox over drivers
bysort driver_uuid: egen avg_earnings = mean(earnings)
bysort driver_uuid: egen avg_hours = mean(hours)
gen farebox = avg_earnings * avg_hours /(1-commission/100)
sum farebox

// make sure hours are 0 for guys who didn't make anything
gen gross_total = earnings * hours / (1-commission/100)
	label var gross_total "Weekly Fares Collected"
gen payout = earnings * hours
	label var payout "Weekly Take-Home Pay"

// numbers for table note
sum gross_total if gross_total>0	// mean farebox conditional on driving
sum payout if gross_total>0			// mean payout conditional on driving

*******************************************************************************

// set up matrices
	matrix results = J(20, 9, .)
	matrix behavioral = results
	matrix ui = results
	matrix ui_behavioral = ui
	local row = 2

// Drop drivers who don't drive
gen zero = (gross_total==0)
	sum zero
	drop if gross_total==0

// fill in matrix: loop over tax rates
foreach t of local taxrates {
	local col = 2
	
	
	* First entry is just the tax rate under consideration
	matrix results[`row', 1] = `t'
	matrix behavioral[`row', 1] = `t'
	matrix ui[`row', 1] = `t'
	matrix ui_behavioral[`row', 1] = `t'
	
	foreach l of local leases {	// loop over potentials leases (columns)
	foreach x in newhours revenue poscv indifferent cv ui {
		cap drop `x'
	}
	
	// save lease value & regular CV
		matrix results[1, `col'] = `l'
		
	// calculate the CV using equation 4
	// dropping 0 subscripts this is: l - twh - twh*epsilon*t/2/(1-t) -- note always taking tax from something positive to 0.
		* wh is the gross total.  l and t are lease parameters
		* ise is set using the local
	
		gen cv = `l' - `t'*gross_total - (`t' * gross_total * `ise'*`t'/(1-`t')/2)
		
		// set CV to 0 for those who don't drive
		replace cv = 0 if gross_total==0

		
		sum cv, det
		
		// row 1: average cv
		matrix results[`row', `col'] = r(mean)
		
		// row 2: median CV
		matrix results[`row'+1, `col'] = r(p50)
		
		// row 3: fraction with positive cv
		gen poscv = (cv>0) if !missing(cv)
		sum poscv
		matrix results[`row'+2, `col'] = r(mean)
		
		// FOR UI TABLE: save the increase in hours
		gen newhours = hours*`ise'*`t'/(1-`t') + hours
		sum newhours
		local new = r(mean)
		
		*****************************************************************
		// CV with UI
		gen ui = (gross_total*(1+`ise'*`t'/(1-`t'))*(1-`ise'/2) - `l'<0) & gross_total>0
		matrix ui[1, `col'] = `l'
		replace cv = (1-`t')*gross_total * (1-`ise'/2) if ui==1
			// set CV to 0 for those who don't drive
		replace cv = 0 if gross_total==0
		sum cv
		
		// row 1: average cv
		matrix ui[`row', `col'] = r(mean)
		sum ui
		// row 2: fraction on UI
		matrix ui[`row'+1, `col'] = r(mean)
		
		// row 3: % change in hours
		replace newhours = hours * `ise'*`t'/(1-`t') + hours
		replace newhours = 0 if ui==1
		sum newhours
		local new_ui = r(mean)
		matrix ui[`row'+2, `col'] = (`new_ui'-`new')/`new'

		
		*****************************************************************
		// BEHAVIORAL STUFF
		// behavioral lease rate & CV
		cap drop cv
		local behav = `l' * `k'
		matrix behavioral[1, `col'] = `behav'
		gen cv = (`behav' - `t'*gross_total) - (`t'*gross_total * `ise'*`t'/(1-`t')/2)
		replace cv = 0 if gross_total==0
		sum cv, det
		
		// row 1: mean CV
		matrix behavioral[`row', `col'] = r(mean)
		
		// row 2: median CV
		matrix behavioral[`row'+1, `col'] = r(p50)
		assert !missing(cv)

		// row 3: fraction with positive cv
		replace poscv = (cv>0) if !missing(cv)
		sum poscv if gross_total>0
		matrix behavioral[`row'+2, `col'] = r(mean)
		
		replace newhours = hours*`ise'*`t'/(1-`t')+hours
		sum newhours
		local new = r(mean)
		sum hours
		local old = r(mean)
		
		*****************************************************************
		// CV with UI - behavioral
		replace ui = (gross_total*(1+`ise'*`t'/(1-`t'))*(1-`ise'/2) - `behav'<0) & gross_total>0
		assert !missing(ui)
		matrix ui_behavioral[1, `col'] = `l' * `k'
		replace cv = (1-`t')*gross_total * (1-`ise'/2) if ui==1
		replace cv = 0 if gross_total==0
		sum cv
		matrix ui_behavioral[`row', `col'] = r(mean)
		
		sum ui
		matrix ui_behavioral[`row'+1, `col'] = r(mean)
		
		// increase in hours
		replace newhours = hours*`ise'*`t'/(1-`t') + hours
		replace newhours = 0 if ui==1
		sum newhours
		local new_ui = r(mean)
		matrix ui_behavioral[`row'+2, `col'] = `new_ui'/`new' - 1
		
		local col = `col' + 1
		
	} // end loop over leases
	
	* Column 8 of Table 8: what value of the lease makes the average person indifferent?
	* what lease makes me indifferent? 
	gen indifferent = `t'*gross_total + (`t' * gross_total * `ise'*`t'/(1-`t')/2)
	sum indifferent if gross_total>0
	matrix results[`row', `col'] = r(mean)
	
	* what lease makes me indifferent? 
	replace indifferent = (`t'*gross_total + (`t' * gross_total * `ise'*`t'/(1-`t')/2))/`k'
	sum indifferent if gross_total>0
	matrix behavioral[`row', `col'] = r(mean)
	
	local row = `row' + 3
	
} // end loop over tax rates

********************************************************************************
*  Save results to excel file
matrix results = results \ behavioral
matlist results
clear
svmat results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("Table8") sheetreplace	

clear
matrix ui = ui \ ui_behavioral
matlist ui
svmat ui
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA9") sheetreplace	
