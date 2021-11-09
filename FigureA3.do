* Graph A3: Distribution Treatment Effects
*******************************************************************************
capture program drop abadie_bs
program define abadie_bs, rclass
	count if treated
	local treated = r(N)
	di `treated'
	local control = _N - `treated'
	di `control'
	
	preserve
	bsample
	
	// take the first n1 elements to be in Y1 and the second n0 to be Y0
	tempvar group
	gen `group' = (_n<=`treated')
	count if `group'==0
	assert r(N)==`control'
	
	// generate the k-s statistic
	ksmirnov Y, by(`group')
	
	return scalar effect = r(D)
	restore
	
end

// Abadie weights & parameters
local X i.strata 
local nvals	200
set seed 4242

// Text for figures
local week_0829 "Aug 29 - Sept 4 (Opt-In Week 1)"
local week_0905 "Sept 5 - Sept 11 (Opt-In Week 2)"
local week_0919 "Sept 19 - Sept 25 (Taxi 1)"
local week_1017 "Oct 17 - Oct 24 (Taxi 2)"

// graph parameters
local title_size	small
*******************************************************************************
* Top 2 graphs are for Free week
use "${`c(username)'_data}/laborsupply", clear


// generate renamed variables for easier use in loop
gen treated_0829 = wave1				// wave 1
gen treated_0905 = 1-wave1				// wave 2
gen optin_0829 = optin*treated_0829		// opt-in wave 1
gen optin_0905 = optin*treated_0905		// opt-in wave 2

* We use the strata_eligible for the free week analysis as that was used for random assignment
cap drop strata
rename strata_eligible strata

// set up matrix
matrix results = J(`nvals'+1, 5, .)
local col = 2

// loop over opt-in week weeks
foreach date in 0829 0905 {

cap drop Y
cap drop D
cap drop Z

	// set variables
	gen Z	= treated_`date'
	gen D 	= optin_`date'
	gen Y	= hours_worked_`date'

	// set values
	sum Y, det
	local ymin = r(min)
	local ymax = r(max)
	local yinc = (`ymax' - `ymin')/`nvals'
	local y = `ymin'
	local count = 1
	
	
	*Estimate FS to get number of compliers
	qui ivreg2 D Z `X'
	local fs=_b[Z]
	di in red `fs'
	sum Z
	local pz=r(mean)
	di in red `pz'
	
	// Get Silverman Rule of Thumb Bandwidth
	*Y0
	quietly {
		preserve
		gen Y0=Y*(1-D)
		gen D0=1-D
		ivreg2 Y0 (D0=Z) `X'
		local EY=_b[D0]
		replace Y0=((Y)^2)*(1-D)
		ivreg2 Y0 (D0=Z) `X'
		local EY2 = _b[D0]
		local sigma=sqrt(`EY2' - (`EY')^2)
		di in red `sigma'
		local h0=1.06*`sigma'*(((1-`pz')*`fs'*_N)^(-1/5))
		di in red `h0'
		restore
	}
	*Y1
	quietly {
		preserve
		gen Y1=Y*D
		ivreg2 Y1 (D=Z) `X'
		local EY=_b[D]
		local EY1=_b[D]
		replace Y1=((Y)^2)*D
		ivreg2 Y1 (D=Z) `X'
		local EY2 = _b[D]
		local sigma=sqrt(`EY2' - (`EY')^2)
		local h1=1.06*`sigma'*((`pz'*`fs'*_N)^(-1/5))
		di in red `h1'
		restore
	}
							
	// set row number & count number
	local i = 1
	local count = 1
	
	while `count'<=`nvals' {
		preserve
		matrix results[`i', 1] = `y'
				
		* Y0 CDF
		gen D0 = 1 - D
		gen Y0 = (Y <=`y')*(1-D)
		qui ivreg2 Y0 (D0 = Z) `X'
		matrix results[`i', `col'] = _b[D0]
		
		* Y1 CDF
		gen Y1 = (Y <= `y')*D
		qui ivreg2 Y1 (D = Z) `X'
		matrix results[`i', `col'+1] = _b[D]
		
		local ++count
		local y = `y' + `yinc'
		local ++i
		restore
	}

local col = `col' + 2

// bootstrap K-S statistic
cap drop treated
rename treated_`date' treated

tempfile current
save `current', replace

ksmirnov hours_worked_`date', by(treated)
local k_`date' = round(r(D), .01)
simulate D = r(effect), reps(1000) seed(71690): abadie_bs
gen p = (`k_`date'' <D)
sum p
local p_`date' = round(r(mean), .01)
if `p_`date''<=.001 {
	local p_`date' = "<.001"
}
use `current', clear 
} // end loop over weeks
clear
matlist results


svmat results
rename (results1 results2 results3 results4 results5) ///
	(level Y0_0829 Y1_0829 Y0_0905 Y1_0905)
drop if missing(level)

foreach x in Y0_0829 Y1_0829 Y0_0905 Y1_0905 {
	replace `x' = `x' * 1
}

// generate smoothed CDF
sort level
gen num = _n
tsset num
forval i=0/1 {
foreach date in 0829 0905 {
	tssmooth ma smoothed_`i'_`date' =  Y`i'_`date', window(2 1 2)
}
}

foreach date in 0829 0905 {
graph twoway ///
	(line smoothed_0_`date' level if !missing(Y0_`date'), color(black)) ///
	(line smoothed_1_`date' level if !missing(Y1_`date'), color(ebblue)) ///
	, ///
		title("Hours Worked `week_`date''", size(`title_size') color(black)) ///
		legend(order(1 "Untreated Compliers" 2 "Treated Compliers") size(small)) ///
		bgcolor(white) graphregion(color(white)) ///
		xlabel(0(20)80, labsize(small)) ///
		ytitle("") xtitle("Hours") ///
		ylabel(0(.2)1, labsize(small)) name(date_`date', replace)  ///
		text(.55 60 "K-S test stat: `k_`date''", size(small)) ///
		text(.45 60 "Bootstrap p-value: `p_`date''", size(small))
}

#delimit ;
grc1leg date_0829 date_0905,
	title("Opt-In Week (Fee Free Driving)", size(`title_size') color(black))
	rows(1) graphregion(color(white)) ysize(4) xsize(10)
	name(freeweek, replace)
;
gr export "${`c(username)'_graphs}/CDF_Abadie_FW_hours.pdf", as(pdf) replace;
#delimit cr

*******************************************************************************
*******************************************************************************
* Bottom 2 graphs are about taxi
use * if optin==1 using "${`c(username)'_data}/laborsupply", clear
assert _N==1031

// dates
local dates_taxi1	0919
local dates_taxi2 	1017

// rename treatment variables for easier use
gen treated_0919 = 1-taxi1_control
gen treated_1017 = 1-taxi2_control


matrix results = J(`nvals'+1,5, .)
local col = 2

forval taxi=1/2 {
local date `dates_taxi`taxi''
cap drop Y
cap drop D
cap drop Z

	// set variables
	gen Z	= treated_`date'
	gen D 	= taxi`taxi'_optin
	gen Y	= hours_worked_`date'

	// set values
	sum Y, det
	local ymin = r(min)
	local ymax = r(max)
	local yinc = (`ymax' - `ymin')/`nvals'
	local y = `ymin'
	local count = 1
	disp `ymin'
	
	*Estimate FS to get number of compliers
	qui ivreg2 D Z `X'
	local fs=_b[Z]
	di in red `fs'
	sum Z
	local pz=r(mean)
	di in red `pz'
	
	// Get Silverman Rule of Thumb Bandwidth
	*Y0
	quietly {
		preserve
		gen Y0=Y*(1-D)
		gen D0=1-D
		ivreg2 Y0 (D0=Z) `X'
		local EY=_b[D0]
		replace Y0=((Y)^2)*(1-D)
		ivreg2 Y0 (D0=Z) `X'
		local EY2 = _b[D0]
		local sigma=sqrt(`EY2' - (`EY')^2)
		di in red `sigma'
		local h0=1.06*`sigma'*(((1-`pz')*`fs'*_N)^(-1/5))
		di in red `h0'
		restore
	}
	*Y1
	quietly {
		preserve
		gen Y1=Y*D
		ivreg2 Y1 (D=Z) `X'
		local EY=_b[D]
		local EY1=_b[D]
		replace Y1=((Y)^2)*D
		ivreg2 Y1 (D=Z) `X'
		local EY2 = _b[D]
		local sigma=sqrt(`EY2' - (`EY')^2)
		local h1=1.06*`sigma'*((`pz'*`fs'*_N)^(-1/5))
		di in red `h1'
		restore
	}
							
	// set row number & count number
	local i = 1
	local count = 1
	
	while `count'<=`nvals' {
		preserve
		matrix results[`i', 1] = `y'
					
		* Y0 CDF
		gen D0 = 1 - D
		gen Y0 = (Y <=`y')*(1-D)
		qui ivreg2 Y0 (D0 = Z) `X'
		matrix results[`i', `col'] = _b[D0]
		
		* Y1 CDF
		gen Y1 = (Y <= `y')*D
		qui ivreg2 Y1 (D = Z) `X'
		matrix results[`i', `col'+1] = _b[D]
		
		local ++count
		local y = `y' + `yinc'
		local ++i
		restore
	}
local col = `col' + 2

// bootstrap kstat
tempfile current
save `current', replace
cap drop treated
rename treated_`date' treated

ksmirnov hours_worked_`date', by(treated)
local k_`date' = round(r(D), .01)
simulate D = r(effect), reps(1000) seed(71690): abadie_bs
gen p = (`k_`date'' <D)
sum p
local p_`date' = round(r(mean), .01)
if `p_`date''<=.001 {
	local p_`date' = "<.001"
}
use `current', clear

} // end loop over weeks


clear
matlist results
svmat results
rename (results1 results2 results3 results4 results5) ///
	(level Y0_0919 Y1_0919 Y0_1017 Y1_1017)


drop if missing(level)

foreach x in Y0_0919 Y1_0919 Y0_1017 Y1_1017 {
	replace `x' = `x' * 1
}
// generate smoothed CDF
sort level
gen num = _n
tsset num
forval i=0/1 {
foreach date in 0919 1017 {
	tssmooth ma smoothed_`i'_`date' =  Y`i'_`date', window(2 1 2)
}
} 

foreach date in 0919 1017 {
graph twoway ///
	(line smoothed_0_`date' level if !missing(Y0_`date') & level <= 80, color(black)) ///
	(line smoothed_1_`date' level if !missing(Y1_`date') & level <= 80, color(ebblue)) ///
	, ///
		title("Hours Worked `week_`date''", size(`title_size') color(black)) ///
		legend(order(1 "Untreated Compliers" 2 "Treated Compliers")) ///
		bgcolor(white) graphregion(color(white)) ///
		ytitle("") xtitle("Hours") ///
		xlabel(0(20)80, labsize(small)) ///
		ylabel(0(.2)1, labsize(small)) name(date_`date', replace)  ///
		text(.55 60 "K-S test stat: `k_`date''", size(small)) ///
		text(.45 60 "Bootstrap p-value: `p_`date''", size(small))
}

#delimit ;
grc1leg date_0919 date_1017,
	title("Taxi Treatment", size(`title_size') color(black))
	rows(1) graphregion(color(white)) ysize(4) xsize(10)
	name(taxi, replace)
;
grc1leg freeweek taxi,
	rows(2) graphregion(color(white))
	;
gr export "${`c(username)'_graphs}/CDF_Abadie_all_hours.pdf", as(pdf) replace;
#delimit cr
