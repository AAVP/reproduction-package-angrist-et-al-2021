**************************************************************************************************************************************************************
* This makes Tables  7 and A8

local ise ${ise}
local breps 500
local seed 716
global iseh 2.2			// ISE for high-hour drivers
global isel 1.45			// ISE for low-hour drivers
**************************************************************************************************************************************************************
// Lags: include separate lags for each taxi, and dummies for 0's (so the sample doesn't change across columns)
local lag_1 laglog_taxi1 laglog_taxi2 dum_lag_taxi1 dum_lag_taxi2
local lag_2 laglog_taxi1 laglog_taxi2 dum_lag_taxi1 dum_lag_taxi2 lag2log_taxi1 lag2log_taxi2 dum_lag2_taxi1 dum_lag2_taxi2
local lag_3 laglog_taxi1 laglog_taxi2 dum_lag_taxi1 dum_lag_taxi2 lag2log_taxi1 lag2log_taxi2 dum_lag2_taxi1 dum_lag2_taxi2 lag3log_taxi1 lag3log_taxi2 dum_lag3_taxi1 dum_lag3_taxi2
**************************************************************************************************************************************************************

capture program drop parametricboot
program define parametricboot, rclass
syntax, lags(string)

	preserve
	
	// bootstrap w/in relevant strata & separately by treatment & control
	bsample, strata(strata_bs)
	
	// generate the predicted regressor:
	reg log_GT `lags' d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 if control
	predict predictedearnings
	
	// adjust predicted earnings by an amount that depends on the ISE
	replace predictedearnings = predictedearnings + log(1+t/(1-commission/100)*${ise}/2) - log(L_t)
	
	// run participation probit
	probit participate predictedearnings if treated
	
	// save results
	return scalar slope = _b[predictedearnings]
	return scalar intercept = _b[_cons]
	return scalar tau = 1/_b[predictedearnings]
	
	nlcom logkappa: exp(-_b[_cons]/_b[predictedearnings]), post
	return scalar k = _b[logkappa]
	
	// restore to non-BS sample
	restore
end
*******************************************************************************

local col = 1
local nrow = 7
matrix results = J(15, 6, .)
local  	lags_1 	1 2 3		// lags for live week
local 	lags_2	1			// lags for optin week
local s = 1
forval sample=1/2 {
use "${`c(username)'_data_generated}/generated_participation", clear
matrix observed = J(1,4,.)

	// keep sample
	keep if sample_`sample'
	keep if positive
	
	// keep relevant variables
	keep log_GT d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 participate t L_t control treated strata_bs ///
		gross_total_live taxi_benefit L lagearnings gross_total  `lag_3' commission
	
foreach j of local lags_`sample' { // lags

	// generate the "predicted" results
	reg log_GT `lag_`j'' d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 if control
		matrix results[7, `s'] = e(rmse)
		
	predict p_earnings
	replace p_earnings = p_earnings + log(1+t/(1-(commission/100))*${ise}/2)
	gen  predictedearnings = p_earnings - log(L_t)

	// participation probit
	probit participate predictedearnings if treated, r
	local N = e(N)
	matrix observed[1,2] = _b[_cons]
	matrix observed[1,1] = _b[predictedearnings]
	matrix observed[1,4] = 1/_b[predictedearnings]
	nlcom k: exp(-_b[_cons]/_b[predictedearnings]), post
	matrix observed[1,3] = _b[k]	
	drop predictedearnings p_earnings

	// BOOSTRAP AND SAVE
	preserve
	local random = `seed'+`s'-1
	simulate slope =r(slope) intercept = r(intercept) k=r(k) tau = r(tau), reps(`breps') seed(`random') : parametricboot, lags(`lag_`j'')
	bstat, stat(observed) n(`N') 
	estat bootstrap, all

	matrix results[1, `s'] = _b[slope]
	matrix results[2, `s'] = _se[slope]
	matrix results[3, `s'] = _b[intercept]
	matrix results[4, `s'] = _se[intercept]
	matrix results[5, `s'] = _b[k]
	matrix results[6, `s'] = _se[k]
	matrix results[8, `s'] = _b[tau]
	matrix results[9, `s'] = _se[tau]
	
	matrix results[10, `s'] = `N'
	restore
local s = `s' + 1
}
} // end loop over bandwidth

**************************************************************************************************************************************************************
* Column 5 of Table 7
use "${`c(username)'_data_generated}/generated_participation", clear
	keep if sample_2==1
	keep if positive
capture program drop inattentionboot
program define inattentionboot, rclass

	local ise 1.8
	preserve

	// bootstrap w/in relevant strata & separately by treatment & control
	bsample, strata(strata_bs)
	
	// generate the predicted regressor:
	reg log_GT laglog_taxi1 laglog_taxi2 dum_lag_taxi1 dum_lag_taxi2 d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 if control
	predict predictedearnings	

	// adjust predicted earnings by an amount that depends on the ISE
	replace predictedearnings = predictedearnings + log(1+0.5*`ise'*t/(1-commission/100)) - log(L_t)
		// note: this equation is the general version when face taxes in both periods.  T is the change in fee. 
		
	// not the same as participation probit
	mlexp (ln(cond(participate==1, normal({alpha})*normal({xb:predictedearnings _cons}), ///
            1-normal({alpha})*normal({xb:}) ))) if treated==1	
	
	// save results
	return scalar ll = e(ll)
	return scalar slope = _b[xb:predictedearnings]
	return scalar intercept = _b[xb:_cons]
	return scalar a = normal(_b[/alpha])
	return scalar atransform = _b[/alpha]
	nlcom logkappa: exp(-_b[xb:_cons]/_b[xb:predictedearnings]), post
	return scalar k = _b[logkappa]	
	
	mlexp (ln(cond(participate==1, normal({alpha})*normal({xb:predictedearnings _cons}), ///
            1-normal({alpha})*normal({xb:}) ))) if treated==1	
	nlcom t: 1/_b[xb:predictedearnings],  post
	return scalar tau = _b[t]
	
	// restore to non-BS sample
	restore
end

reg log_GT laglog_taxi1 laglog_taxi2 dum_lag_taxi1 dum_lag_taxi2 d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 if control
		matrix results[7, `s'] = e(rmse)
		
	predict p_earnings
	replace p_earnings = p_earnings + log(1+t/(1-commission/100)*`ise'*0.5)
	gen  predictedearnings = p_earnings - log(L_t)
	matrix observed = J(1,6,.)

	// participation probit
	mlexp (ln(cond(participate==1, normal({alpha})*normal({xb:predictedearnings _cons}), ///
            1-normal({alpha})*normal({xb:}) ))) if treated==1 
			
	local N = e(N)
	matrix observed[1,6] = 1/_b[xb:predictedearnings]
	matrix observed[1,2] = _b[xb:_cons]
	matrix observed[1,1] = _b[xb:predictedearnings]
	matrix observed[1,4] = normal(_b[/alpha])
	matrix observed[1,5] = _b[/alpha]

	nlcom k: exp(-_b[xb:_cons] * 1/_b[xb:predictedearnings]), post
	matrix observed[1,3] = _b[k]	
		drop predictedearnings p_earnings
	
	matlist observed
	
	// bootstrap the standard errors
	preserve
	simulate slope =r(slope) intercept = r(intercept) k=r(k) alpha = r(a) alphatransform = r(atransform) tau=r(tau) , reps(`breps') seed(41): inattentionboot
		bstat, stat(observed) n(`N') 
		estat bootstrap, all

		// store the results
		matrix results[1, 5] = _b[slope]
		matrix results[2, 5] = _se[slope]
		matrix results[3, 5] = _b[intercept]
		matrix results[4, 5] = _se[intercept]
		matrix results[5, 5] = _b[k]
		matrix results[6, 5] = _se[k]
		matrix results[11, 5] = _b[alpha]
		matrix results[12, 5] = _se[alpha]
		matrix results[13, 5] = _b[alphatransform]
		matrix results[14, 5] = _se[alphatransform]
		matrix results[8, 5] = _b[tau]
		matrix results[9, 5] = _se[tau]
		matrix results[10, 5] = `N'
	restore

//---------------------------
* COLUMN 6: SEPARATELY BY HIGH AND LOW
//---------------------------
use "${`c(username)'_data_generated}/generated_participation", clear
	keep if sample_2==1
	keep if positive

capture program drop inattentionboot
program define inattentionboot, rclass

	preserve
	
	// bootstrap w/in relevant strata & separately by treatment & control
	bsample, strata(strata_bs)
	
	// generate the predicted regressor:
	reg log_GT laglog_taxi1 laglog_taxi2 dum_lag_taxi1 dum_lag_taxi2 d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 if control
	predict predictedearnings
	
	// adjust predicted earnings by an amount that depends on the ISE
	replace predictedearnings = predictedearnings + log(1+0.5*${iseh}*t/(1-commission/100)) - log(L_t) if high == 1
	replace predictedearnings = predictedearnings + log(1+0.5*${isel}*t/(1-commission/100)) - log(L_t) if high == 0

	// not the same as participation probit
	mlexp (ln(cond(participate==1, cond(high==1,normal({alphah})*normal({xb:predictedearnings _cons}),normal({alphal})*normal({xb:})), ///
            cond(high==1,1-normal({alphah})*normal({xb:}),1-normal({alphal})*normal({xb:})) ))) if treated==1
	
	// save results
	return scalar ll = e(ll)
	return scalar slope = _b[xb:predictedearnings]
	return scalar intercept = _b[xb:_cons]
	return scalar ah = normal(_b[/alphah])
	return scalar latentah = _b[/alphah]
	return scalar al = normal(_b[/alphal])
	return scalar latental = _b[/alphal]

	nlcom logkappa: exp(-_b[xb:_cons]/_b[xb:predictedearnings]), post
	return scalar k = _b[logkappa]
	
	// not the same as participation probit
	mlexp (ln(cond(participate==1, cond(high==1,normal({alphah})*normal({xb:predictedearnings _cons}),normal({alphal})*normal({xb:})), ///
            cond(high==1,1-normal({alphah})*normal({xb:}),1-normal({alphal})*normal({xb:})) ))) if treated==1
	nlcom t: 1/_b[xb:predictedearnings],  post
	return scalar tau = _b[t]
	
	// restore to non-BS sample
	restore
end

reg log_GT laglog_taxi1 laglog_taxi2 dum_lag_taxi1 dum_lag_taxi2 d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 if control
		matrix results[7, 6] = e(rmse)
		
	predict p_earnings
	replace p_earnings = p_earnings + log(1+t/(1-commission/100)*${iseh}*0.5) if high == 1
	replace p_earnings = p_earnings + log(1+t/(1-commission/100)*${isel}*0.5) if high == 0
	gen  predictedearnings = p_earnings - log(L_t)
	
	// participation probit
	mlexp (ln(cond(participate==1, cond(high==1,normal({alphah})*normal({xb:predictedearnings _cons}),normal({alphal})*normal({xb:})), ///
            cond(high==1,1-normal({alphah})*normal({xb:}),1-normal({alphal})*normal({xb:})) ))) if treated==1
	
	matrix observed = J(1,8,.)
	local N = e(N)
	matrix observed[1,8] = 1/_b[xb:predictedearnings]
	matrix observed[1,2] = _b[xb:_cons]
	matrix observed[1,1] = _b[xb:predictedearnings]
	matrix observed[1,4] = normal(_b[/alphah])
	matrix observed[1,5] = _b[/alphah]
	matrix observed[1,6] = normal(_b[/alphal])
	matrix observed[1,7] = _b[/alphal]

	nlcom k: exp(-_b[xb:_cons] * 1/_b[xb:predictedearnings]), post
	matrix observed[1,3] = _b[k]	
		drop predictedearnings p_earnings
	
	matlist observed
	
	// bootstrap the standard errors
	preserve	
	simulate slope =r(slope) intercept = r(intercept) k=r(k) ah = r(ah) latentah = r(latentah) al = r(al) latental = r(latental) tau =r(tau), reps(`breps') seed(42): inattentionboot
		bstat, stat(observed) n(`N') 
		estat bootstrap, all
		
		// store the results
		matrix results[1, 6] = _b[slope]
		matrix results[2, 6] = _se[slope]
		matrix results[3, 6] = _b[intercept]
		matrix results[4, 6] = _se[intercept]
		matrix results[5, 6] = _b[k]
		matrix results[6, 6] = _se[k]
		matrix results[11, 6] = _b[ah]
		matrix results[12, 6] = _se[ah]

		matrix results[13, 6] = _b[al]
		matrix results[14, 6] = _se[al]
		matrix results[8, 6] = _b[tau]
		matrix results[9, 6] = _se[tau]
		matrix results[10, 6] = `N'
	restore

preserve
matlist results
clear
svmat results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("Table7") sheetreplace	
restore

**************************************************************************************************************************************************************
// Table A8: within subgroup

local col = 1
local nrow = 7
matrix results = J(11, 6, .)
* single lag
local laggedvars laglog_taxi1 laglog_taxi2 dum_lag_taxi1 dum_lag_taxi2

local s = 1
foreach d in twenty twentyfive high low taxi1 taxi2 {

use "${`c(username)'_data_generated}/generated_participation", clear
matrix observed = J(1,3,.)

	// keep sample
	keep if positive
	keep if live_week==1

	gen twenty = (commission==20)
	gen twentyfive = (commission==25)
	
	keep if `d'==1
	
	// keep relevant variables
	keep log_GT d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 participate t L_t control treated strata_bs ///
		gross_total_live taxi_benefit L lagearnings gross_total  `laggedvars' commission

	// generate the "predicted" results
	reg log_GT `laggedvars' d_1 d_2 d_3 d_4 d_5 d_6 d_7 d_8 if control
		matrix results[7, `s'] = e(rmse)
		
	predict p_earnings
	replace p_earnings = p_earnings + log(1+t/(1-(commission/100))*${ise}/2)
	gen  predictedearnings = p_earnings - log(L_t)

	// participation probit
	probit participate predictedearnings if treated, r
	local N = e(N)
	matrix observed[1,2] = _b[_cons]
	matrix observed[1,1] = _b[predictedearnings]
	nlcom k: exp(-_b[_cons]/_b[predictedearnings]), post
	matrix observed[1,3] = _b[k]	
	drop predictedearnings p_earnings

	// BOOSTRAP AND SAVE
	preserve
	local random = `seed'+100+`s'
	simulate slope =r(slope) intercept = r(intercept) k=r(k), reps(`breps') seed(`random') : parametricboot, lags(`laggedvars')
	bstat, stat(observed) n(`N') 
	estat bootstrap, all

	matrix results[1, `s'] = _b[slope]
	matrix results[2, `s'] = _se[slope]
	
	matrix results[3, `s'] = _b[intercept]
	matrix results[4, `s'] = _se[intercept]
	
	matrix results[5, `s'] = _b[k]
	matrix results[6, `s'] = _se[k]
	
	nlcom tau: 1/_b[slope], post
	matrix results[8, `s'] = _b[tau]
	matrix results[9, `s'] = _se[tau]
	
	matrix results[10, `s'] = `N'
	restore

local s = `s' + 1

} // end loop over bandwidth

preserve
matlist results
clear
svmat results
export excel "${`c(username)'_tables}/tables_raw_deck.xls", sheet("TableA8") sheetreplace	

restore
