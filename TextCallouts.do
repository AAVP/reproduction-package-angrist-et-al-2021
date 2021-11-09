cd "${`c(username)'_data}"
********************************************************************************
** SECTION 4
use laborsupply, clear
	// percent omitted
	di 1-.5*(1176/1600) - .5*(821/1031)
	
	// percent with non-zero 
	keep driver_uuid hours_worked_* optin
	rename *_0* *_*
	
	reshape long hours_worked_, i(driver_uuid optin) j(week)
	keep if inlist(week,829, 905, 919, 1017)
	bysort driver_uuid (week): gen weeknum = _n
	
	drop week
	tab weeknum
	
	// overall percentage that drove
	gen drive = hours_worked_>0
	sum drive
	
	// omitting the guys who didn't opt-in in free week
	replace drive = . if (weeknum==3 | weeknum==4) & !optin
	sum drive
	
********************************************************************************
** SECTION 5

// Calculating the coefficient of loss aversion
use laborsupply, clear
	keep if optin
	
	// Fraction of people who are below breakeven
	keep driver_uuid L_t_? gross_total_* 
	
	rename gross_total_0912		gross_total_taxi1
	rename gross_total_1010		gross_total_taxi2
	
	gen below_be_1 = (gross_total_taxi1<L_t_1) if L_t_1>0
	gen below_be_2 = (gross_total_taxi2<L_t_2) if L_t_2>0
	
	gen ratio_1 = (gross_total_taxi1/L_t_1) if below_be_1
	gen ratio_2 = (gross_total_taxi2/L_t_2) if below_be_2
	
	reshape long below_be_ ratio_, i(driver_uuid) j(week)
	rename *_ *
	
	sum below_be
	sum ratio if below_be
	
// Calculating the average per trip farebox 
use laborsupply, clear
	
	// reshape 
	keep driver_uuid optin gross_total_* completed_trips_*
	rename *_0* *_*
	reshape long gross_total_ completed_trips_, i(driver_uuid optin) j(week)
	bysort driver_uuid (week): gen weeknum = _n
	rename *_ *
	
	gen farebox_trip = gross_total/completed_trips
	sum farebox_trip, det
********************************************************************************
cd "${`c(username)'_code}"
