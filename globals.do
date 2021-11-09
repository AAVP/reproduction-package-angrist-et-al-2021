* Directories and consistent covariates

*****************************************************************************
global `c(username)'_data			"${root}Uber-Data"
global `c(username)'_data_raw		"${root}Uber-Data-Raw"
global `c(username)'_data_generated	"${root}Uber-Data-Intermediate"
global `c(username)'_tables			"${root}Uber-Tables"
global `c(username)'_graphs			"${root}Uber-Graphs"

* make directories if they are not already available
	foreach d in data data_raw data_generated tables graphs {
		capture confirm file "${`c(username)_`d'}/"
		if _rc!=0 {
			mkdir "${`c(username)_`d'}/"
		}	
	}

* Color scheme
set scheme s2color
*****************************************************************************

* Set covariates to use
#delimit ;
global ise 1.8;

global covariates_freeweek
	female months_since_signup log_0822 vehicle_solutions old;
global covariates_taxi1
	female  months_since_signup log_0912 vehicle_solutions old;
global covariates_taxi2
	female  months_since_signup log_1010 vehicle_solutions old
	;	
global covariates_stack
	female months_since_signup lag_log vehicle_solutions old;
	
#delimit cr
