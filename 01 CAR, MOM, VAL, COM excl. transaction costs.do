***********************************************
*** CAR, MOM, VAL, COM, excl. TC***************
***********************************************
clear all
cd "C:\Users\nicol\CBS - Copenhagen Business School\MasterThesis N&J - General\04 - Data and Code\03 Stata\Most current code\01 Data"
use "Data_Halfspreads_1M_1976.dta", clear

// Generate fakedate and use as a time series 
gen fakedate = _n
order date fakedate
tsset fakedate

// Time horizon 1985 to 2024. Change also below when changing time horizon here
// drop if _n<108

// Define parameters
scalar currencies = 9     // Total number of currency pairs
scalar top = 3            // Number of currencies in high-momentum portfolio
scalar bottom = 3         // Number of currencies in low-momentum portfolio
scalar period = 12        // Periods per year
scalar lookback = 1

// Calculate logs for spot and forward rates
forvalues i=1/`=currencies' {
	replace usdmid`i' = log(usdmid`i')
	replace usdbid`i' = log(usdbid`i')  
	replace usdask`i' = log(usdask`i')  
}

forvalues i=1/`=currencies' {
	replace fwusdmid`i' = log(fwusdmid`i')
	replace fwusdbid`i' = log(fwusdbid`i') 
	replace fwusdask`i' = log(fwusdask`i') 
}

// Calculate forward discount / forward premium
forvalues i=1/`=currencies' {
	gen fdfp`i' = fwusdmid`i' - usdmid`i'  if fwusdmid`i'!=. & usdmid`i' !=.
}

***********************************************
*** Traditional Carry Trade *******************
***********************************************
// Generate rank of currency based on fdfp
forvalues i = 1/`=currencies' {
     gen rank_fdfp`i' = 1 if fdfp`i'!=.
    
    forvalues j = 1/`=currencies' {
        replace rank_fdfp`i' = rank_fdfp`i' +1 if fdfp`j' > fdfp`i' & fdfp`j' !=.
    }
}

// generate help variable to find correct number of currency pairs. currmiss shows how many of the 9 currencies are missing in the dataset at the given datapoint. currused shows how many currencies are consequently used in the pf building.

egen currmiss = rowmiss(fdfp1-fdfp9)
gen currused = 9-currmiss

// MANUALLY ADJUST rank as we have seven datapoints which have the same fdfps:
// 1976 to 1985
replace rank_fdfp2 = L.rank_fdfp2 in 3
replace rank_fdfp6 = L.rank_fdfp6 in 16
replace rank_fdfp2 = L.rank_fdfp2 in 25
replace rank_fdfp1 = 5 in 66
replace rank_fdfp9 = 3 in 84
// 1986 - 1995: 
replace rank_fdfp6 = L.rank_fdfp6 in 139 // 07/31/1987 "in 139 (20) when doing full sample (subsample)"
replace rank_fdfp2 = 3 in 198 // 06/30/1992 "in 198 (79) when doing full sample (subsample)"


// calculate the returns of every "enterexit" combination, that is, if a currency remains in a PF or leaves it 
// EX = exit
// NX = no exit,remain

/// EX return, exit
forvalues i=1/`=top' {
	gen exretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen exretshort`i' = .
}


forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace exretlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_fdfp`i' == `j'+(L.currused-`=top') 
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace exretshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_fdfp`i' == `j'	
	}
}


/// NX return, no exit

forvalues i=1/`=top' {
	gen nxretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen nxretshort`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace nxretlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_fdfp`i' == `j'+(L.currused-`=top') 
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace nxretshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_fdfp`i' == `j'	
	}
}


**************************************************
// combine the returns, depending on EX or NX
**************************************************

// Create indicators for portfolio membership (both long and short). If currency 1 (usd1=usdeur) is in the long portfolio, in_long1 has indicator variable "1". By this, we know which three currencies are in the long portfolio and in the short portfolio.

// In long portfolio. For each time period, identify if currency is in long portfolio
forvalues i = 1/`=currencies' {
        gen in_long`i' = (rank_fdfp`i' == currused-`=top'+1 | rank_fdfp`i' == currused-`=top'+2 | rank_fdfp`i' == currused-`=top'+3)
}

// In short portfolio. For each time period, identify if currency is in short portfolio. Here, the formula is a bit more simple as our counter always starts at 1. 
// NOTE: Here we need a manual adjustment if we change the number of currencies in the portfolio. 4L4S requres the inclusion of rank 4 as well.

forvalues i = 1/`=currencies' {
		gen in_short`i' = (rank_fdfp`i' == 1| rank_fdfp`i' == 2 | rank_fdfp`i' == 3)
}

// now generate indicator for two different scenarios. 
// EX = enter at t, EXIT at t+1
// NX = enter at t, REMAIN at t+1


// Generate scenario indicators (if condition is true, value of variable = 1)
forvalues i = 1/`=currencies' {
    gen ex_long`i' = (in_long`i' == 0 & L.in_long`i' == 1 )             // exit
    gen nx_long`i' = (in_long`i' == 1 & L.in_long`i' == 1 )             // remain
        
    gen ex_short`i' = (in_short`i' == 0 & L.in_short`i' == 1 )         // exit
    gen nx_short`i' = (in_short`i' == 1 & L.in_short`i' == 1 )         // remain  
 }


//Calculate final returns using the existing return variables
forvalues i = 1/`=top' {
    gen final_long`i' = 0 
}

forvalues i = 1/`=bottom' {
    gen final_short`i' = 0	
}

// Apply returns based on scenarios
forvalues i=1/`=top' {
    forvalues j = 1/`=currencies' {
        replace final_long`i' = exretlong`i' if ex_long`j' == 1 & L.rank_fdfp`j'== `i'+(L.currused-`=top') 
        replace final_long`i' = nxretlong`i' if nx_long`j' == 1 & L.rank_fdfp`j'== `i'+(L.currused-`=top') 
          
        replace final_short`i' = exretshort`i' if ex_short`j' == 1 & L.rank_fdfp`j' == `i'
        replace final_short`i' = nxretshort`i' if nx_short`j' == 1 & L.rank_fdfp`j' == `i'
       }
}

// To have correct statistics, we kick out the zero values in the first row, as we don't have a return yet.
forvalues i=1/`=top' {
		replace final_long`i' =. in 1	
		replace final_short`i' =. in 1	
}

// manually adjust last return, all exit (use 588 when doing full sample)
forvalues i=1/`=top'{
forvalues j = 1/`=currencies' {
       replace final_long`i' = exretlong`i' in 588 if L.rank_fdfp`j'== `i'+(L.currused-`=top') 
       replace final_short`i' = exretshort`i' in 588 if L.rank_fdfp`j' == `i' 
}
}
	

// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
    rename final_long`i' retlong`i'
	replace retlong`i' = exp(retlong`i')-1
}

forvalues i=1/`=bottom' {
    rename final_short`i' retshort`i'
	replace retshort`i' = exp(retshort`i')-1
}
	

// now combine the returns. EQUAL WEIGHT
egen pflong = rowtotal(retlong*) if _n>1
replace pflong = 1/`=top' * pflong if _n>1

egen pfshort = rowtotal(retshort*) if _n>1
replace pfshort = 1/`=bottom' * pfshort if _n>1

// Calculate HML return and accumulative return
gen HML = pflong + pfshort if _n>1

// Reconvert to log returns
replace HML = log(1+HML)

gen sumHML = sum(HML) if _n>1



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////



***********************************************
*** Momentum Strategy *************************
***********************************************

// This means we look at previous returns. Return is all long forward USD against FCU
forvalues i=1/`=currencies' {
	gen mom`i'  = (usdmid`i' - L.fwusdmid`i')     // Long USD case, lookback = 1
}

// for other lookback periods change to
// +(L.usdmid`i' - L2.fwusdmid`i')+(L2.usdmid`i' - L3.fwusdmid`i')+(L3.usdmid`i' - L4.fwusdmid`i')+(L4.usdmid`i' - L5.fwusdmid`i')+(L5.usdmid`i' - L6.fwusdmid`i')

forvalues i = 1/`=currencies' {
    gen rank_mom`i' = 1 if mom`i'!=.
     forvalues j = 1/`=currencies' {
       replace rank_mom`i' = rank_mom`i' + 1 if mom`j' < mom`i' & mom`i' != . 
    }
}


egen mcurrmiss = rowmiss(rank_mom1-rank_mom9) if _n>1
gen mcurrused = 9-mcurrmiss if _n>1


// Manually adjust rank as we have seven datapoints which have the same fdfp in 1985 - 1994 only
replace rank_mom2 = 2 in 15
replace rank_mom7 = 3 in 17
replace rank_mom2 = 6 in 110
replace rank_mom2 = 4 in 131 // 11/28/1986 in 131 (12) when doing full samplel (subsample)


// calculate the returns of every "enterexit" combination. 
// EX = exit
// NX = no exit,remain

// EX return, exit
forvalues i=1/`=top' {
	gen exmretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen exmretshort`i' = .
}


forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace exmretlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_mom`i' == `j'+(L.mcurrused-`=top') & L.rank_mom`i' !=.
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace exmretshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_mom`i' == `j' & L.rank_mom`i' !=.
	}
}


// NX return, no exit

forvalues i=1/`=top' {
	gen nxmretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen nxmretshort`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace nxmretlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_mom`i' == `j'+(L.mcurrused-`=top') & L.rank_mom`i' !=.
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace nxmretshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_mom`i' == `j' & L.rank_mom`i' !=.	
	}
}


**************************************************
// now combine the returns, depending on EX or NX
**************************************************

// Create indicators for portfolio membership (both long and short). If currency 1 (usd1=usdeur) is in the long portfolio, in_long1 has indicator variable "1". By this, we know which three currencies are in the long portfolio and in the short portfolio.

// In long portfolio. For each time period, identify if currency is in long portfolio
forvalues i = 1/`=currencies' {
        gen in_mlong`i' = (rank_mom`i' == mcurrused-`=top'+1 | rank_mom`i' == mcurrused-`=top'+2 | rank_mom`i' == mcurrused-`=top'+3) if _n>1
}

// In short portfolio. For each time period, identify if currency is in short portfolio. Here, the formula is a bit more simple as our counter always starts at 1. 
// NOTE: Here we need a manual adjustment if we change the number of currencies in the portfolio. 4L4S requres the inclusion of rank 4 as well.

forvalues i = 1/`=currencies' {
		gen in_mshort`i' = (rank_mom`i' == 1| rank_mom`i' == 2 | rank_mom`i' == 3) if _n>1
}



// now generate indicator for two different scenarios. 
// EX = enter at t, EXIT at t+1
// NX = enter at t, REMAIN at t+1


// Generate scenario indicators (note: if condition is true, value of variable = 1)
forvalues i = 1/`=currencies' {
    gen ex_mlong`i' = (in_mlong`i' == 0 & L.in_mlong`i' == 1 )            // exit
    gen nx_mlong`i' = (in_mlong`i' == 1 & L.in_mlong`i' == 1 )            // remain
        
    gen ex_mshort`i' = (in_mshort`i' == 0 & L.in_mshort`i' == 1 )         // exit
    gen nx_mshort`i' = (in_mshort`i' == 1 & L.in_mshort`i' == 1 )         // remain  
 }

 
// Calculate final returns using the existing return variables
forvalues i = 1/`=top' {
    gen final_mlong`i' = . & _n>2
}

forvalues i = 1/`=bottom' {
    gen final_mshort`i' = . & _n>2
}

// Apply returns based on scenarios
forvalues i=1/`=top' {
    forvalues j = 1/`=currencies' {
        replace final_mlong`i' = exmretlong`i' if ex_mlong`j' == 1 & L.rank_mom`j'== `i'+(L.mcurrused-`=top') 
        replace final_mlong`i' = nxmretlong`i' if nx_mlong`j' == 1 & L.rank_mom`j'== `i'+(L.mcurrused-`=top') 
          
        replace final_mshort`i' = exmretshort`i' if ex_mshort`j' == 1 & L.rank_mom`j' == `i' 
        replace final_mshort`i' = nxmretshort`i' if nx_mshort`j' == 1 & L.rank_mom`j' == `i' 
       }
}

forvalues i = 1/`=top' {
    replace final_mlong`i' = . if _n<3 
	replace final_mshort`i' = . if _n<3 
	}


// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
    rename final_mlong`i' mretlong`i'
	replace mretlong`i' = exp(mretlong`i')-1
}

forvalues i=1/`=bottom' {
    rename final_mshort`i' mretshort`i'
	replace mretshort`i' = exp(mretshort`i')-1
}
	

// now combine the returns. EQUAL WEIGHT
egen mpflong = rowtotal(mretlong*) if _n>2
replace mpflong = 1/`=top' * mpflong if _n>2

egen mpfshort = rowtotal(mretshort*) if _n>2
replace mpfshort = 1/`=bottom' * mpfshort if _n>2

// Calculate HML return and accumulative return
gen mHML = mpflong + mpfshort if _n>2

// Reconvert to log returns
replace mHML = log(1+mHML)

gen msumHML = sum(mHML) if _n>2



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


***********************************************
*************** Value 3C 1M NC ****************
***********************************************

// merge CPI data into dataset
merge 1:1 date using "Data_CPI.dta"
drop _merge

// Generate fakedate and use as a time series 
drop fakedate 
gen fakedate = _n
order date fakedate
tsset fakedate


***********************************************
*************** Value Ranking *****************
***********************************************

//install rangestat package
ssc install rangestat, replace

//Hutchinson, Footnote nb. 5, implement a 6-months lag to account for CPI data being published with a delay. We do this too
forvalues i = 1/10 {
    gen cpi`i'_lag6 = L6.cpi`i'
}


//Moving average for US CPI (cpi10)
rangestat (mean) cpi10, interval(fakedate -66 -54) 

//NW: drop first 12 results of cpi10_mean
replace cpi10_mean = . if _n<67



// Convert to simple numbers 
forvalues i=1/`=currencies' {
	replace usdmid`i' = exp(usdmid`i')
	replace usdbid`i' = exp(usdbid`i')  
	replace usdask`i' = exp(usdask`i')  
}

forvalues i=1/`=currencies' {
	replace fwusdmid`i' = exp(fwusdmid`i')
	replace fwusdbid`i' = exp(fwusdbid`i') 
	replace fwusdask`i' = exp(fwusdask`i') 
}



forvalues i = 1/9 {

    //Create moving average over t-66 to t-54 for CPI of country i
    rangestat (mean) cpi`i', interval(fakedate -66 -54)
    replace cpi`i'_mean = . if _n<67 
    
	//Moving average FX (usdmid1 to usdmid9)
    rangestat (mean) usdmid`i', interval(fakedate -66 -54)
     replace usdmid`i'_mean = . if _n<67 
	
	//Calculate log terms
    gen real_cpi_ratio`i' = ln( (cpi`i'_lag6 / cpi10_lag6 ) / ( cpi`i'_mean / cpi10_mean) )

    gen fx_term`i' = ln(usdmid`i' / usdmid`i'_mean)

    //Compute VAL
    gen val`i' = real_cpi_ratio`i' - fx_term`i'
	
	//Drop
	
}

drop real_cpi_ratio* fx_term* 
forvalues i=1/9 {
	drop usdmid`i'_mean 
	drop cpi`i'_mean
}
drop cpi10_mean



// Reconvert to logs 
forvalues i=1/`=currencies' {
	replace usdmid`i' = ln(usdmid`i')
	replace usdbid`i' = ln(usdbid`i')  
	replace usdask`i' = ln(usdask`i')  
}

forvalues i=1/`=currencies' {
	replace fwusdmid`i' = ln(fwusdmid`i')
	replace fwusdbid`i' = ln(fwusdbid`i') 
	replace fwusdask`i' = ln(fwusdask`i') 
}


//Cross-sectional ranking. By this, the currencies with rank 7,8,9 are the most overvalued currencies, and rank 1,2,3 are the most undervalued. This means that we buy the USD against the currencies with VAL_rank 7,8,9 and we sell the USD against the currencies with VAL_rank 1,2,3.

forvalues i = 1/9 {
    gen rank_val`i' = 1 if val`i' !=.
    
    forvalues j = 1/9 {
        replace rank_val`i' = rank_val`i' + 1 if val`j' < val`i' & val`j' != .
    }
}


egen vcurrmiss = rowmiss(val1-val9) if _n>66
gen vcurrused = 9-vcurrmiss if _n>66


// calculate the returns of every "enterexit" combination. 
// EX = exit
// NX = no exit,remain

// EX return, exit
forvalues i=1/`=top' {
	gen exvretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen exvretshort`i' = .
}


forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace exvretlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_val`i' == `j'+(L.vcurrused-`=top') &  L.vcurrused !=.
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace exvretshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_val`i' == `j'	&  L.vcurrused !=.
	}
}


// NX return, no exit

forvalues i=1/`=top' {
	gen nxvretlong`i' = .
}

forvalues i=1/`=bottom' {
	gen nxvretshort`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace nxvretlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_val`i' == `j'+(L.vcurrused-`=top') &  L.vcurrused !=.
	}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace nxvretshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_val`i' == `j'	&  L.vcurrused !=.
	}
}


**************************************************
// now combine the returns, depending on EX or NX
**************************************************

// Create indicators for portfolio membership (both long and short). If currency 1 (usd1=usdeur) is in the long portfolio, in_long1 has indicator variable "1". By this, we know which three currencies are in the long portfolio and in the short portfolio.

// In long portfolio. For each time period, identify if currency is in long portfolio
forvalues i = 1/`=currencies' {
        gen in_vlong`i' = (rank_val`i' == vcurrused-`=top'+1 | rank_val`i' == vcurrused-`=top'+2 | rank_val`i' == vcurrused-`=top'+3) 
		replace in_vlong`i' = . if vcurrused == .
}

// In short portfolio. For each time period, identify if currency is in short portfolio. Here, the formula is a bit more simple as our counter always starts at 1. 
// NOTE: Here we need a manual adjustment if we change the number of currencies in the portfolio. 4L4S requres the inclusion of rank 4 as well.

forvalues i = 1/`=currencies' {
		gen in_vshort`i' = (rank_val`i' == 1| rank_val`i' == 2 | rank_val`i' == 3) 
		replace in_vshort`i' = . if vcurrused == .
}

// now generate indicator for two different scenarios. 
// EX = enter at t, EXIT at t+1
// NX = enter at t, REMAIN at t+1


// Generate scenario indicators (note: if condition is true, value of variable = 1)
forvalues i = 1/`=currencies' {
    gen ex_vlong`i' = (in_vlong`i' == 0 & L.in_vlong`i' == 1 )             // exit
    gen nx_vlong`i' = (in_vlong`i' == 1 & L.in_vlong`i' == 1 )             // remain
        
    gen ex_vshort`i' = (in_vshort`i' == 0 & L.in_vshort`i' == 1 )         // exit
    gen nx_vshort`i' = (in_vshort`i' == 1 & L.in_vshort`i' == 1 )         // remain  
 }

forvalues i = 1/`=currencies' {
    replace ex_vlong`i' = . if vcurrused == . 		                  // exit
    replace nx_vlong`i' = . if vcurrused == .                         // remain
        
    replace ex_vshort`i' = . if vcurrused == .                         // exit
    replace nx_vshort`i' = . if vcurrused == .                         // remain  
 }

//Calculate final returns using the existing return variables
forvalues i = 1/`=top' {
    gen final_vlong`i' = 0 
	replace final_vlong`i' = . if L.vcurrused == .
}

forvalues i = 1/`=bottom' {
    gen final_vshort`i' = 0
	replace final_vshort`i' = . if L.vcurrused == .
}

// Apply returns based on scenarios
forvalues i=1/`=top' {
    forvalues j = 1/`=currencies' {
        replace final_vlong`i' = exvretlong`i' if ex_vlong`j' == 1 & L.rank_val`j'== `i'+(L.vcurrused-`=top') 
        replace final_vlong`i' = nxvretlong`i' if nx_vlong`j' == 1 & L.rank_val`j'== `i'+(L.vcurrused-`=top') 
          
        replace final_vshort`i' = exvretshort`i' if ex_vshort`j' == 1 & L.rank_val`j' == `i'
        replace final_vshort`i' = nxvretshort`i' if nx_vshort`j' == 1 & L.rank_val`j' == `i'
       }
}

// To have correct statistics, we kick out the zero values in the first row, as we don't have a return yet.
forvalues i=1/`=top' {
		replace final_vlong`i' =. if vcurrused == .
		replace final_vshort`i' =. if vcurrused == .
}

// manually adjust last return, all exit (use 588 when doing full sample)
forvalues i=1/`=top'{
forvalues j = 1/`=currencies' {
       replace final_vlong`i' = exvretlong`i' in 588 if L.rank_val`j'== `i'+(L.vcurrused-`=top') 
       replace final_vshort`i' = exvretshort`i' in 588 if L.rank_val`j' == `i' 
}
}
	

// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
    rename final_vlong`i' vretlong`i'
	replace vretlong`i' = exp(vretlong`i')-1
}

forvalues i=1/`=bottom' {
    rename final_vshort`i' vretshort`i'
	replace vretshort`i' = exp(vretshort`i')-1
}
	

// now combine the returns. EQUAL WEIGHT
egen vpflong = rowtotal(vretlong*) if _n>67
replace vpflong = 1/`=top' * vpflong if _n>67

egen vpfshort = rowtotal(vretshort*) if _n>67
replace vpfshort = 1/`=bottom' * vpfshort if _n>67

// Calculate HML return and accumulative return
gen vHML = vpflong + vpfshort if _n>67

// Reconvert to log returns
replace vHML = log(1+vHML)

gen vsumHML = sum(vHML) if _n>67



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////

exit

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


***********************************************
*************** Combined **********************
***********************************************

graph set window fontface "Times New Roman"
format date %tdm/Y

// scaling CAR and MOM to same return observation date as VAL

replace HML = . if _n<68
replace sumHML = sum(HML) 
replace sumHML = . if _n<68

replace mHML = . if _n<68
replace msumHML = sum(mHML)
replace msumHML = . if _n<68

// Drop for charting etc to align starting point
// drop if _n<68

********************************************************************************
*** Combining Signals for Balanced HML Strategy *******************************
********************************************************************************

// Indicator for each currency position across all strategies
forvalues i = 1/`=currencies' {
    // Net position counter
    gen buy_signals`i' = 0
    replace buy_signals`i' = buy_signals`i' + 1 if in_long`i' == 1 & _n > 66
    replace buy_signals`i' = buy_signals`i' + 1 if in_mlong`i' == 1 & _n > 66
    replace buy_signals`i' = buy_signals`i' + 1 if in_vlong`i' == 1 & _n > 66
    
    gen sell_signals`i' = 0
    replace sell_signals`i' = sell_signals`i' + 1 if in_short`i' == 1 & _n > 66
    replace sell_signals`i' = sell_signals`i' + 1 if in_mshort`i' == 1 & _n > 66
    replace sell_signals`i' = sell_signals`i' + 1 if in_vshort`i' == 1 & _n > 66
    
    // Calculate net signal based on all individual signals
    gen net_signal`i' = buy_signals`i' - sell_signals`i'
}

// Create clean long/short indicators , that is, total signal
forvalues i = 1/`=currencies' {
    gen final_long`i' = (net_signal`i' > 0)
}
forvalues i = 1/`=currencies' {
    gen final_short`i' = (net_signal`i' < 0)
    // 
}

// Count currencies in final long and short portfolios
gen long_count = 0
gen short_count = 0
forvalues i = 1/`=currencies' {
    replace long_count = long_count + final_long`i'
    replace short_count = short_count + final_short`i'
}

// Calculate returns for positions in COM
forvalues i = 1/`=currencies' {
    // Long portfolio returns
    gen comb_long_ret`i' = 0
    // For currencies in the long portfolio
    replace comb_long_ret`i' = (usdmid`i' - L.fwusdmid`i') if L.final_long`i' == 1 & final_long`i' == 0  // Exit
    replace comb_long_ret`i' = (usdmid`i' - L.fwusdmid`i') if L.final_long`i' == 1 & final_long`i' == 1  // Hold
    
    // Short portfolio returns
    gen comb_short_ret`i' = 0  
    // For currencies in the short portfolio
    replace comb_short_ret`i' = (L.fwusdmid`i' - usdmid`i') if L.final_short`i' == 1 & final_short`i' == 0  // Exit
    replace comb_short_ret`i' = (L.fwusdmid`i' - usdmid`i') if L.final_short`i' == 1 & final_short`i' == 1  // Hold
    
    // Convert to simple returns
    replace comb_long_ret`i' = exp(comb_long_ret`i') - 1
    replace comb_short_ret`i' = exp(comb_short_ret`i') - 1
}

// Calculate equally weighted returns for long and short pf leg
gen cpflong= 0
gen cpfshort = 0

forvalues i = 1/`=currencies' {
    replace cpflong = cpflong + comb_long_ret`i' if L.long_count > 0
    replace cpfshort = cpfshort + comb_short_ret`i' if L.short_count > 0
}

// Apply equal weighting (1/n) within each side
replace cpflong = cpflong / L.long_count if L.long_count > 0
replace cpfshort = cpfshort / L.short_count if L.short_count > 0

// Calculate the final HML return
gen cHML = cpflong + cpfshort

// Convert back to log returns for consistency with your other calculations
replace cHML = log(1 + cHML)
gen csumHML = sum(cHML)


exit





**********************************************************************
*** Data analytics ***************************************************
**********************************************************************

graph set window fontface "Times New Roman"
format date %tdm/Y

// scaling CAR and MOM to same return observation date

replace HML = . if _n<68
replace sumHML = sum(HML) 
replace sumHML = . if _n<68

replace mHML = . if _n<68
replace msumHML = sum(mHML)
replace msumHML = . if _n<68

replace cHML = . if _n<68
replace csumHML = sum(cHML)
replace csumHML = . if _n<68

// drop if _n<68


**********************************************************************
*** Line graph over total cumulative return **************************
**********************************************************************

// in color line chart
  line  sumHML msumHML vsumHML csumHML date if _n>1, ///
    title("Return comparison between individual and combined strategy") ///
    xtitle("Date") ///
    ytitle("Cumulative excess returns") ///
    graphregion(margin(r=5)) ///
    xlabel(, angle(horizontal)) ///
	xsize(10) ///
    ysize(6) ///
    xlabel(#6) ///
    ylabel(0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%" 2 "200%" 2.5 "250%") ///
    legend(pos(11) ring(0) col(1) textfirst) ///
    legend(label(1 "CAR") ///
           label(2 "MOM") ///
           label(3 "VAL") ///
           label(4 "COM")) ///
		    lcolor(. . . purple)

**********************************************************************
*** CAR **************************************************************
**********************************************************************
//generate summary statistics

gen maxsumHML = sumHML if _n == 2
replace maxsumHML = max(L.maxsumHML, sumHML) if _n > 2
gen DD = (maxsum - sumHML)*-1

sum HML, detail
scalar yrlymean = period*r(mean)*100
scalar yrlysd = sqrt(period)*r(sd)*100
scalar SR = yrlymean / yrlysd
scalar yrlyskew = r(skewness)/sqrt(period)
scalar yrlykurt = (r(kurtosis)-3)/period
scalar min = r(min)*100 
scalar max = r(max)*100 

sum DD, detail
scalar maxdd = r(min)*100 

// Store statistics in a row matrix
matrix stats = (yrlymean, yrlysd, SR, yrlyskew, yrlykurt, maxdd, min, max)
matrix rownames stats = "CAR"
matrix colnames stats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist stats, format(%9.2f)


**********************************************************************
*** MOM **************************************************************
**********************************************************************
//generate summary statistics

gen mmaxsumHML = msumHML if _n == 2
replace mmaxsumHML = max(L.mmaxsumHML, msumHML) if _n > 2
gen mDD = (mmaxsum - msumHML)*-1

sum mHML, detail
scalar myrlymean = period*r(mean)*100
scalar myrlysd = sqrt(period)*r(sd)*100
scalar mSR = myrlymean / myrlysd
scalar myrlyskew = r(skewness)/sqrt(period)
scalar myrlykurt = (r(kurtosis)-3)/period
scalar mmin = r(min)*100 
scalar mmax = r(max)*100 

sum mDD, detail
scalar mmaxdd = r(min)*100 

// Store statistics in a row matrix
matrix mstats = (myrlymean, myrlysd, mSR, myrlyskew, myrlykurt, mmaxdd, mmin, mmax)
matrix rownames mstats = "MOM"
matrix colnames mstats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist mstats, format(%9.2f)



**********************************************************************
*** VAL **************************************************************
**********************************************************************
//generate summary statistics

gen vmaxsumHML = vsumHML if _n == 2
replace vmaxsumHML = max(L.vmaxsumHML, vsumHML) if _n > 2
gen vDD = (vmaxsum - vsumHML)*-1

sum vHML, detail
scalar vyrlymean = period*r(mean)*100
scalar vyrlysd = sqrt(period)*r(sd)*100
scalar vSR = vyrlymean / vyrlysd
scalar vyrlyskew = r(skewness)/sqrt(period)
scalar vyrlykurt = (r(kurtosis)-3)/period
scalar vmin = r(min)*100 
scalar vmax = r(max)*100 

sum vDD, detail
scalar vmaxdd = r(min)*100 

// Store statistics in a row matrix
matrix vstats = (vyrlymean, vyrlysd, vSR, vyrlyskew, vyrlykurt, vmaxdd, vmin, vmax)
matrix rownames vstats = "VAL"
matrix colnames vstats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist vstats, format(%9.2f)




**********************************************************************
*** COM **************************************************************
**********************************************************************
//generate summary statistics

gen cmaxsumHML = csumHML if _n == 2
replace cmaxsumHML = max(L.cmaxsumHML, csumHML) if _n > 2
gen cDD = (cmaxsum - csumHML)*-1

sum cHML, detail
scalar cyrlymean = period*r(mean)*100
scalar cyrlysd = sqrt(period)*r(sd)*100
scalar cSR = cyrlymean / cyrlysd
scalar cyrlyskew = r(skewness)/sqrt(period)
scalar cyrlykurt = (r(kurtosis)-3)/period
scalar cmin = r(min)*100 
scalar cmax = r(max)*100 

sum cDD, detail
scalar cmaxdd = r(min)*100 

// Store statistics in a row matrix
matrix cstats = (cyrlymean, cyrlysd, cSR, cyrlyskew, cyrlykurt, cmaxdd, cmin, cmax)
matrix rownames cstats = "COM"
matrix colnames cstats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist cstats, format(%9.2f)


//Drawdown analysis and return histogram
summarize cDD
local avg_dd = r(mean)
local avg_pct = string(`avg_dd'*100, "%9.1f")

line cDD date if _n>1, title("Maximum percentage drawdown") xtitle("Date") ytitle("Drawdown") ///
graphregion(margin(r=5)) lcolor(red) xlabel(, angle(horizontal)) xsize(10) ysize(6) ///
xlabel(#6) ylabel(0 "0%" -0.1 "-10%" -0.2 "-20%" -0.3 "-30%" `avg_dd' "`avg_pct'%") ///
yline(`avg_dd', lpattern(dash) lcolor(gray) lwidth(normal))


hist cHML, bins(50) normal title("Histogram Combined Strategy Returns") xtitle("Return distribution") ytitle("Density")  xlabel(-0.1 "-10%" -0.05 "-5%" 0 "0" 0.05 "5%" 0.1 "10%") ylabel(0(5)30)









