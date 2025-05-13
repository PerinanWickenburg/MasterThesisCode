***********************************************
*** CCT 3LS 1M NC *****************************
***********************************************
clear all
cd "C:\Users\nicol\CBS - Copenhagen Business School\MasterThesis N&J - General\04 - Data and Code\03 Stata\Most current code\01 Data"
use "Data_1M_1976.dta", clear

// Generate fakedate and use as a time series 
gen fakedate = _n
order date fakedate
tsset fakedate

// to only include 12/1984 to compare to 3M 6M 12M
// drop if _n<108
// from 2000 to 2024: keep if _n>287
// from 1995 to 2024: keep if _n>227

// Define parameters
scalar currencies = 9     // Total number of currency pairs
scalar top = 3            // Number of currencies in high portfolio
scalar bottom = 3         // Number of currencies in low portfolio
scalar period = 12        // Periods per year


// Sample period
// Full sample: no change

// 1985 to 1994 (incl.)
// drop if fakedate > 121

// 1995 to 2004 (incl.)
// drop if fakedate < 121
// drop if fakedate > 241

// 2005 to 2014 (incl.)
// drop if fakedate < 241
// drop if fakedate > 361

// 2015 to 2024 (incl.)
// drop if fakedate < 361

// to only include 12/1984 to compare to 3M 6M 12M
// drop if _n<108

// Calculate logs
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

// Manually adjust rank as we have seven datapoints which have the same fdfp in 1985 - 1994 only
replace rank_fdfp2 = L.rank_fdfp2 in 3
replace rank_fdfp6 = L.rank_fdfp6 in 16
replace rank_fdfp2 = L.rank_fdfp2 in 25
replace rank_fdfp1 = 5 in 66
replace rank_fdfp9 = 3 in 84
replace rank_fdfp6 = L.rank_fdfp6 in 139
replace rank_fdfp2 = 3 in 198

/// total carry return (IRD+FX)
forvalues i=1/`=top' {
	gen retlong`i' = .
}

forvalues i=1/`=bottom' {
	gen retshort`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace retlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_fdfp`i' == `j'+(L.currused-`=top') 
}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace retshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_fdfp`i' == `j'	
	}
}

// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
	replace retlong`i' = exp(retlong`i')-1
}

forvalues i=1/`=bottom' {
	replace retshort`i' = exp(retshort`i')-1
}
	

/// now combine the returns. EQUAL WEIGHT
egen pflong = rowtotal(retlong*) if _n>1
replace pflong = 1/`=top' * pflong if _n>1

egen pfshort = rowtotal(retshort*) if _n>1
replace pfshort = 1/`=bottom' * pfshort if _n>1

// Calculate HML return and accumulative return
gen HML = pflong + pfshort if _n>1

// Reconvert to log returns
replace HML = log(1+HML)

gen sumHML = sum(HML) if _n>1




// only IRD component
forvalues i=1/`=top' {
	gen retlongird`i' = .
}

forvalues i=1/`=bottom' {
	gen retshortird`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace retlongird`j' = (L.usdmid`i' - L.fwusdmid`i') if L.rank_fdfp`i' == `j'+(L.currused-`=top') 
}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace retshortird`j' = (L.fwusdmid`i' - L.usdmid`i') if L.rank_fdfp`i' == `j'	
	}
}

// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
	replace retlongird`i' = exp(retlongird`i')-1
}

forvalues i=1/`=bottom' {
	replace retshortird`i' = exp(retshortird`i')-1
}
	

/// now combine the returns. EQUAL WEIGHT
egen pflongird = rowtotal(retlongird*) if _n>1
replace pflongird = 1/`=top' * pflongird if _n>1

egen pfshortird = rowtotal(retshortird*) if _n>1
replace pfshortird = 1/`=bottom' * pfshortird if _n>1

// Calculate HML return and accumulative return
gen HMLird = pflongird + pfshortird if _n>1

// Reconvert to log returns
replace HMLird = log(1+HMLird)

gen sumHMLird = sum(HMLird) if _n>1



/// only FX component
forvalues i=1/`=top' {
	gen retlongfx`i' = .
}

forvalues i=1/`=bottom' {
	gen retshortfx`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace retlongfx`j' = (usdmid`i' - L.usdmid`i') if L.rank_fdfp`i' == `j'+(L.currused-`=top') 
}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace retshortfx`j' = (L.usdmid`i' - usdmid`i') if L.rank_fdfp`i' == `j'	
	}
}

// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
	replace retlongfx`i' = exp(retlongfx`i')-1
}

forvalues i=1/`=bottom' {
	replace retshortfx`i' = exp(retshortfx`i')-1
}
	

/// now combine the returns. EQUAL WEIGHT
egen pflongfx = rowtotal(retlongfx*) if _n>1
replace pflongfx = 1/`=top' * pflongfx if _n>1

egen pfshortfx = rowtotal(retshortfx*) if _n>1
replace pfshortfx = 1/`=bottom' * pfshortfx if _n>1

// Calculate HML return and accumulative return
gen HMLfx = pflongfx + pfshortfx if _n>1

// Reconvert to log returns
replace HMLfx = log(1+HMLfx)

gen sumHMLfx = sum(HMLfx) if _n>1



exit

graph set window fontface "Times New Roman"
format date %tdm/Y
line sumHML sumHMLird sumHMLfx date if _n>1, ///
    title("Return decomposition into Carry and FX") ///
    xtitle("Date") ///
    ytitle("Cumulative excess returns") ///
    graphregion(margin(r=5)) ///
    xlabel(, angle(horizontal)) ///
    xsize(10) ///
    ysize(6) ///
    xlabel(#6) ///
    ylabel(-0.5 "-50%" 0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%" 2 "200%" 2.5 "250%" 3 "300%" ) ///
    legend(pos(11) ring(0) col(1) textfirst) ///
    legend(label(1 "Carry Trade") ///
           label(2 "Interest rate differential") ///
           label(3 "Change in spot prices") ///
           )


exit


**********************************************************************
*** Data analytics on return decomp **********************************
**********************************************************************

graph set window fontface "Times New Roman"
format date %tdm/Y


**********************************************************************
*** Line graph over total cumulative return **************************
**********************************************************************

// Histogram IRD vs CAR over time, cumulative 

	scatter sumHML sumHMLird, ///
    title("Cumulative IRD vs. Cumulative Carry Trade Return") ///
    xtitle("Cumulative IRD Return") ///
    ytitle("Cumulative Carry Trade Return") ///
    graphregion(margin(r=5)) ///
    xsize(10) ///
    ysize(6) ///
    xlabel(0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%" 2 "200%" 2.5 "250%") ///
    ylabel(0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%" 2 "200%" 2.5 "250%") ///
    legend(pos(11) ring(0) col(1) textfirst) ///
    legend(label(1 "IRD vs. Carry") label(2 "Identity Line")) ///
    msize(vsmall) ///
    || function y = x, range(0 2.5) lcolor(gray) lpattern(dash)

	

	
*****************************************************************
//Summary statistics of IRD and FX

//Carry component
gen maxsumHMLird = sumHMLird if _n == 2
replace maxsumHMLird = max(L.maxsumHMLird, sumHMLird) if _n > 2
gen DDird = (maxsumHMLird - sumHMLird)*-1
sum HMLird, detail

scalar yrlymeanird = period*r(mean)*100
scalar yrlysdird = sqrt(period)*r(sd)*100
scalar SRird = yrlymeanird / yrlysdird
scalar yrlyskewird = r(skewness)/sqrt(period)
scalar yrlykurtird = (r(kurtosis)-3)/period
scalar minird = r(min)*100 
scalar maxird = r(max)*100 

sum DDird, detail
scalar maxddird = r(min)*100 

// Store statistics in a row matrix
matrix statsird = (yrlymeanird, yrlysdird, SRird, yrlyskewird, yrlykurtird, maxddird, minird, maxird)
matrix rownames statsird = "IRD Stats"
matrix colnames statsird = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist statsird, format(%9.2f)	


//FX component
gen maxsumHMLfx = sumHMLfx if _n == 2
replace maxsumHMLfx = max(L.maxsumHMLfx, sumHMLfx) if _n > 2
gen DDfx = (maxsumHMLfx - sumHMLfx)*-1
sum HMLfx, detail

scalar yrlymeanfx = period*r(mean)*100
scalar yrlysdfx = sqrt(period)*r(sd)*100
scalar SRfx = yrlymeanfx / yrlysdfx
scalar yrlyskewfx = r(skewness)/sqrt(period)
scalar yrlykurtfx = (r(kurtosis)-3)/period
scalar minfx = r(min)*100 
scalar maxfx = r(max)*100 

sum DDfx, detail
scalar maxddfx = r(min)*100 

// Store statistics in a row matrix
matrix statsfx = (yrlymeanfx, yrlysdfx, SRfx, yrlyskewfx, yrlykurtfx, maxddfx, minfx, maxfx)
matrix rownames statsfx = "FX Stats"
matrix colnames statsfx = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist statsfx, format(%9.2f)	



// Histogram IRD
hist HMLird, bins(70) normal title("Histogram IRD Returns") xtitle("Return distribution") ytitle("Density") xlabel(0 "0" 0.005 "0.5%" 0.01 "1%"  0.015 "1.5%" 0.02 "2%")    

// Histogram FX
hist HMLfx, bins(70) normal title("Histogram FX Returns") xtitle("Return distribution") ytitle("Density")  xlabel(-0.1 "-10%" -0.05 "-5%" 0 "0" 0.05 "5%" 0.1 "10%") ylabel(0(5)30)

//Scatter plot of CAR vs IRD, monthly return observations	
graph set window fontface "Times New Roman"
format date %tdm/Y
scatter HML HMLfx HMLird date, ///
    title("Carry Trade Return vs. FX Return vs. IRD Return") ///
    xtitle("Date") ///
    ytitle("Return") ///
    graphregion(margin(r=5)) ///
    xsize(10) ///
    ysize(6) ///
    xlabel(#6, angle(horizontal)) ///
    ylabel(-0.1 "-10%" -0.05 "-5%" 0 "0%" 0.05 "5%" 0.1 "10%") ///
    legend(pos(11) ring(0) col(1) textfirst) ///
    legend(label(1 "Carry Trade Return") label(2 "FX Return") label(3 "IRD Return")) ///
    msize(vsmall vsmall vsmall)


//Scatter plot of CAR vs FX, monthly return observations	
scatter HML HMLfx, ///
    title("Carry Trade Return vs. FX Return") ///
    xtitle("FX Return") ///
    ytitle("Carry Trade Return") ///
    graphregion(margin(r=5)) ///
    xsize(10) ///
    ysize(6) ///
    xlabel(-0.1 "-10%" -0.05 "-5%" 0 "0%" 0.05 "5%" 0.1 "10%") ///
    ylabel(-0.1 "-10%" -0.05 "-5%" 0 "0%" 0.05 "5%" 0.1 "10%") ///
    legend(pos(11) ring(0) col(1) textfirst) ///
    legend(label(1 "Carry Trade Return") label(2 "FX Return") ) ///
    msize(vsmall vsmall)
		
//Scatter plot of development of monthly IRD returns over time (monthly carry)
	
	
graph set window fontface "Times New Roman"
format date %tdm/Y
scatter HMLird date, ///
    title("Development of IRD Returns") ///
    xtitle("Date") ///
    ytitle("Return") ///
    graphregion(margin(r=5)) ///
    xsize(10) ///
    ysize(6) ///
    xlabel(#6, angle(horizontal)) ///
    ylabel(0 "0%" 0.005 "0.5%" 0.01 "0.1%" 0.015 "0.15%" 0.02 "0.2%") ///
    msize(vsmall)
