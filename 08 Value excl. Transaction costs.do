***********************************************
*************** Value 3C 1M NC ****************
***********************************************
clear all
cd "C:\Users\nicol\CBS - Copenhagen Business School\MasterThesis N&J - General\04 - Data and Code\03 Stata\Most current code\01 Data"
use "Data_1M_1976.dta", clear
browse

merge 1:1 date using "Data_CPI.dta"
drop _merge

drop usdbid* usdask* fwusdbid* fwusdask*

// Generate fakedate and use as a time series 
gen fakedate = _n
order date fakedate
tsset fakedate

// Define parameters
scalar currencies = 9     // Total number of currency pairs
scalar top = 3            // Number of currencies in high portfolio
scalar bottom = 3         // Number of currencies in low portfolio
scalar period = 12        // Periods per year

// For comparable time horizon as Hutchinson
//drop if _n>532 



/////////////////////NOTE: here, we actually need the discrete (non-log) spot FX rates
// Calculate logs
/*forvalues i=1/`=currencies' {
	replace usdmid`i' = log(usdmid`i')
}*/


***********************************************
*************** Value Ranking *****************
***********************************************
//install rangestat package
ssc install rangestat, replace

// Generate rank of currency based on value measure (Asness & Hutchinson papers)
tsset fakedate

//Hutchinson, Footnote nb. 5, implement a 6-months lag to account for CPI data being published with a delay
forvalues i = 1/10 {
    gen cpi`i'_lag6 = L6.cpi`i'
}
//don't drop non-lagged cpi data as for the historical period, we don't want the lagged version (by then, the publishing delay is no issue anymore)


//Moving average for US CPI (cpi10)
rangestat (mean) cpi10, interval(fakedate -66 -54) 

// drop first 12 results of cpi10_mean
replace cpi10_mean = . if _n<67

forvalues i = 1/9 {

    //Create moving average over t-66 to t-54 for CPI of country i
    rangestat (mean) cpi`i', interval(fakedate -66 -54)
    replace cpi`i'_mean = . if _n<67 // NW ADD
    
	//Moving average FX (usdmid1 to usdmid9)
    rangestat (mean) usdmid`i', interval(fakedate -66 -54)
     replace usdmid`i'_mean = . if _n<67 // NW ADD
	
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


//Cross-sectional ranking. By this, the currencies with rank 7,8,9 are the most overvalued currencies, and rank 1,2,3 are the most undervalued. This means that we buy the USD against the currencies with VAL_rank 7,8,9 and we sell the USD against the currencies with VAL_rank 1,2,3.

forvalues i = 1/9 {
    gen rank_val`i' = 1 if val`i' !=.
    
    forvalues j = 1/9 {
        replace rank_val`i' = rank_val`i' + 1 if val`j' < val`i' & val`j' != .
    }
}


egen currmiss = rowmiss(val1-val9) if _n>66
gen currused = 9-currmiss if _n>66


// Calculate logs
forvalues i=1/`=currencies' {
	replace usdmid`i' = log(usdmid`i')

}

forvalues i=1/`=currencies' {
	replace fwusdmid`i' = log(fwusdmid`i')

}


forvalues i=1/`=top' {
	gen retlong`i' = .
}

forvalues i=1/`=bottom' {
	gen retshort`i' = .
}

forvalues j=1/`=top' {
	forvalues i=1/`=currencies' {
		replace retlong`j' = (usdmid`i' - L.fwusdmid`i') if L.rank_val`i' == `j'+(L.currused-`=top') 
}
}

forvalues j=1/`=bottom' {
	forvalues i=1/`=currencies' {
		replace retshort`j' = (L.fwusdmid`i' - usdmid`i') if L.rank_val`i' == `j'	
	}
}

// Reconvert to simple returns for PF return calculation
forvalues i=1/`=top' {
	replace retlong`i' = exp(retlong`i')-1
}

forvalues i=1/`=bottom' {
	replace retshort`i' = exp(retshort`i')-1
}
	

// now combine the returns. EQUAL WEIGHT
egen pflong = rowtotal(retlong*) if _n>67
replace pflong = 1/`=top' * pflong if _n>67

egen pfshort = rowtotal(retshort*) if _n>67
replace pfshort = 1/`=bottom' * pfshort if _n>67

// Calculate HML return and accumulative return
gen HML = pflong + pfshort if _n>67

// Reconvert to log returns
replace HML = log(1+HML)

gen sumHML = sum(HML) if _n>67

exit


**********************************************************************
*** Data analytics ***************************************************
**********************************************************************

graph set window fontface "Times New Roman"
format date %tdm/Y


**********************************************************************
*** Line graph over total cumulative return **************************
**********************************************************************

line sumHML date if _n>1 , title("Cumulative VAL returns") xtitle("Date") ytitle("Cumulative exess returns") ///
graphregion(margin(r=5)) xlabel(, angle(horizontal)) xsize(10) ysize(6) xlabel(#6) ylabel(0 "0%" 0.5 "50%" 1 "100%" 1.5 "150%" 2 "200%" 2.5 "250%")


**********************************************************************
*** Line Graph Maximum Drawdown **************************************
**********************************************************************

// Percentage drawdown (log returns)

gen maxsumHML = sumHML if _n == 2
replace maxsumHML = max(L.maxsumHML, sumHML) if _n > 2
gen DD = (maxsum - sumHML)*-1


summarize DD
local avg_dd = r(mean)
local avg_pct = string(`avg_dd'*100, "%9.1f")

line DD date if _n>1, title("Maximum percentage drawdown") xtitle("Date") ytitle("Drawdown") ///
graphregion(margin(r=5)) lcolor(red) xlabel(, angle(horizontal)) xsize(10) ysize(6) ///
xlabel(#6) ylabel(0 "0%" -0.1 "-10%" -0.2 "-20%" -0.3 "-30%" `avg_dd' "`avg_pct'%") ///
yline(`avg_dd', lpattern(dash) lcolor(gray) lwidth(normal))


**********************************************************************
*** Summary Statistics **********************************************
**********************************************************************

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
matrix rownames stats = "VAL"
matrix colnames stats = "Mean" "Std Dev" "SR" "Skewness" "Kurtosis" "DD" "Min" "Max"

matlist stats, format(%9.2f)


hist HML, bins(70) normal title("Histogram Currency Value Returns") xtitle("Return distribution") ytitle("Density")  xlabel(-0.1 "-10%" -0.05 "-5%" 0 "0" 0.05 "5%" 0.1 "10%") ylabel(0(5)30)
