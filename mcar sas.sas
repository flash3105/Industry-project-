
FILENAME REFFILE '/home/u63812148/sasuser.v94/MVA880/Capstone project/business_risk_data_dictionary_final.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=data_dictionary
	replace;
	GETNAMES=YES;
RUN;


FILENAME REFFILE '/home/u63812148/sasuser.v94/MVA880/Capstone project/business_risk_unlabeled_capstone_data_final.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=data_final
	replace;
	GETNAMES=Yes;
	DATAROW=3;
RUN;

proc contents data=data_final;
run;

proc means data=data_final nmiss;
run;


proc sort data=data_final 
          out=cleaned_data 
          nodupkey 
          dupout=only_the_duplicates;
    by client_id; 
run; /* duplicates are removed here (0 duplicates?) */

proc means data=data_final n nmiss min max range;
run;

/* 3041 missing values */
/* 206 with 2+ missing values*/
/* 11 with 3+ missing*/
/* 0 with 4+ missing */
data temp_missing;
    set data_final;
    if cmiss(of _all_) > 0;
run;

data data_final;
    set data_final;
    region = propcase(region);
run; /* capitilize */

data data_final;
    set data_final;
    if region = "gauteng" then region = "Gauteng";
    if region = "Free-State" then region = "Free State";
    if region = "Mpum." then region = "Mpumalanga";
    if region = "Kzn" then region = "Kwazulu-Natal";
run;

proc freq data=data_final nlevels;
    tables region / nocum nopercent;
run; 

data data_final;
    set data_final;
    employee_band = propcase(employee_band);
run; /* capitilize */

data data_final;
    set data_final;
    if employee_band = "Med" then employee_band = "Medium";
run;
proc freq data=data_final nlevels;
    tables employee_band / nocum nopercent;
run; 
data data_final;
    set data_final;
    if sector = '' then sector = "Other";
    if acquisition_channel = '' then acquisition_channel = "Other";
run;
proc freq data=data_final nlevels;
    tables acquisition_channel / nocum nopercent;
run; 
proc freq data=data_final nlevels;
    tables sector / nocum nopercent;
run; 

/* littles Test for MCAR */


/* DEFINE ALL MACRO VARIABLES GLOBALLY BEFORE THE MACRO */
%let testvars = annual_revenue_m debt_to_assets current_ratio quick_ratio ebit_margin
cash_conversion_days overdue_30d_rate utilisation_rate txn_volatility chargeback_rate
complaint_rate device_change_rate late_filing_count employee_growth web_traffic_growth 
avg_txn_value monthly_txn_count digital_share cash_share supplier_concentration
customer_concentration years_on_book firm_age_years legacy_risk_score exposure_band_index
client_engagement_score operational_complexity_index days_since_last_review onboarding_year;
%let misscode = .;
%let numvars = %sysfunc(countw(&testvars));

%macro mcartest;

/*******************************/
/* DO NOT ALTER THE CODE BELOW */
/*******************************/
data one;
    set data_final;
    array m[&numvars] &testvars;
    array r[&numvars] r1 - r&numvars;
    do i = 1 to &numvars;
        if m[i] = &misscode then m[i] = .;
    end;
    drop i;
    do i = 1 to &numvars;
        r[i] = 1;
        if m[i] = . then r[i] = 0;
    end;
    drop i;
run;

proc sort data=one;
    by r1-r&numvars;
run;

/* proc mi data=one nimpute=0 noprint; */
/*     var &testvars; */
/*     em outem=emcov; */
/* run; */

proc mi data=one nimpute=0 noprint;
    var annual_revenue_m debt_to_assets current_ratio quick_ratio ebit_margin
cash_conversion_days overdue_30d_rate utilisation_rate txn_volatility chargeback_rate
complaint_rate device_change_rate late_filing_count employee_growth web_traffic_growth 
avg_txn_value monthly_txn_count digital_share cash_share supplier_concentration
customer_concentration years_on_book firm_age_years legacy_risk_score exposure_band_index
client_engagement_score operational_complexity_index days_since_last_review onboarding_year;
    em outem=emcov;
run;

proc iml;
use one;
/* read all var {&testvars} into y; */
/* read all var {quick_ratio ebit_margin employee_growth web_traffic_growth} into y; */
read all var {annual_revenue_m debt_to_assets current_ratio quick_ratio ebit_margin
cash_conversion_days overdue_30d_rate utilisation_rate txn_volatility chargeback_rate
complaint_rate device_change_rate late_filing_count employee_growth web_traffic_growth 
avg_txn_value monthly_txn_count digital_share cash_share supplier_concentration
customer_concentration years_on_book firm_age_years legacy_risk_score exposure_band_index
client_engagement_score operational_complexity_index days_since_last_review onboarding_year} into y;
read all var {%do i = 1 %to &numvars; r&i %end;} into r;
use emcov;
/* read all var {&testvars} into em; */
/* read all var {quick_ratio ebit_margin employee_growth web_traffic_growth} into em; */
read all var {annual_revenue_m debt_to_assets current_ratio quick_ratio ebit_margin
cash_conversion_days overdue_30d_rate utilisation_rate txn_volatility chargeback_rate
complaint_rate device_change_rate late_filing_count employee_growth web_traffic_growth 
avg_txn_value monthly_txn_count digital_share cash_share supplier_concentration
customer_concentration years_on_book firm_age_years legacy_risk_score exposure_band_index
client_engagement_score operational_complexity_index days_since_last_review onboarding_year} into em;
mu = em[1,];
sigma = em[2:nrow(em),];

/* ASSIGN AN INDEX VARIABLE DENOTING EACH CASE'S PATTERN */
jcol = j(nrow(y), 1, 1);
do i = 2 to nrow(y);
    rdiff = r[i,] - r[i - 1,];
    if max(rdiff) = 0 & min(rdiff) = 0 then jcol[i,] = jcol[i - 1,];
    else jcol[i,] = jcol[i - 1,] + 1;
end;

/* NUMBER OF DISTINCT MISSING DATA PATTERNS */
j = max(jcol);

/* PUT THE NUMBER OF CASES IN EACH PATTERN IN A COL VECTOR M */
/* PUT THE MISSING DATA INDICATORS FOR EACH PATTERN IN A MATRIX RJ */
m = j(j, 1, 0);
rj = j(j, ncol(r), 0);
do i = 1 to j;
    count = 0;
    do k = 1 to nrow(y);
        if jcol[k,] = i then do;
            count = count + 1;
        end;
        if jcol[k,] = i & count = 1 then rj[i,] = r[k,];
        m[i,] = count;
    end;
end;

/* COMPUTE D^2 STATISTIC FOR EACH J PATTERN */
d2j = j(j, 1, 0);
do i = 1 to j;
    yj = y[loc(jcol = i), loc(rj[i,] = 1)];
    ybarobsj = yj[+,] / nrow(yj);
    Dj = j(ncol(y), rj[i,+], 0);
    count = 1;
    do k = 1 to ncol(rj);
        if rj[i,k] = 1 then do;
            Dj[k, count] = 1;
            count = count + 1;
        end;
    end;
    muobsj = mu * Dj;
    sigmaobsj = t(Dj) * sigma * Dj;
    d2j[i,] = m[i,] * (ybarobsj - muobsj) * inv(sigmaobsj) * t(ybarobsj - muobsj);
end;

/* THE D^2 STATISTIC */
d2 = d2j[+,];
/* DF FOR D^2 */
df = rj[+,+] - ncol(rj);
p = 1 - probchi(d2, df);

/* PRINT ANALYSIS RESULTS */
file print;
put "Number of Observed Variables = " (ncol(rj)) 3.0;
put "Number of Missing Data Patterns = " (j) 3.0; put;
put "Summary of Missing Data Patterns (0 = Missing, 1 = Observed)"; put;
put "Frequency | Pattern | d2j"; put;
do i = 1 to nrow(rj);
    put (m[i,]) 6.0 "    | " @;
    do j = 1 to ncol(rj);
        put (rj[i,j]) 2.0 @;
    end;
    put " | " (d2j[i,]) 8.6;
end;
put;
put "Sum of the Number of Observed Variables Across Patterns (Sigma psubj) = " (rj[+,+]) 5.0; put;
put "Little's (1988) Chi-Square Test of MCAR"; put;
put "Chi-Square (d2)      = " (d2) 10.3;
put "df (Sigma psubj - p) =    " (df) 7.0;
put "p-value              = " (p) 10.3;
quit;

%mend mcartest;
%mcartest;
run;
/* reject so not MCAR */
/* should probably then assume MAR */






