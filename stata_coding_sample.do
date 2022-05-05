//This code is seeking to replicate the results of Angrist and Evans 1998 study "Children and Their Parents' Labor Supply"

clear
cap clear matrix
set more off
set matsize 100 /* matsize relates to the max # variables you can have in your dataset */

global location "/Users/victorporcelli/Desktop/AEM/Stata Work/Replication Excercise"

* change to your directory
cd "$location"
capture log close
log using "replication_exercise.log", replace

use "raw pums80 slim.dta", clear
*describe
*summ

*First: create a dataset of mothers
*since the dataset will be refined later, for now include all mothers (women who have had a child)
*for mothers, mom_id will be equal to their pernum
gen mom_id = 0
replace mom_id = us80a_pernum if us80a_sex == 2 & us80a_chborn > 1
keep if mom_id != 0

save "mothers.dta", replace

use "raw pums80 slim.dta", clear
*Second: create a dataset of spouses
*since the dataset will be refined later, for now include all spouses
*for spouses, mom_id will be equal to their sploc
gen mom_id = 0
replace mom_id = us80a_sploc if us80a_sex == 1
keep if mom_id != 0 & us80a_qage == 0

*generate variables needed/rename them for use later
gen age_husb = us80a_age
gen hrswork_husb = us80a_uhrswork
gen wkswork_husb = us80a_wkswork1
gen incwage_husb = us80a_incwage * 2.099173554
gen birthqtr_husb = us80a_birthqtr

*recode race variables
gen black_husb = (us80a_race == 3)
gen white_husb = (us80a_race == 1)
gen hispanic_husb = (us80a_hispan > 0) & (us80a_race != 1 & us80a_race != 3)
gen race_other_husb = (black == 0 & white == 0 & hispanic == 0)

keep us80a_serial mom_id age_husb hrswork_husb wkswork_husb incwage_husb black_husb ///
white_husb hispanic_husb race_other_husb birthqtr_husb

save "spouses.dta", replace

use "raw pums80 slim.dta", clear
*generate a mom_id for children that will match their mother's mom_id
bys us80a_serial: gen mom_id = us80a_momloc

*drop all of those without a mother in the dataset
keep if mom_id != 0

*next, generate a birth order variable where youngest children have the lowest birth order
bys us80a_serial mom_id (us80a_age): gen birth_order = _n

*correct birth order for same-age children using birthqtr
bys us80a_serial mom_id (us80a_age us80a_birthqtr): replace birth_order = birth_order[_n+1] if us80a_age == us80a_age[_n+1] & birth_order < birth_order[_n+1]
bys us80a_serial mom_id (us80a_age us80a_birthqtr): replace birth_order = birth_order - 1 if us80a_age == us80a_age[_n-1] & birth_order == birth_order[_n-1]

*make it so the oldest child will have the lowest birth order
replace birth_order = birth_order * -1

*replace birth order with 1, 2, 3 for first, second, third child ...
bys us80a_serial mom_id (birth_order): replace birth_order = _n

*generate a variable for number of children per mom to cross-reference with chborn
gen num_children = 999

/*num_children is the number of observations per mom_id but since chborn = 1 indicates 
0 children,  2 indicates 1, etc. add 1 to _N to match chborn. */
bys us80a_serial mom_id: replace num_children = _N+1

*remove those where first or second child age or sex is allocated
gen flag = 0
replace flag = -1 if ((birth_order == 1 | birth_order == 2) & (us80a_qsex != 0 | us80a_qage != 0 | us80a_qbirthmo !=0))

bys us80a_serial mom_id (flag): replace flag = flag[1]

*only keep the first three children of non-flagged households
keep if flag != -1 & birth_order > 0 & birth_order <4

*keep relevant variables and then reshape the dataset
keep us80a_serial us80a_age us80a_sex us80a_birthqtr birth_order mom_id num_children
reshape wide us80a_age us80a_sex us80a_birthqtr, i(us80a_serial mom_id) j(birth_order)

*generate vars for: twins_2, boy_first, boy_second, two_boys, two_girls, same_sex
gen twins_2 = 0
replace twins_2 = 1 if us80a_birthqtr2 == us80a_birthqtr3 & us80a_age2 == us80a_age3

gen boy_first = 0
replace boy_first = 1 if us80a_sex1 == 1

gen boy_second = 0
replace boy_second = 1 if us80a_sex2 == 1

gen two_boys = 0
replace two_boys = 1 if boy_first == 1 & boy_second == 1

gen two_girls = 0
replace two_girls = 1 if boy_first == 0 & boy_second == 0

gen same_sex = 0
replace same_sex = 1 if two_boys == 1 | two_girls == 1

keep us80a_serial mom_id us80a_age1 us80a_age2 us80a_age3 us80a_sex1 us80a_sex2 us80a_birthqtr1 ///
us80a_sex3 twins_2 boy_first boy_second two_boys two_girls same_sex num_children

*now merge the mothers data by matching mom_ids
merge 1:m us80a_serial mom_id using "mothers.dta"

/* Only include mothers of 2+ children, aged 21-35 whose first-born is younger than
18 and second-born is older than 0, and whose num_children matches their chborn. */
keep if _merge == 3 & (us80a_age >= 21 & us80a_age <= 35) & us80a_chborn > 2 ///
& us80a_age1 < 18 & us80a_age2 > 0 & num_children == us80a_chborn

*correct the chborn variable to actual represent the number of children ever born
replace us80a_chborn = us80a_chborn-1

*generate variables for Table 2
gen more_than_2 = 0
replace more_than_2 = 1 if us80a_chborn > 2
gen age_first_birth = us80a_age - us80a_age1
gen worked_for_pay = 0
replace worked_for_pay = 1 if us80a_wkswork1 != 0

*recode race variables
gen black = (us80a_race == 3)
gen white = (us80a_race == 1)
gen hispanic = (us80a_hispan > 0) & (us80a_race != 1 & us80a_race != 3)
gen race_other = (black == 0 & white == 0 & hispanic == 0)

*put income in 1995 dollars
replace us80a_incwage = us80a_incwage * 2.099173554
replace us80a_ftotinc = us80a_ftotinc * 2.099173554

label var us80a_chborn "Children ever born"
label var more_than_2 "More than 2 children (=1 if mother had more than 2 children, =0 otherwise)"
label var boy_first "Boy 1st (=1 if first child was a boy)"
label var boy_second "Boy 2nd (=1 if second child was a boy)"
label var two_boys "Two Boys (=1 if first two children were boys)"
label var two_girls "Two Girls (=1 if first two children were girls)"
label var same_sex "Same sex (=1 if first two children were the same sex)"
label var twins_2 "Twins-2 (=1 if second birth was a twin)"
label var us80a_age "Age"
label var age_first_birth "Age at first birth (parent's age in years when first child was born)"
label var worked_for_pay "Worked for pay (=1 if worked for pay in year prior to census)"
label var us80a_wkswork1 "Weeks worked (weeks worked in year prior to census)"
label var us80a_uhrswork "Hours/week (average hours worked per week)"
label var us80a_incwage "Labor income (labor earnings in year prior to census, in 1995 dollars)"
label var us80a_ftotinc "Family income (family income in year prior to census, in 1995 dollars)"

*log family income
replace us80a_ftotinc = 1 if us80a_ftotinc <= 0
gen faminc_log = ln(us80a_ftotinc)
label var faminc_log "ln(Family Income)"

save "mothers_and_kids.dta", replace

/*make the dataset for married women given that they were married at the time of the
census, married once, and married when their first child was born */
keep if us80a_marrno == 1 & us80a_marst == 1 & (us80a_agemarr < age_first_birth | ///
(us80a_agemarr == age_first_birth & us80a_marrqtr < us80a_birthqtr1))

*now merge with the spouses dataset by matching mom_ids
capture drop _merge
merge 1:m us80a_serial mom_id using "spouses.dta"
keep if _merge == 3

gen nonwife_inc = us80a_ftotinc - us80a_incwage
gen age_fb_husb = age_husb - us80a_age1
gen wfp_husb = 0
replace wfp_husb = 1 if wkswork_husb != 0

label var nonwife_inc "Non-wife income (family income minus wife's labor income, in 1995 dollars)"

*log non-wife income
replace nonwife_inc = 1 if nonwife_inc <= 0
gen nwinc_log = ln(nonwife_inc)
label var nwinc_log "ln(Non-wife Income)"

save "married_and_kids.dta", replace

*TABLE 2: SUMMARY STATISTICS

use "mothers_and_kids.dta", clear
*create temporary variables for those that overlap with husbands (age, afb, wfp, wksworked, hrsperweek, laborinc)
gen age_temp = us80a_age
gen afb_temp = age_first_birth
gen wfp_temp = worked_for_pay
gen wkswork_temp = us80a_wkswork1
gen hrsper_temp = us80a_uhrswork
gen labinc_temp = us80a_incwage

label var age_temp "Age"
label var afb_temp "Age at first birth (parent's age in years when first child was born)"
label var wfp_temp "Worked for pay (=1 if worked for pay in year prior to census)"
label var wkswork_temp "Weeks worked (weeks worked in year prior to census)"
label var hrsper_temp "Hours/week (average hours worked per week)"
label var labinc_temp "Labor income (labor earnings in year prior to census, in 1995 dollars)"

regress us80a_chborn more_than_2 boy_first boy_second two_boys two_girls same_sex ///
twins_2 age_temp afb_temp wfp_temp wkswork_temp hrsper_temp labinc_temp us80a_ftotinc

outreg2 using "Table 2.doc", replace sortvar(us80a_chborn more_than_2 ///
boy_first boy_second two_boys two_girls same_sex twins_2 age_temp afb_temp wfp_temp wkswork_temp ///
hrsper_temp labinc_temp us80a_ftotinc) sum ctitle("All Women") see label ///
title("Table 2-Descriptive Statistics, Women Aged 21-35 With 2 or More Children") addnote("{\i Notes:} The samples include women aged 21-35 with two or more children except for women whose second child is less than a year old. Married women refers to women who were married at the time of their first birth, married at the time of the survey, and married once.")

capture drop age_temp afb_temp wfp_temp wkswork_temp hrsper_temp labinc_temp

use "married_and_kids.dta", clear
*create temporary variables for those that overlap with husbands (age, afb, wfp, wksworked, hrsperweek, laborinc)
gen age_temp = us80a_age
gen afb_temp = age_first_birth
gen wfp_temp = worked_for_pay
gen wkswork_temp = us80a_wkswork1
gen hrsper_temp = us80a_uhrswork
gen labinc_temp = us80a_incwage

label var age_temp "Age"
label var afb_temp "Age at first birth (parent's age in years when first child was born)"
label var wfp_temp "Worked for pay (=1 if worked for pay in year prior to census)"
label var wkswork_temp "Weeks worked (weeks worked in year prior to census)"
label var hrsper_temp "Hours/week (average hours worked per week)"
label var labinc_temp "Labor income (labor earnings in year prior to census, in 1995 dollars)"

regress us80a_chborn more_than_2 boy_first boy_second two_boys two_girls same_sex ///
twins_2 age_temp afb_temp wfp_temp wkswork_temp hrsper_temp labinc_temp ///
us80a_ftotinc nonwife_inc

outreg2 using "Table 2.doc", append sortvar(us80a_chborn more_than_2 ///
boy_first boy_second two_boys two_girls same_sex twins_2 age_temp afb_temp wfp_temp wkswork_temp ///
hrsper_temp labinc_temp us80a_ftotinc nonwife_inc) sum ctitle("Wives") see label 

replace age_temp = age_husb
replace afb_temp = age_fb_husb
replace wfp_temp = wfp_husb
replace wkswork_temp = wkswork_husb
replace hrsper_temp = hrswork_husb
replace labinc_temp = incwage_husb

regress age_temp afb_temp wfp_temp wkswork_temp hrsper_temp labinc_temp

outreg2 using "Table 2.doc", append sortvar(us80a_chborn more_than_2 ///
boy_first boy_second two_boys two_girls same_sex twins_2 age_temp afb_temp wfp_temp wkswork_temp ///
hrsper_temp labinc_temp us80a_ftotinc nonwife_inc) sum ctitle("Husbands") see label ///

 
capture drop age_temp afb_temp wfp_temp wkswork_temp hrsper_temp labinc_temp

*TABLE 6: FIRST STAGE REGRESSIONS

use "mothers_and_kids.dta", clear
*simple regression first
reg more_than_2 same_sex, r
outreg2 using "Table 6.doc", replace ctitle(" ") drop(more_than_2) noaster dec(4) addnote("{\i Notes:} Other covariates in the models are indicators for {\i Age}, {\i Age at first birth}, {\i Black}, {\i Hispanic}, and {\i Other Race}. The variable {\i Boy 2nd} is excluded from columns (3) and (6). Standard errors are reported in parentheses.") ///
sortvar(boy_first boy_second same_sex two_boys) see label addtext(With other covariates, no) nonotes ///
title("Table 6-OLS Estimates of {\i More Than 2 Children} Equations") noobs nocons

*add covariates
reg more_than_2 boy_first boy_second same_sex us80a_age age_first_birth black hispanic race_other, r
outreg2 using "Table 6.doc", append ctitle("All Women") drop(more_than_2 us80a_age age_first_birth black hispanic race_other) ///
sortvar(boy_first boy_second same_sex two_boys) see label addtext(With other covariates, yes) noaster dec(4) noobs nocons

*specification with two boys/two girls instead of same_sex
reg more_than_2 boy_first two_boys two_girls us80a_age age_first_birth black hispanic race_other, r
outreg2 using "Table 6.doc", append ctitle(" ") drop(more_than_2 us80a_age age_first_birth black hispanic race_other) ///
sortvar(boy_first boy_second same_sex two_boys) see label addtext(With other covariates, yes) noaster dec(4) noobs nocons

use "married_and_kids.dta", clear
*simple regression first
reg more_than_2 same_sex, r
outreg2 using "Table 6.doc", append ctitle(" ") drop(more_than_2) sortvar(boy_first boy_second same_sex two_boys) ///
see label addtext(With other covariates, no) noaster dec(4) noobs nocons

*add covariates
reg more_than_2 boy_first boy_second same_sex us80a_age age_first_birth black hispanic race_other, r
outreg2 using "Table 6.doc", append ctitle("Married Women") drop(more_than_2 us80a_age age_first_birth black hispanic race_other) ///
sortvar(boy_first boy_second same_sex two_boys) see label addtext(With other covariates, yes) noaster dec(4) noobs nocons

*specification with two boys/two girls instead of same_sex
reg more_than_2 boy_first two_boys two_girls us80a_age age_first_birth black hispanic race_other, r
outreg2 using "Table 6.doc", append ctitle(" ") drop(more_than_2 us80a_age age_first_birth black hispanic race_other) ///
sortvar(boy_first boy_second same_sex two_boys) see label addtext(With other covariates, yes) ///
noaster dec(4) noobs nocons

*TABLE 7: IV REGRESSIONS

*set up manual output of the data using putexcel 
putexcel set "/Users/victorporcelli/Desktop/AEM/Stata Work/Replication Excercise/Table 7.xlsx", replace

*set up the table
putexcel (B1:J1) = "Table 7-OLS AND 2SLS ESTIMATES OF LABOR-SUPPLY MODELS USING 1980 CENSUS DATA", merge hcenter vcenter
putexcel (B3:D3) = "All women", merge hcenter vcenter
putexcel (E3:G3) = "Married women", merge hcenter vcenter
putexcel (H3:J3) = "Husbands of married women", merge hcenter vcenter

putexcel B4 = "(1)", hcenter vcenter
putexcel C4 = "(2)", hcenter vcenter
putexcel D4 = "(3)", hcenter vcenter
putexcel E4 = "(4)", hcenter vcenter
putexcel F4 = "(5)", hcenter vcenter
putexcel G4 = "(6)", hcenter vcenter
putexcel H4 = "(7)", hcenter vcenter
putexcel I4 = "(8)", hcenter vcenter
putexcel J4 = "(9)", hcenter vcenter

putexcel A5 = "Estimation method", left
putexcel A7 = "Instrument for More Than 2 Children", left txtwrap
putexcel A9 = "Dependent variable:", left
putexcel A10 = "Worked for pay", left txtindent(2) italic 
putexcel A13 = "Weeks worked", left txtindent(2) italic 
putexcel A16 = "Hours/week", left txtindent(2) italic 
putexcel A19 = "Labor income", left txtindent(2) italic 
putexcel A22 = "ln(Family income)", left italic 
putexcel A25 = "ln(Non-wife income)", left italic

putexcel H22 = "--", hcenter vcenter
putexcel I22 = "--", hcenter vcenter
putexcel J22 = "--", hcenter vcenter

putexcel B25 = "--", hcenter vcenter
putexcel C25 = "--", hcenter vcenter
putexcel D25 = "--", hcenter vcenter
putexcel H25 = "--", hcenter vcenter
putexcel I25 = "--", hcenter vcenter
putexcel J25 = "--", hcenter vcenter

putexcel B5 = "OLS", hcenter vcenter
putexcel E5 = "OLS", hcenter vcenter
putexcel H5 = "OLS", hcenter vcenter

putexcel C5 = "2SLS", hcenter vcenter
putexcel D5 = "2SLS", hcenter vcenter
putexcel F5 = "2SLS", hcenter vcenter
putexcel G5 = "2SLS", hcenter vcenter
putexcel I5 = "2SLS", hcenter vcenter
putexcel J5 = "2SLS", hcenter vcenter

putexcel B7 = "--", hcenter vcenter
putexcel E7 = "--", hcenter vcenter
putexcel H7 = "--", hcenter vcenter

putexcel C7 = "Same sex", hcenter italic
putexcel F7 = "Same sex", hcenter italic
putexcel I7 = "Same sex", hcenter italic

putexcel D7 = "Two boys, Two girls", hcenter italic txtwrap
putexcel G7 = "Two boys, Two girls", hcenter italic txtwrap
putexcel J7 = "Two boys, Two girls", hcenter italic txtwrap

use "mothers_and_kids.dta", clear
*first without an instrument
reg worked_for_pay more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel B10 = _b[more_than_2], hcenter vcenter
putexcel B11 = _se[more_than_2], hcenter vcenter

reg us80a_wkswork1 more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel B13 = _b[more_than_2], hcenter vcenter
putexcel B14 = _se[more_than_2], hcenter vcenter

reg us80a_uhrswork more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel B16 = _b[more_than_2], hcenter vcenter
putexcel B17 = _se[more_than_2], hcenter vcenter

reg us80a_incwage more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel B19 = _b[more_than_2], hcenter vcenter
putexcel B20 = _se[more_than_2], hcenter vcenter

reg faminc_log more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel B22 = _b[more_than_2], hcenter vcenter
putexcel B23 = _se[more_than_2], hcenter vcenter

*now with same_sex as an instrument
ivregress 2sls worked_for_pay (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel C10 = _b[more_than_2], hcenter vcenter
putexcel C11 = _se[more_than_2], hcenter vcenter

ivregress 2sls us80a_wkswork1 (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel C13 = _b[more_than_2], hcenter vcenter
putexcel C14 = _se[more_than_2], hcenter vcenter

ivregress 2sls us80a_uhrswork (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel C16 = _b[more_than_2], hcenter vcenter
putexcel C17 = _se[more_than_2], hcenter vcenter

ivregress 2sls us80a_incwage (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel C19 = _b[more_than_2], hcenter vcenter
putexcel C20 = _se[more_than_2], hcenter vcenter

ivregress 2sls faminc_log (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel C22 = _b[more_than_2], hcenter vcenter
putexcel C23 = _se[more_than_2], hcenter vcenter

*now with two boys, two girls as an instrument
ivregress 2sls worked_for_pay (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel D10 = _b[more_than_2], hcenter vcenter
putexcel D11 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel D12 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls us80a_wkswork1 (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel D13 = _b[more_than_2], hcenter vcenter
putexcel D14 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel D15 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls us80a_uhrswork (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel D16 = _b[more_than_2], hcenter vcenter
putexcel D17 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel D18 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls us80a_incwage (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel D19 = _b[more_than_2], hcenter vcenter
putexcel D20 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel D21 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls faminc_log (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel D22 = _b[more_than_2], hcenter vcenter
putexcel D23 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel D24 = overid_pscore, hcenter vcenter
capture drop overid_pscore

use "married_and_kids.dta", clear
*first do the regressions for mothers

*first without an instrument
reg worked_for_pay more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel E10 = _b[more_than_2], hcenter vcenter
putexcel E11 = _se[more_than_2], hcenter vcenter

reg us80a_wkswork1 more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel E13 = _b[more_than_2], hcenter vcenter
putexcel E14 = _se[more_than_2], hcenter vcenter

reg us80a_uhrswork more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel E16 = _b[more_than_2], hcenter vcenter
putexcel E17 = _se[more_than_2], hcenter vcenter

reg us80a_incwage more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel E19 = _b[more_than_2], hcenter vcenter
putexcel E20 = _se[more_than_2], hcenter vcenter

reg faminc_log more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel E22 = _b[more_than_2], hcenter vcenter
putexcel E23 = _se[more_than_2], hcenter vcenter

reg nwinc_log more_than_2 boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel E25 = _b[more_than_2], hcenter vcenter
putexcel E26 = _se[more_than_2], hcenter vcenter

*now with same_sex as an instrument
ivregress 2sls worked_for_pay (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel F10 = _b[more_than_2], hcenter vcenter
putexcel F11 = _se[more_than_2], hcenter vcenter

ivregress 2sls us80a_wkswork1 (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel F13 = _b[more_than_2], hcenter vcenter
putexcel F14 = _se[more_than_2], hcenter vcenter

ivregress 2sls us80a_uhrswork (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel F16 = _b[more_than_2], hcenter vcenter
putexcel F17 = _se[more_than_2], hcenter vcenter

ivregress 2sls us80a_incwage (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel F19 = _b[more_than_2], hcenter vcenter
putexcel F20 = _se[more_than_2], hcenter vcenter

ivregress 2sls faminc_log (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel F22 = _b[more_than_2], hcenter vcenter
putexcel F23 = _se[more_than_2], hcenter vcenter

ivregress 2sls nwinc_log (more_than_2 = same_sex) boy_first boy_second us80a_age age_first_birth black hispanic race_other, r
putexcel F25 = _b[more_than_2], hcenter vcenter
putexcel F26 = _se[more_than_2], hcenter vcenter

*now with two boys, two girls as an instrument
ivregress 2sls worked_for_pay (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel G10 = _b[more_than_2], hcenter vcenter
putexcel G11 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel G12 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls us80a_wkswork1 (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel G13 = _b[more_than_2], hcenter vcenter
putexcel G14 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel G15 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls us80a_uhrswork (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel G16 = _b[more_than_2], hcenter vcenter
putexcel G17 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel G18 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls us80a_incwage (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel G19 = _b[more_than_2], hcenter vcenter
putexcel G20 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel G21 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls faminc_log (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel G22 = _b[more_than_2], hcenter vcenter
putexcel G23 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel G24 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls nwinc_log (more_than_2 = two_boys two_girls) boy_first us80a_age age_first_birth black hispanic race_other, r
putexcel G25 = _b[more_than_2], hcenter vcenter
putexcel G26 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel G27 = overid_pscore, hcenter vcenter
capture drop overid_pscore

*now do the regressions for husbands

*first without an instrument
reg wfp_husb more_than_2 boy_first boy_second age_husb age_fb_husb black_husb hispanic_husb race_other_husb, r
putexcel H10 = _b[more_than_2], hcenter vcenter
putexcel H11 = _se[more_than_2], hcenter vcenter

reg wkswork_husb more_than_2 boy_first boy_second age_husb age_fb_husb black_husb hispanic_husb race_other_husb, r
putexcel H13 = _b[more_than_2], hcenter vcenter
putexcel H14 = _se[more_than_2], hcenter vcenter

reg hrswork_husb more_than_2 boy_first boy_second age_husb age_fb_husb black_husb hispanic_husb race_other_husb, r
putexcel H16 = _b[more_than_2], hcenter vcenter
putexcel H17 = _se[more_than_2], hcenter vcenter

reg incwage_husb more_than_2 boy_first boy_second age_husb age_fb_husb black_husb hispanic_husb race_other_husb, r
putexcel H19 = _b[more_than_2], hcenter vcenter
putexcel H20 = _se[more_than_2], hcenter vcenter

*now with same_sex as an instrument
ivregress 2sls wfp_husb (more_than_2 = same_sex) boy_first boy_second age_husb age_fb_husb black_husb hispanic_husb race_other_husb, r
putexcel I10 = _b[more_than_2], hcenter vcenter
putexcel I11 = _se[more_than_2], hcenter vcenter

ivregress 2sls wkswork_husb (more_than_2 = same_sex) boy_first boy_second age_husb age_fb_husb black_husb hispanic_husb ///
race_other_husb, r
putexcel I13 = _b[more_than_2], hcenter vcenter
putexcel I14 = _se[more_than_2], hcenter vcenter

ivregress 2sls hrswork_husb (more_than_2 = same_sex) boy_first boy_second age_husb age_fb_husb black_husb hispanic_husb race_other_husb, r
putexcel I16 = _b[more_than_2], hcenter vcenter
putexcel I17 = _se[more_than_2], hcenter vcenter

ivregress 2sls incwage_husb (more_than_2 = same_sex) boy_first boy_second age_husb age_fb_husb black_husb hispanic_husb race_other_husb, r
putexcel I19 = _b[more_than_2], hcenter vcenter
putexcel I20 = _se[more_than_2], hcenter vcenter

*now with two boys, two girls as an instrument
ivregress 2sls wfp_husb (more_than_2 = two_boys two_girls) boy_first age_husb age_fb_husb black_husb hispanic_husb race_other_husb, r
putexcel J10 = _b[more_than_2], hcenter vcenter
putexcel J11 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel J12 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls wkswork_husb (more_than_2 = two_boys two_girls) boy_first age_husb age_fb_husb black_husb hispanic_husb ///
race_other_husb, r
putexcel J13 = _b[more_than_2], hcenter vcenter
putexcel J14 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel J15 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls hrswork_husb (more_than_2 = two_boys two_girls) boy_first age_husb age_fb_husb black_husb hispanic_husb ///
race_other_husb, r
putexcel J16 = _b[more_than_2], hcenter vcenter
putexcel J17 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel J18 = overid_pscore, hcenter vcenter
capture drop overid_pscore

ivregress 2sls incwage_husb (more_than_2 = two_boys two_girls) boy_first age_husb age_fb_husb black_husb hispanic_husb ///
race_other_husb, r
putexcel J19 = _b[more_than_2], hcenter vcenter
putexcel J20 = _se[more_than_2], hcenter vcenter

estat overid
scalar overid_pscore = r(p_score)
putexcel J21 = overid_pscore, hcenter vcenter

*add note
putexcel (A28:J28) = "Notes: The table reports estimates of the coefficient on the More than 2 children variable in equations (4) and (6) in the text. Other covariates in the models are Age, Age at first birth, plus indicators for Boy 1st, Boy 2nd, Black, Hispanic, and Other race. The variable Boy 2nd is excluded from equation (6). The p-value for the test of overidentifying restrictions associated with equation (6) is shown in brackets. Standard errors are reported in parentheses.", merge txtwrap

putexcel save

capture log close
