/* 
############################################################################
This is a program that will generate primary outcome results for Kelly Shaffer's R21 Project

The final data contain N = 100

*/

** Change working directory
cd "/xxx/"

** read in data
use "/xxx/SHUTi CARE_all data_final_new.dta", clear

** #############  Table 1. Sample Descriptives  ##################

* ######################################################################
** Generate caregiver (participant) summary section of the Table 1
* ######################################################################

** Get data ready

* Degree of engagement
gen usertype = (engagement=="Completer") // 1 = complete users
replace usertype = 2 if engagement == "Incompleter" // 2 = incomplete users
replace usertype = 3 if engagement == "Non-user"	// 3 =  non-users

* Age
gen age = age_T1

* Gender
gen gender = (gender_r=="Woman") // 1 = Female
replace gender = 2 if gender_r=="Man" // 2 = Male
replace gender = 3 if gender_r=="Genderqueer / Non-binary" | gender_r == "other" // 3 = other

* Race
gen race = (race_r=="Asian")	// 1 = Asian
replace race = 2 if race_r == "Black or African American"
replace race = 3 if race_r == "White"
replace race = 4 if race_r == "Other or Multiple"

* Ethnicity
gen hispanic = 0	// 0 = non-Hispanic
replace hispanic = 1 if ethnicity_T1 == "Yes - I am Hispanic or Latina/o"	// 1 = Hispanic

* Household income
gen income=(income_T1=="Less than $30,000")
replace income = 2 if income_T1 == "$30,000-$50,000"
replace income = 3 if income_T1 == "$50,000-$75,000"
replace income = 4 if income_T1 == "$75,000-$100,00"
replace income = 5 if income_T1 == "$100,000 or more"
replace income = 77 if income_T1 == "Prefer not to answer"

* Education
gen edu = (education_T1 == "High school degree / GED")
replace edu = 2 if education_T1 == "Associate's Degree (AA, AS, or other) / Technical Degree / Some college"
replace edu = 3 if education_T1 == "Bachelor's Degree (BA, BS, or other)"
replace edu = 4 if education_T1 == "Some graduate school"
replace edu = 5 if education_T1 == "Graduate Degree (Masters, Doctoral degree, or other)"

* Health Literacy
gen hl = (health_literacy_T1 == "A little bit")
replace hl = 2 if health_literacy_T1 == "Quite a bit"
replace hl = 3 if health_literacy_T1 == "Extremely"

* Number of care recipients [Note: this is a censored one: no 5 and 6]
gen numbercare = total_CR_T1
replace numbercare = 4 if total_CR_T1>4

* Care provided to
gen caretype = (cg_type=="Adult(s) Only")
replace caretype = 2 if cg_type == "Kid(s) Only"
replace caretype = 3 if cg_type == "Sandwich Caregiver"

* Care duration in years
replace care_dur = "." if care_dur == "NA"
destring care_dur, gen(duration)

gen dur_11more = (care_duration_y_T1=="11 or more")

* caregiving intensity score
gen intensity = (intensity_class=="High Intensity")


** Generate caregiver (participant) summary table
table (var) (usertype), statistic(mean age) statistic(sd age) statistic(range age)	///
						statistic(fvpercent gender race hispanic income edu hl numbercare caretype) ///
						statistic(fvfrequency gender race hispanic income edu hl numbercare caretype) ///
						statistic(mean duration) statistic(sd duration)	///
						statistic(fvpercent dur_11more)	/// 
						statistic(fvfrequency dur_11more)	/// 
						statistic(mean intensity_points) statistic(sd intensity_points)	///
							statistic(range intensity_points) ///
						statistic(fvpercent intensity)	///
						statistic(fvfrequency intensity)	///
						nformat(%9.0fc frequency) sformat("%s%%" fvpercent) ///
						nformat(%6.2f  mean sd) sformat("(%s)" sd) ///
						style(table-1)
						
collect label levels usertype 1 "Complete users" 2 "Incomplete users" 3 "Non-users"

collect label levels gender 1 "Female" 2 "Male" 3 "Other"

collect label levels race 1 "Asian" 2 "Black or African American" 3 "White" 4 "Other or Multiple"

collect label levels income 1 "Less than $30,000" 2 "$30,000-$50,000" 3 "$50,000-$75,000" ///
							4 "$75,000-$100,00" 5 "$100,000 or more" 77 "Prefer not to answer"

collect label levels edu 1 "High school / GED" 2 "Associate's /Some college" ///
						 3 "Bachelor's Degree" 4 "Some graduate school" ///
						 5 "Graduate Degree"
						 
collect label levels hl 1 "A little bit" 2 "Quite a bit" 3 "Extremely"

collect label levels numbercare 4 "4 or more"

collect label levels caretype 1 "Adult(s) Only" 2 "Kid(s) Only" 3 "Adult(s) and child(ren)"

collect label levels intensity 0 "Medium intensity" 1 "High intensity"


collect preview


*** By users vs. non-users *******
**********************************
gen users=(usertype==1 | usertype==2) // =1 if completers or incompleters; =0 if non-users


** Generate caregiver (participant) summary table
table (var) (users), statistic(mean age) statistic(sd age) statistic(range age)	///
						statistic(fvpercent gender race hispanic income edu hl numbercare caretype) ///
						statistic(fvfrequency gender race hispanic income edu hl numbercare caretype) ///
						statistic(mean duration) statistic(sd duration)	///
						statistic(fvpercent dur_11more)	/// 
						statistic(fvfrequency dur_11more)	/// 
						statistic(mean intensity_points) statistic(sd intensity_points)	///
							statistic(range intensity_points) ///
						statistic(fvpercent intensity)	///
						statistic(fvfrequency intensity)	///
						nformat(%9.0fc frequency) sformat("%s%%" fvpercent) ///
						nformat(%6.2f  mean sd) sformat("(%s)" sd) ///
						style(table-1)
						
collect label levels users 1 "Users" 0 "Non-users"

collect label levels gender 1 "Female" 2 "Male" 3 "Other"

collect label levels race 1 "Asian" 2 "Black or African American" 3 "White" 4 "Other or Multiple"

collect label levels income 1 "Less than $30,000" 2 "$30,000-$50,000" 3 "$50,000-$75,000" ///
							4 "$75,000-$100,00" 5 "$100,000 or more" 77 "Prefer not to answer"

collect label levels edu 1 "High school / GED" 2 "Associate's /Some college" ///
						 3 "Bachelor's Degree" 4 "Some graduate school" ///
						 5 "Graduate Degree"
						 
collect label levels hl 1 "A little bit" 2 "Quite a bit" 3 "Extremely"

collect label levels numbercare 4 "4 or more"

collect label levels caretype 1 "Adult(s) Only" 2 "Kid(s) Only" 3 "Adult(s) and child(ren)"

collect label levels intensity 0 "Medium intensity" 1 "High intensity"


collect preview



* ######################################################################
** Generate primary care recipient (CR) summary section of the Table 1
* ######################################################################


** read in data
use "/Users/wy2nm/My Drive/UVA/Projects/R21_KellyShaffer/SHUTi CARE_all data_final_new.dta", clear

** Get data ready

* Degree of engagement
gen usertype = (engagement=="Completer") // 1 = complete users
replace usertype = 2 if engagement == "Incompleter" // 2 = incomplete users
replace usertype = 3 if engagement == "Non-user"	// 3 =  non-users

* Age
gen cr_age = CR_age_r

gen cr_90 = (CR_age_r>=90)

* Relationship to caregiver
levelsof CR_relationship_T1 // list out unique values of the relationship variables
gen relation = (CR_relationship_T1 == "Husband, wife, or spouse")
replace relation = 2 if CR_relationship_T1 == "Boyfriend, girlfriend, or partner"
replace relation = 3 if CR_relationship_T1 == "Father, mother"
replace relation = 4 if CR_relationship_T1 == "Father-in-law, mother-in-law"
replace relation = 5 if CR_relationship_T1 == "Sister-in-law, brother-in-law"
replace relation = 6 if CR_relationship_T1 == "Son, daughter"
replace relation = 7 if CR_relationship_T1 == "Grandparent"
replace relation = 8 if CR_relationship_T1 == "Other"

* Primary condition requiring care
levelsof CR_condition_T1
gen condition = (CR_condition_T1 == "Alzheimer's, dementia, confusion, or forgetfulness")
replace condition = 2 if CR_condition_T1 == "Arthritis"
replace condition = 3 if CR_condition_T1 == "Autism or Autism Spectrum Disorder"
replace condition = 4 if CR_condition_T1 == "Back problems"
replace condition = 5 if CR_condition_T1 == "Blood pressure / hypertension"
replace condition = 6 if CR_condition_T1 == "Brain damage or injury"
replace condition = 7 if CR_condition_T1 == "Cancer"
replace condition = 8 if CR_condition_T1 == "Developmental or intellectual disorder or disability"
replace condition = 9 if CR_condition_T1 == "Diabetes"
replace condition = 10 if CR_condition_T1 == "Feeble, unsteady, falling"
replace condition = 11 if CR_condition_T1 == "Heart disease or heart attack"
replace condition = 12 if CR_condition_T1 == "Lung disease, emphysema, COPD"
replace condition = 13 if CR_condition_T1 == "Mental illness, emotional illness, or depression"
replace condition = 14 if CR_condition_T1 == "Mobility problem / can't get around"
replace condition = 15 if CR_condition_T1 == "Old age, aging"
replace condition = 16 if CR_condition_T1 == "Parkinson's"
replace condition = 17 if CR_condition_T1 == "Stroke"
replace condition = 18 if CR_condition_T1 == "Other"



** Get table ready
table (var) (usertype), statistic(mean cr_age) statistic(sd cr_age) 	///
						statistic(fvpercent cr_90 relation condition) ///
						statistic(fvfrequency cr_90 relation condition) ///
						nformat(%9.0fc frequency) sformat("%s%%" fvpercent) ///
						nformat(%6.2f  mean sd) sformat("(%s)" sd) ///
						style(table-1)
						
						
collect label levels usertype 1 "Complete users" 2 "Incomplete users" 3 "Non-users"

collect label levels cr_90 1 "90 and older"

collect label levels relation 1 "Husband, wife, or spouse" 2 "Boyfriend, girlfriend, or partner" ///
							  3 "Father, mother" 4  "Father-in-law, mother-in-law" ///
							  5 "Sister-in-law, brother-in-law" 6 "Son, daughter" ///
							  7 "Grandparent" 8 "Other"
							  
collect label levels condition 1 "Alzheimer's, dementia etc." 2 "Arthritis" 3 "Autism etc."	///
							   4 "Back problems" 5 "Blood pressure / hypertension" 			///
							   6 "Brain damage or injury" 7 "Cancer" 8 "Developmental disorder etc." ///
							   9 "Diabetes" 10 "Feeble, unsteady, etc." 11 "Heart disease or heart attack" ///
							  12 "Lung disease, emphysema, COPD" 13 "Mental illness etc." 14 "Mobility problem" ///
							  15 "Old age, aging" 16 "Parkinson's" 17 "Stroke" 18 "Other"

collect preview



** #############  Table for Aim 1a: Association of SHUTi Engagement with Caregiving  ##################
/* test the effect of caregiver context on SHUTi engagement, ordered logistic regressions assuming proportional odds will be fit for each caregiving-related user and environmental characteristics on SHUTi Core completion, which is a 3-level ordinal-dependent variable (ie, nonusers vs incompleters vs completers). Where the proportional odds assumption is violated (P<.05), follow-up sensitivity analyses will be conducted: 2 logistic regressions will compare proportional odds between nonusers and users (incompleters and completers) and between noncompleters (nonusers and incompleters) and completers.*/

** ############# Summary Statistics ##################

** read in data
use "/Users/wy2nm/My Drive/UVA/Projects/R21_KellyShaffer/SHUTi CARE_all data_final_new.dta", clear

** Get data ready

* Degree of engagement
gen usertype = (engagement=="Completer") // 1 = complete users
replace usertype = 2 if engagement == "Incompleter" // 2 = incomplete users
replace usertype = 3 if engagement == "Non-user"	// 3 =  non-users

* Caregiving-related user characteristics
gen stress = overload_T1
gen sefficacy = cgefficacy_T1
gen glt_wrong = cgguilt_wrong_T1
gen glt_fail = cgguilt_fail_T1
gen glt_selfcare = cgguilt_selfcare_T1

* Caregiving-related environmental characteristics
gen proximity = (cr_proximity_T1 == "Bedpartner")
replace proximity = 2 if cr_proximity_T1 == "Live together"
replace proximity = 3 if cr_proximity_T1 == "Other Living Sit"

* Care recipient functional status
gen cr_function = barthel_T1
gen cr_cognitive = pss_cognitive_T1
gen cr_behav = pss_probbehav_T1

* Caregiving tasks
gen adl = adl_T1
gen iadl = iadl_T1
gen tasks = (med_nursing_tasks_T1 == "Yes")

** Get table ready
table (var) (usertype), statistic(mean stress sefficacy glt_wrong glt_fail glt_selfcare) ///
						statistic(sd stress sefficacy glt_wrong glt_fail glt_selfcare) 	///
						statistic(fvpercent proximity) ///
						statistic(fvfrequency proximity) ///
						statistic(mean cr_function cr_cognitive cr_behav adl iadl) ///
						statistic(sd cr_function cr_cognitive cr_behav adl iadl)	///
						statistic(fvpercent tasks) ///
						statistic(fvfrequency tasks) ///
						nformat(%9.0fc frequency) sformat("%s%%" fvpercent) ///
						nformat(%6.2f  mean sd) sformat("(%s)" sd) ///
						style(table-1)
						
						
collect label levels usertype 1 "Complete users" 2 "Incomplete users" 3 "Non-users"
						
collect label levels proximity 1 "Bedpartner" 2 "Live together" 3 "Other Living Sit"


collect preview


** ############# Statistical Inference ##################
foreach var of varlist stress sefficacy glt_wrong glt_fail glt_selfcare {

	ologit usertype `var', or	
	
	oparallel, brant					

}

ologit usertype stress sefficacy glt_wrong glt_fail glt_selfcare, or
oparallel

ologit usertype i.proximity, or
oparallel, brant

foreach var of varlist cr_function cr_cognitive cr_behav adl iadl tasks {
	
	ologit usertype `var', or
	oparallel, brant
}

*** ########### Post hoc analysis on CR functional status, CR cognitive status, and ADL ######
**** Two logistic regressions to compare: between nonusers and users (incompleters and completers); between noncompleters (nonusers and incompleters) and completers
***********************

* define the two dependent variables for the logit regressions
gen users=(usertype==1 | usertype==2) // =1 if completers or incompleters; =0 if non-users

gen completers=(usertype==1) // =1 if completers; =0 if incompleters or non-users

foreach var of varlist cr_function cr_cognitive adl  {
	
	logit users `var', or
	
}

foreach var of varlist cr_function cr_cognitive adl  {
	
	logit completers `var', or
	
}


* define the two dependent variables for the logit regressions [to match with ordered logit]
gen nonusers=(usertype==3) // =0 if completers or incompleters; =1 if non-users

gen noncompleters=(usertype==2 | usertype==3) // =0 if completers; =1 if incompleters or non-users

foreach var of varlist cr_function cr_cognitive adl  {
	
	logit nonusers `var', or
	
}

foreach var of varlist cr_function cr_cognitive adl  {
	
	logit noncompleters `var', or
	
}


									
** #############  Table for Aim 2.   ##################
/*
Test the Association of Change in Known Cognitive Mechanisms of SHUTi with Caregiving Context:

Among SHUTi users, continuous regression modeling will test the association of each caregiving context predictor with cognitive mechanisms assessed at postassessment, controlling for preassessment [83]. Models will control for the level of SHUTi engagement */

** read in data
use "/Users/wy2nm/My Drive/UVA/Projects/R21_KellyShaffer/SHUTi CARE_all data_final_new.dta", clear

** Get data ready

* Degree of engagement
gen usertype = (engagement=="Completer") // 1 = complete users
replace usertype = 2 if engagement == "Incompleter" // 2 = incomplete users
replace usertype = 3 if engagement == "Non-user"	// 3 =  non-users

** Select only user samples
drop if usertype == 3	// 18 removed

*** Dependent variables: cognitive mechanisms 
* Sleep beliefs
gen sbelief_t1 = dbas_T1	// preassessment 

replace dbas_T2u = "." if dbas_T2u == "NA"
destring dbas_T2u, gen(sbelief_t2)	// postassessment (note: 6 obs with missing values)

* Internal sleep locus of controlling
gen sloc_int_t1 = sloc_int_T1 // preassessment

replace sloc_int_T2u = "." if sloc_int_T2u == "NA"
destring sloc_int_T2u, gen(sloc_int_t2)	// postassessment (note: 6 obs with missing values)


* External sleep locus of controlling
replace sloc_ext_T1 = "." if sloc_ext_T1 == "NA"
destring sloc_ext_T1, gen(sloc_ext_t1)	// preassessment (note: 1 obs with missing values)

replace sloc_ext_T2u = "." if sloc_ext_T2u == "NA"
destring sloc_ext_T2u, gen(sloc_ext_t2)	// postassessment (note: 7 obs with missing values)


*** Independent variables: caregiving context predictors
* Caregiving-related user characteristics
gen stress = overload_T1
gen sefficacy = cgefficacy_T1
gen glt_wrong = cgguilt_wrong_T1
gen glt_fail = cgguilt_fail_T1
gen glt_selfcare = cgguilt_selfcare_T1

* Caregiving-related environmental characteristics
gen proximity = (cr_proximity_T1 == "Bedpartner")
replace proximity = 2 if cr_proximity_T1 == "Live together"
replace proximity = 3 if cr_proximity_T1 == "Other Living Sit"

* Care recipient functional status
gen cr_function = barthel_T1
gen cr_cognitive = pss_cognitive_T1
gen cr_behav = pss_probbehav_T1

* Caregiving tasks
gen adl = adl_T1
gen iadl = iadl_T1
gen tasks = (med_nursing_tasks_T1 == "Yes")

**** Regression model to test the association: caregiving-related user characteristics
foreach y in sbelief_ sloc_int_ sloc_ext_ {
	
	foreach x in stress sefficacy glt_wrong glt_fail glt_selfcare {
		
		reg `y't2 `x' `y't1 i.usertype, vce(robust)
	}
}

**** Regression model to test the association: caregiving-related environmental characteristics
foreach y in sbelief_ sloc_int_ sloc_ext_ {
	
	reg `y't2 i.proximity `y't1 i.usertype, vce(robust)
	
}

foreach y in sbelief_ sloc_int_ sloc_ext_ {
	
	foreach x in cr_function cr_cognitive cr_behav adl iadl tasks {
		
		reg `y't2 `x' `y't1 i.usertype, vce(robust)
	}
}


									
** #############  Table for Aim 3.   ##################
/*
The preliminary efficacy of SHUTi for caregivers will be explored by computing within-group effect size on the change from preassessment to postassessment on insomnia symptoms (ie, self-reported severity), sleep outcomes (ie, sleep diary metrics), and related constructs of general distress and
caregiving strain 

use 'levelsof' to check unique values in string variables */

** read in data
use "/Users/wy2nm/My Drive/UVA/Projects/R21_KellyShaffer/SHUTi CARE_all data_final_new.dta", clear

** Get data ready

* Degree of engagement
gen usertype = (engagement=="Completer") // 1 = complete users
replace usertype = 2 if engagement == "Incompleter" // 2 = incomplete users
replace usertype = 3 if engagement == "Non-user"	// 3 =  non-users

* Sleep beliefs
gen sbelief_t1 = dbas_T1	// preassessment 

replace dbas_T2u = "." if dbas_T2u == "NA"
destring dbas_T2u, gen(sbelief_t2)	// postassessment 

* Internal sleep locus of controlling
gen sloc_int_t1 = sloc_int_T1 // preassessment

replace sloc_int_T2u = "." if sloc_int_T2u == "NA"
destring sloc_int_T2u, gen(sloc_int_t2)	// postassessment 


* External sleep locus of controlling
replace sloc_ext_T1 = "." if sloc_ext_T1 == "NA"
destring sloc_ext_T1, gen(sloc_ext_t1)	// preassessment 

replace sloc_ext_T2u = "." if sloc_ext_T2u == "NA"
destring sloc_ext_T2u, gen(sloc_ext_t2)	// postassessment 

* Insomnia severity [non-users also have data]
gen isi_t1 = isi_T1

replace isi_T2u = "." if isi_T2u == "NA"
destring isi_T2u, gen(isi_t2u) 		// 24 missing obs

replace isi_T2n = "." if isi_T2n == "NA"
destring isi_T2n, gen(isi_t2n) 		// 85 missing obs


**** Sleep diary metrics
* SOL
gen sol_t1 = sol_T1		// preassessment

replace sol_T2u = "." if sol_T2u == "NA"
destring sol_T2u, gen(sol_t2)	// postassessment with 20 missing obs

* WASO
gen waso_t1 = wasoema_T1		// preassessment

replace wasoema_T2u = "." if wasoema_T2u == "NA"
destring wasoema_T2u, gen(waso_t2)	// postassessment with 20 missing obs

* No. awakenings
gen numawak_t1 = NumAwake_T1

replace NumAwake_T2u = "." if NumAwake_T2u == "NA"
destring NumAwake_T2u, gen(numawak_t2)	// 20 missing

* Sleep quality
gen slqual_t1 = SleepQ_T1

replace SleepQ_T2u = "." if SleepQ_T2u == "NA"
destring SleepQ_T2u, gen(slqual_t2)	// 20 missing

* Total sleep time
gen tstime_t1 = tst_T1

replace tst_T2u = "." if tst_T2u == "NA"
destring tst_T2u, gen(tstime_t2)	// 20 missing

* Global physical health
gen ph_t_t1 = promis_ph_t_T1

replace promis_ph_t_T2u = "." if promis_ph_t_T2u == "NA"
destring promis_ph_t_T2u, gen(ph_t_t2u)	// 24 missing

replace promis_ph_t_T2n = "." if promis_ph_t_T2n == "NA"
destring promis_ph_t_T2n, gen(ph_t_t2n)	// 86 missing

* General distress
gen phq_t1 = phq_4_T1

replace phq_4_T2u = "." if phq_4_T2u == "NA"
destring phq_4_T2u, gen(phq_t2u)	// 24 missing

replace phq_4_T2n = "." if phq_4_T2n == "NA"
destring phq_4_T2n, gen(phq_t2n)	// 86 missing



*** Summary statistics for pre assessment

table (var) (usertype), statistic(mean sbelief_t1 sloc_int_t1 sloc_ext_t1 isi_t1) ///
						statistic(sd sbelief_t1 sloc_int_t1 sloc_ext_t1 isi_t1) 	///
						statistic(mean sol_t1 waso_t1 numawak_t1 slqual_t1 tstime_t1) ///
						statistic(sd sol_t1 waso_t1 numawak_t1 slqual_t1 tstime_t1)	///
						statistic(mean ph_t_t1 phq_t1)	///
						statistic(sd ph_t_t1 phq_t1)	///
						nformat(%6.2f  mean sd) sformat("(%s)" sd) ///
						style(table-1)					
						
collect label levels usertype 1 "Complete users" 2 "Incomplete users" 3 "Non-users"

collect preview


*** Summary statistics for post assessment

table (var) (usertype), statistic(mean sbelief_t2 sloc_int_t2 sloc_ext_t2 isi_t2u isi_t2n) ///
						statistic(sd sbelief_t2 sloc_int_t2 sloc_ext_t2 isi_t2u isi_t2n) 	///
						statistic(mean sol_t2 waso_t2 numawak_t2 slqual_t2 tstime_t2) ///
						statistic(sd sol_t2 waso_t2 numawak_t2 slqual_t2 tstime_t2)	///
						statistic(mean ph_t_t2u ph_t_t2n phq_t2u phq_t2n)	///
						statistic(sd ph_t_t2u ph_t_t2n phq_t2u phq_t2n)		///
						nformat(%6.2f  mean sd) sformat("(%s)" sd) ///
						style(table-1)					
						
collect label levels usertype 1 "Complete users" 2 "Incomplete users" 3 "Non-users"

collect preview

*** Generate within-group effect size

drop d_*

** for complete users

foreach y in sbelief_ sloc_int_ sloc_ext_ sol_ waso_ numawak_ slqual_ tstime_ {
	
	gen d_`y' = `y't2 - `y't1 if usertype == 1
    sum d_`y'
	display = `r(mean)'/`r(sd)'
	
}


foreach y in isi_ ph_t_ phq_ {
	
	gen d_`y' = `y't2u - `y't1 if usertype == 1
    sum d_`y'
	display = `r(mean)'/`r(sd)'
	
}

** for incomplete users

drop d_*

foreach y in sbelief_ sloc_int_ sloc_ext_ sol_ waso_ numawak_ slqual_ tstime_ {
	
	gen d_`y' = `y't2 - `y't1 if usertype == 2
    sum d_`y'
	display = `r(mean)'/`r(sd)'
	
}

foreach y in isi_ ph_t_ phq_ {
	
	gen d_`y' = `y't2u - `y't1 if usertype == 2
    sum d_`y'
	display = `r(mean)'/`r(sd)'
	
}

** for non-users

drop d_*

foreach y in isi_ ph_t_ phq_ {
	
	gen d_`y' = `y't2n - `y't1 if usertype == 3
    sum d_`y'
	display = `r(mean)'/`r(sd)'
	
}


***** For Supplementary table 1 *****
***************************************
** Note: the specification will be similar as Aim 2

** Select only user samples
drop if usertype == 3	// 18 removed


*** Independent variables: caregiving context predictors
* Caregiving-related user characteristics
gen stress = overload_T1
gen sefficacy = cgefficacy_T1
gen glt_wrong = cgguilt_wrong_T1
gen glt_fail = cgguilt_fail_T1
gen glt_selfcare = cgguilt_selfcare_T1

* Caregiving-related environmental characteristics
gen proximity = (cr_proximity_T1 == "Bedpartner")
replace proximity = 2 if cr_proximity_T1 == "Live together"
replace proximity = 3 if cr_proximity_T1 == "Other Living Sit"

* Care recipient functional status
gen cr_function = barthel_T1
gen cr_cognitive = pss_cognitive_T1
gen cr_behav = pss_probbehav_T1

* Caregiving tasks
gen adl = adl_T1
gen iadl = iadl_T1
gen tasks = (med_nursing_tasks_T1 == "Yes")

**** Regression model to test the association: caregiving-related user characteristics
foreach y in sol_ waso_ numawak_ slqual_ tstime_ {
	
	foreach x in stress sefficacy glt_wrong glt_fail glt_selfcare {
		
		reg `y't2 `x' `y't1 i.usertype, vce(robust)
	}
}

foreach y in isi_ ph_t_ phq_ {
	
	foreach x in stress sefficacy glt_wrong glt_fail glt_selfcare {
		
		reg `y't2u `x' `y't1 i.usertype, vce(robust)
	}
}



**** Regression model to test the association: caregiving-related environmental characteristics
*gen bedpt = (proximity==1)
*replace bedpt=. if proximity == .

* Caregiving-related environmental characteristics
gen proximity = (cr_proximity_T1 == "Bedpartner")
replace proximity = 2 if cr_proximity_T1 == "Live together"
replace proximity = 3 if cr_proximity_T1 == "Other Living Sit"

/*
foreach y in sol_ waso_ numawak_ slqual_ tstime_ {
	
	reg `y't2 i.bedpt `y't1 i.usertype, vce(robust)
	
}

foreach y in isi_ ph_t_ phq_  {
	
	reg `y't2u i.bedpt `y't1 i.usertype, vce(robust)
	
}
*/


foreach y in sol_ waso_ numawak_ slqual_ tstime_ {
	
	reg `y't2 i.proximity `y't1 i.usertype, vce(robust)
	
}

foreach y in isi_ ph_t_ phq_  {
	
	reg `y't2u i.proximity `y't1 i.usertype, vce(robust)
	
}


foreach y in sol_ waso_ numawak_ slqual_ tstime_ {
	
	foreach x in cr_function cr_cognitive cr_behav adl iadl tasks {
		
		reg `y't2 `x' `y't1 i.usertype, vce(robust)
	}
}


foreach y in isi_ ph_t_ phq_  {
	
	foreach x in cr_function cr_cognitive cr_behav adl iadl tasks {
		
		reg `y't2u `x' `y't1 i.usertype, vce(robust)
	}
}
