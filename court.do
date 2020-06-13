clear

cd "D:\Rati\Blog\Blog 23"

use CB2019_Georgia_response_30Jan2020.dta


******************* DV   *******************
gen court_old = OBJCOUR 

/// 1 Favor some citizens 2 - in the middle 3 - treats equally 
recode court_old (1/2 = 1) (-2/-1 = 2)  (5=2) (3/4 = 3)

/// Dummy 
/// 1 Favor some citizens 0 - Treats equally / neither / DK / RA
gen court = OBJCOUR 

recode court (1/2 = 1) (-2/-1 = 0)  (5=0) (3/4 = 0)



*************************************** IV   ****************************************

//// demographic vars: sett, age, gender, education, havejob, minority, internet, 
//// wealth index : ownearship of houshold items (utility)

/// =================================================================================
/// recoding demographic variables 
/// =================================================================================

/// STRATUM
gen sett = STRATUM

/// RESPAGE
gen age = RESPAGE

/// gender
/// recoding Female from 2 to 0
gen gender = RESPSEX
recode gender (2=0) /// female = 0 

//// RESPEDU  => education 
/*  1 = secondary or lower 2 = secodanry technical 3 = higher */
gen education = RESPEDU
recode education (1/4 = 1) (5 = 2) (6/8 = 3) (-9 / -1 = .)

//// EMPLSIT => havejob 
/* 1 = empl 0 = no */
gen havejob = EMPLSIT
recode havejob (5/6 = 1) (1/4 = 0) (7/8 = 0) (-9 / -1 = . )

///  ETHNIC -- Ethnicity of the respondent  => minority
/* 0 = Georgian   1 = Non-Georgian   */
gen minority = ETHNIC
recode minority (4 / 7 = 1)  (3 =0) (2=1) (1=1) (-9 / -1 = .)

///// Internet exposure FRQINTR => internet
/* 1 = Every day 2 = Less often 3 = Never	 */
gen internet = FRQINTR
recode internet (1=1) (2/4 =2) (5/6 = 3) (-9 / -1 = .)


//// Wealth Index => utility
foreach var of varlist OWNCOTV OWNDIGC OWNWASH OWNFRDG OWNAIRC OWNCARS OWNCELL OWNCOMP {
gen `var'r = `var' 
}

foreach var of varlist OWNCOTVr OWNDIGCr OWNWASHr OWNFRDGr OWNAIRCr OWNCARSr OWNCELLr OWNCOMPr {
recode `var' (-9 / -1 = .)
}

gen utility = (OWNCOTVr + OWNDIGCr + OWNWASHr + OWNFRDGr + OWNAIRCr + OWNCARSr + OWNCELLr + OWNCOMPr)

//// =================================================================================
/////  Aditional covariates 
//// =================================================================================


/// Party affiliation
// 1 - GD - goverment 2 - oppositon 3 - NP / RA / DK

gen party_aff = 0
replace party_aff = 2 if PARTYSUPP == 302 | PARTYSUPP == 303 | PARTYSUPP == 304 | PARTYSUPP == 305 | PARTYSUPP == 306 | PARTYSUPP == 307   | PARTYSUPP == 308 | PARTYSUPP == 309  | PARTYSUPP == 999 

replace party_aff = 1 if PARTYSUPP == 301

replace party_aff = 3 if PARTYSUPP == -1 |  PARTYSUPP == -2 |  PARTYSUPP == -5 



/// =================================================================================
//// trust in public institutions: recoding institutional trust index
//// recoding pattern:  recoding vars TRUHLTH - TRUOMB [exluding TRUSTUN and TRUSTEU]
///  -2	Refuse to answer / -1	Don't know	 => No opinion
///  -3	Interviewer error	=> missing 
/// =================================================================================

foreach var of varlist TRUHLTH - TRUOMB {
gen `var'r = `var' 
}
foreach var of varlist TRUHLTHr - TRUOMBr {
recode `var' (-5 = .) (-2 = .) (-1 = 3) (-3 = .)
}
egen instrust = rowmean(TRUHLTHr - TRUOMBr)
drop TRUHLTHr - TRUOMBr



/// FAIRTRT -- Are people like yourself treated fairly by the government?
gen gov_fair = FAIRTRT
recode gov_fair (-5 = .) (-2 = .) (-1 = 3) (-3 = .) 





//// Weighting


svyset PSU [pweight=INDWT], strata(SUBSTRATUM) fpc(NPSUSS) singleunit(certainty) || ID, fpc(NHHPSU) || _n, fpc(NADHH)

stop
/// ============================================================================================================================///
/// model 1 - base demo: court: i.sett age gender i.education havejob  minority internet utility.                               ///
/// ============================================================================================================================///


svy: logit court i.sett age gender i.education havejob  minority i.internet utility 
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)


//// predicted probabilities 
svy: logit court i.sett age gender i.education havejob  minority i.internet utility 
margins, at(sett=( 1 2 3 ))
marginsplot

svy: logit court i.sett age gender i.education havejob  minority i.internet utility 
margins, at(education=( 1 2 3 ))
marginsplot

svy: logit court i.sett age gender i.education havejob  minority i.internet utility 
margins, at(minority=( 0 1 ))
marginsplot

/// ============================================================================================================================///
/// model 2 - base demo  + party_aff : court: i.sett age gender i.education havejob  minority internet utility.                 ///
/// ============================================================================================================================///


svy: logit court i.sett age gender i.education havejob  minority i.internet utility i.party_aff
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)


svy: logit court i.sett age gender i.education havejob  minority i.internet utility i.party_aff
margins, at(party_aff=( 1 2 3))
marginsplot


/// ============================================================================================================================///
/// model 3 - base demo  + instrust : court: i.sett age gender i.education havejob  minority internet utility.                 ///
/// ============================================================================================================================///

svy: logit court  i.sett age gender i.education havejob  minority i.internet utility  instrust 
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

svy: logit court  i.sett age gender i.education havejob  minority i.internet utility  instrust 
margins, at(instrust=( 1 2 3 4 5))
marginsplot

/// ============================================================================================================================///
/// model 4 - base demo  + gov_fair : court: i.sett age gender i.education havejob  minority internet utility.                 ///
/// ============================================================================================================================///

svy: logit court  i.sett age gender i.education havejob  minority i.internet utility  i.gov_fair 
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

svy: logit court  i.sett age gender i.education havejob  minority i.internet utility  i.gov_fair 
margins, at(gov_fair=( 1 2 3 4 ))
marginsplot










