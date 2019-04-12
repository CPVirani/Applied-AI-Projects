(import nrc.fuzzy.*)
(import nrc.fuzz.jess.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)

(deftemplate patient
    (slot name)
    (slot weight (type INTEGER))
    (slot height (type INTEGER))
)

(deftemplate patient_bmi
    "Auto-generated"
	(declare (ordered TRUE))
)

(deftemplate patient_sleep
    "Auto-generated"
    (declare (ordered TRUE))
)

(deftemplate patient_bp_sys
    "Auto-generated"
    (declare (ordered TRUE))
)

(deftemplate patient_bp_dias
    "Auto-generated"
    (declare (ordered TRUE))
)

(deftemplate patient_workout-p
    "Auto-generated"
    (declare (ordered TRUE))
)

(deftemplate patient_stress-level
    "Auto-generated"
    (declare (ordered TRUE))
)

(deftemplate patient_chd-risk
    "Auto-generated"
    (declare (ordered TRUE))
)

(deftemplate patient_sugar-level
    (slot level (type INTEGER))
)

(deftemplate patient_diabeticCondition
    "Auto-generated"
    (declare (ordered TRUE))
)

(deftemplate patient_water-int
    "Auto-generated"
    (declare (ordered TRUE))
)

(deftemplate patient_cal-int
    "Auto-generated"
    (declare (ordered TRUE))
)


(defglobal ?*bmiVar* = (new FuzzyVariable "bmi" 5 50))
(defglobal ?*sleepVar* = (new FuzzyVariable "sleep" 2 10 "Hours"))
(defglobal ?*bloodPressSVar* = (new FuzzyVariable "bloodPressDias" 0 190))
(defglobal ?*bloodPressDVar* = (new FuzzyVariable "bloodPressSys" 0 190))
(defglobal ?*workoutVar* = (new FuzzyVariable "workoutHours" 0 2))
(defglobal ?*stressVar* = (new FuzzyVariable "stressLevels" 0 10))
(defglobal ?*chdVar* = (new FuzzyVariable "chdRisk" 0 3))
(defglobal ?*diabVar* = (new FuzzyVariable "diabetesLevels" 30 200))
(defglobal ?*waterVar* = (new FuzzyVariable "WaterIntakeLevel" 0 500))
(defglobal ?*calVar* = (new FuzzyVariable "CalorieIntakeLevel" 500 2000))

(call nrc.fuzzy.FuzzyValue setMatchThreshold 0.2)


; Startup module for the application that prints the Welcome message
(defmodule application-startup)
(defrule welcome-user
    =>
    (printout t "Welcome! Let's Check And Predict Whether You'll Have A Heart Disease In The Next 10 Years!" crlf)
    (printout t "Type the name of the patient and press Enter> ")
    (bind ?name (read))
    (printout t "Let us begin the prediction for " ?name "." crlf)
    (printout t "This prediction is based on the atient information provided by the user to the program! " crlf)
)

;Initializing Global Variables
(defrule MAIN::init-FuzzyVariables
    (declare (salience 100))
    (initial-fact)
    =>
    (bind ?xunder (create$ 10 15 20))
    (bind ?yunder (create$ 1 1 0))
    (call ?*bmiVar* addTerm "underweight" ?xunder ?yunder 3)
    
    (bind ?xover (create$ 24 30))
    (bind ?yover (create$ 0 1))
    (call ?*bmiVar* addTerm "overweight" ?xover ?yover 2)
    
    (call ?*bmiVar* addTerm "normal" "not underweight and (not overweight)")
    
    (bind ?Sxlow (create$ 70 85 95))
    (bind ?Sylow (create$ 1 1 0))
    (call ?*bloodPressSVar* addTerm "low" ?Sxlow ?Sylow 3)
    
    (bind ?Dxlow (create$ 40 55 65))
    (bind ?Dylow (create$ 1 1 0))
    (call ?*bloodPressDVar* addTerm "low" ?Dxlow ?Dylow 3)
    
    (bind ?Sxhigh (create$ 115 130))
    (bind ?Syhigh (create$ 1 0))
    (call ?*bloodPressSVar* addTerm "hypertension" ?Sxhigh ?Syhigh 2)
    
    (bind ?Dxhigh (create$ 75 105))
    (bind ?Dyhigh (create$ 1 0))
    (call ?*bloodPressDVar* addTerm "hypertension" ?Dxhigh ?Dyhigh 2)
    
    (call ?*bloodPressSVar* addTerm "ideal" "not low and (not hypertension)")
    (call ?*bloodPressDVar* addTerm "ideal" "not low and (not hypertension)")
    
    (bind ?Pxsleep (create$ 0 4 6))
    (bind ?Pysleep (create$ 1 1 0))
    (call ?*sleepVar* addTerm "poor" ?Pxsleep ?Pysleep 3)
    
    (bind ?Nxsleep (create$ 6 8 10))
    (bind ?Nysleep (create$ 0 1 0))
    (call ?*sleepVar* addTerm "normal" ?Nxsleep ?Nysleep 3)
    
    (bind ?wLowx (create$ 0 1))
    (bind ?wLowy (create$ 1 0))
    (call ?*workoutVar* addTerm "light" ?wLowx ?wLowy 2)
    
    (bind ?wModx (create$ 0.5 1 2))
    (bind ?wMody (create$ 0 1 0))
    (call ?*workoutVar* addTerm "moderate" ?wModx ?wMody 3)
    
    (bind ?wIntx (create$ 0 1))
    (bind ?wInty (create$ 1 0))
    (call ?*workoutVar* addTerm "intense" ?wIntx ?wInty 2)
    
    (bind ?sx (create$ 0 5 10))
    (bind ?sy (create$ 1 1 0))
    (call ?*stressVar* addTerm "high" ?sx ?sy 3)
    
    (bind ?sx (create$ 0 5 10))
    (bind ?sy (create$ 0 0 1))
    (call ?*stressVar* addTerm "normal" ?sx ?sy 3)
    
    (bind ?cxm (create$ 0 1 2))
    (bind ?cym (create$ 0 1 0))
    (call ?*chdVar* addTerm "moderate" ?cxm ?cym 2)
    
    (bind ?cxh (create$ 2 3))
    (bind ?cyh (create$ 0 1))
    (call ?*chdVar* addTerm "high" ?cxm ?cym 2)
    
    (bind ?diabLowx (create$ 30 60 80))
    (bind ?diabLowy (create$ 1 1 0))
    (call ?*diabVar* addTerm "low" ?diabLowx ?diabLowy 3)
    
    (bind ?diabModx (create$ 93 99 105))
    (bind ?diabMody (create$ 0 1 0))
    (call ?*diabVar* addTerm "normal" ?diabModx ?diabMody 3)
    
    (bind ?diabHighx (create$ 105 125))
    (bind ?diabHighy (create$ 0 1))
    (call ?*diabVar* addTerm "high" ?diabHighx ?diabHighy 2)
    
)

;Initializing patient values
(defrule init
    (declare (salience 50))
=>
    (assert (patient (name Patient2)(height 1.666)(weight 80))) ; 
    (assert (patient_sleep (new nrc.fuzzy.FuzzyValue ?*sleepVar* "normal")))
    (assert (patient_bp_sys (new nrc.fuzzy.FuzzyValue ?*bloodPressSVar* "hypertension")))
    (assert (patient_bp_dias (new nrc.fuzzy.FuzzyValue ?*bloodPressDVar* "hypertension")))
    (assert (patient_sugar-level (level 70)))    
)

;Compute the BMI and fuzzify outputs
(defrule fuzzify_bmi 
	?p <- (patient (name ?name))
    =>
    (bind ?bm (/ ?p.weight (* ?p.height ?p.height))) 
    (printout t "Calculated BMI : " ?bm crlf)
    (assert (patient_bmi (new nrc.fuzzy.FuzzyValue ?*bmiVar* (new SingletonFuzzySet ?bm))))
)

;Recommend various workout based on the BMI

;Moderate for normal people
(defrule mod_workout_required
	(patient_bmi ?p&:(fuzzy-match ?p "normal"))
     =>
    (printout t "Based on your BMI,NORMAL workout is recommended." crlf)
	(assert (patient_workout-p (new nrc.fuzzy.FuzzyValue ?*workoutVar* "moderate")))  
)	

;Intense for obese people
(defrule extrm_workout_required
	(patient_bmi ?p&:(fuzzy-match ?p "overweight"))
     =>
    (printout t "Based on your BMI, INTENSE workout is recommended." crlf)
	(assert (patient_workout-p (new nrc.fuzzy.FuzzyValue ?*workoutVar* "intense")))  
)

;Compute the stress levels on the basis of poor sleep patterns and hypertension
(defrule stress_lvls_high
    (patient_sleep ?ps&:(fuzzy-match ?ps "poor"))
    (patient_bp_sys ?pbp1&:(fuzzy-match ?pbp1 "more_or_less hypertension"))
    (patient_bp_dias ?pbp2&:(fuzzy-match ?pbp2 "hypertension"))
    =>
	(assert (patient_stress-level (new nrc.fuzzy.FuzzyValue ?*stressVar* "high")))
    (printout t "Sleeping Patterns : POOR ")
    (printout t "HyperTension? : YES ")
    (printout t "Stress degree : " (fuzzy-rule-similarity) crlf)
)

;Compute the risk for coronary heart diseases based on stress level and BMI

;High risk implies higher chances of coronary heart diseases
(defrule chd_risks_high
	(patient_stress-level ?s&:(fuzzy-match ?s "high"))
	(patient_bmi ?p&: (fuzzy-match ?p "extremely overweight"))
    =>
    (assert (patient_chd-risk (new nrc.fuzzy.FuzzyValue ?*chdVar* "extremely high"))) 
    (printout t "You have a HIGH chance of coronory heart diseases, due to very high stress and obesity" crlf)   	    	
)

(defrule chd_risks_mod
	(patient_stress-level ?s&:(fuzzy-match ?s "more_or_less high"))
    (patient_bmi ?p&: (fuzzy-match ?p "not overweight"))
    =>
    (assert (patient_chd-risk (new nrc.fuzzy.FuzzyValue ?*chdVar* "moderate"))) 
    (printout t "You have a moderate chance of coronory heart diseases, due to more or less high stress" crlf)  	    	
)


;Compute the chances of diabetes based on sugar levels
(defrule diabetes_chck
	?s <- (patient_sugar-level (level ?lvl))  
    =>
    (if (< ?s.level 70) then
        (assert (patient_diabeticCondition (new nrc.fuzzy.FuzzyValue ?*diabVar* "low")))
        (printout t "Risk for diabetes : LOW" crlf)
     else (if (< ?s.level 99) then
            (assert (patient_diabeticCondition (new nrc.fuzzy.FuzzyValue ?*diabVar* "normal")))
            (printout t "Risk for diabetes : Little to NO risk" crlf)
            else (if (< ?s.level 125) then
                (assert (patient_diabeticCondition (new nrc.fuzzy.FuzzyValue ?*diabVar* "high")))
                (printout t "Risk for diabetes : HIGH" crlf)
                else 
                (assert (patient_diabeticCondition (new nrc.fuzzy.FuzzyValue ?*diabVar* "extremely high")))
                (printout t "Risk for diabetes: EXTREMELY HIGH" crlf))
            ))
)

;In case of high risk for Heart Disease, advise for immediate attention
(defrule disImmediateDiagnosisHeartCondn
    (patient_diabeticCondition ?d&:(fuzzy-match ?d "high"))
	(patient_chd-risk ?c&:(fuzzy-match ?c "extremely high"))
    =>
    (printout t "There is a high risk of heart disease in the next 10 years! You are recommended to consult your physician!!" crlf)    
)

(reset)
(focus application-startup)
(bind ?numrules (run))


