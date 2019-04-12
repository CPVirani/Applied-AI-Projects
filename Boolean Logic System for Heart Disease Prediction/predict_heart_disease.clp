;Heart Disease prediction system

;Templates

(deftemplate patient
    (slot family_history (default 0))
    (slot age (default 0))
    (slot gender (default m))
    (slot smoking (default 0))
    (slot high_blood_pressure (default 0))
    (slot physical_inactivity (default 0))
    (slot diabetes (default 0))
    (slot cholestrol (default 0))
    (slot overweight (default 0))
    (slot stress (default 0))
    (slot unhealthy_diet (default 0))
)

(deftemplate rating (slot score))
(deftemplate recommendation (slot rating) (slot explanation))
(deftemplate question (slot text) (slot type) (slot ident))
(deftemplate answer (slot ident) (slot text))

;Function to interact with the user to get the necessary details for evaluation

(deffunction ask-user (?question)
    "Ask a question, and return the answer"
    (printout t ?question " ")
    (return (read))
)

;Module containing the rule to assert the answers given by the user to the question asked

(defmodule ask)
(defrule ask::ask-question-by-id
    "Ask a question and assert the answer"
    (declare (auto-focus TRUE))
    (MAIN::question (ident ?id) (text ?text) (type ?type))
    (not (MAIN::answer (ident ?id)))
    ?ask <- (MAIN::ask ?id)
    =>
    (bind ?answer (ask-user ?text ?type))
    (assert (MAIN::answer (ident ?id) (text ?answer)))
    (retract ?ask)
    (return)
)

; Startup module for the application that prints the Welcome message

(defmodule application-startup)
(defrule welcome-user
    =>
    (printout t "Welcome! Let's Check And Predict Whether You'll Have A Heart Disease In The Next 10 Years!" crlf)
    (printout t "Type the name of the patient and press Enter> ")
    (bind ?name (read))
    (printout t "Let us begin the prediction for " ?name "." crlf)
    (printout t "Please provide the below requested information (Keep in mind, for the yes/no questions, 1 = yes and 0 = no): " crlf)
)

;Question Facts

(deffacts questions
    "The questions that are asked to the user by the system."
    (question (ident family_history) (type number)
        (text "Your father or brother under age 55 or your mother or sister under age 65 has had a heart attack, stroke, angioplasty or bypass surgery?"))
    (question (ident age) (type number)
        (text "Your age?"))
    (question (ident gender)(type character)
        (text "Your gender (m/f)?"))
    (question (ident smoking) (type number)
        (text "Either you smoke or you are exposed to secondhand smoke every day."))
    (question (ident high_blood_pressure) (type number)
        (text "Your blood pressure is over 135/85 mm Hg or you have been told that you have high blood pressure."))
    (question (ident physical_inactivity) (type number)
        (text "You do not exercise for at least 30 minutes of moderate-intensity physical activity, like taking a brisk walk, on most days?"))
    (question (ident diabetes) (type number)
        (text "You have been told that you have diabetes or take medicine to help control your blood sugar."))
    (question (ident cholestrol) (type number)
        (text "Your HDL (High Density Lipo-protein or 'good' cholesterol) is less than 50mg/dL."))
    (question (ident overweight) (type number)
        (text "You are 20 pounds or more overweight."))
    (question (ident stress) (type number)
        (text "You have a high demand/low control job with sustained high levels of stress."))
    (question (ident unhealthy_diet) (type number)
        (text "You eat an unhealthy diet. A healthy diet consists of : eating fruits, vegetables, whole-grain high-fiber foods,proteins,limiting saturated fat,limiting alcohol intake to no more than 1 drink per day,
avoiding trans-fatty acids."))
)

;Module containing rules to request the various details and assert the answers based on the different question

(defmodule request-user-details)
(defrule request-family_history
    =>
    (assert (ask family_history))
)
(defrule request-age
    =>
    (assert (ask age))
)
(defrule request-gender
    =>
    (assert (ask gender))
)
(defrule request-smoking
    =>
    (assert (ask smoking))
)
(defrule request-high_blood_pressure
    =>
    (assert (ask high_blood_pressure))
)
(defrule request-physical_inactivity
    =>
    (assert (ask physical_inactivity))
)
(defrule request-diabetes
    =>
    (assert (ask diabetes))
)
(defrule request-cholestrol
    =>
    (assert (ask cholestrol))
)
(defrule request-overweight
    =>
    (assert (ask overweight))
)
(defrule request-stress
    =>
    (assert (ask stress))
)
(defrule request-unhealthy_diet
    =>
    (assert (ask unhealthy_diet))
)

(defrule assert-patient-fact
    (answer (ident family_history) (text ?fh))
    (answer (ident age) (text ?a))
    (answer (ident gender) (text ?g))
    (answer (ident smoking) (text ?sm))
    (answer (ident high_blood_pressure) (text ?hbp))
    (answer (ident physical_inactivity) (text ?pi))
    (answer (ident diabetes) (text ?d))
    (answer (ident cholestrol) (text ?c))
    (answer (ident overweight) (text ?ow))
    (answer (ident stress) (text ?s))
    (answer (ident unhealthy_diet) (text ?ud))
    =>
    (assert (patient (family_history ?fh) (age ?a) (gender ?g) (smoking ?sm) (high_blood_pressure ?hbp) 
            (physical_inactivity ?pi)(diabetes ?d)(cholestrol ?c)(overweight ?ow)
            (stress ?s)(unhealthy_diet ?ud)
            )
    )
)

;Module containing rules that determine what is the risk for heart disease in the next 10 years depending on the values entered and the various combinations of these values in the answers

(defmodule risk-recommendation)
(defrule risk-group1
    (patient 
        (age ?a&:(>= ?a 0)&:(< ?a 20))
        (gender ?g&:(= ?g m))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group2
    (patient 
        (age ?a&:(>= ?a 0)&:(< ?a 20))
        (gender ?g&:(= ?g f))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group3
    (patient 
        (age ?a&:(>= ?a 20)&:(< ?a 40))
        (gender ?g&:(= ?g m))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (+ 0.6) (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group4
    (patient 
        (age ?a&:(>= ?a 20)&:(< ?a 40))
        (gender ?g&:(= ?g f))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (+ 0.6) (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group5
    (patient 
        (age ?a&:(>= ?a 40)&:(< ?a 60))
        (gender ?g&:(= ?g m))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (+ 6.3) (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group6
    (patient 
        (age ?a&:(>= ?a 40)&:(< ?a 60))
        (gender ?g&:(= ?g f))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (+ 5.6) (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group7
    (patient 
        (age ?a&:(>= ?a 60)&:(< ?a 80))
        (gender ?g&:(= ?g m))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (+ 19.9) (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group8
    (patient 
        (age ?a&:(>= ?a 60)&:(< ?a 80))
        (gender ?g&:(= ?g f))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (+ 9.7) (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group9
    (patient 
        (age ?a&:(>= ?a 80))
        (gender ?g&:(= ?g m))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (+ 32.2) (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

(defrule risk-group10
    (patient 
        (age ?a&:(>= ?a 80))
        (gender ?g&:(= ?g f))
        (family_history ?fh)
        (smoking ?sm)
        (high_blood_pressure ?hbp)
        (physical_inactivity ?pi)
        (diabetes ?d)
        (cholestrol ?c)
        (overweight ?ow)
        (stress ?s)
        (unhealthy_diet ?ud)       
        )
    =>
    (bind ?calculated-risk (integer (+ (+ 18.8) (* 10 ?fh) (* 10 ?sm) (* 10 ?hbp) (* 10 ?pi) (* 10 ?d)(* 10 ?c)(* 10 ?ow)(* 10 ?s)(* 10 ?ud))))
    
    (if(> ?calculated-risk 50) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very high risk for a heart disease in the next 10 years! Please go see a doctr for further testing and treatment ASAP!!!")
                )
        )
     elif(> ?calculated-risk 30) then
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have an average risk for a heart disease in the next 10 years! Please go see a doctor for further testing!")
                )
         )
     else
        (assert (recommendation
                (rating ?calculated-risk)
                (explanation "You have very low risk for a heart disease in the next 10 years!")
                )
        )
    )
)

;Module that contains the rules to print out the final result of the evaluation

(defmodule result)
(defrule print-result
    ?p1 <- (recommendation (rating ?r1) (explanation ?e))
    =>
    (printout t "*** The risk for this patient is :" ?r1 crlf)
    (printout t "Explanation: " ?e crlf crlf)
)

;Function to run the various modules of the application in the correct order

(deffunction run-application ()
    (reset)
    (focus application-startup request-user-details risk-recommendation result)
    (run)
)

;Run the above function in a loop to get back the prompt every time we have to enter the values for another candidate or re-run the program

(run-application)