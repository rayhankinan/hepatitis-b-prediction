(deffunction ProposeQuestion (?question)
    (printout t ?question) ; Print question
    (bind ?answer (read)) ; Read from user and bind it to variable "answer"
    (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))) ; Lowercase all characters in variable "answer"

    (while (not (or (eq ?answer positive) (eq ?answer negative))) do ; While the input not in variable "allowed_values"
        (printout t ?question) ; Print question
        (bind ?answer (read)) ; Read from input and bind it to variable "answer"
        (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))) ; Lowercase all characters in variable "answer"
    )

    ?answer ; Return variable "answer"
)

(deffunction PositiveOrNegative (?question)
    (bind ?response (ProposeQuestion ?question)) ; Call function "ProposeQuestion" with the question parameter from input and the allowed_values consists of positive and negative SYMBOL

    ?response ; Return variable "response"
)

(deftemplate Symptom
    (slot symptomType
        (type SYMBOL)
        (allowed-symbols HBsAg antiHDV antiHBc antiHBs IgMantiHBc)
    )
)

(deftemplate Hepatitis
    (slot hepatitisType
        (type SYMBOL)
        (allowed-symbols acuteInfection chronicInfection hepatitisBPlusD cured vaccinated unclear healthy)
    )
)

(deftemplate Diagnosis
    (slot hepatitisType
        (type SYMBOL)
        (allowed-symbols acuteInfection chronicInfection hepatitisBPlusD cured vaccinated unclear healthy)
    )
    (multislot hepatitisSymptoms
        (type SYMBOL)
        (allowed-symbols HBsAg antiHDV antiHBc antiHBs IgMantiHBc)
    )
)

(deffacts SymptomDiagnosis
    (Diagnosis (hepatitisType acuteInfection) (hepatitisSymptoms HBsAg antiHBc IgMantiHBc))
    (Diagnosis (hepatitisType chronicInfection) (hepatitisSymptoms HBsAg antiHBc))
    (Diagnosis (hepatitisType hepatitisBPlusD) (hepatitisSymptoms HBsAg antiHDV))
    (Diagnosis (hepatitisType cured) (hepatitisSymptoms antiHBs antiHBc))
    (Diagnosis (hepatitisType vaccinated) (hepatitisSymptoms antiHBs))
    (Diagnosis (hepatitisType unclear) (hepatitisSymptoms antiHBc))
    (Diagnosis (hepatitisType healthy) (hepatitisSymptoms))
)

(defrule GetHBsAg
    (declare (salience 1))
    =>
    (bind ?response (PositiveOrNegative "HBsAg? [positive/negative] "))
    (if (eq ?response positive) then (assert (Symptom (symptomType HBsAg))))
)

(defrule GetAntiHDV
    (Symptom (symptomType HBsAg))
    =>
    (bind ?response (PositiveOrNegative "anti-HDV? [positive/negative] "))
    (if (eq ?response positive) then (assert (Symptom (symptomType antiHDV))))
)

(defrule GetAntiHBc1
    (Symptom (symptomType HBsAg))
    (not (Symptom (symptomType antiHDV)))
    =>
    (bind ?response (PositiveOrNegative "anti-HBc? [positive/negative] "))
    (if (eq ?response positive) then (assert (Symptom (symptomType antiHBc))))
)

(defrule GetAntiHBs1
    (Symptom (symptomType HBsAg))
    (not (Symptom (symptomType antiHDV)))
    (Symptom (symptomType antiHBc))
    =>
    (bind ?response (PositiveOrNegative "anti-HBs? [positive/negative] "))
    (if (eq ?response positive) then (assert (Symptom (symptomType antiHBs))))
)

(defrule GetIgMantiHBc
    (Symptom (symptomType HBsAg))
    (not (Symptom (symptomType antiHDV)))
    (Symptom (symptomType antiHBc))
    (not (Symptom (symptomType antiHBs)))
    =>
    (bind ?response (PositiveOrNegative "IgM anti-HBc? [positive/negative] "))
    (if (eq ?response positive) then (assert (Symptom (symptomType IgMantiHBc))))
)

(defrule GetAntiHBs2
    (not (Symptom (symptomType HBsAg)))
    =>
    (bind ?response (PositiveOrNegative "anti-HBs? [positive/negative] "))
    (if (eq ?response positive) then (assert (Symptom (symptomType antiHBs))))
)

(defrule GetAntiHBc2
    (not (Symptom (symptomType HBsAg)))
    =>
    (bind ?response (PositiveOrNegative "anti-HBc? [positive/negative] "))
    (if (eq ?response positive) then (assert (Symptom (symptomType antiHBc))))
)

(defrule DiagnoseSymptoms
    (Diagnosis (hepatitisType ?type))
    (forall (Diagnosis (hepatitisType ?type) (hepatitisSymptoms $? ?symptom $?)) (Symptom (symptomType ?symptom)))
    =>
    (assert (Hepatitis (hepatitisType ?type)))
    (assert (DisplayDiagnosis))
)