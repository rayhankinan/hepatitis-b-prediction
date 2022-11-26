(deffunction ProposeQuestion (?question $?allowedValues)
    (printout t ?question) ; Print question
    (bind ?answer (read)) ; Read from user and bind it to variable "answer"
    (if lexemep ?answer) then (bind ?answer (lowcase ?answer)) ; Lowercase all characters in variable "answer"

    (while (not (member ?answer ?allowedValues)) do ; While the input not in variable "allowed_values"
        (printout t ?question) ; Print question
        (bind ?answer (read)) ; Read from input and bind it to variable "answer"
        ((if lexemep ?answer) then (bind ?answer (lowcase ?answer))) ; Lowercase all characters in variable "answer"
    )

    ?answer ; Return variable "answer"
)

(deffunction PositiveOrNegative (?question)
    (bind ?response (ProposeQuestion ?question positive negative)) ; Call function "ProposeQuestion" with the question parameter from input and the allowed_values consists of positive and negative SYMBOL

    ?response ; Return variable "response"
)

(deftemplate Symptom
    (slot symptomType
        (type SYMBOL)
        (allowed-symbols HBsAg antiHDV antiHBc antiHBs IgMantiHBc)
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