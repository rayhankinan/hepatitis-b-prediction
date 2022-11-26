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

(defrule GetHBsAg
    (not (HBsAg ?))
    =>
    (bind ?response (PositiveOrNegative "HBsAg? [positive/negative] "))
    (assert (HBsAg ?response))
)

(defrule GetAntiHDV
    (HBsAg positive)
    (not (antiHDV ?))
    =>
    (bind ?response (PositiveOrNegative "anti-HDV? [positive/negative] "))
    (assert (antiHDV ?response))
)

(defrule GetAntiHBc1
    (HBsAg positive)
    (antiHDV negative)
    (not (antiHBc ?))
    =>
    (bind ?response (PositiveOrNegative "anti-HBc? [positive/negative] "))
    (assert (antiHBc ?response))
)

(defrule GetAntiHBs1
    (HBsAg positive)
    (antiHDV negative)
    (antiHBc positive)
    (not (antiHBs ?))
    =>
    (bind ?response (PositiveOrNegative "anti-HBs? [positive/negative] "))
    (assert (antiHBs ?response))
)

(defrule GetIgMantiHBc
    (HBsAg positive)
    (antiHDV negative)
    (antiHBc positive)
    (antiHBs negative)
    (not (IgMantiHBc ?))
    =>
    (bind ?response (PositiveOrNegative "IgM anti-HBc? [positive/negative] "))
    (assert (IgMantiHBc ?response))
)

(defrule GetAntiHBs2
    (HBsAg negative)
    (not (antiHBs ?))
    =>
    (bind ?response (PositiveOrNegative "anti-HBs? [positive/negative] "))
    (assert (antiHBs ?response))
)

(defrule GetAntiHBc2
    (HBsAg negative)
    (antiHBs ?)
    (not (antiHBc ?))
    =>
    (bind ?response (PositiveOrNegative "anti-HBc? [positive/negative] "))
    (assert (antiHBc ?response))
)

(defrule IsAcuteInfection
    (HBsAg positive)
    (antiHDV negative)
    (antiHBc positive)
    (antiHBs negative)
    (IgMantiHBc positive)
    =>
    (printout t "Hasil Prediksi = Acute Infection" crlf)
    (assert (certain))
)

(defrule IsChronicInfection
    (HBsAg positive)
    (antiHDV negative)
    (antiHBc positive)
    (antiHBs negative)
    (IgMantiHBc negative)
    =>
    (printout t "Hasil Prediksi = Chronic Infection" crlf)
    (assert (certain))
)

(defrule IsHepatitisBPlusD
    (HBsAg positive)
    (antiHDV positive)
    =>
    (printout t "Hasil Prediksi = Hepatitis B + D" crlf)
    (assert (certain))
)

(defrule IsCured
    (HBsAg negative)
    (antiHBs positive)
    (antiHBc positive)
    =>
    (printout t "Hasil Prediksi = Cured" crlf)
    (assert (certain))
)

(defrule IsVaccinated
    (HBsAg negative)
    (antiHBs positive)
    (antiHBc negative)
    =>
    (printout t "Hasil Prediksi = Vaccinated" crlf)
    (assert (certain))
)

(defrule IsUnclear
    (HBsAg negative)
    (antiHBs negative)
    (antiHBc positive)
    =>
    (printout t "Hasil Prediksi = Unclear (possible resolved)" crlf)
    (assert (certain))
)

(defrule IsHealthy
    (HBsAg negative)
    (antiHBs negative)
    (antiHBc negative)
    =>
    (printout t "Hasil Prediksi = Healthy, not vaccinated, or suspicious" crlf)
    (assert (certain))
)

(defrule IsUncertain
    (declare (salience -1))
    (not (certain))
    =>
    (printout t "Hasil Prediksi = Uncertain Configuration" crlf)
)