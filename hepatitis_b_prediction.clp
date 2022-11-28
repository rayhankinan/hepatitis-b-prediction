(deffunction ProposeQuestion (?question)
    (printout t ?question)
    (bind ?answer (read))
    (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))

    (while (not (or (eq ?answer positive) (eq ?answer negative))) do
        (printout t ?question)
        (bind ?answer (read))
        (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
    )

    ?answer
)

(deffunction PositiveOrNegative (?question)
    (bind ?response (ProposeQuestion ?question))

    ?response
)

(deftemplate Symptom
    (slot name
        (type SYMBOL)
        (allowed-values HBsAg antiHDV antiHBc antiHBs IgMantiHBc)
    )
    (slot diagnosis
        (type SYMBOL)
        (allowed-values undiagnosed negative positive)
        (default undiagnosed)
    )
)

(deffacts InitialSymptoms
    (Symptom (name HBsAg))
    (Symptom (name antiHDV))
    (Symptom (name antiHBc))
    (Symptom (name antiHBs))
    (Symptom (name IgMantiHBc))
)

(defrule GetHBsAg
    (Symptom (name HBsAg) (diagnosis undiagnosed))
    =>
    (bind ?response (PositiveOrNegative "HBsAg? [positive/negative] "))
    (assert (Symptom (name HBsAg) (diagnosis ?response)))
)

(defrule GetAntiHDV
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis undiagnosed))
    =>
    (bind ?response (PositiveOrNegative "anti-HDV? [positive/negative] "))
    (assert (Symptom (name antiHDV) (diagnosis ?response)))
)

(defrule GetAntiHBc1
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis undiagnosed))
    =>
    (bind ?response (PositiveOrNegative "anti-HBc? [positive/negative] "))
    (assert (Symptom (name antiHBc) (diagnosis ?response)))
)

(defrule GetAntiHBs1
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis positive))
    (Symptom (name antiHBs) (diagnosis undiagnosed))
    =>
    (bind ?response (PositiveOrNegative "anti-HBs? [positive/negative] "))
    (assert (Symptom (name antiHBs) (diagnosis ?response)))
)

(defrule GetIgMantiHBc
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis positive))
    (Symptom (name antiHBs) (diagnosis negative))
    (Symptom (name IgMantiHBc) (diagnosis undiagnosed))
    =>
    (bind ?response (PositiveOrNegative "IgM anti-HBc? [positive/negative] "))
    (assert (Symptom (name IgMantiHBc) (diagnosis ?response)))
)

(defrule GetAntiHBs2
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis undiagnosed))
    =>
    (bind ?response (PositiveOrNegative "anti-HBs? [positive/negative] "))
    (assert (Symptom (name antiHBs) (diagnosis ?response)))
)

(defrule GetAntiHBc2
    (Symptom (name HBsAg) (diagnosis negative))
    (or (Symptom (name antiHBs) (diagnosis negative)) (Symptom (name antiHBs) (diagnosis positive)))
    (Symptom (name antiHBc) (diagnosis undiagnosed))
    =>
    (bind ?response (PositiveOrNegative "anti-HBc? [positive/negative] "))
    (assert (Symptom (name antiHBc) (diagnosis ?response)))
)

(defrule IsAcuteInfection
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis positive))
    (Symptom (name antiHBs) (diagnosis negative))
    (Symptom (name IgMantiHBc) (diagnosis positive))
    =>
    (printout t "Hasil Prediksi = Acute Infection" crlf)
    (assert (certain))
)

(defrule IsChronicInfection
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis positive))
    (Symptom (name antiHBs) (diagnosis negative))
    (Symptom (name IgMantiHBc) (diagnosis negative))
    =>
    (printout t "Hasil Prediksi = Chronic Infection" crlf)
    (assert (certain))
)

(defrule IsHepatitisBPlusD
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis positive))
    =>
    (printout t "Hasil Prediksi = Hepatitis B + D" crlf)
    (assert (certain))
)

(defrule IsCured
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis positive))
    (Symptom (name antiHBc) (diagnosis positive))
    =>
    (printout t "Hasil Prediksi = Cured" crlf)
    (assert (certain))
)

(defrule IsVaccinated
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis positive))
    (Symptom (name antiHBc) (diagnosis negative))
    =>
    (printout t "Hasil Prediksi = Vaccinated" crlf)
    (assert (certain))
)

(defrule IsUnclear
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis positive))
    =>
    (printout t "Hasil Prediksi = Unclear (Possible Resolved)" crlf)
    (assert (certain))
)

(defrule IsHealthy
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis negative))
    =>
    (printout t "Hasil Prediksi = Healthy, Not Vaccinated, or Suspicious" crlf)
    (assert (certain))
)

(defrule IsUncertain
    (declare (salience -1))
    (not (certain))
    =>
    (printout t "Hasil Prediksi = Uncertain Configuration" crlf)
)