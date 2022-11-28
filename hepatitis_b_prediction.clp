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

(deftemplate Diagnose
    (slot name
        (type SYMBOL)
        (allowed-values uncertain acute chronic BplusD cured vaccinated unclear healthy)
        (default uncertain)
    )
)

(deffacts InitialSymptoms
    (Symptom (name HBsAg))
    (Symptom (name antiHDV))
    (Symptom (name antiHBc))
    (Symptom (name antiHBs))
    (Symptom (name IgMantiHBc))
)

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
    (assert (Diagnose (name acute)))
)

(defrule IsChronicInfection
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis positive))
    (Symptom (name antiHBs) (diagnosis negative))
    (Symptom (name IgMantiHBc) (diagnosis negative))
    =>
    (assert (Diagnose (name chronic)))
)

(defrule IsHepatitisBPlusD
    (Symptom (name HBsAg) (diagnosis positive))
    (Symptom (name antiHDV) (diagnosis positive))
    =>
    (assert (Diagnose (name BplusD)))
)

(defrule IsCured
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis positive))
    (Symptom (name antiHBc) (diagnosis positive))
    =>
    (assert (Diagnose (name cured)))
)

(defrule IsVaccinated
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis positive))
    (Symptom (name antiHBc) (diagnosis negative))
    =>
    (assert (Diagnose (name vaccinated)))
)

(defrule IsUnclear
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis positive))
    =>
    (assert (Diagnose (name unclear)))
)

(defrule IsHealthy
    (Symptom (name HBsAg) (diagnosis negative))
    (Symptom (name antiHBs) (diagnosis negative))
    (Symptom (name antiHBc) (diagnosis negative))
    =>
    (assert (Diagnose (name healthy)))
)

(defrule IsUncertain
    (declare (salience -1))
    (not (Diagnose (name ?)))
    =>
    (assert (Diagnose (name uncertain)))
)

(defrule DisplayAcuteInfection
    (Diagnose (name acute))
    =>
    (printout t "Hasil Prediksi = Acute Infection" crlf)
)

(defrule DisplayChronicInfection
    (Diagnose (name chronic))
    =>
    (printout t "Hasil Prediksi = Chronic Infection" crlf)
)

(defrule DisplayHepatitisBPlusD
    (Diagnose (name BplusD))
    =>
    (printout t "Hasil Prediksi = Hepatitis B+D" crlf)
)

(defrule DisplayCured
    (Diagnose (name cured))
    =>
    (printout t "Hasil Prediksi = Cured" crlf)
)

(defrule DisplayVaccinated
    (Diagnose (name vaccinated))
    =>
    (printout t "Hasil Prediksi = Vaccinated" crlf)
)

(defrule DisplayUnclear
    (Diagnose (name unclear))
    =>
    (printout t "Hasil Prediksi = Unclear (Possible Resolved)" crlf)
)

(defrule DisplayHealthy
    (Diagnose (name healthy))
    =>
    (printout t "Hasil Prediksi = Healthy, Not Vaccinated, or Suspicious" crlf)
)

(defrule DisplayUncertain
    (Diagnose (name uncertain))
    =>
    (printout t "Hasil Prediksi = Uncertain Configuration" crlf)
)