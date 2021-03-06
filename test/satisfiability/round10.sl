(set-logic LIA)
(synth-fun round ((x Int)) Int)
(define-fun distance ((a Int) (b Int)) Int (ite (<= a b) (- b a) (- a b)))
(declare-var x Int)
(declare-var alt Int)
(constraint (= (mod (round x) 10) 0))
(constraint (or (not (= (mod alt 10) 0))
		(<= (distance (round x) x)
		    (distance alt x))))
(check-synth)
