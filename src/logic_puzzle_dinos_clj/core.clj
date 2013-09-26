(ns logic-puzzle-dinos-clj.core
  (:gen-class)
  (:refer-clojure :exclude [== != < get-dom])
  (:use clojure.core.logic
        clojure.pprint
       [clojure.tools.macro :only [symbol-macrolet]]))

;; learning core.logic through logic puzzles.  Translated for this puzzle from 
;; Kris Jenkins' awesome blog post. 

(defn show
  [x]
  (time (pprint [x (java.util.Date.)])))

(defne lefto
  "x appears to the left of y in collection l."
  [x y l]
  ([_ _ [x . tail]] (membero y tail))
  ([_ _ [_ . tail]] (lefto x y tail)))

;; clues
;; The mirasaurus wasn't found in Guatemala.
;; The pilodontus costs 500 dollars less than the fossil discovered in Yemen.
;; The $1250 fossil is either the mirasaurus or the archadon.
;; The $1500 fossil isn't 90 million years old.
;; The rotosaurus wasn't found in Guatemala.
;; The archadon costs 250 dollars less than the mirasaurus.
;; The fossil discovered in Serbia isn't 78 million years old.
;; The $500 fossil isn't 85 million years old.
;; The pilodontus isn't 90 million years old.
;; The $500 fossil was found in Iceland.
;; The fossil discovered in Yemen costs 500 dollars less than the verasaurus.
;; The 78 million year old fossil costs 250 dollars more than the 87 million year old fossil.
;; The archadon is 87 million years old.

;;

;; steps to converting logic problems
;; classes of rules
;;   simple facts
;;   negated facts
;;   related pairs
;;   mutual exclusion
;;   relative facts  (use lefto)

;; dino ages prices countries

;;   simple facts
;; The archadon is 87 million years old.
;; The $500 fossil was found in Iceland.
(defn simple-rules
  "The archadon is 87 million years old.
   The $500 fossil was found in Iceland."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
      (fresh [s1 s2 s3 s4]
             (== [:archodon 87 _ _] s1)
             (== [_ _ 500 :iceland] s2)
             (membero s1 answers)
             (membero s2 answers))))

;;   negated facts
;; The mirasaurus wasn't found in Guatemala.
;; The $1500 fossil isn't 90 million years old.
;; The pilodontus isn't 90 million years old.
;; The $500 fossil isn't 85 million years old.
;; The fossil discovered in Serbia isn't 78 million years old.
;; The rotosaurus wasn't found in Guatemala.
(defn negated-rules
  [answers]
  (symbol-macrolet
   [_ (lvar)]
     (fresh [s1 s2
             s3 s4
             s5 s6
             s7 s8
             s9 s10
             s11 s12]
           (== [:mirasaurus _ _ _ ] s1)
           (== [_ _ _ :guatemala ] s2)
           (membero s1 answers)
           (membero s2 answers)
           (!= s1 s2)
           (== [_ 90 _ _ ] s3)
           (== [_ _ 1500 _] s4)
           (membero s3 answers)
           (membero s4 answers)
           (!= s3 s4)
           (== [:pilodontus _ _ _] s5)
           (== [_ 90 _ _] s6)
           (membero s5 answers)
           (membero s6 answers)
           (!= s5 s6)
           (== [_ 85 _ _] s7)
           (== [_ _ 500 _] s8)
           (membero s7 answers)
           (membero s8 answers)
           (!= s7 s8)
           (== [_ 78 _ _] s9)
           (== [_ _ _ :serbia] s10)
           (membero s9 answers)
           (membero s10 answers)
           (!= s9 s10)
           (== [:rotosaurus _ _ _] s11)
           (== [_ _ _ :guatemala] s12)
           (membero s11 answers)
           (membero s12 answers)
           (!= s11 s12))))


;;   mutual exclusion
;; The $1250 fossil is either the mirasaurus or the archadon.
(defn related-rules
  "The $1250 fossil is either the mirasaurus or the archadon."
  [answers]
  (symbol-macrolet
   [_ (lvar)]
     (fresh [p1 p2 s1 s2]
           (membero s1 answers)
           (membero s2 answers)
            (== [:mirasaurus _ p1 _ ] s1)
            (== [:archodon _ p2 _ ] s2)
            (!= s1 s2))))


;;   relative facts  (use lefto)
;; The pilodontus costs 500 dollars less than the fossil discovered in Yemen.
;; The 78 million year old fossil costs 250 dollars more than the 87 million year old fossil.
;; The fossil discovered in Yemen costs 500 dollars less than the verasaurus.
;; The archadon costs 250 dollars less than the mirasaurus.
(defn relative-rules
  [answers]
  (let [interval-250 [[500 750] [750 1000] [1000 1250] [1250 1500]]
        interval-500 [[500 1000] [750 1250] [1000 1500]]]
  (symbol-macrolet
   [_ (lvar)]
     (fresh [p1 p2
             p3 p4
             p5 p6
             p7 p8]
            (membero [:pilodontus _ p1 _ ] answers)
            (membero [_ _ p2 :yemen ] answers)
            (membero [p1 p2] interval-500)
            (lefto p1 p2 [500 750 1000 1250 1500])

            (membero [_ 78 p3 _ ] answers)
            (membero [_ 87 p4 _ ] answers)
            (membero [p4 p3] interval-250 )
            (lefto p4 p3 [500 750 1000 1250 1500])

            (membero [_ _ p5 :yemen ] answers)
            (membero [:verasaurus _ p6 _ ] answers)
            (membero [p5 p6] interval-500)
            (lefto p5 p6 [500 750 1000 1250 1500])

            (membero[:archodon _ p7 _ ] answers)
            (membero [:mirasaurus _ p8 _ ] answers)
            (membero [p7 p8] interval-250 )
            (lefto p7 p8 [500 750 1000 1250 1500])
            ))))



;; the query run
(defn run-it [n]
   ;; construct lvars list
  (let [dinos       (repeatedly 5 lvar)
        ages       (repeatedly 5 lvar)
        prices    (repeatedly 5 lvar)
        countries (repeatedly 5 lvar)
        answers (map list dinos ages prices countries)]
    ;; run logic engine
    (run n [q]

         ;; set our 'domain' and set an order to one of the rows
          (== q answers)
          (== dinos [:archodon :mirasaurus :pilodontus :rotosaurus :verasaurus])

         ;; apply rules
          (simple-rules answers)
          (negated-rules answers)
          (related-rules answers)
          (relative-rules answers)

         ;; allow permutations on unset rows
          (permuteo ages [69 78 85 87 90])
          (permuteo prices [500 750 1000 1250 1500])
          (permuteo countries [:guatemala :iceland :serbia :uzbekistan :yemen]))))

;; main
(defn -main 
  [& args]
   (println (run-it 1)))

;;(run-it 1)


