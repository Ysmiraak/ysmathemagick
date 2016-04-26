(ns ysmathemagick.wg-problem
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eine Studenten-WG hat fünf Zimmer nebeneinander. Das erste Zimmer ist ganz links. In jedem Zimmer wohnt ein           ;;
;; Student. Alle Studenten haben verschiedene Vornamen und studieren verschiedene Fächer in verschiedenen                ;;
;; Semestern. Sie kommen aus verschiedenen Städten und ihre Zimmer sind unterschiedlich gestrichen.                      ;;
;;                                                                                                                       ;;
;; (a) Anna studiert im ersten Semester.                                                                                 ;;
;; (b) Petra studiert Kognitionswissenschaften.                                                                          ;;
;; (c) Der Student im 17. Semester bewohnt das Zimmer (direkt) links neben dem Zimmer des Studenten im dritten Semester. ;;
;; (d) Der Student, dessen Zimmer weiß gestrichen ist, kommt aus Frankfurt.                                              ;;
;; (e) Der Student im 17. Semester studiert Medieninformatik.                                                            ;;
;; (f) Der Bewohner des mittleren Zimmers studiert Bioinformatik.                                                        ;;
;; (g) Der Student im vierten Semester bewohnt das blau gestrichene Zimmer.                                              ;;
;; (h) Jonas wohnt im ersten Zimmer.                                                                                     ;;
;; (i) Edgar kommt aus Leipzig.                                                                                          ;;
;; (j) Pauls Zimmer ist rot gestrichen.                                                                                  ;;
;; (k) Der Student mit dem gelb gestrichenen Zimmer wohnt neben dem Studenten aus Hamburg.                               ;;
;; (l) Der Bewohner des grün gestrichenen Zimmers studiert Informatik.                                                   ;;
;; (m) Der Student aus Stuttgart wohnt neben dem Studenten mit dem blau gestrichenen Zimmer.                             ;;
;; (n) Jonas wohnt neben dem Studenten im zweiten Semester.                                                              ;;
;; (o) Der Bewohner des gelb gestrichenen Zimmers hat einen Nachbarn, der Medizininformatik studiert.                    ;;
;;                                                                                                                       ;;
;; Einer der Studenten kommt aus Tübingen. Wie heißt er/sie?                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->student
  "Returns a map with six fields representing a student. New logical
  variables are created as values for any of the missing fields:
  :name :room :semester :subject :wall-color :home-city"
  [& {:keys [name room subject semester wall-color home-city] :as student}]
  (apply assoc student
         (interleave (reduce disj #{:name :room :semester :subject :wall-color :home-city}
                             (keys student))
                     (repeatedly lvar))))

(defn solve-wg-problem
  "Finds n solutions." [n]
  (let [students  (mapv #(->student :room %) (range 1 6))
        studento  #(membero (apply ->student %&) students)
        neighboro (fn [s1 s2 & {d :direction :or {d :both}}]
                    (let [s1 (apply ->student s1), r1 (:room s1)
                          s2 (apply ->student s2), r2 (:room s2)]
                      (all
                       (membero s1 students)
                       (membero s2 students)
                       (case d
                         :left  (fd/- r2 r1 1)
                         :right (fd/- r1 r2 1)
                         :both  (conde
                                 ((fd/- r2 r1 1))
                                 ((fd/- r1 r2 1)))))))]
    (run n [q]
      (== q students)
      ;; Einer der Studenten kommt aus Tübingen. Wie heißt er/sie?
      (studento :home-city "Tübingen")
      ;; (a) Anna studiert im ersten Semester.
      (studento :name "Anna" :semester 1)
      ;; (b) Petra studiert Kognitionswissenschaften.
      (studento :name "Petra" :subject "Kognitionswissenschaft")
      ;; (c) Der Student im 17. Semester bewohnt das Zimmer (direkt) links neben dem Zimmer des Studenten im dritten Semester.
      (neighboro [:semester 17] [:semester 3] :direction :left)
      ;; (d) Der Student, dessen Zimmer weiß gestrichen ist, kommt aus Frankfurt.
      (studento :wall-color "weiß" :home-city "Frankfurt")
      ;; (e) Der Student im 17. Semester studiert Medieninformatik.
      (studento :semester 17 :subject "Medieninformatik")
      ;; (f) Der Bewohner des mittleren Zimmers studiert Bioinformatik.
      (studento :room 3 :subject "Bioinformatik")
      ;; (g) Der Student im vierten Semester bewohnt das blau gestrichene Zimmer.
      (studento :semester 4 :wall-color "blau")
      ;; (h) Jonas wohnt im ersten Zimmer.
      (studento :name "Jonas" :room 1)
      ;; (i) Edgar kommt aus Leipzig.
      (studento :name "Edgar" :home-city "Leipzig")
      ;; (j) Pauls Zimmer ist rot gestrichen.
      (studento :name "Paul" :wall-color "rot")
      ;; (k) Der Student mit dem gelb gestrichenen Zimmer wohnt neben dem Studenten aus Hamburg.
      (neighboro [:wall-color "gelb"] [:home-city "Hamburg"])
      ;; (l) Der Bewohner des grün gestrichenen Zimmers studiert Informatik.
      (studento :wall-color "grün" :subject "Informatik")
      ;; (m) Der Student aus Stuttgart wohnt neben dem Studenten mit dem blau gestrichenen Zimmer.
      (neighboro [:home-city "Stuttgart"] [:wall-color "blau"])
      ;; (n) Jonas wohnt neben dem Studenten im zweiten Semester.
      (neighboro [:name "Jonas"] [:semester 2])
      ;; (o) Der Bewohner des gelb gestrichenen Zimmers hat einen Nachbarn, der Medizininformatik studiert.
      (neighboro [:wall-color "gelb"] [:subject "Medizininformatik"]))))

;; (solve-wg-problem 1)

(def solutions (solve-wg-problem (reduce * (repeat 6 5))))

;; (count solutions) 1
;; (first solutions)
[{:home-city "Hamburg", :name "Jonas", :wall-color "blau", :room 1, :subject "Medizininformatik", :semester 4}
 {:home-city "Stuttgart", :name "Petra", :wall-color "gelb", :room 2, :subject "Kognitionswissenschaft", :semester 2}
 {:home-city "Frankfurt", :name "Anna", :wall-color "weiß", :room 3, :subject "Bioinformatik", :semester 1}
 {:home-city "Tübingen", :name "Paul", :wall-color "rot", :room 4, :subject "Medieninformatik", :semester 17}
 {:home-city "Leipzig", :name "Edgar", :wall-color "grün", :room 5, :subject "Informatik", :semester 3}]
