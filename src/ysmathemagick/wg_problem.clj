(ns ysmathemagick.wg-problem
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(def room-numbers (fd/domain 1 2 3 4 5))

(def students (->> (repeatedly (* 5 6) lvar) (partition 6) (mapv vec)))

(let [[names rooms semesters subjects wall-colors home-cities]
      (apply map vector students)]
  (def names names)
  (def rooms rooms)
  (def semester semesters)
  (def subjects subjects)
  (def wall-colors wall-colors)
  (def home-cities home-cities))

(defn studento
  [& {name :name room :room subject :subject semester :semester wall-color :wall-color home-city :home-city
      :or {name (lvar) room (lvar) subject (lvar) semester (lvar) wall-color (lvar) home-city (lvar)}}]
  (membero [name room semester subject wall-color home-city] students))

(defn neighboro
  [[& {name :name room :room subject :subject semester :semester wall-color :wall-color home-city :home-city
       :or {name (lvar) room (lvar) subject (lvar) semester (lvar) wall-color (lvar) home-city (lvar)}}]
   [& {name' :name room' :room subject' :subject semester' :semester wall-color' :wall-color home-city' :home-city
       :or {name' (lvar) room' (lvar) subject' (lvar) semester' (lvar) wall-color' (lvar) home-city' (lvar)}}]
   & {direction :direction :or {direction :both}}]
  (all
   (membero [name room semester subject wall-color home-city] students)
   (membero [name' room' semester' subject' wall-color' home-city'] students)
   (case direction
     :left  (fd/- room' room 1)
     :right (fd/- room room' 1)
     :both  (conde
             ((fd/- room room' 1))
             ((fd/- room' room 1))))))

(defn solve-wg-problem [query-target number-of-solutions]
  (run number-of-solutions [q]
    (== q query-target)
    (everyg #(fd/dom % room-numbers) rooms)
    (fd/distinct rooms)
    ;; Einer der Studenten kommt aus Tübingen. Wie heißt er/sie?
    (studento :home-city :Tübingen)
    ;; (a) Anna studiert im ersten Semester.
    (studento :name :Anna :semester 1)
    ;; (b) Petra studiert Kognitionswissenschaften.
    (studento :name :Petra :subject :Kognitionswissenschaft)
    ;; (c) Der Student im 17. Semester bewohnt das Zimmer (direkt) links neben dem Zimmer des Studenten im dritten Semester.
    (neighboro [:semester 17] [:semester 3] :direction :left)
    ;; (d) Der Student, dessen Zimmer weiß gestrichen ist, kommt aus Frankfurt.
    (studento :wall-color :weiß :home-city :Frankfurt)
    ;; (e) Der Student im 17. Semester studiert Medieninformatik.
    (studento :semester 17 :subject :Medieninformatik)
    ;; (f) Der Bewohner des mittleren Zimmers studiert Bioinformatik.
    (studento :room 3 :subject :Bioinformatik)
    ;; (g) Der Student im vierten Semester bewohnt das blau gestrichene Zimmer.
    (studento :semester 4 :wall-color :blau)
    ;; (h) Jonas wohnt im ersten Zimmer.
    (studento :name :Jonas :room 1)
    ;; (i) Edgar kommt aus Leipzig.
    (studento :name :Edgar :home-city :Leipzig)
    ;; (j) Pauls Zimmer ist rot gestrichen.
    (studento :name :Paul :wall-color :rot)
    ;; (k) Der Student mit dem gelb gestrichenen Zimmer wohnt neben dem Studenten aus Hamburg.
    (neighboro [:wall-color :gelb] [:home-city :Hamburg])
    ;; (l) Der Bewohner des grün gestrichenen Zimmers studiert Informatik.
    (studento :wall-color :grün :subject :Informatik)
    ;; (m) Der Student aus Stuttgart wohnt neben dem Studenten mit dem blau gestrichenen Zimmer.
    (neighboro [:home-city :Stuttgart] [:wall-color :blau])
    ;; (n) Jonas wohnt neben dem Studenten im zweiten Semester.
    (neighboro [:name :Jonas] [:semester 2])
    ;; (o) Der Bewohner des gelb gestrichenen Zimmers hat einen Nachbarn, der Medizininformatik studiert.
    (neighboro [:wall-color :gelb] [:subject :Medizininformatik])))

(def student-map
  (as-> (fn [[name room semester subject wall-color home-city]]
          [[name home-city] {:room [room wall-color] :study [subject semester]}]) $
    (map $ students)
    (into {} $)))

(def results (solve-wg-problem student-map (int (Math/pow 5 6))))

;; (count results) 120
;; (def unique-results (set results))
;; (count unique-results) 1
;; (vec (sort-by #(-> % second :room first) (first unique-results)))
[[[:Jonas :Hamburg]   {:room [1 :blau], :study [:Medizininformatik 4]}]
 [[:Petra :Stuttgart] {:room [2 :gelb], :study [:Kognitionswissenschaft 2]}]
 [[:Anna  :Frankfurt] {:room [3 :weiß], :study [:Bioinformatik 1]}]
 [[:Paul  :Tübingen]  {:room [4 :rot],  :study [:Medieninformatik 17]}]
 [[:Edgar :Leipzig]   {:room [5 :grün], :study [:Informatik 3]}]]
