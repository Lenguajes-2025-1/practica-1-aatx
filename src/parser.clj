(ns parser
  (:require [grammars :as wae]))

(defn parser-AE [exp]
    (cond 
      (number? exp) (wae/numG exp)
      (sequential? exp)
      (case (first exp)
        + (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
        - (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2))))
      :else (throw (Exception. (str "Expresión no valida: " exp)))))

(defn parser-WAE [exp]
  (cond
      (number? exp) (wae/numG exp)
      (symbol? exp) (wae/idG exp)
      (sequential? exp)
      (case (first exp)
        + (wae/addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        - (wae/subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        with (let [[_ id value body] exp]
               (wae/withG (parser-WAE id) (parser-WAE value) (parser-WAE body))))
      :else (throw (Exception. (str "Expresión no valida: " exp)))))
