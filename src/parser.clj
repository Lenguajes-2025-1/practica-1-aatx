(ns parser
  (:require [grammars :as wae]))

(defn parser-AE
    (cond 
      (number? exp) (numG exp)
      (symbol? exp) (idG exp)
      (sequential? exp)
      (case (first exp)
        + (addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
        - (subG (parser-AE (second exp)) (parser-AE (nth exp 2))))
      :else (throw (Exception. (str "Expresión no valida: " exp)))))

(defn parser-WAE [exp]
  (cond
      (number? exp) (numG exp)
      (symbol? exp) (idG exp)
      (sequential? exp)
      (case (first exp)
        + (addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        - (subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
        with (let [[_ id value body] exp]
               (WithG (parser-WAE id) (parser-WAE value) (parser-WAE body))))
      :else (throw (Exception. (str "Expresión no valida: " exp)))))
