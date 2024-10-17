(ns interp
    (:require [grammars :as wae]
              [parser :as ps]))
  
  (defn interprete-AE [exp]
    (let [pars (ps/parser-AE exp)]
    (cond
      (instance? wae/NumG pars) (:n pars)
      (instance? wae/AddG pars) (+ (interprete-AE (:left pars)) (interprete-AE (:right pars)))
      (instance? wae/SubG pars) (- (interprete-AE (:left pars)) (interprete-AE (:right pars))))
      :else (throw (Exception. (str "La expresión no es valida: " exp))) ))



(defn interprete-WAE [exp]
  (let [pars (ps/parser-WAE exp)]
    (cond
      (instance? wae/NumG pars) (:n pars)
      (instance? wae/IdG pars) (get pars (:id exp))
      (instance? wae/AddG pars) (+ (interprete-WAE (:left pars)) (interprete-WAE (:right pars)))
      (instance? wae/SubG pars) (- (interprete-WAE (:left pars)) (interprete-WAE (:right pars)))
      (instance? wae/WithG pars) (let [id (:id exp) value (interprete-WAE (:value exp) pars) body (:body exp)]
                                   interprete-WAE body (update pars id value)))
       :else (throw (Exception. (str "La expresión no es valida: " exp)))))
