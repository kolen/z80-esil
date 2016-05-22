(ns z80-esil.esil-tests
  (:require [clojure.string :as s]
            [clojure.test :refer :all]
            [instaparse.core :as insta]
            [z80-esil.opcodes :refer [opcodes]]
            [z80-esil.esil :as esil]))

(def all-ops
  (filter some? (flatten (vals opcodes))))

(deftest parsing
  (let [failures (filter insta/failure?
                         (for [opc (flatten (vals opcodes)) :when (some? opc)]
                           (esil/parse-instr opc)))]
    (is (empty? failures) (str "Unparsed instructions: " failures))))

(deftest opdata
  (let [instructions (for [opc (flatten (vals opcodes)) :when (some? opc)]
                       (esil/parse-instr opc))
        opdatas (for [instr instructions] (esil/opdata instr))]
    (is (every? map? opdatas))))

(deftest args
  (dorun
   (for [op all-ops]
     (let [ast (esil/parse-instr op)
           opdata (esil/opdata ast)]
       (is
        (cond
          (s/includes? op "%02") (= :arg-8 (opdata :arg))
          (s/includes? op "%04") (= :arg-16 (opdata :arg))
          :else (not (contains? opdata :arg)))
        (str "Op " op ": invalid arg " (opdata :arg) ", op: "
             opdata ", ast: " ast))))))
