(ns z80-esil.esil-tests
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [z80-esil.opcodes :refer [opcodes]]
            [z80-esil.esil :as esil]))

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
