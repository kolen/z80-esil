(ns z80-esil.esil
  (:require [clojure.string :as s]
            [clojure.java.io :refer [resource]]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [z80-esil.opcodes :refer [opcodes]]))

(def parse-instr
  (insta/parser (resource "z80.bnf")))

(parse-instr "ld hl, [bc]")

(defn build [& args]
  (s/join "," args))

(defn flag-arg [arg])

(defn flags [& args]
  (for [[flag arg] (partition 2 args)]
    (if (or (= arg 0) (= arg 1))
      (s/join "," [arg (name flag) "="])
      (case flag
        :sf
        :zf
        :yf
        :hf
        :xf
        :pf
        :nf
        :cf))))

(defn esil [instr]
  (match instr
         [:instruction [opname & args]] (apply (handler-fn opname) args)))

(defn handler-fn [opname]
  {:post [%]}
  (prn opname)
  (find-var (symbol "z80-esil.esil" (str "esil-" (name opname)))))

(defn esil-ld [pair]
  (match
   pair
   ;; ld a, i; ld a,r special cases
   [:pair-8 [:register-8 "a"] [:register-8 (rs :guard #{"i" "r"})]]
   (build rs "a" "=" "$z" "zf" "=" "iff2" "pf" "=") ;; todo: s flag
   ;; copy from one register to another (1 or 2 bytes)
   [_ [(:or :register-8 :register-16) r1] [(:or :register-8 :register-16) r2]]
   (build r2 r1 "=")
   ;; read 1 byte from memory
   [:pair-8 [:register-8 rd] [:addr _]]
   (build :arg-16 "[1]" rd "=")
   [:pair-8 [:register-8 rd] [:addr-from-register [:register-16 rs]]]
   (build rs "[1]" rd "=")
   ;; read 2 bytes from memory
   [:pair-16 [:register-16 rd] [:addr _]]
   (build :arg-16 "[2]" rd "=")
   [:pair-16 [:register-16 rd] [:addr-from-register [:register-16 rs]]]
   (build rs "[2]" rd "=")
   ;; write 1 byte to memory
   [:pair-8 [:addr _] [:register-8 rs]]
   (build rs :arg-16 "=[1]")
   [:pair-8 [:addr-from-register [:register-16 rd]] [:register-8 rs]]
   (build rs rd "=[1]")
   ;; write 2 bytes to memory
   [:pair-16 [:addr _] [:register-16 rs]]
   (build rs :arg-16 "=[2]")
   [:pair-16 [:addr-from-register [:register-16 rd]] [:register-16 rs]]
   (build rs rd "=[2]")))

(esil (parse-instr "ld a, r"))



(filter insta/failure?
        (for [opc (flatten (vals opcodes)) :when (some? opc)]
          (parse-instr opc)))
