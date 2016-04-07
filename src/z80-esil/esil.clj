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

;; set flag to 1 if arg is non-zero otherwise 0
(defn flag-set [flag]
  (str "?{1," flag ",=},!,?{0," flag ",=}"))

(defn flags [& args]
  (->>
   (for [[flag arg] (partition 2 args)]
     (if (or (= arg 0) (= arg 1) (= arg "iff2"))
       [arg (name flag) "="]
       (do
         (prn arg)
         (assert (#{"a" "b" "c" "d" "e" "h" "l"} arg) "Only for 8-bit registers")
         (case flag
           :sf [arg "0x40" "&" (flag-set "sf")] ;; only for 8-bit
           :zf ["$z" "zf" "="] ;; arg ignored
           :yf [arg "0x20" "&" (flag-set "sf")] ;; copy of bit 5, undocumented
           :hf (assert false "Not implemented")
           :xf [arg "0x04" "&" (flag-set "xf")] ;; copy of bit 3, undocumented
           :pf (assert false "Not implemented")
           :nf (assert false "Not implemented")
           :cf ["$c8" "cf" "="]                 ;; relies on $c8 and notion of
           ;; 'result'
           ))))
   flatten
   (s/join ",")))

(defn esil [instr]
  (match instr
         [:instruction [opname & args]] (apply (handler-fn opname "esil-") args)))

(defn esil-flags [instr]
  (match instr
         [:instruction [opname & args]] (apply (handler-fn opname "flags-") args)))

(defn handler-fn [opname prefix]
  {:post [%]}
  (prn opname)
  (find-var (symbol "z80-esil.esil" (str prefix (name opname)))))

(defn esil-ld [pair]
  (match
   pair
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

(defn flags-ld [pair]
  (match pair
         ;; ld a, i; ld a, r -- special case
         [:pair-8 [:register-8 "a"] [:register-8 (rs :guard #{"i" "r"})]]
         (flags :sf "a" :zf "a" :yf "a" :hf 0 :xf "a" :pf "iff2" :nf 0)))

(esil (parse-instr "ld a, r"))

(esil-flags (parse-instr "ld a, r"))

(filter insta/failure?
        (for [opc (flatten (vals opcodes)) :when (some? opc)]
          (parse-instr opc)))
