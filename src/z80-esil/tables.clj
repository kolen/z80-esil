(ns z80-esil.tables
  (:require [clojure.string :as s]
            [z80-esil.esil :refer [opdata parse-instr]]))

(defn const
  ([kw] (const nil kw))
  ([prefix kw]
   (if kw
     (s/upper-case
      (s/replace
       (str (if prefix (str prefix "-") "") (if (keyword? kw) (name kw) kw)) "-" "_"))
     "NULL")))

;; instruction, reg1, reg2, arg, cond, addr
(defn instr-record [i]
  [(const "op" (i :i))
   (const "reg" (some->> i :r first))
   (const "reg" (some->> i :r second))
   (const (i :arg))
   (const "cond" (i :cond))
   (if-let [addr (i :addr)] (str addr) "NULL")])

(defn instr-record-s [rec raw]
  {:pre [(= (count rec) 6)]}
  (s/trimr
   (apply format "\t{%-20s %s, %s, %s, %s, %s}, // %s"
          (str (first rec) ",")
          (concat (rest rec)
                  [(-> raw
                       (s/replace "0x%02x" "*")
                       (s/replace "0x%04x" "**"))]))))

(defn block [name type instrs]
  (format
   "static %s %s[]={\n%s\n};\n"
   type
   name
   (s/join "\n"
           (for [instr instrs]
             (-> instr parse-instr opdata instr-record (instr-record-s instr))))))

(defn all-blocks []
  (s/join
   "\n"
   (for [[key name] '([nil "main"] [:dd "dd"] [:fd "ed"] [:ed "fd"])]
     (block (str "z80_ops_" name) "loltype" (z80-esil.opcodes/opcodes key)))))

(spit "/tmp/test.c" (all-blocks))
