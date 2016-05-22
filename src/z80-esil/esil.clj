(ns z80-esil.esil
  (:require [clojure.string :as s]
            [clojure.java.io :refer [resource]]
            [clojure.core.match :refer [match]]
            [instaparse.core :as insta]
            [z80-esil.opcodes :refer [opcodes]]))

(def parse-instr
  (insta/parser (resource "z80.bnf")))

(defn build [& args]
  (s/join "," args))

(defn handler-fn [opname prefix]
  (or
   (find-var (symbol "z80-esil.esil" (str prefix (name opname))))
   (throw (Exception. (str "handler " prefix (name opname) " not found")))))

(defn opdata "Return op data structure for instr (parsed tree)"
  [instr]
  (match instr
         [:instruction [opname]]
         {:i opname} ;; when without args
         [:instruction
          [_
           [op-ld
            [:register-8 rd]
            [_ [:addr [:literal-8-with-index ixiy [:literal-8]]]]]]]
         {:i op-ld :r [rd ixiy]}
         [:instruction
          [:ld [:pair-8
                [:register-8 rd]
                op
                [:addr [:literal-8-with-index ixiy [:literal-8]]]]]]
         {:i (keyword (str op "-ld")) :r [rd ixiy]}
         [:instruction [opname & args]]
         (apply (handler-fn opname "opdata-") args)
         (fail :guard insta/failure?)
         (throw (IllegalStateException.
                 (prn-str (insta/get-failure fail))))))

(defn opdata-ld [pair]
  (match
   pair
   ;; copy from one register to another (1 or 2 bytes)
   [_ [(:or :register-8 :register-16) r1] [(:or :register-8 :register-16) r2]]
   {:i :ld-reg-reg :r [r1 r2]}
   ;; read 1 byte from memory
   [:pair-8 [:register-8 rd] [:addr _]]
   {:i :ld-8-reg-addr :r [rd] :arg :arg-16}
   [:pair-8 [:register-8 rd] [:addr-from-register [:register-16 rs]]]
   {:i :ld-8-reg-areg :r [rd rs]}
   [:pair-8 [:addr [:literal-8-with-index in [:literal-8]]] [:literal-8]]
   {:i :ld-8-ixiy-arg :r [in] :arg :arg-8}
   ;; read 2 bytes from memory
   [:pair-16 [:register-16 rd] [:addr _]]
   {:i :ld-16-reg-addr :r [rd] :arg :arg-16}
   [:pair-16 [:register-16 rd] [:addr-from-register [:register-16 rs]]]
   {:i :ld-16-reg-areg :r [rd rs]}
   ;; write 1 byte to memory
   [:pair-8 [:addr _] [:register-8 rs]]
   {:i :ld-8-addr-reg :r [rs] :arg :arg-16}
   [:pair-8 [:addr-from-register [:register-16 rd]] [:register-8 rs]]
   {:i :ld-8-areg-reg :r [rd rs]}
   [:pair-8 [:addr-from-register [:register-16 rd]] [:literal-8]]
   {:i :ld-8-areg-arg :r [rd] :arg :arg-8}
   ;; write 2 bytes to memory
   [:pair-16 [:addr _] [:register-16 rs]]
   {:i :ld-16-addr-reg :r [rs] :arg :arg-16}
   [:pair-16 [:addr-from-register [:register-16 rd]] [:register-16 rs]]
   {:i :ld-16-areg-reg :r [rd rs]}
   ;; write arg to register
   [:pair-16 [:register-16 rd] [:literal-16]]
   {:i :ld-16-reg-arg :r [rd] :arg :arg-16}
   [:pair-8 [:register-8 rd] [:literal-8]]
   {:i :ld-8-reg-arg :r [rd] :arg :arg-8}
   ;; ix and bits
   [:pair-8 [:register-8 rd] []] nil))

(defn opdata-push [register]
  (match register [:register-16 r]
         {:i :push :r [r]}))

(defn opdata-pop [register]
  (match register [:register-16 r]
         {:i :pop :r r}))

(defn opdata-ex [pair]
  (match
   pair
   [:pair-16 [:register-16 "de"] [:register-16 "hl"]]
   {:i :ex-de-hl}
   [:pair-16 [:addr-from-register [:register-16 "sp"]] [:register-16 r2]]
   {:i :ex-a-sp-reg :r [r2]}))

(defn opdata-add [pair]
  (match
   pair
   [:pair-8 [:register-8 "a"] [:register-8 rs]]
   {:i :add-reg :r [rs]}
   [:pair-8 [:register-8 "a"] [:addr-from-register [:register-16 "hl"]]]
   {:i :add-ahl}
   [:pair-8 [:register-8 "a"] [:addr [:literal-8-with-index in [:literal-8]]]]
   {:i :add-ixiy :r [in] :arg :arg-8}
   [:pair-8 [:register-8 "a"] [:literal-8]]
   {:i :add-arg :arg :arg-8}
   [:pair-16 [:register-16 rd] [:register-16 rs]]
   {:i :add-16-reg-reg :r [rd rs]}))

(defn opdata-adc [pair]
  (match
   pair
   [:pair-8 [:register-8 "a"] [:register-8 rs]]
   {:i :adc :r [rs]}
   [:pair-8 [:register-8 "a"] [:addr-from-register [:register-16 "hl"]]]
   {:i :adc-ahl}
   [:pair-8 [:register-8 "a"] [:literal-8]]
   {:i :adc-arg :arg :arg-8}
   [:pair-8 [:register-8 "a"] [:addr [:literal-8-with-index in [:literal-8]]]]
   {:i :adc-ixiy :arg :arg-8}
   [:pair-16 [:register-16 rd] [:register-16 rs]]
   {:i :adc-16-reg-reg :r [rd rs]}))

(defn opdata-sub [reg]
  (match
   reg
   [:register-8 rs]
   {:i :sub :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :sub-ahl}
   [:literal-8]
   {:i :sub-arg :arg :arg-8}
   [:pair-8 [:register-8 "a"] [:register-8 rs]]
   {:i :sub :r [rs]}
   [:addr [:literal-8-with-index in [:literal-8]]]
   {:i :sub-ixiy :arg :arg-8}))

(defn opdata-sbc [pair]
  (match
   pair
   [:pair-8 [:register-8 "a"] [:register-8 rs]]
   {:i :sbc :r [rs]}
   [:pair-8 [:register-8 "a"] [:addr-from-register [:register-16 "hl"]]]
   {:i :sbc-ahl}
   [:pair-8 [:register-8 "a"] [:literal-8]]
   {:i :sbc-arg}
   [:pair-8 [:register-8 "a"] [:addr [:literal-8-with-index in [:literal-8]]]]
   {:i :sbc-ixiy :arg :arg-8}
   [:pair-16 [:register-16 rd] [:register-16 rs]]
   {:i :sbc-16-reg-reg :r [rd rs]}))

(defn opdata-and [reg]
  (match
   reg
   [:register-8 rs]
   {:i :and :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :and-ahl}
   [:literal-8]
   {:i :and-arg :arg :arg-8}
   [:addr [:literal-8-with-index in [:literal-8]]]
   {:i :and-ixiy :arg :arg-8}))

(defn opdata-or [reg]
  (match
   reg
   [:register-8 rs]
   {:i :or :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :or-ahl}
   [:literal-8]
   {:i :or-arg :arg :arg-8}
   [:addr [:literal-8-with-index in [:literal-8]]]
   {:i :or-ixiy :arg :arg-8}))

(defn opdata-xor [reg]
  (match
   reg
   [:register-8 rs]
   {:i :xor :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :xor-ahl}
   [:literal-8]
   {:i :xor-arg :arg :arg-8}
   [:addr [:literal-8-with-index in [:literal-8]]]
   {:i :xor-ixiy :arg :arg-8}))

(defn opdata-cp [reg]
  (match
   reg
   [:register-8 rs]
   {:i :cp :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :cp-ahl}
   [:literal-8]
   {:i :cp-arg :arg :arg-8}
   [:addr [:literal-8-with-index in [:literal-8]]]
   {:i :cp-ixiy :arg :arg-8}))

(defn opdata-inc [reg]
  (match
   reg
   [:register-8 rs]
   {:i :inc-reg :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :inc-ahl}
   [:addr [:literal-8-with-index in [:literal-8]]]
   {:i :inc-aixiy :r [in] :arg :arg-8}
   [:register-16 rs]
   {:i :inc-16-reg :r [rs]}))

(defn opdata-dec [reg]
  (match
   reg
   [:register-8 r]
   {:i :dec-8-reg :r [r]}
   [:register-16 rs]
   {:i :dec-16-reg :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :dec-16-ahl}
   [:addr [:literal-8-with-index in [:literal-8]]]
   {:i :dec-8-ixiy :r [in] :arg :arg-8}))

(defn opdata-rlc [reg]
  (match
   reg
   [:register-8 r]
   {:i :rlc-reg :r [r]}
   [:addr-from-register [:register-8 r]]
   {:i :rlc-areg :r [r]}
   [:rlc-simple [:addr [:literal-8-with-index ixiy [:literal-8]]]]
   {:i :rlc-ixiy :r [ixiy]}))

(defn opdata-rrc [reg]
  (match
   reg
   [:register-8 r]
   {:i :rrc-reg :r [r]}
   [:addr-from-register [:register-8 r]]
   {:i :rrc-areg :r [r]}
   [:rrc-simple [:addr [:literal-8-with-index ixiy [:literal-8]]]]
   {:i :rrc-ixiy :r [ixiy]}))


(defn opdata-jp [unc-or-cond]
  (match
   unc-or-cond
   [:jp-unc [:literal-16]]
   {:i :jp-unc-addr :arg :arg-16}
   [:jp-unc [:addr-from-register [:register-16 r]]]
   {:i :jp-unc-areg :r [r]}
   [:jp-cond [:condition c] [:literal-16]]
   {:i :jp-cond-addr :cond c :arg :arg-16}))

(defn opdata-jr [unc-or-cond]
  (match
   unc-or-cond
   [:jr-unc [:literal-8]]
   {:i :jr-unc-addr :arg :arg-8}
   [:jr-cond [:condition c] [:literal-8]]
   {:i :jr-cond-addr :cond c :arg :arg-8}))

(defn opdata-djnz [literal]
  (match
   literal
   [:literal-8]
   {:i :djnz :arg :arg-8}))

(defn opdata-call [unc-or-cond]
  (match
   unc-or-cond
   [:call-unc [:literal-16]]
   {:i :call-unc :arg :arg-16}
   [:call-cond [:condition c] [:literal-16]]
   {:i :call-cond :arg :arg-16 :cond c}))

(defn opdata-ret [unc-or-cond]
  (match
   unc-or-cond
   [:ret-unc]
   {:i :ret-unc}
   [:ret-cond [:condition c]]
   {:i :ret-cond :cond c}))

(defn opdata-rst [addr]
  {:i :rst :addr addr})

(defn opdata-in
  ([position-8 port]
   (match
    [position-8 port]
    [[:register-8 "a"] [:port [:literal-8]]]
    {:i :in-a :arg :arg-8}
    [[:register-8 r] [:port "[c]"]]
    {:i :in-reg-ac :r [r]}))
  ([port]
   (match
    port
    [:port :arg-8]
    {:i :in-f :arg :arg-8}
    [:port "[c]"]
    {:i :in-c})))

(defn opdata-out [port pos-or-0]
  (match
   [port pos-or-0]
   [[:port [:literal-8]] [:register-8 "a"]]
   {:i :out-a :arg :arg-8}
   [[:port "[c]"] [:register-8 r]]
   {:i :out-ac-reg :r [r]}
   [[:port "[c]"] "0"]
   {:i :out-ac-0}))

(defn opdata-im [arg]
  {:i :im :addr (Integer/parseInt arg)})

(defn opdata-rl [arg]
  (match
   arg
   [:addr [:literal-8-with-index ixiy [:literal-8]]]
   {:i :rl :r [ixiy] :arg :arg-8}))

(defn opdata-rr [arg]
  (match
   arg
   [:addr [:literal-8-with-index ixiy [:literal-8]]]
   {:i :rr :r [ixiy] :arg :arg-8}))

(defn opdata-sla [arg]
  (match
   arg
   [:sla-simple [:addr [:literal-8-with-index ixiy [:literal-8]]]]
   {:i :sla :r [ixiy] :arg :arg-8}))

(defn opdata-sra [arg]
  (match
   arg
   [:sra-simple [:addr [:literal-8-with-index ixiy [:literal-8]]]]
   {:i :sra :r [ixiy] :arg :arg-8}))

(defn opdata-sll [arg]
  (match
   arg
   [:addr [:literal-8-with-index ixiy [:literal-8]]]
   {:i :sll :r [ixiy] :arg :arg-8}))

(defn opdata-srl [arg]
  (match
   arg
   [:addr [:literal-8-with-index ixiy [:literal-8]]]
   {:i :srl :r [ixiy] :arg :arg-8}))

(defn opdata-bit [bit arg]
  (match
   arg
   [:addr [:literal-8-with-index ixiy [:literal-8]]]
   {:i :bit :r [ixiy] :addr bit :arg :arg-8}))

(defn opdata-set
  ([bit arg]
   (match
    arg
    [:addr [:literal-8-with-index ixiy [:literal-8]]]
    {:i :set-ixiy :r [ixiy] :addr bit :arg :arg-8}))
  ([bit]
   {:i :set :addr bit}))

(defn opdata-res
  ([bit arg]
   (match
    arg
    [:addr [:literal-8-with-index ixiy [:literal-8]]]
    {:i :res-ixiy :r [ixiy] :addr bit :arg :arg-8}))
  ([bit]
   {:i :res :addr bit}))


(defn opdata-invalid [_]
  {:i :invalid})

;; :i -- instruction
;; :r -- list of registers
;; :addr -- address or literal argument, included in opcode (im, rst)
;; :cond -- condition (jp, jr, ret)
;; :arg -- arg length (after opcode)
