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

;; (defn swap-registers [reg1 reg2]
;;   [reg2 reg1 "^=" reg1 reg2 "^=" reg2 reg1 "^="])

;; set flag to 1 if arg is non-zero otherwise 0
;; (defn flag-set [flag]
;;   (str "?{1," flag ",=},!,?{0," flag ",=}"))

;; (defn flags [& args]
;;   (->>
;;    (for [[flag arg] (partition 2 args)]
;;      (if (or (= arg 0) (= arg 1) (= arg "iff2"))
;;        [arg (name flag) "="]
;;        (do
;;          (prn arg)
;;          (assert (#{"a" "b" "c" "d" "e" "h" "l"} arg) "Only for 8-bit registers")
;;          (case flag
;;            :sf [arg "0x40" "&" (flag-set "sf")] ;; only for 8-bit
;;            :zf ["$z" "zf" "="] ;; arg ignored
;;            :yf [arg "0x20" "&" (flag-set "sf")] ;; copy of bit 5, undocumented
;;            :hf (assert false "Not implemented")
;;            :xf [arg "0x04" "&" (flag-set "xf")] ;; copy of bit 3, undocumented
;;            :pf (assert false "Not implemented")
;;            :nf (assert false "Not implemented")
;;            :cf ["$c8" "cf" "="]                 ;; relies on $c8 and notion of
;;            ;; 'result'
;;            ))))
;;    flatten
;;    (s/join ",")))

;; (defn position-prefix [position]
;;   (match position
;;          [:register-8 _] :reg
;;          [:register-16 _] :reg
;;          [:addr-from-register _] :areg
;;          [:literal-8 _] :))

;; (defn pair-prefix [pair]
;;   (match pair
;;          [:pair-8]))

(defn handler-fn [opname prefix]
  {:post [%]}
  (prn "opname:" opname)
  (find-var (symbol "z80-esil.esil" (str prefix (name opname)))))

(defn opdata "Return op data structure for instr (parsed tree)"
  [instr]
  (match instr
         [:instruction [opname]]
         {:i opname} ;; when without args
         [:instruction [opname & args]]
         (apply (handler-fn opname "opdata-") args)
         ))

(defn opdata-ld [pair]
  (match
   pair
   ;; copy from one register to another (1 or 2 bytes)
   [_ [(:or :register-8 :register-16) r1] [(:or :register-8 :register-16) r2]]
   {:i :ld-reg-reg :r [r1 r2]}
   ;; read 1 byte from memory
   [:pair-8 [:register-8 rd] [:addr _]]
   {:i :ld-8-reg-addr :r [rd] :arg :arg16}
   [:pair-8 [:register-8 rd] [:addr-from-register [:register-16 rs]]]
   {:i :ld-8-reg-areg :r [rd rs]}
   [:pair-8 [:addr [:literal-8-with-index in [:literal-8]]] [:literal-8]]
   {:i :ld-8-ixiy-arg :r [in] :arg :arg-8}
   ;; read 2 bytes from memory
   [:pair-16 [:register-16 rd] [:addr _]]
   {:i :ld-16-reg-addr :r [rd] :arg :arg16}
   [:pair-16 [:register-16 rd] [:addr-from-register [:register-16 rs]]]
   {:i :ld-16-reg-areg :r [rd rs]}
   ;; write 1 byte to memory
   [:pair-8 [:addr _] [:register-8 rs]]
   {:i :ld-8-addr-reg :r [rs] :arg :arg16}
   [:pair-8 [:addr-from-register [:register-16 rd]] [:register-8 rs]]
   {:i :ld-8-areg-reg :r [rd rs]}
   [:pair-8 [:addr-from-register [:register-16 rd]] [:literal-8]]
   {:i :ld-8-areg-arg :reg [rd] :arg :arg8}
   ;; write 2 bytes to memory
   [:pair-16 [:addr _] [:register-16 rs]]
   {:i :ld-16-addr-reg :r [rs] :arg :arg16}
   [:pair-16 [:addr-from-register [:register-16 rd]] [:register-16 rs]]
   {:i :ld-16-areg-reg :r [rd rs]}
   ;; write arg to register
   [:pair-16 [:register-16 rd] [:literal-16]]
   {:i :ld-16-reg-arg :r [rd] :arg :arg16}
   [:pair-8 [:register-8 rd] [:literal-8]]
   {:i :ld-8-reg-arg :r [rd] :arg :arg8}))

;; (defn esil-push [register]
;;   (match register [:register-16 r] (build 2 "sp" "-=" r "sp" "=[2]")))

;; (defn esil-pop [register]
;;   (match register [:register-16 r] (build "sp" "[2]" r "=" 2 "sp" "+=")))

(defn opdata-push [register]
  (match register [:register-16 r]
         {:i :push :r [r]}))

(defn opdata-pop [register]
  (match register [:register-16 r]
         {:i :pop :r r}))


;; (defn esil-ex [pair]
;;   (match
;;    pair
;;    [:pair-16 [:register-16 "de"] [:register-16 "hl"]]
;;    (apply build (swap-registers "de" "hl"))
;;    [:pair-16 [:addr-from-register [:register-16 "sp"]] [:register-16 r2]]
;;    (apply build "sp" "[]" r2 "sp" "=[2]" r2 "=")))

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
   {:i :add-a-reg :r [rs]}
   [:pair-8 [:register-8 "a"] [:addr-from-register [:register-16 "hl"]]]
   {:i :add-a-ahl}
   [:pair-8 [:register-8 "a"] [:addr [:literal-8-with-index in]]]
   {:i :add-a-aixiy :r [in] :arg :arg-8}
   [:pair-8 [:register-8 "a"] [:literal-8]]
   {:i :add-a-arg :arg :arg-8}
   [:pair-16 [:register-16 rd] [:register-16 rs]]
   {:i :add-16-reg-reg :r [rd rs]}))

(defn opdata-adc [pair]
  (match
   pair
   [:pair-8 [:register-8 "a"] [:register-8 rs]]
   {:i :adc :r [rs]}
   [:pair-8 [:register-8 "a"] [:addr-from-register [:register-16 "hl"]]]
   {:i :adc-a-ahl}
   [:pair-8 [:register-8 "a"] [:literal-8]]
   {:i :adc-a-arg :arg :arg-8}))

(defn opdata-sub [reg]
  (match
   reg
   [:register-8 rs]
   {:i :sub :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :sub-ahl}
   [:literal-8]
   {:i :sub-arg :arg :arg-8}))

(defn opdata-sbc [pair]
  (match
   pair
   [:pair-8 [:register-8 "a"] [:register-8 rs]]
   {:i :sbc :r [rs]}
   [:pair-8 [:register-8 "a"] [:addr-from-register [:register-16 "hl"]]]
   {:i :sbc-ahl}
   [:pair-8 [:register-8 "a"] [:literal-8]]
   {:i :sbc-arg}))

(defn opdata-and [reg]
  (match
   reg
   [:register-8 rs]
   {:i :and :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :and-ahl}
   [:literal-8]
   {:i :and-arg :arg :arg-8}))

(defn opdata-or [reg]
  (match
   reg
   [:register-8 rs]
   {:i :or :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :or-ahl}
   [:literal-8]
   {:i :or-arg :arg :arg-8}))

(defn opdata-xor [reg]
  (match
   reg
   [:register-8 rs]
   {:i :xor :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :xor-ahl}
   [:literal-8]
   {:i :xor-arg :arg :arg-8}))

(defn opdata-cp [reg]
  (match
   reg
   [:register-8 rs]
   {:i :cp :r [rs]}
   [:addr-from-register [:register-16 "hl"]]
   {:i :cp-ahl}
   [:literal-8]
   {:i :cp-arg :arg :arg-8}))

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
   {:i :rlc-areg :r [r]}))

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
    {:i :in-f :arg :arg-8})))

(defn opdata-out [port pos-or-0]
  (match
   [port pos-or-0]
   [[:port [:literal-8]] [:register-8 "a"]]
   {:i :out-a :arg :arg-8}
   [[:port "[c]"] [:register-8 r]]
   {:i :out-ac-reg :r [r]}
   [[:port "[c]"] "0"]
   {:i :out-ac-0}))

(defn opdata-invalid [_]
  {:i :invalid})

;; (defn esil-ex-af-af []
;;   (apply build (concat (swap-registers "a" "a1") (swap-registers "f" "f1"))))

;; (defn esil-exx []
;;   (apply build (flatten (for [[r1 r2] [["bc" "bc1"] ["de" "de1"] ["hl" "hl1"]]]
;;                          (swap-registers r1 r2)))))

;; (defn esil-ldi []
;;   (build "hl" "[1]" "de" "=[1]" 1 "de" "+=" 1 "hl" "+=" 1 "bc" "-="))

;; (defn esil-ldir []
;;   (build (esil-ldi) "bc" "!" "?{,BREAK,}" "0" "GOTO"))

;; (defn esil-ldd []
;;   (build "hl" "[1]" "de" "=[1]" 1 "de" "-=" 1 "hl" "-=" 1 "bc" "-="))

;; (defn esil-lddr []
;;   (build (esil-ldd) "bc" "!" "?{,BREAK,}" "0" "GOTO"))

;; (defn esil-cpi [])

;; (defn esil-cpir [])

;; (defn esil-cpd [])

;; (defn esil-cpdr [])

;; (defn esil-add [])

;; (defn flags-ld [pair]
;;   (match pair
;;          ;; ld a, i; ld a, r -- special case
;;          [:pair-8 [:register-8 "a"] [:register-8 (rs :guard #{"i" "r"})]]
;;          (flags :sf "a" :zf "a" :yf "a" :hf 0 :xf "a" :pf "iff2" :nf 0)))

(esil (parse-instr "ldir"))

(parse-instr "add a, [iy+0x%02x]")

;; (esil-flags (parse-instr "ld a, r"))

(filter insta/failure?
        (for [opc (flatten (vals opcodes)) :when (some? opc)]
          (parse-instr opc)))
