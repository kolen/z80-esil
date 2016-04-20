#_[
 (defn swap-registers [reg1 reg2]
   [reg2 reg1 "^=" reg1 reg2 "^=" reg2 reg1 "^="])

 ;;set flag to 1 if arg is non-zero otherwise 0
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

 (defn position-prefix [position]
   (match position
          [:register-8 _] :reg
          [:register-16 _] :reg
          [:addr-from-register _] :areg
          [:literal-8 _] :))

 (defn pair-prefix [pair]
   (match pair
          [:pair-8]))

 (defn esil-push [register]
   (match register [:register-16 r] (build 2 "sp" "-=" r "sp" "=[2]")))

 (defn esil-pop [register]
   (match register [:register-16 r] (build "sp" "[2]" r "=" 2 "sp" "+=")))

 (defn esil-ex [pair]
   (match
    pair
    [:pair-16 [:register-16 "de"] [:register-16 "hl"]]
    (apply build (swap-registers "de" "hl"))
    [:pair-16 [:addr-from-register [:register-16 "sp"]] [:register-16 r2]]
    (apply build "sp" "[]" r2 "sp" "=[2]" r2 "=")))


 (defn esil-ex-af-af []
   (apply build (concat (swap-registers "a" "a1") (swap-registers "f" "f1"))))

 (defn esil-exx []
   (apply build (flatten (for [[r1 r2] [["bc" "bc1"] ["de" "de1"] ["hl" "hl1"]]]
                           (swap-registers r1 r2)))))

 (defn esil-ldi []
   (build "hl" "[1]" "de" "=[1]" 1 "de" "+=" 1 "hl" "+=" 1 "bc" "-="))

 (defn esil-ldir []
   (build (esil-ldi) "bc" "!" "?{,BREAK,}" "0" "GOTO"))

 (defn esil-ldd []
   (build "hl" "[1]" "de" "=[1]" 1 "de" "-=" 1 "hl" "-=" 1 "bc" "-="))

 (defn esil-lddr []
   (build (esil-ldd) "bc" "!" "?{,BREAK,}" "0" "GOTO"))

 (defn esil-cpi [])

 (defn esil-cpir [])

 (defn esil-cpd [])

 (defn esil-cpdr [])

 (defn esil-add [])

 (defn flags-ld [pair]
   (match pair
          ;; ld a, i; ld a, r -- special case
          [:pair-8 [:register-8 "a"] [:register-8 (rs :guard #{"i" "r"})]]
          (flags :sf "a" :zf "a" :yf "a" :hf 0 :xf "a" :pf "iff2" :nf 0)))

 (parse-instr "add a, [iy+0x%02x]")

 (esil-flags (parse-instr "ld a, r"))]
