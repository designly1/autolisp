;;; ------------------------------------------------------------------------
;;; LISTCONDUCTORS.lsp
;;;
;;; Command: LISTCONDUCTORS
;;;   Reads conductor dynamic-block properties on the selection and prints
;;;   one line per unique spec. Blocks that match on every value except
;;;   LENGTH are combined.
;;;
;;;   Values (the spec): CONDUCTORS, AWG, CONDUCTOR_MATERIAL, TEMP_RATING,
;;;   SETS, AMPACITY, GROUND_AWG, GROUND_MATERIAL, WIRING_METHOD, PHASE.
;;;
;;;   Quantities:
;;;     QTY     number of blocks
;;;     WIRES   sum of CONDUCTORS x SETS x LENGTH
;;;     GROUNDS sum of SETS x LENGTH when GROUND_AWG is set, otherwise 0
;;;   Missing CONDUCTORS or SETS counts as 1. Missing LENGTH counts as 1.
;;;   A blank GROUND_AWG adds no grounds.
;;;
;;;   Uses the current selection. With nothing selected, every block in
;;;   the drawing is listed. Blocks with no AWG value are skipped.
;;; ------------------------------------------------------------------------

(vl-load-com)

(defun lc:props (obj / r)
  (setq r (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p r) nil r)
)

(defun lc:find (props name / p hit)
  (setq hit nil)
  (foreach p props
    (if (and (null hit)
             (= (strcase name) (strcase (vla-get-propertyname p)))
        )
      (setq hit p)
    )
  )
  hit
)

(defun lc:raw (prop / r)
  (if prop
    (progn
      (setq r (vl-catch-all-apply 'vla-get-value (list prop)))
      (if (vl-catch-all-error-p r) nil (vlax-variant-value r))
    )
  )
)

(defun lc:disp (v)
  (cond
    ((null v) "")
    ((= (type v) 'STR) (vl-string-trim " \t" v))
    ((= (type v) 'INT) (itoa v))
    ((numberp v)
     (if (equal v (fix v) 1e-8)
       (itoa (fix v))
       (vl-string-right-trim "0" (vl-string-right-trim "." (rtos v 2 6)))
     )
    )
    (t "")
  )
)

(defun lc:numstr (n)
  (cond
    ((null n) "0")
    ((and (numberp n) (equal n (fix n) 1e-6)) (itoa (fix n)))
    ((numberp n)
     (vl-string-right-trim "0" (vl-string-right-trim "." (rtos n 2 4)))
    )
    (t "0")
  )
)

;; Missing or blank -> missing (nil). Unreadable text -> 0.
(defun lc:real (v / s n)
  (cond
    ((null v) nil)
    ((numberp v) (float v))
    ((= (type v) 'STR)
     (setq s (vl-string-trim " \t" v))
     (if (= s "")
       nil
       (progn
         (setq n (distof s))
         (if n n 0.0)
       )
     )
    )
    (t nil)
  )
)

;; Whole number. `missing` is used when the property is absent or blank.
(defun lc:whole (v missing / n)
  (setq n (lc:real v))
  (cond
    ((null n) missing)
    ((>= n 0) (fix (+ n 0.5)))
    (t 0)
  )
)

(defun lc:get (vals name / a)
  (setq a (assoc name vals))
  (if a (cdr a) "")
)

(defun lc:join (parts sep / s p)
  (setq s "")
  (foreach p parts
    (if (and p (> (strlen p) 0))
      (setq s (if (= s "") p (strcat s sep p)))
    )
  )
  s
)

(defun lc:norm-size (s / u suf)
  (setq u (strcase (vl-string-trim " \t" s)))
  (while (and (> (strlen u) 0) (= (substr u 1 1) "#"))
    (setq u (substr u 2))
  )
  (setq u (vl-string-translate " " "" u))
  (foreach suf '("KCMIL" "MCM" "AWG" "KCM")
    (if (and (> (strlen u) (strlen suf))
             (= (substr u (1+ (- (strlen u) (strlen suf)))) suf)
        )
      (setq u (substr u 1 (- (strlen u) (strlen suf))))
    )
  )
  u
)

(defun lc:rank (awg / sizes i)
  (setq sizes '("14" "12" "10" "8" "6" "4" "3" "2" "1"
                "1/0" "2/0" "3/0" "4/0"
                "250" "300" "350" "400" "500" "600" "700" "750"
                "800" "900" "1000" "1250" "1500" "1750" "2000")
        i (vl-position (lc:norm-size awg) sizes)
  )
  (if i i 1000)
)

(defun lc:spec (vals / cond awg mat temp sets amp gnd gmat method phase ampstr wires)
  (setq cond (lc:get vals "CONDUCTORS")
        awg (lc:get vals "AWG")
        mat (lc:get vals "CONDUCTOR_MATERIAL")
        temp (lc:get vals "TEMP_RATING")
        sets (lc:get vals "SETS")
        amp (lc:get vals "AMPACITY")
        gnd (lc:get vals "GROUND_AWG")
        gmat (lc:get vals "GROUND_MATERIAL")
        method (lc:get vals "WIRING_METHOD")
        phase (lc:get vals "PHASE")
        ampstr (cond
                 ((= amp "") "")
                 ((wcmatch (strcase amp) "*A") amp)
                 (t (strcat amp "A"))
               )
        wires (lc:join (list (if (> (strlen cond) 0) (strcat cond " x " awg) awg) mat temp) " ")
  )
  (lc:join
    (list
      (if (> (strlen sets) 0)
        (strcat sets " set" (if (= sets "1") "" "s") " of " wires)
        wires
      )
      ampstr
      (if (> (strlen gnd) 0)
        (strcat "+ " gnd (if (> (strlen gmat) 0) (strcat " " gmat) "") " EGC")
        ""
      )
      method
      phase
    )
    ", "
  )
)

;;; (spec rank length wires gnds), or nil when the block has no AWG.
(defun lc:read (ename / obj props awg cond sets gnd lenV len vals)
  (setq obj (vlax-ename->vla-object ename)
        props (lc:props obj)
        awg (lc:disp (lc:raw (lc:find props "AWG")))
  )
  (if (= awg "")
    nil
    (progn
      (setq cond (lc:raw (lc:find props "CONDUCTORS"))
            sets (lc:raw (lc:find props "SETS"))
            gnd (lc:disp (lc:raw (lc:find props "GROUND_AWG")))
            lenV (lc:real (lc:raw (lc:find props "LENGTH")))
            len (if lenV lenV 1.0)
            vals (list
                   (cons "CONDUCTORS" (lc:disp cond))
                   (cons "AWG" awg)
                   (cons "CONDUCTOR_MATERIAL" (lc:disp (lc:raw (lc:find props "CONDUCTOR_MATERIAL"))))
                   (cons "TEMP_RATING" (lc:disp (lc:raw (lc:find props "TEMP_RATING"))))
                   (cons "SETS" (lc:disp sets))
                   (cons "AMPACITY" (lc:disp (lc:raw (lc:find props "AMPACITY"))))
                   (cons "GROUND_AWG" gnd)
                   (cons "GROUND_MATERIAL" (lc:disp (lc:raw (lc:find props "GROUND_MATERIAL"))))
                   (cons "WIRING_METHOD" (lc:disp (lc:raw (lc:find props "WIRING_METHOD"))))
                   (cons "PHASE" (lc:disp (lc:raw (lc:find props "PHASE"))))
                 )
      )
      (list
        (lc:spec vals)
        (lc:rank awg)
        len
        (* (lc:whole cond 1) (lc:whole sets 1) len)
        (if (= gnd "") 0.0 (* (lc:whole sets 1) len))
      )
    )
  )
)

;;; group: (spec rank qty length wires gnds)
(defun lc:tally (groups rec / hit)
  (if (setq hit (assoc (car rec) groups))
    (subst
      (list (car hit)
            (cadr hit)
            (1+ (nth 2 hit))
            (+ (nth 3 hit) (nth 2 rec))
            (+ (nth 4 hit) (nth 3 rec))
            (+ (nth 5 hit) (nth 4 rec))
          )
      hit
      groups
    )
    (cons
      (list (car rec) (cadr rec) 1 (nth 2 rec) (nth 3 rec) (nth 4 rec))
      groups
    )
  )
)

(defun lc:padl (s w)
  (while (< (strlen s) w)
    (setq s (strcat " " s))
  )
  s
)

(defun lc:colw (groups i head / m s)
  (setq m (strlen head))
  (foreach g groups
    (setq s (lc:numstr (nth i g)))
    (if (> (strlen s) m) (setq m (strlen s)))
  )
  m
)

(defun lc:line (g wq wl ww wg)
  (strcat
    (lc:padl (lc:numstr (nth 2 g)) wq) "  "
    (lc:padl (lc:numstr (nth 3 g)) wl) "  "
    (lc:padl (lc:numstr (nth 4 g)) ww) "  "
    (lc:padl (lc:numstr (nth 5 g)) wg) "  "
    (car g)
  )
)

(defun lc:lessp (a b)
  (cond
    ((< (cadr a) (cadr b)) t)
    ((> (cadr a) (cadr b)) nil)
    (t (< (strcase (car a)) (strcase (car b))))
  )
)

;;; (ss groups skip picked). groups are sorted. ss is nil when no inserts
;;; were gathered. picked is T when a selection was active.
(defun lc:collect (/ picked ss i rec groups skip)
  (setq picked (ssget "_I")
        ss (if picked
             (ssget "_I" '((0 . "INSERT")))
             (ssget "_X" '((0 . "INSERT")))
           )
        groups nil
        skip 0
        i 0
  )
  (if ss
    (repeat (sslength ss)
      (setq rec (vl-catch-all-apply 'lc:read (list (ssname ss i))))
      (cond
        ((vl-catch-all-error-p rec)
         (princ (strcat "\nConductor: " (vl-catch-all-error-message rec)))
         (setq skip (1+ skip))
        )
        ((null rec)
         (setq skip (1+ skip))
        )
        (t (setq groups (lc:tally groups rec)))
      )
      (setq i (1+ i))
    )
  )
  (if groups (setq groups (vl-sort groups 'lc:lessp)))
  (list ss groups skip (and picked t))
)

(defun lc:say-empty (res)
  (cond
    ((null (car res))
     (princ (if (cadddr res) "\nNo blocks in the selection." "\nNo blocks in the drawing."))
    )
    ((null (cadr res))
     (princ "\nNo conductor blocks found.")
    )
  )
  (if (and (car res) (> (caddr res) 0))
    (princ (strcat " " (itoa (caddr res)) " skipped."))
  )
)

(defun c:LISTCONDUCTORS (/ *error* res groups wq wl ww wg tq tl tw tg g)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*,*QUIT*")))
      (princ (strcat "\nLISTCONDUCTORS error: " msg))
    )
    (princ)
  )

  (setq res (lc:collect)
        groups (cadr res)
  )
  (if (null groups)
    (lc:say-empty res)
    (progn
      (setq wq (lc:colw groups 2 "QTY")
            wl (lc:colw groups 3 "LENGTH")
            ww (lc:colw groups 4 "WIRES")
            wg (lc:colw groups 5 "GROUNDS")
            tq 0
            tl 0.0
            tw 0.0
            tg 0.0
      )
      (princ "\nQTY is blocks. WIRES = CONDUCTORS x SETS x LENGTH. GROUNDS = SETS x LENGTH.")
      (princ "\nMissing CONDUCTORS, SETS, or LENGTH counts as 1.")
      (princ (strcat "\n"
                     (lc:padl "QTY" wq) "  "
                     (lc:padl "LENGTH" wl) "  "
                     (lc:padl "WIRES" ww) "  "
                     (lc:padl "GROUNDS" wg) "  "
                     "CONDUCTOR"
             )
      )
      (foreach g groups
        (princ (strcat "\n" (lc:line g wq wl ww wg)))
        (setq tq (+ tq (nth 2 g))
              tl (+ tl (nth 3 g))
              tw (+ tw (nth 4 g))
              tg (+ tg (nth 5 g))
        )
      )
      (princ (strcat "\n" (itoa (length groups)) " unique, "
                     (itoa tq) " block(s), length " (lc:numstr tl)
                     ", wires " (lc:numstr tw)
                     ", grounds " (lc:numstr tg) "."
             )
      )
      (lc:say-empty res)
    )
  )
  (princ)
)

(princ "\nLISTCONDUCTORS loaded. Select conductor blocks, or run it with nothing selected to list the whole drawing.")
(princ)
