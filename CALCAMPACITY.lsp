;;; ------------------------------------------------------------------------
;;; CALCAMPACITY.lsp
;;;
;;; Command: CALCAMPACITY
;;;   On each selected block, reads AWG, CONDUCTOR_MATERIAL, TEMP_RATING,
;;;   and SETS and writes AMPACITY. Ampacity is NEC Table 310.16 (same
;;;   table as dj-comelec src/lib/conductor.ts): the 60, 75, or 90 C
;;;   column named by TEMP_RATING ("75°C", "75C", "75"), times SETS.
;;;   Missing SETS counts as 1. No ambient or bundling derating.
;;;
;;;   Uses the current selection. With nothing selected, every conductor
;;;   block in the drawing is updated. A selection that contains no
;;;   blocks is left alone. Blocks with no AWG value are not conductors.
;;;   Example: #3/0 AL, 75 C, 1 set -> 155. 90 C -> 175.
;;; ------------------------------------------------------------------------

(vl-load-com)

;; (size cu60 cu75 cu90 al60 al75 al90). 0 means that column has no rating.
(setq *amp-table*
  '(
    ("14"   15  20  25   0   0   0)
    ("12"   20  25  30  15  20  25)
    ("10"   30  35  40  25  30  35)
    ("8"    40  50  55  35  40  45)
    ("6"    55  65  75  40  50  55)
    ("4"    70  85  95  55  65  75)
    ("3"    85 100 115  65  75  85)
    ("2"    95 115 130  75  90 100)
    ("1"   110 130 150  85 100 115)
    ("1/0" 125 150 170 100 120 135)
    ("2/0" 145 175 195 115 135 150)
    ("3/0" 165 200 225 130 155 175)
    ("4/0" 195 230 260 150 180 205)
    ("250"  215 255 290 170 205 230)
    ("300"  240 285 320 195 230 260)
    ("350"  260 310 350 210 250 280)
    ("400"  280 335 380 225 270 305)
    ("500"  320 380 430 260 310 350)
    ("600"  350 420 475 285 340 385)
    ("700"  385 460 520 315 375 425)
    ("750"  400 475 535 320 385 435)
    ("800"  410 490 555 330 395 445)
    ("900"  435 520 585 355 425 480)
    ("1000" 455 545 615 375 445 500)
    ("1250" 495 590 665 405 485 545)
    ("1500" 525 625 705 435 520 585)
    ("1750" 545 650 735 455 545 615)
    ("2000" 555 665 750 470 560 630)
  )
)

(defun amp:props (obj / r)
  (setq r (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (vl-catch-all-error-p r) nil r)
)

(defun amp:find (props name / p hit)
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

(defun amp:value (prop / r)
  (setq r (vl-catch-all-apply 'vla-get-value (list prop)))
  (if (vl-catch-all-error-p r) nil (vlax-variant-value r))
)

(defun amp:as-str (v)
  (cond
    ((= (type v) 'STR) (vl-string-trim " \t" v))
    ((= (type v) 'INT) (itoa v))
    ((numberp v) (rtos v 2 8))
    (t "")
  )
)

;; "#3/0", "3/0 AWG", "250 kcmil", "500 MCM" -> "3/0" / "250" / "500".
(defun amp:norm-size (s / u suf)
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

(defun amp:norm-mat (v / u)
  (setq u (strcase (amp:as-str v)))
  (cond
    ((member u '("AL" "ALUM" "ALUMINUM")) "AL")
    ((member u '("CU" "CU." "COPPER")) "CU")
    (t nil)
  )
)

;; First integer in the value: 75, "75", "75C", "75°C", "75%%dC".
(defun amp:leading-int (v / s i c n)
  (cond
    ((numberp v) (fix (+ v 0.5)))
    ((= (type v) 'STR)
     (setq s (strcase (vl-string-trim " \t" v))
           s (vl-string-subst "" "%%D" s)
           i 1
           n ""
     )
     (while (and (<= i (strlen s))
                 (setq c (substr s i 1))
                 (<= 48 (ascii c) 57)
            )
       (setq n (strcat n c)
             i (1+ i)
       )
     )
     (if (> (strlen n) 0) (atoi n) nil)
    )
    (t nil)
  )
)

;; 60, 75, or 90 from a TEMP_RATING value. nil when it is not one of those.
(defun amp:parse-temp (v / n)
  (setq n (amp:leading-int v))
  (if (member n '(60 75 90)) n nil)
)

;; Whole number >= 1, or nil. A missing property is the caller's default.
(defun amp:whole (v / n)
  (setq n (amp:leading-int v))
  (if (and n (>= n 1)) n nil)
)

(defun amp:per-conductor (size mat temp / row col)
  (setq row (assoc size *amp-table*)
        col (cond ((= temp 60) 0) ((= temp 75) 1) ((= temp 90) 2))
  )
  (if (and row col)
    (progn
      (setq col (nth (+ col (if (= mat "CU") 1 4)) row))
      (if (and col (> col 0)) col nil)
    )
  )
)

(defun amp:readonly-p (prop)
  (and (vlax-property-available-p prop 'ReadOnly)
       (= :vlax-true (vla-get-readonly prop))
  )
)

;; Writes amps in the parameter's own type. Returns an error string, or nil.
(defun amp:put (prop amps / raw cur vt text attempts r err)
  (setq raw (vla-get-value prop)
        vt (vlax-variant-type raw)
        cur (vlax-variant-value raw)
        text (if (and (= (type cur) 'STR)
                      (wcmatch (strcase (vl-string-trim " \t" cur)) "*A")
                 )
               (strcat (itoa amps) "A")
               (itoa amps)
             )
        attempts (cond
                   ((= vt 8) (list text))
                   ((member vt '(2 3)) (list amps (float amps)))
                   (t (list (float amps) amps text))
                 )
        err nil
  )
  (while attempts
    (setq r (vl-catch-all-apply 'vla-put-value (list prop (car attempts)))
          attempts (cdr attempts)
    )
    (cond
      ((not (vl-catch-all-error-p r))
       (setq attempts nil err nil)
      )
      ((null attempts)
       (setq err (vl-catch-all-error-message r))
      )
    )
  )
  err
)

(defun amp:blkname (obj)
  (if (and (vlax-property-available-p obj 'IsDynamicBlock)
           (= :vlax-true (vla-get-isdynamicblock obj))
      )
    (vla-get-effectivename obj)
    (vla-get-name obj)
  )
)

;; A conductor has a dynamic-block AWG value. Other inserts are ignored
;; when the command scans the whole drawing.
(defun amp:conductor-p (ename / obj props awg)
  (setq obj (vlax-ename->vla-object ename)
        props (amp:props obj)
        awg (and props (amp:find props "AWG"))
  )
  (and awg (> (strlen (amp:norm-size (amp:as-str (amp:value awg)))) 0))
)

;; Sets AMPACITY. Returns T, or prints why and returns nil.
(defun amp:apply (ename / obj props name awg mat sets temp per total err ampProp)
  (setq obj (vlax-ename->vla-object ename)
        name (amp:blkname obj)
        props (amp:props obj)
  )
  (cond
    ((null props)
     (princ (strcat "\nCALCAMPACITY: " name " is not a dynamic block."))
     nil
    )
    ((null (setq awg (amp:find props "AWG")))
     (princ (strcat "\nCALCAMPACITY: " name " has no AWG parameter."))
     nil
    )
    ((null (setq mat (amp:find props "CONDUCTOR_MATERIAL")))
     (princ (strcat "\nCALCAMPACITY: " name " has no CONDUCTOR_MATERIAL parameter."))
     nil
    )
    ((null (setq mat (amp:norm-mat (amp:value mat))))
     (princ (strcat "\nCALCAMPACITY: " name " CONDUCTOR_MATERIAL must be AL or CU."))
     nil
    )
    ((and (amp:find props "SETS")
          (null (setq sets (amp:whole (amp:value (amp:find props "SETS")))))
     )
     (princ (strcat "\nCALCAMPACITY: " name " SETS must be a whole number >= 1."))
     nil
    )
    ((null (setq temp (amp:find props "TEMP_RATING")))
     (princ (strcat "\nCALCAMPACITY: " name " has no TEMP_RATING parameter."))
     nil
    )
    ((null (setq temp (amp:parse-temp (amp:value temp))))
     (princ (strcat "\nCALCAMPACITY: " name " TEMP_RATING must be 60C, 75C, or 90C."))
     nil
    )
    ((null (setq ampProp (amp:find props "AMPACITY")))
     (princ (strcat "\nCALCAMPACITY: " name " has no AMPACITY parameter."))
     nil
    )
    ((amp:readonly-p ampProp)
     (princ (strcat "\nCALCAMPACITY: " name " AMPACITY is read-only."))
     nil
    )
    (t
     (if (null sets) (setq sets 1))
     (setq awg (amp:norm-size (amp:as-str (amp:value awg)))
           per (amp:per-conductor awg mat temp)
     )
     (cond
       ((not (assoc awg *amp-table*))
        (princ (strcat "\nCALCAMPACITY: " name " size \"" awg "\" is not in NEC 310.16."))
        nil
       )
       ((null per)
        (princ (strcat "\nCALCAMPACITY: " name " " awg " " mat " has no " (itoa temp) "C ampacity."))
        nil
       )
       ((setq total (* per sets)
              err (amp:put ampProp total)
        )
        (princ (strcat "\nCALCAMPACITY: " name " could not set AMPACITY: " err))
        nil
       )
       (t
        (vl-catch-all-apply 'vla-update (list obj))
        (princ (strcat "\nCALCAMPACITY: " name " "
                       awg " " mat " x " (itoa sets)
                       " @ " (itoa temp) "C = " (itoa total)
               )
        )
        t
       )
     )
    )
  )
)

(defun c:CALCAMPACITY (/ *error* doc ss i en ok skip picked)
  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*,*QUIT*")))
      (princ (strcat "\nCALCAMPACITY error: " msg))
    )
    (princ)
  )

  ;; Nothing picked: every insert in the drawing. A selection that
  ;; contains no blocks stays empty instead of falling through.
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
        picked (ssget "_I")
        ss (if picked
             (ssget "_I" '((0 . "INSERT")))
             (ssget "_X" '((0 . "INSERT")))
           )
  )

  (if (null ss)
    (princ (if picked "\nNo blocks in the selection." "\nNo blocks in the drawing."))
    (progn
      (vla-startundomark doc)
      (setq i 0 ok 0 skip 0)
      (repeat (sslength ss)
        (setq en (ssname ss i))
        (if (or picked (amp:conductor-p en))
          (if (amp:apply en)
            (setq ok (1+ ok))
            (setq skip (1+ skip))
          )
        )
        (setq i (1+ i))
      )
      (vla-endundomark doc)
      (setq doc nil)
      (sssetfirst nil nil)
      (if (and (not picked) (= ok 0) (= skip 0))
        (princ "\nNo conductor blocks found.")
        (progn
          (princ (strcat "\nSet AMPACITY on " (itoa ok) " block(s)."))
          (if (> skip 0)
            (princ (strcat " " (itoa skip) " skipped."))
          )
        )
      )
    )
  )
  (princ)
)

(princ "\nCALCAMPACITY loaded. Type CALCAMPACITY. With nothing selected, every conductor is updated.")
(princ)
