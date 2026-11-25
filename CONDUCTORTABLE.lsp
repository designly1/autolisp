;;; ------------------------------------------------------------------------
;;; CONDUCTORTABLE.lsp
;;;
;;; Command: CONDUCTORTABLE
;;;   Places a conductor schedule table. Same blocks and quantities as
;;;   LISTCONDUCTORS: the current selection, or the whole drawing when
;;;   nothing is selected. One data row per unique spec, plus a total.
;;;
;;;   CONDUCTOR is the spec, for example
;;;   "1 set of 4 x #6 CU 75°C, 385A, + #10 CU EGC, PVC-SCH40, 3Φ".
;;;   QTY is the block count. LENGTH is the sum of LENGTH.
;;;   WIRE QTY is CONDUCTORS x SETS x LENGTH, summed.
;;;   GROUND QTY is SETS x LENGTH when GROUND_AWG is set, otherwise 0.
;;;   Missing CONDUCTORS, SETS, or LENGTH counts as 1.
;;;
;;;   Uses the current table style. Pick the upper-left corner.
;;;   Loads LISTCONDUCTORS.lsp from this folder when it is not loaded.
;;; ------------------------------------------------------------------------

(vl-load-com)

(if *load-truename*
  (setq *ct-helpers* (strcat (vl-filename-directory *load-truename*) "/LISTCONDUCTORS.lsp"))
)

(defun ct:ready (/ f)
  (if (and (boundp 'lc:collect) (= 'SUBR (type lc:collect))
           (boundp 'lc:numstr) (= 'SUBR (type lc:numstr))
      )
    t
    (progn
      (setq f (cond
                ((and (boundp '*ct-helpers*) *ct-helpers* (findfile *ct-helpers*)) *ct-helpers*)
                ((findfile "LISTCONDUCTORS.lsp"))
              )
      )
      (if f
        (progn
          (load f)
          (and (boundp 'lc:collect) (= 'SUBR (type lc:collect)))
        )
      )
    )
  )
)

(defun ct:wcs (pt / w)
  (setq w (trans pt 1 0))
  (vlax-3d-point (list (car w) (cadr w) (if (caddr w) (caddr w) 0.0)))
)

(defun ct:cell (tbl r c text align)
  (vla-settext tbl r c (if text text ""))
  (vl-catch-all-apply 'vla-setcellalignment (list tbl r c align))
)

(defun ct:num (fn obj fallback / m)
  (setq m (vl-catch-all-apply fn (list obj)))
  (if (and (not (vl-catch-all-error-p m)) (numberp m) (>= m 0.0))
    m
    fallback
  )
)

(defun ct:text-h (tbl r c fallback / h)
  (setq h (vl-catch-all-apply 'vla-gettextheight (list tbl r c)))
  (if (and (not (vl-catch-all-error-p h)) (numberp h) (> h 0.0))
    h
    fallback
  )
)

(defun ct:width (th margin texts / m s)
  (setq m 0)
  (foreach s texts
    (if (> (strlen s) m) (setq m (strlen s)))
  )
  (+ (* (float m) th) (* 2.0 margin) (* th 0.5))
)

;;; Size columns to the text and grow any row that would clip it.
(defun ct:size (tbl groups th tq tl tw tg / hm vm hh specs nums wspec wnum i n cur need)
  (setq hm (ct:num 'vla-get-horizontalcellmargin tbl (* th 0.5))
        vm (ct:num 'vla-get-verticalcellmargin tbl (* th 0.25))
        hh (max (ct:text-h tbl 1 0 th) (ct:text-h tbl 2 0 th))
        specs '("CONDUCTOR" "TOTAL")
        nums (list "QTY" "LENGTH" "WIRE QTY" "GROUND QTY"
                   (lc:numstr tq) (lc:numstr tl) (lc:numstr tw) (lc:numstr tg)
             )
  )
  (foreach g groups
    (setq specs (cons (car g) specs)
          nums (append
                 (list (lc:numstr (nth 2 g))
                       (lc:numstr (nth 3 g))
                       (lc:numstr (nth 4 g))
                       (lc:numstr (nth 5 g))
                 )
                 nums
               )
    )
  )
  (setq wspec (ct:width hh hm specs)
        wnum (ct:width hh hm nums)
        i 1
  )
  (vla-setcolumnwidth tbl 0 wspec)
  (repeat 4
    (vla-setcolumnwidth tbl i wnum)
    (setq i (1+ i))
  )
  (setq n (+ (length groups) 3)
        i 0
  )
  (while (< i n)
    (setq cur (vl-catch-all-apply 'vla-getrowheight (list tbl i))
          need (+ (ct:text-h tbl i 0 hh) (* 2.0 vm) (* hh 0.35))
    )
    (if (or (vl-catch-all-error-p cur) (< cur need))
      (vla-setrowheight tbl i need)
    )
    (setq i (1+ i))
  )
)

(defun ct:make (pt groups / doc space n th tbl r g tq tl tw tg)
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
        space (if (= 1 (getvar "CVPORT"))
                (vla-get-paperspace doc)
                (vla-get-modelspace doc)
              )
        n (length groups)
        th (getvar "TEXTSIZE")
        tq 0
        tl 0.0
        tw 0.0
        tg 0.0
  )
  (if (or (null th) (<= th 0.0)) (setq th 0.125))
  (setq tbl (vla-addtable space (ct:wcs pt) (+ n 3) 5 (* th 2.5) (* th 10.0)))
  (vl-catch-all-apply 'vla-put-titlesuppressed (list tbl :vlax-false))
  (vl-catch-all-apply 'vla-put-headersuppressed (list tbl :vlax-false))
  (ct:cell tbl 0 0 "CONDUCTOR SCHEDULE" 5)
  (ct:cell tbl 1 0 "CONDUCTOR" 5)
  (ct:cell tbl 1 1 "QTY" 5)
  (ct:cell tbl 1 2 "LENGTH" 5)
  (ct:cell tbl 1 3 "WIRE QTY" 5)
  (ct:cell tbl 1 4 "GROUND QTY" 5)
  (setq r 2)
  (foreach g groups
    (ct:cell tbl r 0 (car g) 4)
    (ct:cell tbl r 1 (lc:numstr (nth 2 g)) 5)
    (ct:cell tbl r 2 (lc:numstr (nth 3 g)) 5)
    (ct:cell tbl r 3 (lc:numstr (nth 4 g)) 5)
    (ct:cell tbl r 4 (lc:numstr (nth 5 g)) 5)
    (setq tq (+ tq (nth 2 g))
          tl (+ tl (nth 3 g))
          tw (+ tw (nth 4 g))
          tg (+ tg (nth 5 g))
          r (1+ r)
    )
  )
  (ct:cell tbl r 0 "TOTAL" 4)
  (ct:cell tbl r 1 (lc:numstr tq) 5)
  (ct:cell tbl r 2 (lc:numstr tl) 5)
  (ct:cell tbl r 3 (lc:numstr tw) 5)
  (ct:cell tbl r 4 (lc:numstr tg) 5)
  (ct:size tbl groups th tq tl tw tg)
  (vl-catch-all-apply 'vla-recomputetableblock (list tbl :vlax-true))
  tbl
)

(defun c:CONDUCTORTABLE (/ *error* doc res groups pt)
  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*,*QUIT*")))
      (princ (strcat "\nCONDUCTORTABLE error: " msg))
    )
    (princ)
  )

  (cond
    ((not (ct:ready))
     (princ "\nCONDUCTORTABLE: LISTCONDUCTORS.lsp was not found.")
    )
    (t
     (setq res (lc:collect)
           groups (cadr res)
     )
     (cond
       ((null groups)
        (lc:say-empty res)
       )
       ((setq pt (getpoint "\nSpecify insertion point: "))
        (setq doc (vla-get-activedocument (vlax-get-acad-object)))
        (vla-startundomark doc)
        (ct:make pt groups)
        (vla-endundomark doc)
        (setq doc nil)
        (princ (strcat "\nConductor schedule placed, "
                       (itoa (length groups))
                       (if (= (length groups) 1) " row." " rows.")
               )
        )
        (lc:say-empty res)
       )
     )
    )
  )
  (princ)
)

(if (not (and (boundp 'lc:collect) (= 'SUBR (type lc:collect))))
  (cond
    ((and (boundp '*ct-helpers*) *ct-helpers* (findfile *ct-helpers*))
     (load *ct-helpers*)
    )
    ((findfile "LISTCONDUCTORS.lsp")
     (load (findfile "LISTCONDUCTORS.lsp"))
    )
  )
)

(princ "\nCONDUCTORTABLE loaded. Select conductor blocks, or run it with nothing selected to schedule the whole drawing.")
(princ)
