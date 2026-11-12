;;; ------------------------------------------------------------------------
;;; PLL.lsp  -  Polyline with Live running Length
;;;
;;; Command: PLL
;;;   Draws an LWPOLYLINE like PLINE, but while you draw it shows the total
;;;   length so far (including the segment on the cursor) in a label next to
;;;   the cursor, and logs each segment and the running total to the command
;;;   line.
;;;
;;;   While drawing:
;;;     Pick point          add a vertex (running osnaps and ortho are applied)
;;;     <number> Enter      add a vertex at that distance toward the cursor
;;;     x,y  / @x,y         absolute / relative coordinates
;;;     d<a  / @d<a         absolute / relative polar coordinates
;;;     U Enter             undo the last vertex
;;;     C Enter             close the polyline and finish
;;;     Enter / Space /     finish
;;;     right-click
;;;     Esc                 stop (the vertices you already placed are kept)
;;;     Backspace           edit the typed input
;;;
;;;   Uses the current layer, PLINEWID (constant width) and PLINEGEN.
;;;   Draws straight segments only (no arc mode).
;;; ------------------------------------------------------------------------

(vl-load-com)

;;; ---------- helpers ------------------------------------------------------

(defun pll:upp ()                                   ; drawing units per pixel
  (/ (getvar "VIEWSIZE") (cadr (getvar "SCREENSIZE")))
)

(defun pll:normal () (trans '(0 0 1) 1 0 t))        ; UCS Z axis in WCS

(defun pll:dist2d (a b)
  (distance (list (car a) (cadr a)) (list (car b) (cadr b)))
)

(defun pll:length (lst cls / tot)
  (setq tot 0.0)
  (mapcar '(lambda (a b) (setq tot (+ tot (pll:dist2d a b)))) lst (cdr lst))
  (if (and cls (> (length lst) 2))
    (setq tot (+ tot (pll:dist2d (last lst) (car lst))))
  )
  tot
)

(defun pll:osmode-str (/ m s)
  (setq m (getvar "OSMODE") s "")
  (if (zerop (logand m 16384))                      ; osnap not switched off (F3)
    (foreach pr '((1 . "_end") (2 . "_mid") (4 . "_cen") (8 . "_nod")
                  (16 . "_qua") (32 . "_int") (64 . "_ins") (128 . "_per")
                  (256 . "_tan") (512 . "_nea") (2048 . "_app"))
      (if (= (car pr) (logand m (car pr)))
        (setq s (strcat s (if (= s "") "" ",") (cdr pr)))
      )
    )
  )
  s
)

;;; Apply osnap, then ortho, to a raw cursor point. Sets SNP to the snapped
;;; point (or nil). Returns a UCS point flattened to the polyline elevation.
(defun pll:resolve (raw / lp os s)
  (setq lp (last pts) os (pll:osmode-str) snp nil)
  (setvar "LASTPOINT" lp)                           ; base for _per / _tan
  (cond
    ((and (/= os "") (setq s (osnap raw os)))
     (setq snp s)
    )
    ((= 1 (getvar "ORTHOMODE"))
     (setq s (if (> (abs (- (car raw) (car lp))) (abs (- (cadr raw) (cadr lp))))
               (list (car raw) (cadr lp))
               (list (car lp) (cadr raw))
             )
     )
    )
    (t (setq s raw))
  )
  (list (car s) (cadr s) z0)
)

;;; ---------- entities -----------------------------------------------------

(defun pll:make-pline (lst cls / nrm)
  (setq nrm (pll:normal))
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
            (cons 90 (length lst))
            (cons 70 (logior (if cls 1 0) (if (= 1 (getvar "PLINEGEN")) 128 0)))
            (cons 43 (getvar "PLINEWID"))
            (cons 38 (caddr (trans (car lst) 1 nrm)))
      )
      (mapcar '(lambda (p / o) (setq o (trans p 1 nrm)) (list 10 (car o) (cadr o))) lst)
      (list (cons 210 nrm))
    )
  )
)

(defun pll:rebuild ()
  (if (and ent (entget ent)) (entdel ent))
  (setq ent (if (> (length pts) 1) (pll:make-pline pts closed)))
)

;;; Label sits below-right of the cursor, outside the osnap aperture so it
;;; never gets snapped to.
(defun pll:set-label (pt str / upp off h nrm ip ed)
  (setq upp (pll:upp)
        off (* upp (+ (getvar "APERTURE") 12))
        h   (* upp 13)
        nrm (pll:normal)
        ip  (trans (list (+ (car pt) off) (- (cadr pt) off h) (caddr pt)) 1 nrm)
  )
  (if (and lbl (setq ed (entget lbl)))
    (entmod
      (subst (cons 1 str) (assoc 1 ed)
        (subst (cons 40 h) (assoc 40 ed)
          (subst (cons 10 ip) (assoc 10 ed) ed)
        )
      )
    )
    (setq lbl
      (entmakex
        (list '(0 . "TEXT")
              (cons 10 ip) (cons 40 h) (cons 1 str)
              (cons 50 (angle '(0 0 0) (trans (getvar "UCSXDIR") 0 nrm t)))
              (cons 7 (getvar "TEXTSTYLE"))
              (cons 210 nrm)
        )
      )
    )
  )
)

(defun pll:marker (p / r x y z)
  (setq r (* (pll:upp) (getvar "APERTURE") 0.6)
        x (car p) y (cadr p) z (caddr p))
  (grvecs
    (list 2 (list (- x r) (- y r) z) (list (+ x r) (- y r) z)
          2 (list (+ x r) (- y r) z) (list (+ x r) (+ y r) z)
          2 (list (+ x r) (+ y r) z) (list (- x r) (+ y r) z)
          2 (list (- x r) (+ y r) z) (list (- x r) (- y r) z))
  )
)

;;; ---------- live display -------------------------------------------------

(defun pll:update (/ p seg tot)
  (setq p   (pll:resolve cur)
        seg (pll:dist2d (last pts) p)
        tot (+ (pll:length pts nil) seg))
  (pll:set-label cur
    (strcat "Total: " (rtos tot) "    Seg: " (rtos seg)
            (if (/= buf "") (strcat "    > " buf) ""))
  )
  (redraw)
  (grdraw (last pts) p 7)
  (if snp (pll:marker snp))
)

;;; ---------- actions ------------------------------------------------------

(defun pll:add (p)
  (if (and p (not (equal p (last pts) 1e-9)))
    (progn
      (setq pts (append pts (list p)))
      (pll:rebuild)
      (princ (strcat "\n  Segment " (itoa (1- (length pts))) ": "
                     (rtos (pll:dist2d (cadr (reverse pts)) p))
                     "    Running total: " (rtos (pll:length pts nil))))
    )
  )
)

(defun pll:undo ()
  (if (> (length pts) 1)
    (progn
      (setq pts (reverse (cdr (reverse pts))))
      (pll:rebuild)
      (princ (strcat "\n  Undo.    Running total: " (rtos (pll:length pts nil))))
    )
    (princ "\n  Nothing to undo.")
  )
)

(defun pll:close ()
  (if (> (length pts) 2)
    (progn (setq closed t) (pll:rebuild) (setq done t))
    (princ "\n  Need at least 3 vertices to close.")
  )
)

(defun pll:polar-ang (s / a)
  (if (setq a (angtof s))
    (if (= 1 (getvar "ANGDIR"))
      (- (getvar "ANGBASE") a)
      (+ (getvar "ANGBASE") a)
    )
  )
)

(defun pll:parse (s / rel lp i d a x y ys p)
  (setq lp (last pts))
  (if (= (substr s 1 1) "@") (setq rel t s (substr s 2)))
  (setq p
    (cond
      ((setq i (vl-string-search "<" s))                       ; d<a
       (setq d (distof (substr s 1 i))
             a (pll:polar-ang (substr s (+ i 2))))
       (if (and d a) (polar (if rel lp (list 0.0 0.0 z0)) a d))
      )
      ((setq i (vl-string-search "," s))                       ; x,y[,z]
       (setq x  (distof (substr s 1 i))
             ys (substr s (+ i 2)))
       (if (setq i (vl-string-search "," ys)) (setq ys (substr ys 1 i)))
       (setq y (distof ys))
       (if (and x y)
         (if rel (list (+ (car lp) x) (+ (cadr lp) y)) (list x y))
       )
      )
      ((and (setq d (distof s)) cur)                           ; direct distance
       (polar lp (angle lp (pll:resolve cur)) d)
      )
    )
  )
  (if p (list (car p) (cadr p) z0))
)

(defun pll:key (k / s p)
  (cond
    ((= k 8)                                                    ; backspace
     (if (> (strlen buf) 0) (setq buf (substr buf 1 (1- (strlen buf)))))
    )
    ((or (= k 13) (= k 32))                                     ; Enter / Space
     (setq s (strcase (vl-string-trim " " buf)) buf "")
     (cond
       ((= s "")  (setq done t))
       ((= s "U") (pll:undo))
       ((= s "C") (pll:close))
       ((setq p (pll:parse s)) (pll:add p))
       (t (princ (strcat "\n  Invalid input: " s)))
     )
    )
    ((= k 6)  (setvar "OSMODE" (boole 6 (getvar "OSMODE") 16384)))   ; Ctrl+F / F3
    ((= k 15) (setvar "ORTHOMODE" (- 1 (getvar "ORTHOMODE"))))       ; Ctrl+O / F8
    ((< 32 k 127) (setq buf (strcat buf (chr k))))
  )
)

;;; ---------- command ------------------------------------------------------

(defun c:PLL (/ *error* pll:cleanup doc pts p0 z0 ent lbl cur snp buf done
                closed gr code val)

  (defun pll:cleanup ()
    (if (and lbl (entget lbl)) (entdel lbl))
    (setq lbl nil)
    (redraw)
    (if (> (length pts) 1)
      (princ (strcat "\nPolyline length: " (rtos (pll:length pts closed))
                     (if closed "  (closed)" "")))
    )
    (vla-EndUndoMark doc)
  )

  (defun *error* (msg)
    (pll:cleanup)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  (if (setq p0 (getpoint "\nSpecify start point: "))
    (progn
      (setq z0  (caddr p0)
            pts (list p0)
            buf ""
            cur p0)
      (princ "\nSpecify next point or [Close/Undo] <finish>  (type distance, x,y, @x,y, @d<a then Enter):")
      (pll:update)
      (while (not done)
        (setq gr (grread t 15 0) code (car gr) val (cadr gr))
        (cond
          ((= code 5)                                  ; cursor moved
           (setq cur val)
           (pll:update)
          )
          ((= code 3)                                  ; left click
           (setq cur val)
           (pll:add (pll:resolve val))
           (pll:update)
          )
          ((member code '(11 25))                      ; right click
           (setq done t)
          )
          ((= code 2)                                  ; keyboard
           (pll:key val)
           (if (not done) (pll:update))
          )
        )
      )
    )
  )
  (pll:cleanup)
  (princ)
)

(princ "\nPLL loaded. Type PLL to draw a polyline with a live running length.")
(princ)
