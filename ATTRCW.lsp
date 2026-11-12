;;; ATTRCW.lsp
;;;
;;; ATTRCW - Rotates the attribute text of the selected block(s) 90 degrees
;;; clockwise, each attribute about its own geometric center, so the text
;;; turns in place. Running the command four times restores the original.
;;;
;;; Single-line attributes are rotated with vla-rotate. Multiline attributes
;;; cannot be: vla-rotate drops their text. Those get 90 degrees subtracted from their
;;; rotation, which turns them about the insertion point, and are then shifted
;;; so the center of the text lands back where it was.
;;;
;;; Works noun-verb (select blocks, then run) or verb-noun (run, then select).
;;;
;;; Constant attributes belong to the block definition and are skipped.

(vl-load-com)

(setq attrcw:angle (/ pi -2.0))  ; clockwise (negative), in radians

(defun attrcw:mtext-p (att)
  (and (vlax-property-available-p att 'MTextAttribute)
       (= :vlax-true (vla-get-MTextAttribute att))
  )
)

;; Min and max corners of the bounding box, or nil when the object has none.
(defun attrcw:box (att / mn mx)
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-getboundingbox (list att 'mn 'mx))
           )
      )
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
  )
)

(defun attrcw:mid (a b)
  (mapcar '(lambda (p q) (/ (+ p q) 2.0)) a b)
)

;; WCS center of an attribute's text. The center of a bounding box is also the
;; center of the rotated text inside it, so this holds at any rotation and for
;; any justification. Empty attributes have no extents; use their position.
(defun attrcw:center (att / box)
  (if (or (= (vla-get-textstring att) "")
          (not (setq box (attrcw:box att)))
      )
    (vlax-get att 'InsertionPoint)
    (attrcw:mid (car box) (cadr box))
  )
)

(defun attrcw:add-angle (ang delta / a)
  (setq a (rem (+ ang delta) (+ pi pi)))
  (if (minusp a) (+ a pi pi) a)
)

;; A real text box contains the insertion point. A box in the wrong coordinate
;; system does not, and must not be used to shift the attribute.
(defun attrcw:box-holds-p (box pt fuzz)
  (and box
       (<= (- (car (car box)) fuzz) (car pt) (+ (car (cadr box)) fuzz))
       (<= (- (cadr (car box)) fuzz) (cadr pt) (+ (cadr (cadr box)) fuzz))
  )
)

;; Rotating PT about BASE by ANG in the XY plane.
(defun attrcw:rotate-pt (pt base ang / dx dy c s)
  (setq dx (- (car pt) (car base))
        dy (- (cadr pt) (cadr base))
        c  (cos ang)
        s  (sin ang)
  )
  (list (+ (car base) (- (* dx c) (* dy s)))
        (+ (cadr base) (+ (* dx s) (* dy c)))
        (caddr pt)
  )
)

;; Changing the rotation turns a multiline attribute about its insertion point,
;; which carries its center to a new spot. Move it back by the difference.
(defun attrcw:rotate-mtext (att / ins box cen moved)
  (setq ins (vlax-get att 'InsertionPoint)
        box (attrcw:box att)
        cen (if (attrcw:box-holds-p box ins (max (vla-get-height att) 1e-6))
              (attrcw:mid (car box) (cadr box))
            )
  )
  (vla-put-rotation att (attrcw:add-angle (vla-get-rotation att) attrcw:angle))
  (if cen
    (progn
      (setq moved (attrcw:rotate-pt cen ins attrcw:angle))
      (if (not (equal moved cen 1e-8))
        (vla-move att (vlax-3d-point moved) (vlax-3d-point cen))
      )
    )
  )
  (vla-update att)
)

(defun attrcw:rotate-att (att)
  (if (attrcw:mtext-p att)
    (attrcw:rotate-mtext att)
    (vla-rotate att (vlax-3d-point (attrcw:center att)) attrcw:angle)
  )
)

;; Rotates every attribute of REF. Returns T if all of them rotated.
(defun attrcw:rotate-insert (ref / ok)
  (setq ok T)
  (foreach att (vlax-invoke ref 'GetAttributes)
    (if (vl-catch-all-error-p (vl-catch-all-apply 'attrcw:rotate-att (list att)))
      (setq ok nil)
    )
  )
  (vl-catch-all-apply 'vla-update (list ref))
  ok
)

(defun c:ATTRCW (/ *error* doc filter ss i rotated failed)
  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
      (princ (strcat "\nATTRCW error: " msg))
    )
    (princ)
  )

  (setq doc    (vla-get-activedocument (vlax-get-acad-object))
        filter '((0 . "INSERT") (66 . 1))
        ss     (cond ((ssget "_I" filter))
                     ((ssget "_:L" filter))
               )
  )

  (if ss
    (progn
      (vla-startundomark doc)
      (setq i 0 rotated 0 failed 0)
      (repeat (sslength ss)
        (if (attrcw:rotate-insert (vlax-ename->vla-object (ssname ss i)))
          (setq rotated (1+ rotated))
          (setq failed (1+ failed))
        )
        (setq i (1+ i))
      )
      (vla-endundomark doc)
      (sssetfirst nil nil)
      (princ (strcat "\nRotated attributes 90 degrees CW in " (itoa rotated) " block(s)."))
      (if (> failed 0)
        (princ (strcat " " (itoa failed) " could not be fully rotated (locked layer?)."))
      )
    )
    (princ "\nNo blocks with attributes selected.")
  )
  (princ)
)

(princ "\nATTRCW loaded. Type ATTRCW to run.")
(princ)
