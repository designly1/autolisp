;;; ATTRCCW.lsp
;;;
;;; ATTRCCW - Rotates the attribute text of the selected block(s) 90 degrees
;;; counterclockwise, each attribute about its own geometric center, so the text
;;; turns in place. Running the command four times restores the original.
;;;
;;; Single-line attributes are rotated with vla-rotate. Multiline attributes
;;; cannot be: vla-rotate drops their text. Those get 90 degrees added to their
;;; rotation, which turns them about the insertion point, and are then shifted
;;; so the center of the text lands back where it was.
;;;
;;; Works noun-verb (select blocks, then run) or verb-noun (run, then select).
;;;
;;; Constant attributes belong to the block definition and are skipped.

(vl-load-com)

(setq attrccw:angle (/ pi 2.0))   ; counterclockwise, in radians

(defun attrccw:mtext-p (att)
  (and (vlax-property-available-p att 'MTextAttribute)
       (= :vlax-true (vla-get-MTextAttribute att))
  )
)

;; Min and max corners of the bounding box, or nil when the object has none.
(defun attrccw:box (att / mn mx)
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-getboundingbox (list att 'mn 'mx))
           )
      )
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
  )
)

(defun attrccw:mid (a b)
  (mapcar '(lambda (p q) (/ (+ p q) 2.0)) a b)
)

;; WCS center of an attribute's text. The center of a bounding box is also the
;; center of the rotated text inside it, so this holds at any rotation and for
;; any justification. Empty attributes have no extents; use their position.
(defun attrccw:center (att / box)
  (if (or (= (vla-get-textstring att) "")
          (not (setq box (attrccw:box att)))
      )
    (vlax-get att 'InsertionPoint)
    (attrccw:mid (car box) (cadr box))
  )
)

(defun attrccw:add-angle (ang delta / a)
  (setq a (rem (+ ang delta) (+ pi pi)))
  (if (minusp a) (+ a pi pi) a)
)

;; A real text box contains the insertion point. A box in the wrong coordinate
;; system does not, and must not be used to shift the attribute.
(defun attrccw:box-holds-p (box pt fuzz)
  (and box
       (<= (- (car (car box)) fuzz) (car pt) (+ (car (cadr box)) fuzz))
       (<= (- (cadr (car box)) fuzz) (cadr pt) (+ (cadr (cadr box)) fuzz))
  )
)

;; Rotating PT about BASE by ANG in the XY plane.
(defun attrccw:rotate-pt (pt base ang / dx dy c s)
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
(defun attrccw:rotate-mtext (att / ins box cen moved)
  (setq ins (vlax-get att 'InsertionPoint)
        box (attrccw:box att)
        cen (if (attrccw:box-holds-p box ins (max (vla-get-height att) 1e-6))
              (attrccw:mid (car box) (cadr box))
            )
  )
  (vla-put-rotation att (attrccw:add-angle (vla-get-rotation att) attrccw:angle))
  (if cen
    (progn
      (setq moved (attrccw:rotate-pt cen ins attrccw:angle))
      (if (not (equal moved cen 1e-8))
        (vla-move att (vlax-3d-point moved) (vlax-3d-point cen))
      )
    )
  )
  (vla-update att)
)

(defun attrccw:rotate-att (att)
  (if (attrccw:mtext-p att)
    (attrccw:rotate-mtext att)
    (vla-rotate att (vlax-3d-point (attrccw:center att)) attrccw:angle)
  )
)

;; Rotates every attribute of REF. Returns T if all of them rotated.
(defun attrccw:rotate-insert (ref / ok)
  (setq ok T)
  (foreach att (vlax-invoke ref 'GetAttributes)
    (if (vl-catch-all-error-p (vl-catch-all-apply 'attrccw:rotate-att (list att)))
      (setq ok nil)
    )
  )
  (vl-catch-all-apply 'vla-update (list ref))
  ok
)

(defun c:ATTRCCW (/ *error* doc filter ss i rotated failed)
  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
      (princ (strcat "\nATTRCCW error: " msg))
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
        (if (attrccw:rotate-insert (vlax-ename->vla-object (ssname ss i)))
          (setq rotated (1+ rotated))
          (setq failed (1+ failed))
        )
        (setq i (1+ i))
      )
      (vla-endundomark doc)
      (sssetfirst nil nil)
      (princ (strcat "\nRotated attributes 90 degrees CCW in " (itoa rotated) " block(s)."))
      (if (> failed 0)
        (princ (strcat " " (itoa failed) " could not be fully rotated (locked layer?)."))
      )
    )
    (princ "\nNo blocks with attributes selected.")
  )
  (princ)
)

(princ "\nATTRCCW loaded. Type ATTRCCW to run.")
(princ)
