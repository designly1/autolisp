;;; FLIPATTR.lsp
;;;
;;; FLIPATTR - Flips the attribute text of the selected block(s) in place: each
;;; attribute is rotated 180 degrees about its own center, so text that reads
;;; upside down in a rotated block reads upright and stays where it was. Running
;;; the command twice restores the original.
;;;
;;; Single-line attributes are rotated with vla-rotate. Multiline attributes
;;; cannot be: vla-rotate drops their text. Those get 180 degrees added to their
;;; rotation, which turns them about the insertion point, and are then shifted
;;; so the center of the text lands back where it was.
;;;
;;; Works noun-verb (select blocks, then run) or verb-noun (run, then select).
;;;
;;; Constant attributes belong to the block definition and are skipped.

(vl-load-com)

(defun flipattr:mtext-p (att)
  (and (vlax-property-available-p att 'MTextAttribute)
       (= :vlax-true (vla-get-MTextAttribute att))
  )
)

;; Min and max corners of the bounding box, or nil when the object has none.
(defun flipattr:box (att / mn mx)
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-getboundingbox (list att 'mn 'mx))
           )
      )
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
  )
)

(defun flipattr:mid (a b)
  (mapcar '(lambda (p q) (/ (+ p q) 2.0)) a b)
)

;; WCS center of an attribute's text. The center of a bounding box is also the
;; center of the rotated text inside it, so this holds at any rotation and for
;; any justification. Empty attributes have no extents; use their position.
(defun flipattr:center (att / box)
  (if (or (= (vla-get-textstring att) "")
          (not (setq box (flipattr:box att)))
      )
    (vlax-get att 'InsertionPoint)
    (flipattr:mid (car box) (cadr box))
  )
)

(defun flipattr:add-pi (ang / a)
  (setq a (rem (+ ang pi) (+ pi pi)))
  (if (minusp a) (+ a pi pi) a)
)

;; A real text box contains the insertion point. A box in the wrong coordinate
;; system does not, and must not be used to shift the attribute.
(defun flipattr:box-holds-p (box pt fuzz)
  (and box
       (<= (- (car (car box)) fuzz) (car pt) (+ (car (cadr box)) fuzz))
       (<= (- (cadr (car box)) fuzz) (cadr pt) (+ (cadr (cadr box)) fuzz))
  )
)

;; Rotation turns a multiline attribute about its insertion point. Reflect the
;; center back through that point so the 180 degree turn stays on the same spot.
(defun flipattr:flip-mtext (att / ins box cen dest)
  (setq ins (vlax-get att 'InsertionPoint)
        box (flipattr:box att)
        cen (if (flipattr:box-holds-p box ins (max (vla-get-height att) 1e-6))
              (flipattr:mid (car box) (cadr box))
            )
  )
  (vla-put-rotation att (flipattr:add-pi (vla-get-rotation att)))
  (if cen
    (progn
      (setq dest (mapcar '(lambda (c i) (- (+ c c) i)) cen ins))
      (if (not (equal ins dest 1e-8))
        (vla-move att (vlax-3d-point ins) (vlax-3d-point dest))
      )
    )
  )
  (vla-update att)
)

(defun flipattr:flip-att (att)
  (if (flipattr:mtext-p att)
    (flipattr:flip-mtext att)
    (vla-rotate att (vlax-3d-point (flipattr:center att)) pi)
  )
)

;; Flips every attribute of REF. Returns T if all of them flipped.
(defun flipattr:flip-insert (ref / ok)
  (setq ok T)
  (foreach att (vlax-invoke ref 'GetAttributes)
    (if (vl-catch-all-error-p (vl-catch-all-apply 'flipattr:flip-att (list att)))
      (setq ok nil)
    )
  )
  (vl-catch-all-apply 'vla-update (list ref))
  ok
)

(defun c:FLIPATTR (/ *error* doc filter ss i flipped failed)
  (defun *error* (msg)
    (if doc (vla-endundomark doc))
    (if (not (wcmatch (strcase msg T) "*break*,*cancel*,*exit*"))
      (princ (strcat "\nFLIPATTR error: " msg))
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
      (setq i 0 flipped 0 failed 0)
      (repeat (sslength ss)
        (if (flipattr:flip-insert (vlax-ename->vla-object (ssname ss i)))
          (setq flipped (1+ flipped))
          (setq failed (1+ failed))
        )
        (setq i (1+ i))
      )
      (vla-endundomark doc)
      (sssetfirst nil nil)
      (princ (strcat "\nFlipped attributes in " (itoa flipped) " block(s)."))
      (if (> failed 0)
        (princ (strcat " " (itoa failed) " could not be fully flipped (locked layer?)."))
      )
    )
    (princ "\nNo blocks with attributes selected.")
  )
  (princ)
)

(princ "\nFLIPATTR loaded. Type FLIPATTR to run.")
(princ)
