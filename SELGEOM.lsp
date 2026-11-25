(vl-load-com)

;; SELGEOM
;; Translation-only matching of exploded geometry.
;; AutoCAD for Windows.
;;
;; Supported:
;; LINE, ARC, CIRCLE, LWPOLYLINE, ELLIPSE,
;; SOLID, TRACE, 3DFACE, POINT,
;; TEXT, MTEXT, ATTRIB, ATTDEF and INSERT (non-xref).
;;
;; No drawing entities are changed.

(defun SG:Point3 (p)
  (list
    (car p)
    (cadr p)
    (if (caddr p) (caddr p) 0.0)
  )
)

(defun SG:Wcs (e p)
  (if p (trans (SG:Point3 p) e 0))
)

(defun SG:Mid (a b)
  (list
    (/ (+ (car a) (car b)) 2.0)
    (/ (+ (cadr a) (cadr b)) 2.0)
    (/ (+ (caddr a) (caddr b)) 2.0)
  )
)

(defun SG:AngPi (a)
  (setq a (rem a pi))
  (if (< a 0.0) (setq a (+ a pi)))
  a
)

;; Angle equality with wraparound. PERIOD is pi for undirected
;; lines, 2pi for rotations. Without this, a horizontal line with
;; float noise gives 0.0 on one copy and 3.14159 on another.
(defun SG:AngEq (a b period tol / d)
  (if (and a b)
    (progn
      (setq d (rem (abs (- a b)) period))
      (or (< d tol) (< (- period d) tol))
    )
    (and (null a) (null b))
  )
)

(defun SG:Num (v def)
  (if v v def)
)

(defun SG:SameXY (a b tol)
  (and a b
    (equal (car a) (car b) tol)
    (equal (cadr a) (cadr b) tol)
  )
)

(defun SG:BoxMin (e / obj lo hi result)
  (setq obj (vlax-ename->vla-object e))
  (setq result
    (vl-catch-all-apply
      'vla-getboundingbox
      (list obj 'lo 'hi)
    )
  )
  (if (not (vl-catch-all-error-p result))
    (vlax-safearray->list lo)
  )
)

(defun SG:BoxSize (e / obj lo hi result)
  (setq obj (vlax-ename->vla-object e))
  (setq result
    (vl-catch-all-apply
      'vla-getboundingbox
      (list obj 'lo 'hi)
    )
  )
  (if (not (vl-catch-all-error-p result))
    (mapcar '-
      (vlax-safearray->list hi)
      (vlax-safearray->list lo)
    )
  )
)

;; Diagonal of the combined bounding box of the sample records.
(defun SG:SampleSize (refs / obj lo hi r xs ys)
  (foreach r refs
    (setq obj (vlax-ename->vla-object (car r)))
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vla-getboundingbox
                                   (list obj 'lo 'hi))))
      (progn
        (setq lo (vlax-safearray->list lo)
              hi (vlax-safearray->list hi)
              xs (cons (car lo) (cons (car hi) xs))
              ys (cons (cadr lo) (cons (cadr hi) ys)))
      )
    )
  )
  (if xs
    (distance
      (list (apply 'min xs) (apply 'min ys))
      (list (apply 'max xs) (apply 'max ys))
    )
    0.0
  )
)

(defun SG:IsXref (name / blk)
  (and
    (setq blk (tblsearch "BLOCK" name))
    (= 4 (logand 4 (cdr (assoc 70 blk))))
  )
)

(defun SG:MTextString (d / s item)
  (setq s "")
  (foreach item d
    (if (member (car item) '(1 3))
      (setq s (strcat s (cdr item)))
    )
  )
  s
)

(defun SG:InsertName (e d / obj name)
  (setq obj  (vlax-ename->vla-object e)
        name (vl-catch-all-apply 'vla-get-effectivename (list obj)))
  (if (vl-catch-all-error-p name)
    (cdr (assoc 2 d))
    name
  )
)

(defun SG:Supported (d / typ normal)
  (setq typ    (cdr (assoc 0 d))
        normal (cdr (assoc 210 d)))
  (and
    (member typ
      '("LINE" "ARC" "CIRCLE" "LWPOLYLINE"
        "ELLIPSE" "SOLID" "TRACE" "3DFACE" "POINT"
        "TEXT" "MTEXT" "ATTRIB" "ATTDEF" "INSERT"))
    (or
      (/= typ "INSERT")
      (not (SG:IsXref (cdr (assoc 2 d))))
    )
    ;; Restrict OCS entities to the normal world XY plane.
    (or
      (null normal)
      (equal normal '(0.0 0.0 1.0) 1e-10)
    )
  )
)

(defun SG:RelData (e d base codes / item code val data)
  (foreach item d
    (setq code (car item)
          val  (cdr item))
    (if (member code codes)
      (progn
        (cond
          ((and (member (cdr (assoc 0 d)) '("ELLIPSE" "MTEXT"))
                (= code 11))
            (setq val (SG:Point3 val))
          )
          ((member code '(10 11 12 13))
            (setq val (mapcar '- (SG:Wcs e val) base))
          )
          ((and (= (cdr (assoc 0 d)) "LWPOLYLINE") (= code 38))
            (setq val (- val (caddr base)))
          )
        )
        (setq data (cons (cons code val) data))
      )
    )
  )
  (reverse data)
)

(defun SG:Record (e / d typ base data p10 p11)
  (setq d (entget e))
  (if (SG:Supported d)
    (progn
      (setq typ (cdr (assoc 0 d)))
      (cond
        ((= typ "LINE")
          (setq p10  (SG:Wcs e (cdr (assoc 10 d)))
                p11  (SG:Wcs e (cdr (assoc 11 d)))
                base (SG:Mid p10 p11)
                data (list
                  (cons 40 (distance p10 p11))
                  (cons 50
                    (SG:AngPi
                      (atan
                        (- (cadr p11) (cadr p10))
                        (- (car p11) (car p10))
                      )
                    )
                  )
                )
          )
        )
        ((= typ "POINT")
          (setq base (SG:Wcs e (cdr (assoc 10 d)))
                data nil)
        )
        ((= typ "CIRCLE")
          (setq base (SG:Wcs e (cdr (assoc 10 d)))
                data (list (cons 40 (cdr (assoc 40 d)))))
        )
        ((= typ "ARC")
          (setq base (SG:Wcs e (cdr (assoc 10 d)))
                data (list
                  (cons 40 (cdr (assoc 40 d)))
                  (cons 50 (cdr (assoc 50 d)))
                  (cons 51 (cdr (assoc 51 d)))
                )
          )
        )
        ((member typ '("TEXT" "ATTRIB" "ATTDEF"))
          (setq base
                (if
                  (and
                    (assoc 11 d)
                    (or
                      (not (zerop (SG:Num (cdr (assoc 72 d)) 0)))
                      (not (zerop (SG:Num (cdr (assoc 73 d)) 0)))
                    )
                  )
                  (SG:Wcs e (cdr (assoc 11 d)))
                  (SG:Wcs e (cdr (assoc 10 d)))
                )
                data (list
                  (cons 1
                    (if (= typ "ATTDEF")
                      (strcat (SG:Num (cdr (assoc 2 d)) "")
                              "|" (SG:Num (cdr (assoc 1 d)) ""))
                      (SG:Num (cdr (assoc 1 d)) "")
                    )
                  )
                  (cons 40 (cdr (assoc 40 d)))
                  (cons 50 (SG:Num (cdr (assoc 50 d)) 0.0))
                  (cons 41 (SG:Num (cdr (assoc 41 d)) 1.0))
                  (cons 72 (SG:Num (cdr (assoc 72 d)) 0))
                  (cons 73 (SG:Num (cdr (assoc 73 d)) 0))
                )
          )
        )
        ((= typ "MTEXT")
          (setq base (SG:Wcs e (cdr (assoc 10 d)))
                data (list
                  (cons 1  (SG:MTextString d))
                  (cons 40 (cdr (assoc 40 d)))
                  (cons 50 (SG:Num (cdr (assoc 50 d)) 0.0))
                  (cons 71 (SG:Num (cdr (assoc 71 d)) 1))
                )
          )
        )
        ((= typ "INSERT")
          (setq base (SG:Wcs e (cdr (assoc 10 d)))
                data (list
                  (cons 2  (SG:InsertName e d))
                  (cons 41 (SG:Num (cdr (assoc 41 d)) 1.0))
                  (cons 42 (SG:Num (cdr (assoc 42 d)) 1.0))
                  (cons 43 (SG:Num (cdr (assoc 43 d)) 1.0))
                  (cons 50 (SG:Num (cdr (assoc 50 d)) 0.0))
                  (cons 60 (SG:BoxSize e))
                )
          )
        )
        ((= typ "LWPOLYLINE")
          (setq base (SG:BoxMin e))
          (if base
            (setq data
              (SG:RelData e d base '(10 38 39 40 41 42 70 90))
            )
          )
        )
        ((= typ "ELLIPSE")
          (setq base (SG:Wcs e (cdr (assoc 10 d)))
                data (SG:RelData e d base '(10 11 40 41 42)))
        )
        (T
          (setq base (SG:BoxMin e))
          (if (null base)
            (setq base (SG:Wcs e (cdr (assoc 10 d))))
          )
          (if base
            (setq data (SG:RelData e d base '(10 11 12 13 39 70)))
          )
        )
      )
      (if base
        (list e typ base data (SG:Key d))
      )
    )
  )
)

;; Layer + color key. Overlapping hatches (e.g. blue shrubs over red
;; dots) are told apart by this, so one cannot stand in for the other.
(defun SG:Key (d)
  (strcat
    (cdr (assoc 8 d))
    " / color "
    (if (assoc 62 d) (itoa (cdr (assoc 62 d))) "BYLAYER")
  )
)

;; ((key . count) ...) for a record list, most common first.
(defun SG:KeyCounts (recs / rec k pair counts)
  (foreach rec recs
    (setq k (nth 4 rec))
    (if (setq pair (assoc k counts))
      (setq counts (subst (cons k (1+ (cdr pair))) pair counts))
      (setq counts (cons (cons k 1) counts))
    )
  )
  (vl-sort counts '(lambda (a b) (> (cdr a) (cdr b))))
)

;; If the sample spans several layer/color keys, ask which to keep.
;; Returns the (possibly reduced) record list.
(defun SG:PickKey (refs / counts i choice k)
  (setq counts (SG:KeyCounts refs))
  (if (> (length counts) 1)
    (progn
      (prompt
        "\nSample spans several layers/colors (overlapping hatches?):"
      )
      (setq i 0)
      (foreach pair counts
        (setq i (1+ i))
        (princ
          (strcat "\n  " (itoa i) ". " (car pair)
                  "  (" (itoa (cdr pair)) " objects)")
        )
      )
      (princ "\n  0. Use all of them together")
      (initget 4)
      (setq choice
        (getint "\nMatch which layer/color? <0>: ")
      )
      (if (null choice) (setq choice 0))
      (if (and (> choice 0) (<= choice (length counts)))
        (progn
          (setq k (car (nth (1- choice) counts)))
          (vl-remove-if-not
            '(lambda (r) (= (nth 4 r) k))
            refs
          )
        )
        refs
      )
    )
    refs
  )
)

(defun SG:SameShape (a b tol / ta da db na nb)
  (setq ta (cadr a)
        da (nth 3 a)
        db (nth 3 b))
  (and
    (= ta (cadr b))
    (= (nth 4 a) (nth 4 b))
    (cond
      ((= ta "POINT") T)

      ((= ta "LINE")
        (and
          (equal (cdr (assoc 40 da)) (cdr (assoc 40 db)) tol)
          (SG:AngEq (cdr (assoc 50 da)) (cdr (assoc 50 db)) pi
            (max 1e-4 (/ tol (max 1e-6 (cdr (assoc 40 da))))))
        )
      )

      ((= ta "ARC")
        (and
          (equal (cdr (assoc 40 da)) (cdr (assoc 40 db)) tol)
          (SG:AngEq (cdr (assoc 50 da)) (cdr (assoc 50 db)) (* 2 pi) 1e-4)
          (SG:AngEq (cdr (assoc 51 da)) (cdr (assoc 51 db)) (* 2 pi) 1e-4)
        )
      )

      ((member ta '("TEXT" "ATTRIB" "ATTDEF" "MTEXT"))
        (and
          (= (strcase (cdr (assoc 1 da)))
             (strcase (cdr (assoc 1 db))))
          (equal (cdr (assoc 40 da)) (cdr (assoc 40 db)) tol)
          (SG:AngEq (cdr (assoc 50 da)) (cdr (assoc 50 db)) (* 2 pi) 1e-4)
          (equal (cdr (assoc 41 da)) (cdr (assoc 41 db)) tol)
          (equal (cdr (assoc 71 da)) (cdr (assoc 71 db)))
          (equal (cdr (assoc 72 da)) (cdr (assoc 72 db)))
          (equal (cdr (assoc 73 da)) (cdr (assoc 73 db)))
        )
      )

      ((= ta "INSERT")
        (setq na (cdr (assoc 2 da))
              nb (cdr (assoc 2 db)))
        (and
          (or
            (= (strcase na) (strcase nb))
            (and
              (= "*" (substr na 1 1))
              (= "*" (substr nb 1 1))
              (equal (cdr (assoc 60 da)) (cdr (assoc 60 db)) tol)
            )
          )
          (equal (cdr (assoc 41 da)) (cdr (assoc 41 db)) tol)
          (equal (cdr (assoc 42 da)) (cdr (assoc 42 db)) tol)
          (equal (cdr (assoc 43 da)) (cdr (assoc 43 db)) tol)
          (SG:AngEq (cdr (assoc 50 da)) (cdr (assoc 50 db)) (* 2 pi) 1e-4)
        )
      )

      (T (equal da db tol))
    )
  )
)

(defun SG:Records (ss / i rec result)
  (setq i 0)
  (repeat (sslength ss)
    (if (setq rec (SG:Record (ssname ss i)))
      (setq result (cons rec result))
    )
    (setq i (1+ i))
  )
  (reverse result)
)

(defun SG:TypeList (ss / i typ types)
  (setq i 0)
  (repeat (sslength ss)
    (setq typ (cdr (assoc 0 (entget (ssname ss i)))))
    (if (not (member typ types))
      (setq types (cons typ types))
    )
    (setq i (1+ i))
  )
  (reverse types)
)

(defun SG:JoinTypes (types / s)
  (setq s "")
  (foreach typ types
    (setq s
      (if (= s "")
        typ
        (strcat s ", " typ)
      )
    )
  )
  s
)

(defun SG:FindAt (ref target pool used tol / found rec)
  (while (and pool (not found))
    (setq rec  (car pool)
          pool (cdr pool))
    (if
      (and
        (not (member (car rec) used))
        (SG:SameXY target (nth 2 rec) tol)
        (SG:SameShape ref rec tol)
      )
      (setq found rec)
    )
  )
  found
)

;; Diagnostic: for each sample member, count search objects with the
;; same shape (ignoring position). A count of 1 means only the sample
;; itself matched, so that member is what breaks the group.
(defun SG:Diagnose (refs pool tol / ref n d)
  (prompt "\nPer-member shape matches in search area:")
  (foreach ref refs
    (setq n 0
          d (nth 3 ref))
    (foreach rec pool
      (if (SG:SameShape ref rec tol) (setq n (1+ n)))
    )
    (princ
      (strcat
        "\n  " (cadr ref)
        (if (assoc 1 d) (strcat " \"" (cdr (assoc 1 d)) "\"") "")
        (if (and (= (cadr ref) "INSERT") (assoc 2 d))
          (strcat " " (cdr (assoc 2 d)))
          ""
        )
        (if (and (= (cadr ref) "LINE") (assoc 40 d))
          (strcat " len=" (rtos (cdr (assoc 40 d)) 2 4)
                  " ang=" (rtos (cdr (assoc 50 d)) 2 6))
          ""
        )
        ": " (itoa n)
      )
    )
  )
)

(defun c:SELGEOM
  (/ *error* seed refs tol mode scope pool
     anchor candidate delta remaining ref target hit
     used ok result copies skipped space filter)

  ;; Capture pickfirst before any other work can consume it.
  (setq seed (ssget "_I"))
  (if (and (null seed) (cadr (ssgetfirst)))
    (setq seed (cadr (ssgetfirst)))
  )
  (sssetfirst nil nil)

  (defun *error* (msg)
    (if
      (and msg
        (not
          (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")
        )
      )
      (princ (strcat "\nSELGEOM error: " msg))
    )
    (princ)
  )

  (if (null seed)
    (progn
      (if
        (and
          (= 1 (getvar "CVPORT"))
          (= 0 (getvar "TILEMODE"))
        )
        (prompt
          (strcat
            "\nPaper space is active. Model-space objects"
            " inside a viewport cannot be window-selected."
            "\nDouble-click into the viewport first."
            "\nSelect ONE complete sample (plus, text, block, etc.): "
          )
        )
        (prompt "\nSelect ONE complete exploded sample: ")
      )
      (setq seed (ssget))
    )
  )

  (cond
    ((null seed)
      (prompt "\nNothing selected.")
    )

    ((null (setq refs (SG:Records seed)))
      (prompt
        (strcat
          "\nNone of the selected objects are supported."
          "\nFound: " (SG:JoinTypes (SG:TypeList seed)) "."
          "\nUse LINE, ARC, CIRCLE, LWPOLYLINE, ELLIPSE,"
          " SOLID, TRACE, 3DFACE, POINT,"
          " TEXT, MTEXT, ATTRIB, ATTDEF or INSERT."
          "\nEntities must have the standard world XY normal."
          "\nNothing was selected."
        )
      )
    )

    (T
      (if (/= (length refs) (sslength seed))
        (princ
          (strcat
            "\nIgnored "
            (itoa (- (sslength seed) (length refs)))
            " unsupported sample object(s)."
            " Found types: "
            (SG:JoinTypes (SG:TypeList seed))
            "."
            "\nMatching the remaining "
            (itoa (length refs))
            " supported object(s)."
          )
        )
      )

      (setq refs (SG:PickKey refs))

      ;; Default tolerance: 3% of the sample size. PDF-imported
      ;; geometry is rounded per copy, so a fixed 0.01 is far too
      ;; tight in inch drawings and only some copies line up.
      (setq tol (max 0.01 (* 0.03 (SG:SampleSize refs))))
      (initget 6)
      (setq mode
        (getreal
          (strcat "\nMatching tolerance <" (rtos tol 2 4) ">: ")
        )
      )
      (if mode (setq tol mode))

      (initget "Entire Window")
      (setq mode
        (getkword
          "\nSearch [Entire/Window] <Entire>: "
        )
      )

      ;; On a layout, search model space and this sheet.
      (setq space
        (if (= 1 (getvar "TILEMODE"))
          "Model"
          (getvar "CTAB")
        )
      )
      (setq filter
        (if (= space "Model")
          (list (cons 410 "Model"))
          (list
            '(-4 . "<OR")
              (cons 410 "Model")
              (cons 410 space)
            '(-4 . "OR>")
          )
        )
      )

          (if (= mode "Window")
            (progn
              (prompt
                "\nSelect search-area objects, then press Enter: "
              )
              (setq scope (ssget filter))
            )
            (setq scope (ssget "_X" filter))
          )

          (if scope
            (progn
              (prompt "\nReading search geometry...")
              (setq pool    (SG:Records scope)
                    skipped (- (sslength scope) (length pool))
                    anchor  (car refs)
                    result  (ssadd)
                    copies  0)

              (prompt "\nSearching for complete matching groups...")

              (foreach candidate pool
                ;; Already-selected anchor means this copy
                ;; was found previously.
                (if
                  (and
                    (not (ssmemb (car candidate) result))
                    (SG:SameShape anchor candidate tol)
                  )
                  (progn
                    (setq delta
                      (mapcar '-
                        (nth 2 candidate)
                        (nth 2 anchor)
                      )
                    )

                    (setq used      (list (car candidate))
                          remaining (cdr refs)
                          ok        T)

                    (while (and ok remaining)
                      (setq ref       (car remaining)
                            remaining (cdr remaining)
                            target
                              (mapcar '+ (nth 2 ref) delta)
                            hit
                              (SG:FindAt
                                ref target pool used tol
                              ))

                      (if hit
                        (setq used (cons (car hit) used))
                        (setq ok nil)
                      )
                    )

                    ;; Only add geometry after the whole
                    ;; reference group has matched.
                    (if ok
                      (progn
                        (foreach e used
                          (ssadd e result)
                        )
                        (setq copies (1+ copies))
                      )
                    )
                  )
                )
              )

              (if (> (sslength result) 0)
                (progn
                  (setvar "PICKFIRST" 1)
                  (sssetfirst nil result)
                  (princ
                    (strcat
                      "\nFound " (itoa copies)
                      " matching groups; selected "
                      (itoa (sslength result))
                      " objects."
                    )
                  )
                )
                (prompt "\nNo complete matching groups found.")
              )

              (if (<= copies 1)
                (SG:Diagnose refs pool tol)
              )

              (if (> skipped 0)
                (princ
                  (strcat
                    "\nSkipped " (itoa skipped)
                    " unsupported search-area objects."
                  )
                )
              )
            )
            (prompt "\nNo search-area objects selected.")
          )
    )
  )
  (princ)
)

(princ "\nSELGEOM loaded. Type SELGEOM to run.")
(princ)