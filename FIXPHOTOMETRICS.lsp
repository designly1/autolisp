(vl-load-com)

;; FIXPHOTOMETRICS
;; Finds photometric calc points -- a mark made of crossing short
;; segments (LINE or 2-vertex LWPOLYLINE) next to a numeric MTEXT/TEXT
;; such as 0.3, 1.4, 10.4 -- and moves them to layer E-LITE-PHOTO.
;;
;; The mark may be a plus, an X, or an asterisk: any group of short
;; strokes at different angles whose midpoints coincide. Strokes are
;; sized against the number's own text height, so a long line that
;; only passes through the mark, such as a grid line, is not taken.
;; Plus signs with no number are also taken when they are sized like
;; the numbered marks found in the same drawing.
;;
;; Works on PDF-imported geometry: matching is tolerance based,
;; not exact. One UNDO reverses the whole command.

(setq FP:LAYER "E-LITE-PHOTO")

;; ---------------------------------------------------------------
;; Text helpers
;; ---------------------------------------------------------------

(defun FP:MTextString (d / s item)
  (setq s "")
  (foreach item d
    (if (member (car item) '(3 1))
      (setq s (strcat s (cdr item)))
    )
  )
  s
)

;; Strip simple MTEXT formatting: {\fArial|b0;0.3} -> 0.3
(defun FP:Clean (s / p)
  (setq s (vl-string-trim " " s))
  (while (and (wcmatch s "{*") (wcmatch s "*}"))
    (setq s (substr s 2 (- (strlen s) 2)))
  )
  (while (and (wcmatch s "\\*") (setq p (vl-string-search ";" s)))
    (setq s (substr s (+ p 2)))
  )
  (vl-string-trim " {}" s)
)

;; Decimal number: optional minus, digits, one dot, digits.
(defun FP:IsNumber (s / chars dots ok)
  (if (wcmatch s "-*") (setq s (substr s 2)))
  (setq chars (vl-string->list s)
        dots  0
        ok    (> (length chars) 2))
  (foreach c chars
    (cond
      ((= c 46) (setq dots (1+ dots)))
      ((and (>= c 48) (<= c 57)))
      (T (setq ok nil))
    )
  )
  (and ok
       (= dots 1)
       (wcmatch s "#*`.#*"))
)

;; Returns (ename x y height tab) for numeric texts, else nil.
(defun FP:TextRec (e / d typ s p)
  (setq d   (entget e)
        typ (cdr (assoc 0 d)))
  (setq s
    (FP:Clean
      (if (= typ "MTEXT")
        (FP:MTextString d)
        (cdr (assoc 1 d))
      )
    )
  )
  (if (FP:IsNumber s)
    (progn
      (setq p
        (if (and (= typ "TEXT")
                 (or (/= 0 (cdr (assoc 72 d)))
                     (/= 0 (cdr (assoc 73 d)))))
          (cdr (assoc 11 d))
          (cdr (assoc 10 d))
        )
      )
      (if (/= typ "MTEXT") (setq p (trans p e 0)))
      (list e (car p) (cadr p) (cdr (assoc 40 d)) (cdr (assoc 410 d)))
    )
  )
)

;; ---------------------------------------------------------------
;; Segment helpers
;; ---------------------------------------------------------------

;; Returns (ename mx my len tab ang) for a short straight segment,
;; where ang is its direction folded into [0, pi); else nil.
(defun FP:SegRec (e maxlen / d typ p1 p2 pts len ang)
  (setq d   (entget e)
        typ (cdr (assoc 0 d)))
  (cond
    ((= typ "LINE")
      (setq p1 (cdr (assoc 10 d))
            p2 (cdr (assoc 11 d)))
    )
    ((and (= typ "LWPOLYLINE")
          (= 2 (cdr (assoc 90 d)))
          (equal (cdr (assoc 210 d)) '(0.0 0.0 1.0) 1e-8)
          (not (vl-some
                 '(lambda (x) (and (= (car x) 42)
                                   (not (equal (cdr x) 0.0 1e-8))))
                 d)))
      (setq pts (vl-remove-if-not '(lambda (x) (= (car x) 10)) d)
            p1  (cdar pts)
            p2  (cdadr pts))
    )
  )
  (if (and p1 p2)
    (progn
      (setq len (distance (list (car p1) (cadr p1))
                          (list (car p2) (cadr p2))))
      (if (and (> len 0.0) (<= len maxlen))
        (progn
          (setq ang (angle p1 p2))
          (if (>= ang pi) (setq ang (- ang pi)))
          (list e
                (/ (+ (car p1) (car p2)) 2.0)
                (/ (+ (cadr p1) (cadr p2)) 2.0)
                len
                (cdr (assoc 410 d))
                ang)
        )
      )
    )
  )
)

;; Returns (ename cx cy size tab) for a straight-edged LWPOLYLINE of
;; 3+ vertices whose bounding box is no larger than maxlen, where
;; (cx cy) is the box center and size its larger side; else nil.
;; PDF imports draw some asterisk arms as one such polyline.
(defun FP:ClusterRec (e maxlen / d pts xs ys size)
  (setq d (entget e))
  (if (and (= "LWPOLYLINE" (cdr (assoc 0 d)))
           (> (cdr (assoc 90 d)) 2)
           (equal (cdr (assoc 210 d)) '(0.0 0.0 1.0) 1e-8)
           (not (vl-some
                  '(lambda (x) (and (= (car x) 42)
                                    (not (equal (cdr x) 0.0 1e-8))))
                  d)))
    (progn
      (setq pts  (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) d))
            xs   (mapcar 'car pts)
            ys   (mapcar 'cadr pts)
            size (max (- (apply 'max xs) (apply 'min xs))
                      (- (apply 'max ys) (apply 'min ys))))
      (if (and (> size 0.0) (<= size maxlen))
        (list e
              (/ (+ (apply 'max xs) (apply 'min xs)) 2.0)
              (/ (+ (apply 'max ys) (apply 'min ys)) 2.0)
              size
              (cdr (assoc 410 d)))
      )
    )
  )
)

;; Enames of cluster records centered on mark (enames cx cy len tab).
;; win is the x-sorted cluster list starting at or left of the mark.
(defun FP:MarkClusters (mark win / tol c found)
  (setq tol (* 0.2 (nth 3 mark)))
  (while (and win (<= (cadr (car win)) (+ (cadr mark) tol)))
    (setq c   (car win)
          win (cdr win))
    (if (and (>= (cadr c) (- (cadr mark) tol))
             (<= (abs (- (caddr c) (caddr mark))) tol)
             (<= (nth 3 c) (* 2.0 (nth 3 mark)))
             (= (nth 4 c) (nth 4 mark)))
      (setq found (cons (car c) found))
    )
  )
  found
)

;; Plus signs with no number: two near-perpendicular segments whose
;; midpoints coincide, each 0.7-1.4 times len (the typical mark arm
;; length). segs must be x-sorted. Returns a list of (ename ename).
(defun FP:LonePluses (segs len / tol lo hi s win t2 result)
  (setq tol (* 0.2 len)
        lo  (* 0.7 len)
        hi  (* 1.4 len))
  (while segs
    (setq s    (car segs)
          segs (cdr segs))
    (if (and (>= (nth 3 s) lo) (<= (nth 3 s) hi))
      (progn
        (setq win segs)
        (while (and win (<= (cadr (car win)) (+ (cadr s) tol)))
          (setq t2  (car win)
                win (cdr win))
          (if (and (<= (abs (- (caddr t2) (caddr s))) tol)
                   (>= (nth 3 t2) lo)
                   (<= (nth 3 t2) hi)
                   (= (nth 4 t2) (nth 4 s))
                   (> (FP:AngDiff (nth 5 t2) (nth 5 s)) 1.4))
            (setq result (cons (list (car s) (car t2)) result))
          )
        )
      )
    )
  )
  result
)

(defun FP:Median (nums)
  (if nums
    (nth (nth (/ (length nums) 2) (vl-sort-i nums '<)) nums)
    0.0
  )
)

(defun FP:SortX (recs)
  (vl-sort recs '(lambda (a b) (< (cadr a) (cadr b))))
)

;; Smallest angle between two directions in [0, pi).
(defun FP:AngDiff (a b / d)
  (setq d (abs (- a b)))
  (min d (- pi d))
)

;; Finds the marks for one number. rec is an FP:TextRec record; win
;; is the x-sorted segment list starting at or left of the search
;; area. Candidates are segments within 3 text heights whose length
;; is 0.3-2.0 text heights. A mark is a group of candidates whose
;; midpoints coincide and that includes two directions at least
;; 10 degrees apart. Overlapping calc grids put two numbers on one
;; mark, so every mark within 2 text heights is returned rather than
;; only the nearest; neighboring grid points are farther than that.
;; Returns a list of (enames cx cy len tab), else nil.
(defun FP:FindMarks (rec win / x y h tab r cands s c tol grp cross
                     result)
  (setq x   (cadr rec)
        y   (caddr rec)
        h   (nth 3 rec)
        tab (nth 4 rec)
        r   (* 3.0 h))
  (while (and win (<= (cadr (car win)) (+ x r)))
    (setq s   (car win)
          win (cdr win))
    (if (and (<= (abs (- (caddr s) y)) r)
             (>= (cadr s) (- x r))
             (>= (nth 3 s) (* 0.3 h))
             (<= (nth 3 s) (* 2.0 h))
             (= (nth 4 s) tab))
      (setq cands (cons s cands))
    )
  )
  (foreach c cands
    (setq tol   (* 0.2 (nth 3 c))
          grp   nil
          cross nil)
    (foreach s cands
      (if (and (<= (abs (- (cadr s)  (cadr c)))  tol)
               (<= (abs (- (caddr s) (caddr c))) tol))
        (progn
          (setq grp (cons (car s) grp))
          (if (> (FP:AngDiff (nth 5 s) (nth 5 c)) 0.17)
            (setq cross T)
          )
        )
      )
    )
    (if (and cross
             (<= (distance (list x y) (list (cadr c) (caddr c)))
                 (* 2.0 h)))
      (setq result
        (cons (list grp (cadr c) (caddr c) (nth 3 c) tab) result))
    )
  )
  result
)

;; ---------------------------------------------------------------
;; Layer
;; ---------------------------------------------------------------

(defun FP:EnsureLayer (name)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 name)
        '(70 . 0)
        '(62 . 7)
        '(6 . "Continuous")
      )
    )
  )
  (tblsearch "LAYER" name)
)

(defun FP:SetLayer (e name / r)
  (setq r
    (vl-catch-all-apply
      'vla-put-layer
      (list (vlax-ename->vla-object e) name)
    )
  )
  (not (vl-catch-all-error-p r))
)

;; ---------------------------------------------------------------
;; Command
;; ---------------------------------------------------------------

(defun c:FIXPHOTOMETRICS
  (/ *error* doc ss i e rec texts maxh maxlen segs clusters win cwin
     marks mark pr lens lone pairs nomatch result moved failed ans)

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (vla-endundomark doc)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nFIXPHOTOMETRICS error: " msg))
    )
    (princ)
  )

  ;; 1. Numeric texts.
  (prompt "\nReading text...")
  (if (setq ss (ssget "_X" '((0 . "MTEXT,TEXT"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (if (setq rec (FP:TextRec (ssname ss i)))
          (setq texts (cons rec texts))
        )
        (setq i (1+ i))
      )
    )
  )

  (if (null texts)
    (prompt "\nNo numeric text (like 0.3 or 10.4) found.")
    (progn
      (setq maxh   (apply 'max (mapcar '(lambda (r) (nth 3 r)) texts))
            maxlen (* 2.0 maxh))

      ;; 2. Short straight segments.
      (prompt "\nReading line segments...")
      (if (setq ss (ssget "_X" '((0 . "LINE,LWPOLYLINE"))))
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq e (ssname ss i))
            (cond
              ((setq rec (FP:SegRec e maxlen))
                (setq segs (cons rec segs)))
              ((setq rec (FP:ClusterRec e maxlen))
                (setq clusters (cons rec clusters)))
            )
            (setq i (1+ i))
          )
        )
      )

      ;; 3. Each number takes every mark within 2 text heights.
      (prompt "\nMatching numbers to marks...")
      (setq segs (FP:SortX segs)
            win  segs
            cwin (FP:SortX clusters))
      (foreach rec (FP:SortX texts)
        (while (and win (< (cadr (car win)) (- (cadr rec) (* 3.0 maxh))))
          (setq win (cdr win))
        )
        (while (and cwin (< (cadr (car cwin)) (- (cadr rec) (* 4.0 maxh))))
          (setq cwin (cdr cwin))
        )
        (if (setq marks (FP:FindMarks rec win))
          (progn
            (setq pr (list (car rec)))
            (foreach mark marks
              (setq lens (cons (nth 3 mark) lens)
                    pr   (append pr (car mark) (FP:MarkClusters mark cwin)))
            )
            (setq pairs (cons pr pairs))
          )
          (setq nomatch (1+ (if nomatch nomatch 0)))
        )
      )

      (if (null pairs)
        (prompt
          (strcat "\nFound " (itoa (length texts))
                  " numeric texts and " (itoa (length segs))
                  " short segments, but no crossing marks next"
                  " to the numbers.")
        )
        (progn
          (setq result (ssadd))
          (foreach pr pairs
            (foreach e pr (ssadd e result))
          )
          ;; 4. Plus signs with no number, sized like the matched marks.
          (setq lone 0)
          (foreach pr (FP:LonePluses segs (FP:Median lens))
            (if (not (or (ssmemb (car pr) result)
                         (ssmemb (cadr pr) result)))
              (progn
                (setq lone (1+ lone))
                (foreach e pr (ssadd e result))
              )
            )
          )
          (sssetfirst nil result)

          (initget "Yes No")
          (setq ans
            (getkword
              (strcat "\nFound " (itoa (length pairs))
                      " numbered and " (itoa lone)
                      " unnumbered photometric symbols ("
                      (itoa (sslength result))
                      " objects, highlighted). Move to "
                      FP:LAYER "? [Yes/No] <Yes>: ")
            )
          )

          (if (/= ans "No")
            (progn
              (vla-startundomark doc)
              (FP:EnsureLayer FP:LAYER)
              (setq moved 0 failed 0 i 0)
              (repeat (sslength result)
                (if (FP:SetLayer (ssname result i) FP:LAYER)
                  (setq moved (1+ moved))
                  (setq failed (1+ failed))
                )
                (setq i (1+ i))
              )
              (vla-endundomark doc)
              (princ
                (strcat "\nMoved " (itoa moved) " objects ("
                        (itoa (+ lone (length pairs))) " symbols) to "
                        FP:LAYER ".")
              )
              (if (> failed 0)
                (princ (strcat "\n" (itoa failed)
                               " objects could not be changed"
                               " (locked layer?)."))
              )
            )
            (prompt "\nNothing changed.")
          )

          (if nomatch
            (princ (strcat "\n" (itoa nomatch)
                           " numeric texts had no mark nearby"
                           " and were left alone."))
          )
        )
      )
    )
  )
  (princ)
)

(princ "\nFIXPHOTOMETRICS loaded. Type FIXPHOTOMETRICS to run.")
(princ)
