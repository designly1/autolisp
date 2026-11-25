;;; ------------------------------------------------------------------------
;;; ENUM.lsp  -  Enumerated block options (lookup table helper)
;;;
;;; Command: ENUM
;;;   Runs only in the Block Editor (BLOCKEDITOR = 1). Builds a hidden
;;;   linear index parameter (NAME_IDX) and a lookup parameter (NAME) per
;;;   your enumerated-options guide, then starts the Lookup action so the
;;;   Property Lookup Table dialog opens. Row values are printed to the
;;;   command line for you to enter; set Allow reverse lookup, Audit, OK.
;;;
;;;   Prompts: property name, description, comma-separated options, grips.
;;;   Press Enter at the name prompt to add another list; blank name exits.
;;; ------------------------------------------------------------------------

(vl-load-com)

(if (not (boundp '*enum-stack-y*)) (setq *enum-stack-y* nil))
(setq *enum-stack-step* 8.0)

(defun enum:trim (s)
  (if s (vl-string-trim " \t" s) "")
)

(defun enum:valid-name-p (s)
  (and s (> (strlen s) 0)
       (null (vl-string-search " " s))
       (null (vl-string-search "," s))
       (vl-every
         '(lambda (c)
            (or (and (>= c 65) (<= c 90))
                (and (>= c 97) (<= c 122))
                (and (>= c 48) (<= c 57))
                (= c 95)
            )
          )
         (vl-string->list s)
       )
  )
)

(defun enum:parse-options (s / parts out p seen)
  (setq out '()
        parts (enum:split-str s ",")
  )
  (foreach p parts
    (setq p (enum:trim p))
    (if (and (> (strlen p) 0) (not (member (strcase p) seen)))
      (progn
        (setq out (append out (list p))
              seen (cons (strcase p) seen)
        )
      )
    )
  )
  (if out out (progn (princ "\nENUM: no valid options.") nil))
)

(defun enum:split-str (s delim / pos out)
  (setq out '())
  (while (and s (setq pos (vl-string-search delim s)))
    (setq out (append out (list (substr s 1 pos)))
          s (substr s (+ pos (strlen delim)))
    )
  )
  (if s (setq out (append out (list s))))
  out
)

(defun enum:join-str (lst sep / r)
  (setq r "")
  (foreach item lst
    (setq r (if (= r "") item (strcat r sep item)))
  )
  r
)

(defun enum:int-list->value-str (n / lst i)
  (setq lst '() i 1)
  (while (<= i n)
    (setq lst (append lst (list (itoa i))))
    (setq i (1+ i))
  )
  (enum:join-str lst ",")
)

(defun enum:param-entities (/ ss n i lst)
  (setq lst '())
  (if (setq ss (ssget "_X"
                       '((0 . "BLOCKLINEARPARAMETER,BLOCKLOOKUPPARAMETER,BLOCKUSERPARAMETER"))
         )
      )
    (repeat (setq n (sslength ss) i 0)
      (setq lst (cons (ssname ss i) lst)
            i (1+ i)
      )
    )
  )
  lst
)

(defun enum:new-entity (before types / after e found)
  (setq after (enum:param-entities)
        found nil
  )
  (foreach e after
    (if (and (null found)
             (not (member e before))
             (member (cdr (assoc 0 (entget e))) types)
        )
      (setq found e)
    )
  )
  found
)

;;; Return the parameter's public name, or nil when unknown.
(defun enum:param-name (e / typ r)
  (setq typ (cdr (assoc 0 (entget e))))
  (setq r
        (cond
          ((= typ "BLOCKLINEARPARAMETER")
           (vl-catch-all-apply 'getpropertyvalue (list e "DistanceName"))
          )
          ((= typ "BLOCKLOOKUPPARAMETER")
           (vl-catch-all-apply 'getpropertyvalue (list e "Name"))
          )
          ((= typ "BLOCKUSERPARAMETER")
           (vl-catch-all-apply 'getpropertyvalue (list e "UserParameterName"))
          )
          (t nil)
        )
  )
  (if (vl-catch-all-error-p r)
    (progn
      (setq r (cond
                ((= typ "BLOCKLOOKUPPARAMETER")
                 (cdr (assoc 303 (entget e)))
                )
                ((= typ "BLOCKLINEARPARAMETER")
                 (cdr (assoc 305 (entget e)))
                )
                (t nil)
              )
      )
    )
  )
  (if (and r (= (type r) 'STR) (> (strlen r) 0)) r nil)
)

(defun enum:name-in-block-p (name / n e)
  (setq n (strcase name))
  (vl-some
    '(lambda (e)
       (equal n (strcase (enum:param-name e)))
     )
    (enum:param-entities)
  )
)

(defun enum:set-prop (e prop val / r)
  (setq r (vl-catch-all-apply 'setpropertyvalue (list e prop val)))
  (if (vl-catch-all-error-p r)
    (progn
      (princ (strcat "\nENUM: setpropertyvalue \"" prop "\" failed: "
                     (vl-catch-all-error-message r)
             )
      )
      nil
    )
    t
  )
)

(defun enum:try-props (e props / ok)
  (setq ok t)
  (foreach pair props
    (if (not (enum:set-prop e (car pair) (cdr pair)))
      (setq ok nil)
    )
  )
  ok
)

(defun enum:grip-count (e n)
  (if e
    (vl-catch-all-apply 'command (list "_.BGRIPSET" e (itoa n)))
  )
)

(defun enum:stack-origin (/ base)
  (if (not *enum-stack-y*)
    (setq *enum-stack-y* 0.0)
  )
  (setq base (list 500.0 *enum-stack-y* 0.0))
  (setq *enum-stack-y* (+ *enum-stack-y* *enum-stack-step*))
  base
)

(defun enum:make-linear (origin idxName nOpts / before pt1 pt2 lbl ent
                                valStr)
  (setq before (enum:param-entities)
        pt1 origin
        pt2 (mapcar '+ origin '(1.0 0.0 0.0))
        lbl (mapcar '+ origin '(0.0 -1.0 0.0))
        valStr (enum:int-list->value-str nOpts)
  )
  (vl-catch-all-apply 'command
                      (list "_.-BPARAMETER" "_Linear" pt1 pt2 lbl "0")
  )
  (setq ent (enum:new-entity before '("BLOCKLINEARPARAMETER")))
  (if (not ent)
    (progn (princ "\nENUM: failed to create linear index parameter.") nil)
    (progn
      (enum:try-props ent (list (cons "DistanceName" idxName)))
      (enum:try-props ent (list (cons "UpdatedDistance" 1.0)))
      (if (not (enum:try-props ent (list (cons "BlockParamValueSet/Type" "List"))))
        (enum:try-props ent (list (cons "BlockParamValueSet/Type" 2)))
      )
      (enum:try-props ent (list (cons "BlockParamValueSet/Values" valStr)))
      (enum:grip-count ent 0)
      (enum:try-props ent (list (cons "ShowProperties" 0)))
      (enum:try-props ent (list (cons "UpdatedDistance/ShowProperties" 0)))
      ent
    )
  )
)

(defun enum:make-lookup (origin name desc grips / before pt ent)
  (setq before (enum:param-entities)
        pt origin
  )
  (vl-catch-all-apply 'command
                      (list "_.-BPARAMETER" "_Lookup" pt)
  )
  (setq ent (enum:new-entity before '("BLOCKLOOKUPPARAMETER")))
  (if (not ent)
    (progn (princ "\nENUM: failed to create lookup parameter.") nil)
    (progn
      (enum:try-props ent
                      (list (cons "Name" name)
                            (cons "LookupDescription" (if desc desc ""))
                            (cons "ShowProperties" 1)
                      )
      )
      (enum:grip-count ent grips)
      ent
    )
  )
)

(defun enum:print-table (idxName name options / i n w1 w2)
  (setq n (length options)
        w1 (max (strlen idxName) 6)
        w2 (max (strlen name) (apply 'max (mapcar 'strlen options)))
  )
  (princ "\n--- Property Lookup Table (enter in the dialog) ---")
  (princ (strcat "\n  Add Properties: check \"" idxName "\" under Input Properties."))
  (princ (strcat "\n  "
                 (enum:pad-right idxName w1)
                 "  "
                 (enum:pad-right name w2)
         )
  )
  (setq i 1)
  (foreach opt options
    (princ (strcat "\n  "
                   (enum:pad-right (itoa i) w1)
                   "  "
                   (enum:pad-right opt w2)
           )
    )
    (setq i (1+ i))
  )
  (princ (strcat "\n  Lookup column \"" name "\": set Allow reverse lookup."))
  (princ "\n  Architectural units may show 1\", 2\" in the index column; values are whole numbers.")
  (princ "\n  Audit, then OK.")
  (princ "\n---------------------------------------------------")
)

(defun enum:pad-right (s w / len)
  (setq len (strlen s))
  (if (>= len w) s (strcat s (enum:rep " " (- w len))))
)

(defun enum:rep (c n / r)
  (setq r "")
  (repeat n (setq r (strcat r c)))
  r
)

(defun enum:start-lookup-action (lookupEnt pt / actPt)
  (setq actPt (mapcar '+ pt '(1.5 1.5 0.0)))
  (princ "\nENUM: Property Lookup Table dialog should open.")
  (vl-catch-all-apply 'command
                      (list "_.BACTIONTOOL"
                            "_Lookup"
                            (list lookupEnt pt)
                            actPt
                      )
  )
)

(defun enum:one-list (propName desc options grips / idxName origin idxEnt lookEnt)
  (setq idxName (strcat propName "_IDX"))
  (if (enum:name-in-block-p propName)
    (progn
      (princ (strcat "\nENUM: \"" propName "\" already exists in this block. Nothing changed."))
      nil
    )
    (if (enum:name-in-block-p idxName)
      (progn
        (princ (strcat "\nENUM: \"" idxName "\" already exists in this block. Nothing changed."))
        nil
      )
      (progn
        (setq origin (enum:stack-origin))
        (setq idxEnt (enum:make-linear origin idxName (length options)))
        (if (not idxEnt)
          nil
          (progn
            (setq lookEnt
                  (enum:make-lookup
                    (mapcar '+ origin '(0.0 2.0 0.0))
                    propName
                    desc
                    grips
                  )
            )
            (if (not lookEnt)
              (princ "\nENUM: index was created; lookup failed.")
              (progn
                (enum:print-table idxName propName options)
                (enum:start-lookup-action lookEnt (mapcar '+ origin '(0.0 2.0 0.0)))
                t
              )
            )
          )
        )
      )
    )
  )
)

(defun enum:clear-initget ()
  (initget)
)

;;; Dynamic Input treats a typed comma as the X,Y separator, so it is off
;;; while the option list is typed.
(defun enum:prompt-options (/ oldDyn s one lst)
  (setq oldDyn (getvar "DYNMODE"))
  (setvar "DYNMODE" 0)
  (enum:clear-initget)
  (setq s (enum:trim
            (getstring T "\nOptions, comma-separated (Enter to type them one per line): ")
          )
  )
  (if (= s "")
    (progn
      (setq lst '())
      (while (/= "" (setq one (enum:trim
                                (getstring T (strcat "\nOption " (itoa (1+ (length lst)))
                                                     " (Enter when done): "
                                             )
                                )
                              )
                    )
             )
        (setq lst (append lst (list one)))
      )
      (setq s (enum:join-str lst ","))
    )
  )
  (setvar "DYNMODE" oldDyn)
  s
)

(defun enum:prompt-grips (/ g def)
  (setq def 1)
  (initget "0 1")
  (setq g (getkword (strcat "\nLookup grips for Properties/dropdown [0/1] <" (itoa def) ">: ")))
  (enum:clear-initget)
  (if (not g) def (atoi g))
)

(defun c:ENUM (/ prop desc optStr options grips oldEcho oldDyn done *error*)
  (defun *error* (msg)
    (if oldEcho (setvar "CMDECHO" oldEcho))
    (if oldDyn (setvar "DYNMODE" oldDyn))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nENUM error: " msg))
    )
    (princ)
  )
  (if (/= 1 (getvar "BLOCKEDITOR"))
    (progn
      (princ "\nENUM: run this command in the Block Editor (BEDIT).")
      (princ)
    )
    (progn
      (setq *enum-stack-y* 0.0
            oldEcho (getvar "CMDECHO")
            oldDyn (getvar "DYNMODE")
      )
      (setvar "CMDECHO" 0)
      (setq done nil)
      (enum:clear-initget)
      (while (not done)
        (setq prop (getstring T "\nENUM property name (e.g. TRIP_UNIT) or Enter to finish: "))
        (setq prop (enum:trim prop))
        (if (= prop "")
          (setq done t)
          (progn
            (if (not (enum:valid-name-p prop))
              (progn
                (enum:clear-initget)
                (princ
                  "\nENUM: name must be letters, digits, underscores only (no commas or spaces)."
                )
                (princ "\n       Enter the option list at the Options prompt, not the name.")
              )
              (progn
                (enum:clear-initget)
                (setq desc (getstring T "\nDescription (tooltip, Enter for none): "))
                (setq desc (enum:trim desc))
                (setq optStr (enum:prompt-options))
                (setq options (enum:parse-options optStr))
                (if options
                  (progn
                    (setq grips (enum:prompt-grips))
                    (enum:one-list prop desc options grips)
                  )
                )
              )
            )
          )
        )
      )
      (setvar "CMDECHO" oldEcho)
      (princ)
    )
  )
)

(princ "\nENUM loaded. In the Block Editor, type ENUM to add an enumerated lookup.")
(princ)