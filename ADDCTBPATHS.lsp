(defun c:ADDCTBPATHS (/ acad pref files oldPath paths newPath path)

  (vl-load-com)

  ;; CTB / Plot Style paths to add
  (setq paths
    '(
      "N:\\Design\\AutoCAD\\AutoCAD\\Plot Styles"
      "N:\\Design\\AutoCAD\\AutoCAD\\Plot Styles\\AEC"
      "N:\\Design\\AutoCAD\\AutoCAD\\Plot Styles\\AIA"
      "N:\\Design\\AutoCAD\\AutoCAD\\Plot Styles\\DACH"
      "N:\\Design\\AutoCAD\\AutoCAD\\Plot Styles\\JAY"
     )
  )

  ;; Get AutoCAD Preferences > Files
  (setq acad  (vlax-get-acad-object))
  (setq pref  (vla-get-Preferences acad))
  (setq files (vla-get-Files pref))

  ;; Get existing Plot Style Table Search Path
  (setq oldPath (vla-get-PrinterStyleSheetPath files))
  (setq newPath oldPath)

  ;; Add each path if it isn't already present
  (foreach path paths

    (if (vl-string-search
          (strcase path)
          (strcase newPath)
        )

      (princ
        (strcat "\nAlready exists: " path)
      )

      (progn
        (if (= newPath "")
          (setq newPath path)
          (setq newPath (strcat newPath ";" path))
        )

        (princ
          (strcat "\nAdding: " path)
        )
      )
    )
  )

  ;; Write the updated search path
  (vla-put-PrinterStyleSheetPath files newPath)

  (princ "\n\nCTB search paths updated successfully.")
  (princ)
)