;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GridGeorefLay_v2_4.lsp

; Code developed from an example found on the web by Samuel Saez Lopez (IMGA S.L.P) for the creation of a georeferenced grid. 
; Contact: https://www.linkedin.com/in/samuel-saez-lopez-7603091a2/

; Esta versión:
; - Pregunta al inicio: “Do you want to use the default settings? [Y/N]:”
;   Si se usan los ajustes predefinidos se establece:
;     • Texto siempre fuera ("O")
;     • Líneas sobre el marco ("ON")
;     • Punto de inicio en X e Y = 0
;     • Distancia del texto al borde = 3
;     • Longitud de línea pequeña ("S")
;     • El step size se calcula automáticamente para que, partiendo de 0, las marcas se dibujen
;       en números redondos (por ejemplo, 0, 50000, 100000, …) respetando que la separación sea 
;       igual a 3 veces la anchura estimada del texto.
; - Si no se usan ajustes predefinidos se solicitan los parámetros manualmente.
; - Las líneas se dibujan en la capa "GridGeorefLay_marks" y el texto en "GridGeorefLay_text",
;   creando dichas capas si no existen.
; - Los números se formatean con espacios como separadores de miles.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "\nStarting GridGeorefLay")

;--------------------------------------------------------------------
; Función para formatear números con espacios como separadores de miles.
(defun format-number (num / str len firstGroup formatted)
  (setq str (rtos num 2 0))
  (if (< (strlen str) 4)
      str
      (progn
         (setq len (strlen str))
         (setq firstGroup (mod len 3))
         (if (= firstGroup 0) (setq firstGroup 3))
         (setq formatted (substr str 1 firstGroup))
         (setq i (+ firstGroup 1))
         (while (<= i len)
           (setq formatted (strcat formatted " " (substr str i 3)))
           (setq i (+ i 3))
         )
         formatted
      )
  )
)

;--------------------------------------------------------------------
; Función de manejo de errores.
(defun my_error (msg)
  (print (strcat "\nError occurred: " msg))
  (command "_undo" "_back")
  (setq *error* alterror)
  (setvar "blipmode" sblip)
  (setvar "cmdecho" scmde)
  (setvar "osmode" sosmode)
  (setvar "angbase" sangbase)
  (setvar "angdir" sangdir)
  (setvar "aunits" saunits)
  (princ)
)

;--------------------------------------------------------------------
; Función auxiliar para crear (si no existe) una capa.
(defun ensure-layer (layerName / ent)
  (if (not (tblsearch "LAYER" layerName))
    (entmake (list
               (cons 0 "LAYER")
               (cons 2 layerName)
               (cons 70 0)
               (cons 62 7)  ; Color blanco por defecto.
             ))
  )
)

;--------------------------------------------------------------------
; Función auxiliar para solicitar respuesta Y/N.
(defun prompt-yes-no (promptStr)
  (initget "Y N")
  (setq answer (getkword promptStr))
  (setq answer (strcase (substr answer 1 1)))
  (if (or (equal answer "Y") (equal answer "N"))
    answer
    (progn
      (prompt "\nInvalid answer. Please enter Y or N.")
      (prompt-yes-no promptStr)
    )
  )
)

;--------------------------------------------------------------------
; Función auxiliar para solicitar respuesta I/O (Inside/Outside) para el texto.
(defun prompt-inside-outside (promptStr)
  (initget "I O")
  (setq answer (getkword promptStr))
  (setq answer (strcase (substr answer 1 1)))
  (if (or (equal answer "I") (equal answer "O"))
    answer
    (progn
      (prompt "\nInvalid answer. Please enter I or O.")
      (prompt-inside-outside promptStr)
    )
  )
)

;--------------------------------------------------------------------
; Función auxiliar para solicitar la opción de líneas: I, ON o O.
(defun prompt-lines-option (promptStr)
  (initget "I ON O")
  (setq answer (getkword promptStr))
  (setq answer (strcase answer))
  (if (or (equal answer "I") (equal answer "ON") (equal answer "O"))
    answer
    (progn
      (prompt "\nInvalid answer. Please enter I, ON or O.")
      (prompt-lines-option promptStr)
    )
  )
)

;--------------------------------------------------------------------
; Función auxiliar para obtener el valor permitido más cercano.
(defun nearest-allowed (val allowedList / best bestDiff curr)
  (setq best (car allowedList))
  (setq bestDiff (abs (- val best)))
  (foreach item allowedList
    (setq currDiff (abs (- val item)))
    (if (< currDiff bestDiff)
      (progn
        (setq best item)
        (setq bestDiff currDiff)
      )
    )
  )
  best
)

;--------------------------------------------------------------------
; Función #VPT_BOX: Calcula los 4 puntos del recorte del viewport en Modelo.
(defun #VPT_BOX (view / v_eed v_psw v_psh v_msw v_msh v_msxp v_msyp v_ang 
                      v_xy1 v_xy2 v_xy3 v_xy4 v_dpx v_dpy v_eed_dp)
  (defun #ENT_GC (gc list) (cdr (assoc gc list)))
  (defun #TRG_ROT (point angle offsetx offsety)
    (setq x (car point))
    (setq y (cadr point))
    (setq ca (cos angle))
    (setq sa (sin angle))
    (list (+ (- (* x ca) (* y sa)) offsetx)
          (+ (* x sa) (* y ca) offsety)
    )
  )
  (if (and (= 'ENAME (type view))
           (setq v_eed (entget view '("ACAD")))
           (= "VIEWPORT" (#ENT_GC 0 v_eed))
      )
    (progn
      (setq v_psw (#ENT_GC 40 v_eed)
            v_psh (#ENT_GC 41 v_eed)
            v_eed (cdar (#ENT_GC -3 v_eed))
      )
      (setq v_eed_dp (member (assoc 1010 v_eed) v_eed))
      (setq v_dpxy (#ENT_GC 1010 v_eed_dp))
      (setq v_dpx (car v_dpxy))
      (setq v_dpy (cadr v_dpxy))
      (setq v_eed (member (assoc 1040 v_eed) v_eed))
      (setq v_ang (#ENT_GC 1040 v_eed))
      (setq v_eed (cdr v_eed))
      (setq v_msh (#ENT_GC 1040 v_eed))
      (setq v_eed (cdr v_eed))
      (setq v_msxp (#ENT_GC 1040 v_eed))
      (setq v_eed (cdr v_eed))
      (setq v_msyp (#ENT_GC 1040 v_eed))
      (setq v_scale (/ v_psh v_msh)
            v_msw (/ v_psw v_scale)
            v_xy1 (list (- v_msxp (/ v_msw 2.0))
                        (- v_msyp (/ v_msh 2.0))
                  )
      )
      (setq v_ang (- (* 2.0 pi) v_ang))
      (setq v_xy1 (#TRG_ROT v_xy1 v_ang v_dpx v_dpy)
            v_xy2 (polar v_xy1 v_ang v_msw)
            v_ang (+ v_ang (/ pi 2.0))
            v_xy3 (polar v_xy2 v_ang v_msh)
            v_ang (+ v_ang (/ pi 2.0))
            v_xy4 (polar v_xy3 v_ang v_msw)
      )
      (list v_xy1 v_xy2 v_xy3 v_xy4)
    )
  )
)

;--------------------------------------------------------------------
; Función auxiliar para dibujar líneas y colocar etiquetas en un borde.
; Se añade el parámetro centerOption: si es T, la línea se dibuja centrada en el punto de referencia.
(defun draw-edge-labels (startVal minVal maxVal deltaVal modelStart modelDiff
                              paperStart paperDiff fixedVal fixedAxis
                              lineDir textDir textAlign txtPos txtLineDist textHeight textAngle centerOption)
  (setq val startVal)
  (while (<= val minVal)
    (setq val (+ val deltaVal))
  )
  (while (< val maxVal)
    (setq ratio (/ (- val modelStart) modelDiff))
    (setq paperCoord (+ paperStart (* ratio paperDiff)))
    (setq ctext (format-number val)) ; se formatea el número con separadores.
    (if (eq fixedAxis 'x)
      (setq ptCoord (list fixedVal paperCoord))  ; Fijo en X, variable en Y.
      (setq ptCoord (list paperCoord fixedVal))  ; Fijo en Y, variable en X.
    )
    (if centerOption
      (progn
        (setq halfLength (/ txtLineDist 2.0))
        ; Se calcula p1 y p2 para que ptCoord sea el centro de la línea.
        (setq p1 (polar ptCoord (- lineDir pi) halfLength))
        (setq p2 (polar ptCoord lineDir halfLength))
      )
      (progn
        (setq p1 ptCoord)
        (setq p2 (polar p1 lineDir txtLineDist))
      )
    )
    (setq pt (polar p1 textDir txtPos))
    (command "_.-layer" "Set" "GridGeorefLay_marks" "")
    (command "_.line" p1 p2 "")
    (command "_.-layer" "Set" "GridGeorefLay_text" "")
    (command "_.text" "_j" textAlign pt textHeight textAngle ctext)
    (setq val (+ val deltaVal))
  )
)

;--------------------------------------------------------------------
; Función principal: C:GridGeorefLay
(defun C:GridGeorefLay ( / alterror sblip scmde sosmode sangbase sangdir saunits
                                 anz al x axl
                                 zen_af zen_af_x zen_af_y zen_modw zen_mod_xw zen_mod_yw
                                 br_af h_af h_mod affakt alpha
                                 liun_af_x liob_af_x reun_af_x reob_af_x
                                 liun_af_y liob_af_y reun_af_y liob_af_y
                                 element punkte
                                 liun_mb reun_mb reob_mb liob_mb
                                 liun_mb_x liun_mb_y liob_mb_x liob_mb_y
                                 reun_mb_x reun_mb_y reob_mb_x reob_mb_y
                                 startx starty delta_l txtpos baseLineLength finalLineLength
                                 distfactor
                                 delta_model-bottom delta_paper-bottom
                                 delta_model-top    delta_paper-top
                                 delta_model-left   delta_paper-left
                                 delta_model-right  delta_paper-right
                                 fixedX fixedY
                                 textOutside
                                 txtOutAns lineOpt
                                 lineInside lineOn lineOutside lineCenter
                                 lineDir-bottom textDir-bottom
                                 lineDir-top textDir-top
                                 lineDir-left textDir-left
                                 lineDir-right textDir-right
                                 lineLengthOption defaultAns useDefaults
                                 stepInput viewportWidth_model viewportWidth_paper scaleFactor
                                 refText n estimatedCharWidth_model estimatedTextWidth_model
                                 candidateTextStep desiredMarks candidate allowedList chosenMeasure
                                 )
  (setq alterror *error*)
  (setq *error* my_error)
  (command "_undo" "_mark")
  
  ;; Guardar parámetros del sistema.
  (setq distfactor (getvar "TEXTSIZE"))
  (setq sblip    (getvar "blipmode"))
  (setq scmde    (getvar "cmdecho"))
  (setq sosmode  (getvar "osmode"))
  (setq sangbase (getvar "angbase"))
  (setq sangdir  (getvar "angdir"))
  (setq saunits  (getvar "aunits"))
  
  (setvar "blipmode" 0)
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "angbase" 0)
  (setvar "angdir" 0)
  (setvar "aunits" 0)
  
  ;; Preguntar si se desean usar los ajustes predefinidos.
  (setq defaultAns (prompt-yes-no "\nDo you want to use the default settings? [Y/N]: "))
  (setq useDefaults (equal defaultAns "Y"))
  
  ;; Crear (o asegurar) las capas para líneas y textos.
  (ensure-layer "GridGeorefLay_marks")
  (ensure-layer "GridGeorefLay_text")
  
  ;; Seleccionar el viewport.
  (setq al (ssget '((0 . "VIEWPORT"))))
  (if al (setq anz (sslength al)) (print "\nNo viewport selected"))
  
  ;; Para el texto: si se usan ajustes predefinidos se fuerza a "O", sino se pregunta.
  (if useDefaults
    (setq txtOutAns "O")
    (setq txtOutAns (prompt-inside-outside "\nDo you want to draw the text inside or outside? [I/O]: "))
  )
  (setq textOutside (equal txtOutAns "O"))
  
  ;; Para las líneas: si se usan ajustes predefinidos se fuerza la opción "ON", sino se pregunta.
  (if useDefaults
    (setq lineOpt "ON")
    (setq lineOpt (prompt-lines-option "\nDo you want to draw the lines inside, on or outside? [I/ON/O]: "))
  )
  (setq lineInside (equal lineOpt "I"))
  (setq lineOn     (equal lineOpt "ON"))
  (setq lineOutside (equal lineOpt "O"))
  (setq lineCenter lineOn)  ; Si se eligió "ON", se desea centrar la línea.
  
  ;; Definir offsets para cada borde según la opción elegida.
  ;; Borde inferior:
  (setq lineDir-bottom 
        (cond ((equal lineOpt "I") (/ pi 2))
              ((equal lineOpt "O") (- (/ pi 2)))
              ((equal lineOpt "ON") (/ pi 2))
              (T (/ pi 2))))
  (setq textDir-bottom (if textOutside (- (/ pi 2)) (/ pi 2)))
  
  ;; Borde superior:
  (setq lineDir-top 
        (cond ((equal lineOpt "I") (- (/ pi 2)))
              ((equal lineOpt "O") (/ pi 2))
              ((equal lineOpt "ON") (- (/ pi 2)))
              (T (- (/ pi 2)))))
  (setq textDir-top (if textOutside (/ pi 2) (- (/ pi 2))))
  
  ;; Borde izquierdo:
  (setq lineDir-left 
        (cond ((equal lineOpt "I") 0)
              ((equal lineOpt "O") pi)
              ((equal lineOpt "ON") 0)
              (T 0)))
  (setq textDir-left (if textOutside pi 0))
  
  ;; Borde derecho:
  (setq lineDir-right 
        (cond ((equal lineOpt "I") pi)
              ((equal lineOpt "O") 0)
              ((equal lineOpt "ON") pi)
              (T pi)))
  (setq textDir-right (if textOutside 0 pi))
  
  (setq x 0)
  (while (< x anz)
    (setq axl (entget (ssname al x)))
    (if (= "VIEWPORT" (cdr (assoc 0 axl)))
      (progn
        ;; Extraer datos del viewport (Paper y Modelo)
        (setq zen_af   (cdr (assoc 10 axl))
              zen_af_x (car zen_af)
              zen_af_y (cadr zen_af)
              zen_modw (cdr (assoc 12 axl))
              zen_mod_xw (car zen_modw)
              zen_mod_yw (cadr zen_modw)
              br_af (cdr (assoc 40 axl))
              h_af  (cdr (assoc 41 axl))
              h_mod (cdr (assoc 45 axl))
              affakt (/ h_af h_mod)
              br_mod (/ br_af affakt)
              alpha (cdr (assoc 51 axl))
        )
        ;; Calcular esquinas en Paper (asumiendo sin rotación)
        (setq liun_af_x (- zen_af_x (/ br_af 2.0))
              liob_af_x liun_af_x
              reun_af_x (+ zen_af_x (/ br_af 2.0))
              reob_af_x reun_af_x
              liun_af_y (- zen_af_y (/ h_af 2.0))
              reun_af_y liun_af_y
              liob_af_y (+ zen_af_y (/ h_af 2.0))
              reob_af_y liob_af_y
        )
        ;; Obtener los puntos en Modelo del viewport mediante #VPT_BOX.
        (setq element (cdr (assoc -1 axl)))
        (setq punkte (#VPT_BOX element))
        (setq liun_mb (nth 0 punkte)
              reun_mb (nth 1 punkte)
              reob_mb (nth 2 punkte)
              liob_mb (nth 3 punkte)
        )
        (setq liun_mb_x (car liun_mb)
              liun_mb_y (cadr liun_mb)
              liob_mb_x (car liob_mb)
              liob_mb_y (cadr liob_mb)
              reun_mb_x (car reun_mb)
              reun_mb_y (cadr reun_mb)
              reob_mb_x (car reob_mb)
              reob_mb_y (cadr reob_mb)
        )
        
        ;; En modo predefinido forzamos startx = 0 y starty = 0.
        (if useDefaults
          (progn
            (setq startx 0)
            (setq starty 0)
            (setq txtpos 4)
            (setq lineLengthOption "S")
          )
          (progn
            (initget 1)
            (setq startx (getreal "\nSpecify starting value for x coordinates: "))  
            (setq starty (getreal "\nSpecify starting value for y coordinates: "))
            (initget 3)
            (setq txtpos  (getreal "\nSpecify text distance from frame: "))
            (initget "L M S")
            (setq lineLengthOption (strcase (getkword "\nSelect line length option [L/M/S]: ")))
          )
        )
        
        ;; Para el step size:
        ;; Si se usan ajustes predefinidos, se calcula automáticamente de modo que, partiendo de 0,
        ;; las marcas queden en múltiplos exactos de un valor permitido.
        (if useDefaults
          (progn
            (setq viewportWidth_model (- reun_mb_x liun_mb_x))
            (setq viewportWidth_paper (- reun_af_x liun_af_x))
            (setq scaleFactor (/ viewportWidth_model viewportWidth_paper))
            (setq refText (rtos reun_mb_x 2 0))
            (setq n (strlen refText))
            (setq estimatedCharWidth_model (* 0.6 distfactor scaleFactor))
            (setq estimatedTextWidth_model (* n estimatedCharWidth_model))
            (setq candidateTextStep (* 3 estimatedTextWidth_model))
            (setq desiredMarks 10)
            (setq candidateMarks (/ viewportWidth_model desiredMarks))
            (setq candidate (if (> candidateTextStep candidateMarks)
                                candidateTextStep candidateMarks))
            (setq allowedList (list 25 50 100 200 500 1000 2000 5000 10000 20000 25000 50000 75000 100000 150000 200000 500000 1000000))
            (setq chosenMeasure (nearest-allowed candidate allowedList))
            (setq delta_l chosenMeasure)
          )
          (setq stepInput (getstring "\nSpecify step size for coordinates (or enter 'p' for predefined calculation): "))
        )
        (if (and (not useDefaults) (equal (strcase stepInput) "P"))
          (progn
            (setq viewportWidth_model (- reun_mb_x liun_mb_x))
            (setq viewportWidth_paper (- reun_af_x liun_af_x))
            (setq scaleFactor (/ viewportWidth_model viewportWidth_paper))
            (setq refText (rtos reun_mb_x 2 0))
            (setq n (strlen refText))
            (setq estimatedCharWidth_model (* 0.6 distfactor scaleFactor))
            (setq estimatedTextWidth_model (* n estimatedCharWidth_model))
            (setq candidateTextStep (* 3 estimatedTextWidth_model))
            (setq desiredMarks 10)
            (setq candidateMarks (/ viewportWidth_model desiredMarks))
            (setq candidate (if (> candidateTextStep candidateMarks)
                                candidateTextStep candidateMarks))
            (setq allowedList (list 25 50 100 200 500 1000 2000 5000 10000 20000 25000 50000 75000 100000 150000 200000 500000 1000000))
            (setq chosenMeasure (nearest-allowed candidate allowedList))
            (setq delta_l chosenMeasure)
          )
          (if (not useDefaults)
            (setq delta_l (atof stepInput))
          )
        )
        
        ;; Calcular longitud base de línea y definir finalLineLength según la opción de longitud.
        (setq baseLineLength (+ txtpos (* 5 distfactor)))
        (cond
          ((equal lineLengthOption "L") (setq finalLineLength baseLineLength))
          ((equal lineLengthOption "M") (setq finalLineLength (* 0.5 baseLineLength)))
          ((equal lineLengthOption "S") (setq finalLineLength (* 0.25 baseLineLength)))
          (T (setq finalLineLength baseLineLength))
        )
        
        ;; Calcular diferencias (delta) en Modelo y Paper para cada borde.
        (setq delta_model-bottom (- reun_mb_x liun_mb_x))
        (setq delta_paper-bottom (- reun_af_x liun_af_x))
        (setq delta_model-top    (- reob_mb_x liob_mb_x))
        (setq delta_paper-top    (- reob_af_x liob_af_x))
        (setq delta_model-left   (- liob_mb_y liun_mb_y))
        (setq delta_paper-left   (- liob_af_y liun_af_y))
        (setq delta_model-right  (- reob_mb_y reun_mb_y))
        (setq delta_paper-right  (- reob_af_y reun_af_y))
        
        ;; Dibujar marcas en cada borde usando draw-edge-labels.
        ;; Borde inferior (coordenadas X):
        (setq fixedY liun_af_y)
        (draw-edge-labels startx
                          (min liun_mb_x reun_mb_x)
                          (max liun_mb_x reun_mb_x)
                          delta_l
                          liun_mb_x delta_model-bottom
                          liun_af_x delta_paper-bottom
                          fixedY 'y
                          lineDir-bottom textDir-bottom
                          "M"
                          txtpos finalLineLength distfactor 0
                          lineCenter)
                          
        ;; Borde superior (coordenadas X):
        (setq fixedY liob_af_y)
        (draw-edge-labels startx
                          (min liob_mb_x reob_mb_x)
                          (max liob_mb_x reob_mb_x)
                          delta_l
                          liob_mb_x delta_model-top
                          liob_af_x delta_paper-top
                          fixedY 'y
                          lineDir-top textDir-top
                          "M"
                          txtpos finalLineLength distfactor 0
                          lineCenter)
                          
        ;; Borde izquierdo (coordenadas Y):
        (setq fixedX liun_af_x)
        (draw-edge-labels starty
                          (min liun_mb_y liob_mb_y)
                          (max liun_mb_y liob_mb_y)
                          delta_l
                          liun_mb_y delta_model-left
                          liun_af_y delta_paper-left
                          fixedX 'x
                          lineDir-left textDir-left
                          "M"
                          txtpos finalLineLength distfactor 90
                          lineCenter)
                          
        ;; Borde derecho (coordenadas Y):
        (setq fixedX reun_af_x)
        (draw-edge-labels starty
                          (min reun_mb_y reob_mb_y)
                          (max reun_mb_y reob_mb_y)
                          delta_l
                          reun_mb_y delta_model-right
                          reun_af_y delta_paper-right
                          fixedX 'x
                          lineDir-right textDir-right
                          "M"
                          txtpos finalLineLength distfactor 90
                          lineCenter)
      )
    )
    (setq x (1+ x))
  )
  
  ;; Restaurar parámetros del sistema.
  (setvar "blipmode" sblip)
  (setvar "cmdecho" scmde)
  (setvar "osmode" sosmode)
  (setvar "angbase" sangbase)
  (setvar "angdir" sangdir)
  (setvar "aunits" saunits)
  (setq *error* alterror)
  (prompt "\nCoordinates set.")
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fin del programa GridGeorefLay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ)
