;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GridGeorefLay_v2_3.lsp
; Code developed from an example found on the web by Samuel Saez Lopez (IMGA S.L.P) for the creation of a georeferenced grid. 
; Contact: https://www.linkedin.com/in/samuel-saez-lopez-7603091a2/
; Código desarrollado a partir de ejemplo encontrado en la web por Samuel Saez Lopez (IMGA S.L.P) para la creación de una rejilla georreferenciada 
; Contacto: https://www.linkedin.com/in/samuel-saez-lopez-7603091a2/
; GridGeorefLay / GridGeorefLay
; Version complete modified for: / Versión completa modificada para:
; - Allow choosing if TEXT and LINES are drawn outside or inside the viewport / - Permitir elegir si el TEXTO y las LÍNEAS se dibujan fuera o dentro de la ventana,
; - Use different offsets for each border according to the choice / - Se usan desplazamientos distintos para cada borde según la elección,
; - Center the text relative to the line using justification "M" (middle center) / - Centrar el texto respecto a la línea utilizando la justificación "M" (middle center),
; - Option to choose three different line lengths: Large, Medium, Short / - Opción para elegir tres longitudes de línea distintas: Large, Medium, Short.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "\nStarting GridGeorefLay modified...")  ; Starting message / Mensaje de inicio

;--------------------------------------------------------------------
; Auxiliary function to prompt for a Y/N answer / Función auxiliar para solicitar respuesta Y/N
(defun prompt-yes-no (promptStr)
  (initget "Y N")  ; Set allowed keys / Establece las teclas permitidas
  (setq answer (getkword promptStr))
  (setq answer (strcase (substr answer 1 1)))
  (if (or (equal answer "Y") (equal answer "N"))
    answer
    (progn
      (prompt "\nInvalid answer. Please enter Y or N.")  ; Error message / Mensaje de error
      (prompt-yes-no promptStr)
    )
  )
)

;--------------------------------------------------------------------
; Error handling function / Función de manejo de errores
(defun my_error (msg)
  (print (strcat "\nError occurred: " msg))  ; Print error / Muestra el error
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
; Function #VPT_BOX: Calculates the 4 points of the viewport cut in Model space / 
; Función #VPT_BOX: Calcula los 4 puntos del recorte del viewport en Modelo.
(defun #VPT_BOX (view / v_eed v_psw v_psh v_msw v_msh v_msxp v_msyp v_ang 
                      v_xy1 v_xy2 v_xy3 v_xy4 v_dpx v_dpy v_eed_dp)
  (defun #ENT_GC (gc list) (cdr (assoc gc list)))  ; Get group code value / Obtener valor de código de grupo
  (defun #TRG_ROT (point angle offsetx offsety)  ; Rotate point with offset / Rota el punto con desplazamiento
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
      ; Extract data from EED: angle, view height and center / Extraer datos de EED: ángulo, altura de vista y centro
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
; Auxiliary function to draw lines and place labels on a border / 
; Función auxiliar para dibujar líneas y colocar etiquetas en un borde.
; Parameters: / Parámetros:
;   1. startVal   : Starting value in Model space (x or y) to iterate / Valor inicial en Modelo (x o y) para iterar.
;   2. minVal, 3. maxVal : Extents in Model space of the border to label / Extremos en Modelo del borde a etiquetar.
;   4. deltaVal   : Step size / Tamaño del paso.
;   5. modelStart : Starting value in Model space (for variable coordinate) to compute ratio / Valor inicial en Modelo (para la coordenada variable) para calcular la proporción.
;   6. modelDiff  : Total difference in Model space in that direction / Diferencia total en Modelo en esa dirección.
;   7. paperStart : Starting value in Paper space for the variable coordinate / Valor inicial en Paper para la coordenada variable.
;   8. paperDiff  : Total difference in Paper space in that direction / Diferencia total en Paper en esa dirección.
;   9. fixedVal   : Fixed value in Paper space (the coordinate that does not vary) / Valor fijo en Paper (la coordenada que no varía).
;  10. fixedAxis  : 'x or 'y (indicates which coordinate is fixed) / 'x o 'y (indica cuál es la coordenada fija).
;  11. lineDir    : Direction (in radians) to draw the auxiliary line / Dirección (en radianes) para dibujar la línea auxiliar.
;  12. textDir    : Direction (in radians) to locate the text / Dirección (en radianes) para ubicar el texto.
;  13. textAlign  : Text justification (not used since always centered) / Justificación del texto (no se usa ya que siempre se centra).
;  14. txtPos     : Distance to position the text from the line / Distancia para posicionar el texto desde la línea.
;  15. txtLineDist: Distance to draw the auxiliary line / Distancia para dibujar la línea auxiliar.
;  16. textHeight : Text height (normally based on TEXTSIZE) / Altura del texto (normalmente basada en TEXTSIZE).
;  17. textAngle  : Text angle (in degrees) / Ángulo del texto (en grados).
(defun draw-edge-labels (startVal minVal maxVal deltaVal modelStart modelDiff
                              paperStart paperDiff fixedVal fixedAxis
                              lineDir textDir textAlign txtPos txtLineDist textHeight textAngle)
  (setq val startVal)
  (while (<= val minVal)
    (setq val (+ val deltaVal))
  )
  (while (< val maxVal)
    (setq ratio (/ (- val modelStart) modelDiff))
    (setq paperCoord (+ paperStart (* ratio paperDiff)))
    (setq ctext (rtos val 2 0))
    (if (eq fixedAxis 'x)
      (setq ptCoord (list fixedVal paperCoord))  ; Fixed X, variable Y / X fija, Y variable.
      (setq ptCoord (list paperCoord fixedVal))  ; Fixed Y, variable X / Y fija, X variable.
    )
    (setq p1 ptCoord)
    (setq p2 (polar p1 lineDir txtLineDist))
    (setq pt (polar p1 textDir txtPos))
    (command "_.line" p1 p2 "")  ; Draw auxiliary line / Dibuja línea auxiliar.
    (command "_.text" "_j" "M" pt textHeight textAngle ctext)  ; Place centered text / Coloca texto centrado.
    (setq val (+ val deltaVal))
  )
)

;--------------------------------------------------------------------
; Main function: C:GridGeorefLay / Función principal: C:GridGeorefLay
(defun C:GridGeorefLay ( / alterror sblip scmde sosmode sangbase sangdir saunits
                                 anz al x axl
                                 zen_af zen_af_x zen_af_y zen_modw zen_mod_xw zen_mod_yw
                                 br_af h_af h_mod affakt alpha
                                 liun_af_x liob_af_x reun_af_x reob_af_x
                                 liun_af_y liob_af_y reun_af_y reob_af_y
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
                                 textOutside lineOutside
                                 txtOutAns lineOutAns
                                 lineDir-bottom textDir-bottom
                                 lineDir-top textDir-top
                                 lineDir-left textDir-left
                                 lineDir-right textDir-right
                                 lineLengthOption
                                 )
  (setq alterror *error*)
  (setq *error* my_error)
  (command "_undo" "_mark")
  
  ;; Save system parameters / Guardar parámetros del sistema
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
  
  ;; Select the viewport / Seleccionar el viewport
  (setq al (ssget '((0 . "VIEWPORT"))))
  (if al (setq anz (sslength al)) (print "\nNo viewport selected"))  ; English message / Mensaje en inglés
  
  ;; Ask if TEXT should be drawn outside the viewport / Preguntar si el TEXT se dibuja fuera de la ventana
  (setq txtOutAns (prompt-yes-no "\nDraw TEXT outside the viewport? [Y/N]: "))
  (setq textOutside (equal txtOutAns "Y"))
  
  ;; Ask if LINES should be drawn outside the viewport / Preguntar si las LÍNEAS se dibujan fuera de la ventana
  (setq lineOutAns (prompt-yes-no "\nDraw LINES outside the viewport? [Y/N]: "))
  (setq lineOutside (equal lineOutAns "Y"))
  
  ;; Define offsets for each border according to the choice / Definir direcciones (offsets) para cada borde según la elección:
  ;; For the bottom border / Para el borde inferior:
  (setq lineDir-bottom (if lineOutside (- (/ pi 2)) (/ pi 2)))  ; outside: -π/2, inside: +π/2 / fuera: -π/2, dentro: +π/2
  (setq textDir-bottom (if textOutside (- (/ pi 2)) (/ pi 2)))
  
  ;; For the top border / Para el borde superior:
  (setq lineDir-top (if lineOutside (/ pi 2) (- (/ pi 2))))       ; outside: +π/2, inside: -π/2 / fuera: +π/2, dentro: -π/2
  (setq textDir-top (if textOutside (/ pi 2) (- (/ pi 2))))
  
  ;; For the left border / Para el borde izquierdo:
  (setq lineDir-left (if lineOutside pi 0))                      ; outside: π, inside: 0 / fuera: π, dentro: 0
  (setq textDir-left (if textOutside pi 0))
  
  ;; For the right border / Para el borde derecho:
  (setq lineDir-right (if lineOutside 0 pi))                     ; outside: 0, inside: π / fuera: 0, dentro: π
  (setq textDir-right (if textOutside 0 pi))
  
  (setq x 0)
  (while (< x anz)
    (setq axl (entget (ssname al x)))
    (if (= "VIEWPORT" (cdr (assoc 0 axl)))
      (progn
        ;; Extract viewport data (Paper and Model) / Extraer datos del viewport (Paper y Modelo)
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
        ;; Calculate Paper space corners (assuming no rotation) / Calcular esquinas en Paper (suponiendo sin rotación)
        (setq liun_af_x (- zen_af_x (/ br_af 2.0))
              liob_af_x liun_af_x
              reun_af_x (+ zen_af_x (/ br_af 2.0))
              reob_af_x reun_af_x
              liun_af_y (- zen_af_y (/ h_af 2.0))
              reun_af_y liun_af_y
              liob_af_y (+ zen_af_y (/ h_af 2.0))
              reob_af_y liob_af_y
        )
        ;; Get the Model space points of the viewport via #VPT_BOX / Obtener los puntos de la ventana en Modelo mediante #VPT_BOX
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
        
        ;; Display min and max values and ask for initial values and step size / Mostrar valores min y max y solicitar valores iniciales y tamaño del paso
        (print "\nMinimum and maximum value for x:")  ; English / Inglés
        (princ (min liun_mb_x liob_mb_x reun_mb_x reob_mb_x))
        (princ "   ")
        (princ (max liun_mb_x liob_mb_x reun_mb_x reob_mb_x))
        (print "\nMinimum and maximum value for y:")  ; English / Inglés
        (princ (min liun_mb_y liob_mb_y reun_mb_y reob_mb_y))
        (princ "   ")
        (princ (max liun_mb_y liob_mb_y reun_mb_y reob_mb_y))
        (terpri)
        (initget 1)
        (setq startx (getreal "\nSpecify starting value for x coordinates: "))  ; English / Inglés
        (setq starty (getreal "\nSpecify starting value for y coordinates: "))
        (initget 3)
        (setq delta_l (getreal "\nSpecify step size for coordinates: "))  ; English / Inglés
        (setq txtpos  (getreal "\nSpecify text distance from frame: "))   ; English / Inglés
        
        ;; Calculate base line length and ask for line length option / Calcular longitud base de línea y solicitar opción de longitud de línea
        (setq baseLineLength (+ txtpos (* 5 distfactor)))
        (initget "LARGE MEDIUM SHORT")
        (setq lineLengthOption (strcase (getkword "\nSelect line length option [Large/Medium/Short]: ")))
        (cond
          ((equal lineLengthOption "LARGE") (setq finalLineLength baseLineLength))
          ((equal lineLengthOption "MEDIUM") (setq finalLineLength (* 0.5 baseLineLength)))
          ((equal lineLengthOption "SHORT")  (setq finalLineLength (* 0.25 baseLineLength)))
          (T (setq finalLineLength baseLineLength))
        )
        
        ;; Calculate differences (delta) in Model and Paper space for each border / Calcular las diferencias (delta) en Modelo y Paper para cada borde:
        (setq delta_model-bottom (- reun_mb_x liun_mb_x))
        (setq delta_paper-bottom (- reun_af_x liun_af_x))
        (setq delta_model-top    (- reob_mb_x liob_mb_x))
        (setq delta_paper-top    (- reob_af_x liob_af_x))
        (setq delta_model-left   (- liob_mb_y liun_mb_y))
        (setq delta_paper-left   (- liob_af_y liun_af_y))
        (setq delta_model-right  (- reob_mb_y reun_mb_y))
        (setq delta_paper-right  (- reob_af_y reun_af_y))
        
        ;; Bottom border (X coordinates) / Borde inferior (coordenadas X):
        (setq fixedY liun_af_y)
        (draw-edge-labels startx
                          (min liun_mb_x reun_mb_x)
                          (max liun_mb_x reun_mb_x)
                          delta_l
                          liun_mb_x delta_model-bottom
                          liun_af_x delta_paper-bottom
                          fixedY 'y
                          lineDir-bottom textDir-bottom
                          "M"  ; Always centered / Siempre centrado
                          txtpos finalLineLength distfactor 0)
                          
        ;; Top border (X coordinates) / Borde superior (coordenadas X):
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
                          txtpos finalLineLength distfactor 0)
                          
        ;; Left border (Y coordinates) / Borde izquierdo (coordenadas Y):
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
                          txtpos finalLineLength distfactor 90)
                          
        ;; Right border (Y coordinates) / Borde derecho (coordenadas Y):
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
                          txtpos finalLineLength distfactor 90)
      )
    )
    (setq x (1+ x))
  )
  
  ;; Restore system parameters / Restaurar parámetros del sistema
  (setvar "blipmode" sblip)
  (setvar "cmdecho" scmde)
  (setvar "osmode" sosmode)
  (setvar "angbase" sangbase)
  (setvar "angdir" sangdir)
  (setvar "aunits" saunits)
  (setq *error* alterror)
  (prompt "\nCoordinates set.")  ; English / Inglés
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End of GridGeorefLay_Modificado program / Fin del programa GridGeorefLay_Modificado
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(princ)
