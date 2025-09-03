
;; Para hacer pruebas automatizadas de las funciones de pablo-emacs.el
;; Quiero simular algunos escenarios del fichero casos-prueba.txt
(defun probar-todo ()
  "Una función que ejecuta todas las pruebas."

  ;; Recorrer todas las pruebas

  ;; Las pruebas son funciones en este fichero "pruebas-pablo-emacs.el" cuyo nombre comienza con "prueba-"
  ;; obarray
  ;; find-lisp-object-file-name

  ;; Un conjunto de las pruebas vienen del fichero casos pruebas.

  ;; Lee el fichero de las prueba
  ;; (insert-file-contents file &optional visit beg end replace)

  ;; Parte el fichero en pruebas individuales.

  ;; Crea el escenario

  ;; Ejecuta la prueba

  ;; Si el resultado es el esperado la prueba pasó.

  ;; Sino la prueba falló.

  ;; Hacer la notificación y pasar a la siguiente prueba.
  )

(defun poner-en-tbuffer(bdest borig ini fin)
  (set-buffer bdest)
  (erase-buffer)
  (insert-buffer-substring borig ini fin) )

(defun comparar-resultado-prueba (tbresultado tbprueba)
  (let ((txigual "Diff finished (no differences).")
        (buffer-actual (current-buffer))
        dbuffer resultado)
    (diff-no-select tbresultado tbprueba nil 't nil)
    (setq dbuffer (get-buffer "*Diff*"))
    (set-buffer dbuffer)
    (setq resultado (re-search-forward txigual nil 'end))
    ;; Reportar si está igual o no el resultado de la prueba
    (if resultado
        (message "La prueba fue exitosa!")
      (progn
        (message "La prueba falló!")
        (goto-char (point-min))
        (forward-line)
        (message (buffer-substring-no-properties
                  (point) (point-max))) ))
    (set-buffer buffer-actual)
    (kill-buffer dbuffer)
    resultado ))

(defun probar-caso-avanzar (expdiv posfin tbfich tbprueba tbresultado)
  (let ((expdiv2 "-\\{4\\}")
        (cursor "<cursor>")
        posini posbar titulo comando subini subfin temptit exito)

    (setq posini posfin)

    ;; Lee el título y ponlo en un mensaje para señalar la prueba actual.
    (setq titulo (buffer-substring (line-beginning-position)
                                   (line-end-position)))

    (setq temptit (format "Prueba: %s" titulo))
    (setq temptit (format-spec
                   "%a\n%t\n%a"
                   (list (cons ?a (make-string
                                   (length temptit)
                                   (string-to-char "*")))
                         (cons ?t temptit) )))
    (message temptit)

    ;; Lee el comando que hay que ejecutar para la prueba.
    (forward-line)
    (setq comando (read (buffer-substring (line-beginning-position)
                                          (line-end-position))))
    (message "Comando: %s" comando)

    (if (re-search-forward expdiv nil 'end)
        (setq posbar (match-beginning 0)
              posfin (match-end 0))
      (setq posbar (point-max)
            posfin (point-max)))

    ;; Copia la prueba a tbprueba
    (goto-char posini)
    (re-search-forward (concat expdiv2 "prueba" expdiv2 "\n") nil 'end)
    (setq subini (match-end 0))

    (re-search-forward (concat expdiv2 "resultado" expdiv2 "\n") nil 'end)
    (setq subfin (match-beginning 0))

    (poner-en-tbuffer tbprueba tbfich subini subfin)

    ;; Copia el resultado esperado a tbresultado
    (set-buffer tbfich)
    (setq subini (match-end 0)
          subfin posbar)

    (poner-en-tbuffer tbresultado tbfich subini subfin)

    ;; Pon el cursor en su sitio en tbprueba, en donde está el texto "<cursor>"
    (set-buffer tbprueba)
    (goto-char (point-min))
    (re-search-forward cursor nil 'end)

    ;; Quita el texto "<cursor>"
    (replace-match "")

    ;; Corre la función en cuestión
    (eval comando)

    ;; Vuelve a poner el texto "<cursor>" donde quedó el cursor.
    (insert cursor)

    ;; Compara el contenido tbresultado con tbprueba, deberían ser
    ;; iguales, sino lo son falló la prueba.
    (setq exito (comparar-resultado-prueba tbresultado tbprueba))

    ;; Pasar al siguiente caso de prueba
    (set-buffer tbfich)
    (goto-char posfin)
    (list posfin exito) ))

(defun probar-casos-prueba (&optional n)
  "Una función para hacer las pruebas de casos-prueba.txt."
  (interactive "P")
  (let
      ((tbfich (generate-new-buffer "*tbfich*" 't))
       (tbprueba (generate-new-buffer "*tbprueba*"))
       (tbresultado (generate-new-buffer "*tbresultado*"))
       (cont 0) (cexito 0) (expdiv "=\\{70\\}\n")
       pos rescaso final)
    (save-excursion
      (save-restriction

        ;; Necesitamos diff para comparar lo esperado con el
        ;; resultado.
        (require 'diff)

        ;; Cambia al buffer del fichero con los casos de prueba.
        (set-buffer tbfich)

        ;; Lee todo el fichero
        (insert-file-contents "casos-prueba.txt")
        (goto-char (point-min))
        (setq pos (point-min)
              final (point-max))

        ;; Si n tiene un número solo ejecuta la prueba n, sino ejecuta
        ;; cada caso.
        (when (not (equal n nil))
          (message "Solo vamos a ejecutar el caso %d" n)

          ;; Busca el punto en el que inicia este caso
          (setq pos (re-search-forward expdiv nil 't n))

          ;; Busca el punto en que termina este caso
          (if (re-search-forward expdiv nil 'end)
              (setq final (match-beginning 0))
            (setq final (point)))
          (goto-char pos) )

        ;; Mientras no sea el final
        (while (< pos final)

          (setq rescaso (probar-caso-avanzar expdiv pos tbfich
                                             tbprueba tbresultado))
          (when (car (cdr rescaso))
            (setq cexito (1+ cexito)))

          (setq pos (car rescaso)
                cont (1+ cont)) )

        ) )
    ;; Cuando se terminen todos los casos de prueba hay que hacer un
    ;; sumario.
    (message (concat "Pruebas realizadas %d\n"
                     "Pruebas exitosas %d\n"
                     "Pruebas fallidas %d")
             cont cexito (- cont cexito))

    ;; Antes de cerrar el let hay que eliminar los buffers porque se
    ;; van a perder las variables
    (kill-buffer tbfich)
    (kill-buffer tbprueba)
    (kill-buffer tbresultado) )
  ;; Fin
  )
