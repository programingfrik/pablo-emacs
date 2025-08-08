
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

(defun probar-caso-avanzar (posfin tbfich tbprueba tbresultado)
  (let ((expdiv "=\\{70\\}\n")
        (expdiv2 "-\\{4\\}")
        (cursor "<cursor>")
        posini posbar titulo comando subini subfin)

    (setq posini posfin)

    ;; Lee el título y ponlo en un mensaje para señalar la prueba actual.
    (setq titulo (buffer-substring (line-beginning-position)
                                   (line-end-position)))
    (message "Prueba: %s" titulo)

    ;; Lee el comando que hay que ejecutar para la prueba.
    (forward-line)
    (setq comando (read (buffer-substring (line-beginning-position)
                                          (line-end-position))))
    (message "Comando: %s" comando)

    (re-search-forward expdiv nil 'end)
    (setq posbar (match-beginning 0)
          posfin (match-end 0))

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

    ;; Compara el contenido tbprueba con tbresultado, deberían ser iguales, sino lo son falló la prueba.
    ;; (diff-buffers tbprueba tbresultado)

    ;; Reportar si está igual o no el resultado de la prueba

    ;; Pasar al siguiente caso de prueba
    (set-buffer tbfich)
    (goto-char posfin) ))

(defun probar-casos-prueba (&optional n)
  "Una función para hacer las pruebas de casos-prueba.txt."
  (interactive)
  (let
      ((tbfich (generate-new-buffer "*tbfich*" 't))
       (tbprueba (generate-new-buffer "*tbprueba*"))
       (tbresultado (generate-new-buffer "*tbresultado*"))
       (cont 0)
       pos)
    (save-excursion
      (save-restriction
        ;; Cambia al buffer del fichero con los casos de prueba.
        (set-buffer tbfich)

        ;; Lee todo el fichero
        (insert-file-contents "casos-prueba.txt")
        (goto-char (point-min))
        (setq pos (point-min))

        ;; Si n tiene un número solo ejecuta la prueba n, sino ejecuta
        ;; cada caso.

        ;; Mientras no sea el final del fichero
        (while (< pos (point-max))

          (setq pos (probar-caso-avanzar pos tbfich tbprueba
                                         tbresultado))

          )

        ;; Cuando se terminen todos los casos de prueba

        ) )
    ;; Antes de cerrar el let hay que eliminar los buffers porque se van a perder las variables
    (kill-buffer tbfich)
    (kill-buffer tbprueba)
    (kill-buffer tbresultado)
    )
  ;; Fin
  )
