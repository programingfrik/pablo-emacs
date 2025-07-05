
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

(defun probar-casos-prueba ()
  "Una función para hacer las pruebas de casos-prueba.txt."
  (interactive)
  ;; Crea un buffer temporal
  (let
      ((tbuffich (generate-new-buffer "*buffer-todo-caso*" 't))
       (tbuffer (generate-new-buffer "*buffer-caso*")))
    (save-excursion
      (save-restriction
        ;; Cambia el buffer actual a otro
        (set-buffer tbuffich)

        ;; Lee todo el fichero
        (insert-file-contents "casos-prueba.txt")

        ;; Para cada caso

        ;; Copia un caso de prueba a otro buffer temporal

        ;; Lee un caso de prueba

        ;; Pon el estado inicial del caso

        ;; Pon el cursor en su sitio

        ;; Corre la función en cuestión

        ;; Compara el estado del buffer con el resultado esperado.

        ;; Reportar si está igual o no el resultado de la prueba

        ;; Pasar al siguiente caso de prueba

        ;; Cuando se terminen todos los casos de prueba

        ;; Cerrar el buffer temporal que creamos
        ) )
    ;; (kill-buffer tbuffer)
    ;; (kill-buffer tbuffich)
    )
  ;; Fin
  )
