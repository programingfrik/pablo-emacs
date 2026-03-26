
(defun escribir-fecha-hora ()
  "Una función para escribir la fecha y hora acutal en el punto actual.

La función escribe la fecha y la hora en formato aaaa-mm-dd hh:MM PP. Donde \"a\" son dígitos del año, \"m\" son dígitos del mes, \"d\" son dígitos del día, \"h\" son dígitos de la hora, \"M\" son dígitos de los minutos y \"P\" son dos letras que forma AM o PM según el caso. La hora se escribe en formato de 12 horas."
  (interactive)
  (let* ((m (decode-time (current-time) nil))
         (h24 (nth 2 m))
         (h12 (if (> h24 12) (- h24 12) h24))
         (aop (if (>= h24 12) "P" "A")) )
    (insert (format "%04d-%02d-%02d %02d:%02d %sM"
                    (nth 5 m) (nth 4 m) (nth 3 m)
                    h12 (nth 1 m) aop)) ))


(defun deactivate-proxy (&optional put-back)
  "\"Hides\" the values of http_proxy and https_proxy environment variables.
To hide the values of those variables this function puts them on temporal variables temp_http_proxy and temp_https_proxy. If the optional parameter, put-back, is given a non-nil value the values of the temp variables are put back in the http_proxy environment values. I created this function because curl calls to omnisharp block when they pass through cntlm proxy."
  (interactive "P")
  (if put-back
      (progn
        (when (getenv "temp_http_proxy")
          (setenv "http_proxy" (getenv "temp_http_proxy") nil)
          (setenv "temp_http_proxy" nil nil)
          (setenv "https_proxy" (getenv "temp_https_proxy") nil)
          (setenv "temp_https_proxy" nil nil) )
        (message "Proxy activado!") )
    (progn
      (when (getenv "http_proxy")
        (setenv "temp_http_proxy" (getenv "http_proxy") nil)
        (setenv "http_proxy" nil nil)
        (setenv "temp_https_proxy" (getenv "https_proxy") nil)
        (setenv "https_proxy" nil nil) )
      (message "Proxy desactivado!") )
    )
  )

(defun buscar-valor (nombre variable texto)
  "Extrae el valor de la variable por su nombre dentro de texto.
Si encuentra la variable en texto pone su valor en la variable, si no pone una cadena vacia.
"
  (let ((inicio-val 0)
        (fin-val nil))
    (if (setq inicio-val (string-match-p (concat " *" nombre " *=") texto))
        (progn
          (setq inicio-val (string-match-p "=" texto inicio-val))
          (setq inicio-val (string-match-p "[[:alnum:]]" texto inicio-val))
          (or (setq fin-val (string-match-p " *[;\"]" texto inicio-val))
              (setq fin-val (string-match-p " *$" texto inicio-val)))
          (set variable (substring texto inicio-val fin-val)) )
      (set variable "") )
    )
  )

; Una función para ayudar a recoger los valores de las diferentes variables del texto guiandose de sus alias.
(defun buscar-valores (aliases texto)
  "Recoge todos los valores del texto y los pone en las variables que corresponden guiandoce de aliases.
"
  (let ((nombres))
    (dolist (mapa aliases)
      (setq nombres (cdr mapa))
      (while (and nombres (string= (buscar-valor (car nombres) (car mapa) texto) ""))
        (setq nombres (cdr nombres)) ) ) ) )


;; "\[\([^]]+\)\]  \( \[[^]]*\]  \|  ( \([^) ]+\)\( *\"[^"]+\" *\)?) \)?"

;; "\[\([^]]+\)\]"  [ejemplo]   Reference link
;; "[][]"   [otro ejemplo][]    Reference link
;; "[][]"   [ejemplo diferente][EjemploDiferente]
;; "[]()"   [google](http://google.com/)
;; "[]()"   [prueba](http://otro.url.com/ "un titulo")
;; "[]()"   [probando](relative/path/to/index.html "otro título")
;; "[]()"   [sigo probando](fichero.html)
;; "[]()"   [probando referencias](@ref UnaReferencia)

;; "[ejemplo]:       http://yajoo.com"
;; "[otro ejemplo]:  http://blender3d.org  "la página de blender""
;; "[otro ejemplo]:  relative/path/to/index.html "otro título""
;; "[otro ejemplo]:  fichero.html"
;; "[otro ejemplo]:  @ref UnaReferencia"

;; (defun ir-md-link (texto)
;;   "Sigue un link de mark down al estilo de doxygen.
;; Si se trata de una referencia a otra parte de este mismo, o de otro documento mueve el cursor a ese punto. Si se trata de un link de internet abre el link en el browser"
;;   (
;;
;;   )

;; Modificando de la función "open-file-at cursor" que se encuentra en:
;; http://ergoemacs.org/emacs/emacs_open_file_path_fast.html
;; Cree esta función ...
;; TODO: También se podría hacer que cuando a esta función se ejecute estando sobre un texto de la forma AppSettings["configuracion"] que emacs habra el fichero "web.config" correspondiente y ponga point en la configuración.
(defun abrir-cosa ()
  "Abre la \"cosa\" que esté bajo el cursor.
Si se trata de un connection string se conecta a la base de datos.
Si se trata de un URL lo abre en el browser.
Si se trata de un fichero o un directorio lo abre en el propio Emacs.
"
  (interactive)
  (save-excursion
    (let ( (texto (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (thing-at-point 'filename)) )
           (aliases '((sql-server "Data Source" "SERVER")
                      (sql-database "Initial Catalog" "DATABASE")
                      (sql-user "User ID" "UID" "USER")
                      (sql-password "Password" "PWD"))) )
      (buscar-valores aliases texto)
      ; si la cadena contiene "DRIVER={SQL Server"
      (cond ((string-match-p "DRIVER={MySQL ODBC" texto)
             (sql-mysql 't))
            ((string-match-p "DRIVER={SQL Server" texto)
             (sql-ms 't))
            ; si no, si la cadena contiene "Provider=SQLOLEDB"
            ((string-match-p "Provider=SQLOLEDB" texto)
             (sql-ms 't))
            ; si no, si la cadena contiene "Provider=OraOLEDB.Oracle"
            ((string-match-p "Provider=OraOLEDB.Oracle" texto)
             (progn
               (setq sql-database sql-server)
               (sql-oracle 't)))
            ; si no, si la cadena contiene "User ID" y "Password" y "Data Source"
            ((and (string-match-p "User ID" texto)
                  (string-match-p "\\(Password\\|PWD\\)" texto)
                  (string-match-p "Data Source" texto))
             (if (string-match-p "Initial Catalog" texto)
                 ; si la cadena contiene "Initial Catalog" es ms-sql-server
                 (sql-ms 't)
               ; si no es oracle
               (progn
                 (setq sql-database sql-server)
                 (sql-oracle 't))) )
            ; si no, si la cadena empieza con "http://"
            ((string-match-p "\\`https?://" texto)
             (browse-url texto))
            ; si no, si la cadena empieza coincide con [texto](#ancla)
            ; es un link de mark down en este mismo texto.

            ; si no, entonces se trata de un fichero.
            ; not starting 'http://"
            ('t
             (progn
               (if (and (equal system-type 'cygwin)
                        (or (string-match-p "^[[:alpha:]]:\\\\" texto)
                            (string-match-p "^\\(\\\\\\\\\\)?[^\\\\]+\\(\\\\.*\\)+" texto) ))
                   (progn
                     ;; si estamos en cygwin y se trata de una ruta
                     ;; estilo windows hay que transformarla a algo
                     ;; que entienda cygwin
                     (setq texto (replace-regexp-in-string
                                  "^\\([[:alpha:]]\\):\\\\\\(.*\\)$"
                                  "/cygdrive/\\1/\\2" texto ))
                     (setq texto (replace-regexp-in-string
                                  "\\\\" "/" texto ))))
               (cond ((file-exists-p texto)
                      (find-file texto))
                     ((file-exists-p (concat texto ".el"))
                      (find-file (concat texto ".el")))
                     ((y-or-n-p (format "El fichero \"%s\" no existe. ¿Lo creo?" texto))
                      (find-file texto) )))
             )
            )
      )
    )
  )



(defun pablo-select-text (text)
  "Select TEXT, a string, according to the window system.

On X, if `x-select-enable-clipboard' is non-nil, copy TEXT to the
clipboard.  If `x-select-enable-primary' is non-nil, put TEXT in
the primary selection.

On MS-Windows, make TEXT the current selection.  If
`x-select-enable-clipboard' is non-nil, copy the text to the
clipboard as well.

On Nextstep, put TEXT in the pasteboard (`x-select-enable-clipboard'
is not used).

Esta versión fue modificada por Pablo Mercader Alcántara para que
 permita copiar texto al clipboard desde la consola como se
permitia en la versión 23 de Emacs."
  (cond ((or (eq (framep (selected-frame)) 'w32)
             (and (eq (framep (selected-frame)) 't) (equal system-type 'windows-nt)))
     (if x-select-enable-clipboard
         (w32-set-clipboard-data text))
     (setq x-last-selected-text text))
    ((featurep 'ns)
     ;; Don't send the pasteboard too much text.
     ;; It becomes slow, and if really big it causes errors.
     (ns-set-pasteboard text)
     (setq ns-last-selected-text text))
    (t
     ;; With multi-tty, this function may be called from a tty frame.
     (when (eq (framep (selected-frame)) 'x)
       (when x-select-enable-primary
         (x-set-selection 'PRIMARY text)
         (setq x-last-selected-text-primary text))
       (when x-select-enable-clipboard
         ;; When cutting, the selection is cleared and PRIMARY set to
         ;; the empty string.  Prevent that, PRIMARY should not be reset
         ;; by cut (Bug#16382).
         (setq saved-region-selection text)
         (x-set-selection 'CLIPBOARD text)
         (setq x-last-selected-text-clipboard text))))))
(when (and (eq emacs-major-version 24) (not (display-graphic-p)))
  (setq interprogram-cut-function 'pablo-select-text))

(defun abrir-codigo-plantilla ()
  "Si el buffer tiene el código de un formulario aspx, ascx, asax abre la plantilla, y viceversa.
Creada por Pablo Mercader Alcántara"
  (interactive)
  (save-excursion
    ;; Obtengo el nombre del fichero que contiene el buffer actualmente
    (let ((fichero (buffer-file-name))
          (pos nil))

      ;; Si el nombre está en la forma nombre.ext1.ext2 donde ext1 es
      ;; ascx o aspx trato de abrir el fichero nombre.ext1
      (cond ((string-match
              "^.+/[[:alnum:]()_-]+\\.\\(aspx\\|ascx\\|asax\\)\\.[[:alnum:]]+$"
              fichero)
             (progn
               (setq pos (match-end 1))
               (setq fichero (substring fichero 0 pos))
               (find-file fichero)))
            ;; Si no si el fichero está en la forma nombre.ext1
            ;; trato de abrir un fichero nombre.ext1.algo
            ;; FIXME: Como está actualmente solo funciona para csharp
            ((string-match-p
              "^.+/[[:alnum:]()_-]+\\.\\(aspx\\|ascx\\|asax\\)$"
              fichero)
             (progn
               (find-file (concat fichero ".cs"))))
            ;; Si no encuentro nada hay que poner un mensajito.
            ('t
             (message "No se trata ni de una plantilla ni de un code behind."))
            )
      ))
  )

(defun push-point-tag-marker ()
  "Pushes point to the \\[find-tag-marker-ring].
The purpose of this is helping the programmer remember the locations of function wich have been found by hand and not through the natural use of \\[find-tag]. When you push a mark using this function you can get back to the this point using \\[pop-tag-mark]."
  (interactive)
  (progn
    (message "Point annotated!")
    (ring-insert find-tag-marker-ring (point-marker))
    )
  )

;; Esta función que está aquí abajo no fuera necesaria si yo pudiera
;; de alguna forma configurar "find-tag" para que cuando busca
;; definiciones que empiece buscando primero en los tags del propio
;; fichero al que señala el buffer actual y si no aparece la
;; definición que busque en ficheros con el mismo nombre pero con
;; diferentes extensiones, luego si la definición todavía no aparece
;; entonces que continúe buscando.
(defun find-function-here ()
  "Finds the definition for a function in the current buffer
This function takes point to the definition of the CSharp function in the current buffer whos name is in front of point or wich point is over."
  (interactive)
  (let ((fname nil)
        (startp nil)
        (endp nil)
        (searchexp nil)
        (found nil))
    (progn
      ;; Collect the function name (fname)
      (right-word)
      (setq endp (point-marker))
      (left-word)
      (setq startp (point-marker))
      (setq fname (buffer-substring-no-properties startp endp))

      ;; Save the the current position in the find-tag-marker-ring
      (push-point-tag-marker)

      ;; Build a regexp to search for the definition.
      (setq searchexp (concat fname "[ \n\r\t]*([^)]*)[ \n\r\t]*{"))

      ;; Take me to the place in this buffer where the regexp matches.
      (goto-char (point-max))
      (setq found (re-search-backward searchexp nil t))

      ;; When there is no match notify the user and return to the begining.
      (when (not found)
        (message (concat "Couldn't find a definition for " fname))
        (pop-tag-mark) ))
    )
  )

(defun join-string-list (string-list separator)
  "Concats each of the strings in string-list putting separator between them."
  (let ((result nil))
    (setq result (car string-list))
    (setq string-list (cdr string-list))
    (dolist (fichero string-list result)
      (setq result (concat result separator fichero)) )
    )
  )

(defun parent-directory (dir)
  "Returns the parent directory of dir"
  (let ((separator (cond ((string-match-p "/" dir)
                          "/")
                         ((string-match-p "\\\\" dir)
                          "\\") ))
        (adress nil))
    (setq adress (cdr (reverse (split-string dir (concat "\\" separator) t))))
    (setq adress (reverse adress))
    (if (string-prefix-p separator dir)
        (concat separator (join-string-list adress separator))
      (join-string-list adress separator) )
    )
  )

(defun find-nearest-file (suffix dir depth)
  "Finds the nearest file in dir herarchy that has the given suffix"
  (let ((cont 0)
        posibles
        file
        (dir-actual dir)
        project)
    (while (and (not project) (> (- depth cont) 0))
      (setq posibles (directory-files dir-actual t (concat "^.+\\" suffix "$")))
      (while (and (not project) posibles)
        (setq file (car posibles))
        (when (string-suffix-p suffix file t)
          (setq project file))
        (setq posibles (cdr posibles))
        )
      (setq cont (1+ cont))
      (setq dir-actual (parent-directory dir-actual))
      )
    project
    ) )

(defun get-vsproject-here (this-file only-sln depth)
  "Gets the main vsproject file for a given source file."
  (let ((dir-actual (parent-directory this-file))
        project )
    (setq project (find-nearest-file ".sln" dir-actual depth))
    (if (and (not project) (not only-sln))
        (setq project (find-nearest-file ".csproj" dir-actual depth)) )
    project
    )
  )

(defvar pablo-construirvs-ddepth
  4
  "La profundidad máxima a la que va a tratar de llegar la función find-nearest-file cuando este buscando el fichero del proyecto.")

(defvar pablo-construirvs-path
  "/cygdrive/e/pablo/comun/codigo/lang/bat/construirvs2019.bat"
  "La ruta del bat que pone el ambiente y ejecuta a msbuild en ese ambiente.")

(defun compile-vsproject-here ()
  "Compiles the project of the current source.
This function searches for the .net project nearest to the
current path and puts it on a command to build that project."
  (interactive)
  (progn
    ;; If compile-command is empty or has its standard value, try to create a compile command for the vs-project.
    (if (or (equal compile-command "")
            (equal compile-command (eval (car (get 'compile-command 'standard-value)))))
        (let ((project (get-vsproject-here buffer-file-name 'nil pablo-construirvs-ddepth)) )
          (when (not project)
            (setq project "(no project.csproj or project.sln found)"))
          (setq compile-command (concat pablo-construirvs-path " /p:Configuration=Debug `cygpath -wa \"" project "\"`"))
          ))
    ;; any way, call the compile command.
    (call-interactively 'compile)
    )
  )

(defun start-omnisharp-here (vs-project)
  "Starts omnisharp for the project of the current source.
This function, deactivates the \"http_proxy\" variable if it is set, stops the omnisharp server if it is already running, searches for the .net project closest to the current path and starts omnisharp for that project."
  (interactive
   (list
    (progn
      (unless (and (boundp 'omnisharp-vs-project) omnisharp-vs-project)
        (setq omnisharp-vs-project (get-vsproject-here buffer-file-name 't pablo-construirvs-ddepth))
        )
      (setq omnisharp-vs-project
            (read-file-name "Start OmniSharpServer.exe for solution: "
                            nil omnisharp-vs-project t nil 'file-readable-p))
      )
    ) )
  (if (and vs-project (not (string-equal vs-project "")) (string-suffix-p ".sln" vs-project t))
      (progn
        (message (format "Starting Omnisharp.exe for solution %s." vs-project))
        ;; (deactivate-proxy)
        ;; (omnisharp-stop-server)
        (omnisharp-start-omnisharp-server vs-project)
        )
    (message (format "No se puede iniciar Omnisharp para %s." vs-project))
    )
  )

(defun repair-sequence-region (start end)
  "This function repairs the numbers of a sequence in a list of bullets.
This function recognizes the numbers in a sequence of bullets in a list and modify them so they can be in order. The numbers must be the first characters in the line. This version does not support sub levels, but thats something that could be done in the future.

The function should take a list like this:
3223. una
3. tres
7. maria
8. pedro 88.
#. pachanga
323. pollo

... and turn it into this:
3223. una
3224. tres
3225. maria
3226. pedro 88.
3227. pachanga
3228. pollo
"
  (interactive "r")
  (save-excursion
    (let ((numexp "^[ \t]*\\([[:digit:]]+\\|#\\)")
          (counter 0)
          (delta-end 0))
      ;; Find the number of the first line in the region that has a number.
      (goto-char start)
      ;; If a number is found
      (if (re-search-forward numexp end 't)
          (progn
            ;; Use that number to initialize the counter
            (setq counter (string-to-number
                           (match-string-no-properties 0)))
            (while (re-search-forward numexp end 't)
              ;; As long as you keep finding numbers, replace them with the counter value in turn.
              (setq delta-end (- (length (number-to-string counter))
                                 (length (match-string-no-properties 0)) ))
              (replace-match (number-to-string
                              (setq counter (1+ counter)))
                             't 't)
              ;; Recalculate the end of the region
              (setq end (+ end delta-end) )
              )

            )
        ;; Else if the first number is not found just show a message.
        (message "Numbers were not found!")
        )

      )
    )
  )

(defun set-column-info (column-list pos type)
  "Sets the info of a column in a list of items"
  (let ((value (nth pos column-list)))
    (if value
        (if (or (and (equal value 'number) (equal type 'string))
                (equal value 'unknown))
            (setcar (nthcdr pos column-list) type))
      (progn
        (while (< (length column-list) (1+ pos))
          (setq column-list (reverse (cons nil (reverse column-list)))) )
        (setcar (nthcdr pos column-list) type)
        )
      )
    column-list
    )
  )

(defun convert-csv-to-select (start end)
  "Transforms a region containing data in form of a csv to a sql select with unions

This function should turn this:
uno, 1, 1, \"dos\", tres, null
cacho, 2, 2, maco, 'caco', taco
juan, 4cuatro, 3, \"miguel's\", pollo

... into this:
select 'uno', '1', 1, 'dos', 'tres', null union all
select 'cacho', '2', 2, 'maco', 'caco', 'taco' union all
select 'juan', '4cuatro', 3, 'miguel''s', 'pollo', ''

Right now it doesn't sopport comma characters as values embeded in quotes.
"
  (interactive "r")
  (save-excursion
    (let ((lineexp "^[-.'\"[:alnum:]]+\\(, *[-.'\"[:alnum:]]+\\)*")
          (line "")
          (numexp "^[-+.[:digit:]]+$")
          (row-count 0)
          (column-count 0)
          (value "")
          (columns ())
          (start-line 0)
          (end-line 0)
          (values ()) )
      ;; Run through the rows checking the number of columns and the types of each column.
      ;; The number of columns is dictated by the row that has the greatest number of columns.
      ;; If a column has only numbers then it is numbers only column.
      ;; If only one row in the column has a non numeric character, then that column is treated as text.
      ;; If the value has quotes it has to be taken in account

      ;; Find the first line in the region
      (goto-char start)
      (while (re-search-forward lineexp end 't)
        (setq line (match-string-no-properties 0))
        (setq column-count 0)
        ;; Find the number of columns in this row
        ;; For each column check if its only numbers or if it has alphanumeric characters.
        (dolist (value (split-string line ","))
          ;; If the value has spaces before or after, erase them.
          (setq value (replace-regexp-in-string "\\(^ +\\| +$\\)" "" value))
          ;; If the value has quotes or double quotes, throw them away
          (if (or (and (string-prefix-p "'" value)
                       (string-suffix-p "'" value))
                  (and (string-prefix-p "\"" value)
                       (string-suffix-p "\"" value)))
              (setq value (substring value 1 (1- (length value)))) )
          ;; If the value is "NULL" then we have nothing else to check
          (if (or (string-match-p "NULL" value)
                  (string-match-p numexp value))
              (setq columns (set-column-info columns column-count 'number))
            (setq columns (set-column-info columns column-count 'string)) )
          (setq column-count (1+ column-count)) )
        (setq row-count (1+ row-count))
      )
      ;; Replace each row with a select taking into account the number of fields it should have
      (goto-char start)
      (while (re-search-forward lineexp end 't)
        (setq line (match-string-no-properties 0))
        (setq start-line (match-beginning 0))
        (setq end-line (match-end 0))
        (setq values (split-string line ","))
        (setq line "select ")
        (dotimes (column-count (length columns))
          (setq value (or (nth column-count values) ""))
          ;; Deleting spaces again
          (setq value (replace-regexp-in-string "\\(^ +\\| +$\\)" "" value))
          ;; Deleteng quotes again
          (if (or (and (string-prefix-p "'" value)
                       (string-suffix-p "'" value) )
                  (and (string-prefix-p "\"" value)
                       (string-suffix-p "\"" value) ))
              (setq value (substring value 1 (1- (length value)))) )
          ;; Escape simple quote and put quotes to strings
          (if (and (not (string-match-p "NULL" value))
                   (equal (nth column-count columns) 'string))
              (progn
                (setq value (replace-regexp-in-string "'" "''" value))
                (setq value (concat "'" value "'")) ))
          (setq line (concat line value ", ")) )
        (setq line (substring line 0 (- (length line) 2)))
        (if (not (= row-count 1))
            (setq line (concat line " union all")) )
        (delete-region start-line end-line)
        (goto-char start-line)
        (insert line)
        (setq end (+ end (- (length line)
                            (- end-line start-line)) ))
        (setq row-count (1- row-count))
        )
      )
    )
  )

(defun write-note-separator ()
  "Function that I use to write separation between the entries of notes of some kind of diary on a text file."
  (interactive)
  (let ((time-now (decode-time)))
    (insert (format "\n%02d-%02d-%04d ----------------------------------------------------\n"
                    (nth 3 time-now) (nth 4 time-now) (nth 5 time-now) ))
    )
  )

(defun windowsACygwin (winpath)
  "This function turns a normal windows path into a cygwin one."
  (let* ((cygpath (replace-regexp-in-string
                   "\\([A-Za-z]\\):/" "/cygdrive/\\1/"
                   (replace-regexp-in-string
                    "[A-Za-z]:/" 'downcase
                    (replace-regexp-in-string "\\\\" "/" winpath t nil)
                    t nil)
                   t nil)))
    cygpath)
  )

(defun csharp--compilation-error-file-resolve()
  "This is a replacement of the function of the same name at \"csharp-compilation.el\". I made this to make it work with cygwin."
  (cons (match-string 1) (file-name-directory (windowsACygwin (match-string 4)))))



(defun mssql-listar-apariciones(sujeto inicio fin)
  "Hace una lista de las posiciones en las que aparece una cadena en
una región."
  (let (lpos post)
    (when sujeto
      (goto-char inicio)
      (setq lpos (list))
      (while (setq post (search-forward sujeto fin 'end))
        (setq lpos (append lpos
                           (list (- (1- post) inicio)) ))))
    lpos ))



(defun mssql-expresion-registro (longr  ;; longitud del registro
                                 llmarcas  ;; lista de lista de marcas
                                 lsep ) ;; La lista de separadores
  "Arma una expresión regular, con la longitud del registros los
separadores y los trunques, cada registro que forme parte de esta tabla
va a coincidir con esa expresión y se va a encontrar justo después de la
división, el cuerpo, o justo antes, la cabecera, si está."
  (let ((lconmar (make-list (length llmarcas) 0)) ;; lista de contadores de marcas
        proxsalt    ;; proximo salto
        (saltant 0) ;; salto anterior
        sel         ;; marca seleccionada
        (salto 0)   ;; el salto que hace en una iteración
        expreg      ;; expresión registro coincide con todos
        sepact )    ;; separador actual

    ;; Lo principal en esta parte es determinar, recorriendo el
    ;; registro de inicio a fin que cantidad de caracteres de
    ;; distancia hay a los siguientes separadores y los trunques (cada
    ;; "salto") para saber que cantidad exacta de repeticiones hay que
    ;; ponerle a cada sub-expresión regular que va a coincidir con una
    ;; parte del texto de un registro. El siguiente algoritmo busca
    ;; cual es la siguiente cosa y determina la distancia.
    (while (< salto (1- longr)) ;; Cada iteración es un salto.
      (setq salto (1- longr)
            sel nil
            sepact "" )
      ;; Buscando cual de las sub listas de la lista de lista de
      ;; marcas tiene una cosa más cercana, o sea con el salto menor.
      (dotimes (i (length llmarcas))
        (when (and (nth i llmarcas)
                   (setq proxsalt
                         (nth (nth i lconmar)
                              (nth i llmarcas) ))
                   (< proxsalt salto) )
          (setq sel i
                salto proxsalt) ))
      ;; Cuando la cosa está segura, seleccionada, incrementa el
      ;; contador de esa lista y selecciona cual es el separador
      ;; actual.
      (when sel
        (setcar (nthcdr sel lconmar)
                (1+ (nth sel lconmar)) )
        (setq sepact (nth sel lsep)) )
      ;; Finalmente agrega una sub-expresión regular que se va a
      ;; repetir la cantidad exacta encontrada. También guarda el
      ;; salto que acabamos de dar para poder calcular la diferencia
      ;; en el próximo salto.
      (setq expreg (concat expreg
                           (format "<caracter>\\{%d\\}%s"
                                   (- salto saltant) sepact) )
            saltant (+ salto (length sepact)) ))
    ;; Retorna la expresión regular que armamos.
    (concat expreg "\n") ))



(defun mssql-buscar-tabla ()
  "Busca y recoge la información inicial de una tabla mssql cli."
  (let (init     ;; Inicio de la tabla, hasta donde sabemos.
        fint     ;; Fin de la tabla, hasta donde sabemos.
        cab      ;; true si tiene cabecera
        longr    ;; longitud del registro
        llmarcas ;; lista de lista de marcas
        lsep )   ;; La lista de separadores,
                 ;; el primero separa las columnas,
                 ;; el segundo los trunques.

    ;; Busca hacia atras la division vertical entre la cabecera y el
    ;; cuerpo. Una tabla por pequeña que sea tiene que tener por lo
    ;; menos 2 lineas, la división vertical y un registro, ambos con
    ;; la misma longitud las mismas separaciones y los mismos
    ;; trunques..
    (when (re-search-backward
           (concat "^\\(\\(\n\t\\)\\|-\\)+"                  ;; Primera columna
                   "\\(\\([^\n-]\\)\\(\\(\n\t\\)\\|-\\)+"    ;; Primer separador y segunda columna, opcional.
                   "\\(\\4\\(\\(\n\t\\)\\|-\\)+\\)*\\)?$" )  ;; Segundo separador (haciendo referencia al primero),
           nil 't)                                           ;; tercera columna y n repeticiones subsiguientes del conjunto.
      (setq init (match-beginning 0)
            fint (1+ (match-end 0))
            longr (1+ (length (match-string-no-properties 0)))
            lsep (list (match-string-no-properties 4)
                       (or (match-string-no-properties 2)
                           (match-string-no-properties 6)
                           (match-string-no-properties 9) )))
      ;; TODO: Tal vez con el mismo texto de la división vertical se puede armar la expresión regular para detectar los registros y así nos ahorramos un paso.

      ;; De la division vertical toma las divisiones horizontales, el
      ;; caracter separador y los puntos donde está truncada la
      ;; división vertical.
      (dotimes (i (length lsep))
        (setq llmarcas (append
                        llmarcas
                        (cons (mssql-listar-apariciones
                               (nth i lsep) init fint)
                              nil ))))

      ;; Con toda la información de la tabla arma una expresión
      ;; regular en la que todas las lineas que son registros deberían
      ;; coincidir.
      (setq expreg (mssql-expresion-registro longr llmarcas lsep))

      ;; Usa esa expresión para detectar tanto la cabecera como el
      ;; cuerpo de la tabla.

      ;; Primero la cabecera mirando hacia atras
      (goto-char init)
      (when (looking-back (string-replace
                           "<caracter>" "[^\n\t]" expreg ))
        (setq init (match-beginning 0)
              cab 't ))

      ;; Después el cuerpo.
      (setq expreg (string-replace
                    "<caracter>" "\\(.\\|\n\\)" expreg))
      (while (and (goto-char fint)
                  (looking-at expreg) )
        (setq fint (match-end 0)) )

      ;; Ahora tienes el inicio y final de la tabla real

      ;; Pon toda esa información de la tabla en una lista, esta lista
      ;; es lo que vamos a retornar.
      ;;            0    1    2   3            4
      (append (list init fint cab (nth 0 lsep) longr)
              ;; 5 6         7
              llmarcas (cons nil nil) ))))



(defun mssql-quitar-trunques (tabla)
  "Quita los trunques de la tabla de texto y corrige las posiciones de la
información de la tabla para queden todas las posiciones señalando su
sitio real."
  (let (lposep         ;; Lista de posiciones de separador
        lpostrun       ;; Lista de posiciones de trunque
        init           ;; Posicion de inicio de la tabla
        fint           ;; Posición de final de la tabla
        longr          ;; Longitud de un registro
        canr           ;; Cantidad de registros
        (trun "\n\t")  ;; La cadena que hace el trunque.
        i              ;; Indice para señalar el registro que se está
                       ;; trabajando en cada iteración
        j )            ;; Indice para señalar el trunque que se está
                       ;; eliminando en cada iteración.

    ;; Si hay datos en la lista de posiciones de trunque
    (when (setq lpostrun (nth 6 tabla))
      ;; Toma los valores iniciales de las variables
      (setq init (nth 0 tabla)
            fint (nth 1 tabla)
            longr (nth 4 tabla)
            canr (/ (- fint init) longr)
            lposep (nth 5 tabla)
            i fint ) ;; i comienza en la posición final de la tabla.

      ;; Elimina todos los trunques de cada registros. Se trabaja de
      ;; atras para adelante porque según se vayan eliminando los
      ;; trunques se van a ir alterando las posiciones. Si
      ;; recorrieramos la tabla desde el inicio hasta el final habría
      ;; que modificar las posiciones cada vez que se elimina un
      ;; trunque. Haciendo las eliminaciones de atras hacia adelante
      ;; no importa que se afecten las posiciones de atras porque ya
      ;; fueron modificadas y las de adelante que todavía no se han
      ;; tocado permanecen intactas.
      (while (> i init)
        (setq j (1- (length lpostrun))) ;; j comienza en la última
                                        ;; posicion de la lista de
        (while (>= j 0)                 ;; posiciones de trunque.
          ;; Mueve el cursor a la posición de ese trunque.
          (goto-char (- i (- longr (nth j lpostrun))))
          (delete-char (length trun) nil) ;; Elimina el trunque.
          (setq j (1- j)) ) ;; Retrocede una posición en la lista.
        (setq i (- i longr)) ) ;; Retrocede un registro.

      ;; Modifica la longitud del registro eliminando los trunques.
      (setq longr (- longr (* (length lpostrun) (length trun)))
            fint (+ init (* canr longr)) ;; Recalcula el final
            i 0 ;; Para la siguiente operación i y j empiezan en 0.
            j 0 )

      ;; Restale a las posiciones de los separadores la longitud de
      ;; los trunques que le correspondan.

      ;; Restale a las posiciones de los separadores la cantidad de
      ;; trunques que están antes de ese separador.
      (while (< i (length lposep)) ;; Mientras haya separadores
        (while (and (< j (length lpostrun)) ;; Mientras haya trunques
                    (> (nth i lposep) (nth j lpostrun)) ) ;; y el separador
          (setq j (1+ j)) )                               ;; esté por delante
        (setcar (nthcdr i lposep) ;; restale a la posición separador los trunques
                (- (nth i lposep) (* (length trun) j)) )
        (setq i (1+ i)) ) ;; pasa al siguiente separador

      ;; Pon de regreso en la lista de información de la tabla los
      ;; campos que cambiaron por la eliminación de los trunques o sea
      ;; final de la tabla, longitud de registro y separadores.
      (setcar (nthcdr 1 tabla) fint)
      (setcar (nthcdr 4 tabla) longr)
      (setcar (nthcdr 6 tabla) nil) )
    tabla ))



(defun mssql-reemplazar-fines-tabs (tabla)
  "A veces ocurre que algunos valores de texto de algunos campos tienen
fines de lineas y tabs. Para poder reformatear las tablas en las
que estan esos campos hay que cambiar esos caracteres de fines de
lineas y tab por un caracter de espacio. Ver los casos de prueba
Prueba reparar tabla descripción servidor y Prueba reparar tabla
descripción servidor 2."
  (let* ((init (nth 0 tabla))    ;; inicio de la tabla
        (fint (nth 1 tabla))     ;; final de la tabla
        (longr (nth 4 tabla))    ;; longitud de un registro
        (inil init) )            ;; inicio de la linea actual

    (replace-string-in-region   ;; Reemplaza cada tab por un espacio
     "\t" " " init fint)        ;; en la región de la tabla.

    ;; Con los caracteres de fines de linea es un poco más complicado
    ;; porque hay que reemplazar los caracteres que son parte del
    ;; texto pero no los que separan un registro del siguiente.
    (while (< inil fint)
      ;; Reemplaza los caracteres de fin de linea dentro de la región
      ;; del cuerpo del registro dejando fuera el final del registro.
      (replace-string-in-region "\n" " " inil (+ inil (1- longr)))
      (setq inil (+ inil longr)) )
    ))



(defun mssql-revisar-espacios-bloque-m1(init   ;; Inicio de la tabla
                                        fint   ;; Fin de la tabla
                                        longr  ;; Longitud del registro
                                        inic   ;; Inicio de la columna
                                        finc   ;; Fin de la columna
                                        desde  ;; Linea inicial del bloque
                                        hasta) ;; Linea limite bloque no incluida
  "Revisa un bloque de una columna. Si hay una cabecera esta se toma como
un bloque, el cuerpo es otro bloque. Esta función recorre el
bloque indicado buscando los espacios en cada registro y tomando
el menor de todo el bloque para el inicio y para el final, o sea
izquierda y derecha de la columna."
  (let ((linea desde) ;; Indice indica linea actual
        (meini longr) ;; Mínimo espacio inicio izquierda recortable
        (mefin longr) ;; Mínimo espacio fin derecha recortable
        inicel        ;; Inicio de la celda
        fincel        ;; Fin de la celda
        textocel      ;; El texto de la celda actual
        espini        ;; Espacio al inicio
        espfin)       ;; Espacio al final

    ;; Recorre todos los valores de la columna siempre que no
    ;; encontremos un valor que cubra todo el ancho, en cuyo caso el
    ;; mínimo espacio se va a 0 y no tiene sentido seguir buscando
    ;; porque no se va a poder recortar nada sin dañar ese valor.
    (while (and (< linea hasta) (or (> meini 0) (> mefin 0)))

      (setq inicel (+ (* linea longr) inic init) ;; Toma el inicio de la división.
            fincel (+ (* linea longr) finc init) ;; Toma el final de la división.
            textocel (buffer-substring-no-properties inicel fincel) ) ;; Toma el texto de la celda.

      (string-match "^\\( *\\)\\(.*?\\)?\\( *\\)$" textocel)
      (setq espini (length (match-string-no-properties 1 textocel))
            espfin (length (match-string-no-properties 3 textocel)) )

      ;; Hay casos en los que la celda completa está en blanco, pero
      ;; el espacio completo se toma al inicio o al final, entonces
      ;; queda el otro lado completamente en vacio, en 0, como si no
      ;; hubiera espacio, esto provoca que el algoritmo entienda
      ;; erroneamente que no se puede recortar nada de ese lado, por
      ;; eso este método ignora las celdas completamente en blanco,
      ;; que de todas formas no afectaría los espacios mínimos.
      (when (< (+ espini espfin) (- finc inic))
        ;; si la suma de los espacios es menor que el tamaño de la
        ;; celda, toma en cuenta esta celda

        (when (< espini meini) (setq meini espini))

        (when (< espfin mefin) (setq mefin espfin)) )

      (setq linea (1+ linea)) )
    (list meini mefin) ))



(defun mssql-revisar-espacios-m1 (init  ;; Inicio de la tabla
                                  fint  ;; Fin de la tabla
                                  longr ;; Longitud del registro
                                  inic  ;; Inicio de la columna
                                  finc  ;; Fin de la columna
                                  cab)  ;; True si tiene cabecera
  "Revisa los espacios de una columna usando el método 1, recorriendo valor
por valor usando una expresión regular para saber que espacio hay al
inicio y al final del texto de la celda."
  (let ((alto (/ (- fint init) longr)) ;; El alto de la tabla, cuantas lineas hay
        (inicue 1) ;; Linea en la que inicia el cuerpo
        espcab     ;; Espacios de la cabecera
        sumcab     ;; Sumatoria de la cabecera, total de espacio recortable.
        espcue     ;; Espacios del cuerpo
        sumcue     ;; Sumatoria del cuerpo, total de espacio recortable.
        sobra)     ;; Diferencia entre el cuerpo y la cabecera

    (when cab
      ;; Si hay una cabecera trae los espacios de la cabecera.
      (setq espcab (mssql-revisar-espacios-bloque-m1
                    init fint longr inic finc 0 1)
            sumcab (apply '+ espcab)
            inicue 2))

    ;; Trae los espacios mínimos del cuerpo.
    (setq espcue (mssql-revisar-espacios-bloque-m1
                  init fint longr inic finc inicue alto)
          sumcue (apply '+ espcue))

    ;; Si hay cabecera y sus espacios no son 0 y los espacios de la
    ;; cabecera son menores que los espacios del cuerpo hay que
    ;; ajustarlos para poder recortar sin dañar la cabecera
    (when (and cab (< 0 (+ sumcab sumcue)) (< sumcab sumcue))
      (setq sobra (- sumcue sumcab))

      ;; Si la sobra es mayor que el espacio del final
      (if (> sobra (nth 1 espcue))
          (progn ;; Que consuma ese espacio por completo
            (setq sobra (- sobra (nth 1 espcue)))
            (setcar (nthcdr 1 espcue) 0)
            ;; Luego consumir lo que haga falta del inicial
            (setcar espcue (- (car espcue) sobra)))
        (progn
          ;; sino que consuma lo que haga falta solo del espacio final
          (setcar (nthcdr 1 espcue) (- (nth 1 espcue) sobra))
          (setq sobra 0) )))
    ;; Retorna la lista con los 2 espacios del cuerpo, modificados o no
    espcue ))



(defun mssql-revisar-espacios-m2 (inic finc inif finf)
  "Revisa los espacios de una columna usando el método 2, revisando las
columnas de caracteres una a una para detenerse cuando encuentre texto
que no esté en blanco."
  (let (lespr cespt espizqt flancoiz flancode colt cole)

    (while (not (encflanco))

      )
    ))



(defun mssql-revisar-espacios-m3 (tabla)
  "Revisa los espacios de una columna usando el método 3, tratando de hacer
verificaciones de las columnas de caracteres de forma eficiente, dando
brincos, tratando de adivinar, de forma que no tenga que verificar todas
las columnas."
  )



(defun mssql-revisar-espacios (tabla)
  "Recorre las columnas llamando a la función que revisa los
espacios de columna una a la vez."
  (let* ((init (nth 0 tabla))  ;; Inicio de la tabla
         (fint (nth 1 tabla))  ;; Final de la tabla
         (cab (nth 2 tabla))   ;; Si tiene cabecera o no
         (longr (nth 4 tabla)) ;; La longitud de cada registro
         (lcolsep (copy-sequence (nth 5 tabla))) ;; Lista de posiciones de los separadores
         (i 0)                 ;; Indice para recorrer las columnas
         (lrec (cons nil nil)) ;; Lista de recortes inicia con un eslabón
         (ultrec lrec)         ;; Último recorte último eslabón
         penrec)               ;; Penúltimo recorte

    ;; Inserta un separador en la pocisión -1 del registro en el 0 de
    ;; la lista de separadores.
    (setq lcolsep (cons -1 lcolsep))

    ;; Agrega un elemento más a la lista de separadores al final, como
    ;; si hubiera un separador en el caracter de la última posición
    ;; del registro.
    (setcdr (nthcdr (1- (length lcolsep)) lcolsep) (cons (1- longr) nil))

    (dotimes (i (1- (length lcolsep))) ;; Recorre los separadores - 1
      (setcar ultrec (mssql-revisar-espacios-m1 ;; Revisa los espacios de la columna i
                      init fint longr (1+ (nth i lcolsep))
                      (nth (1+ i) lcolsep) cab ))
      (setq penrec ultrec) ;; Apunta el penúltimo eslabón al último
      (setcdr ultrec (cons nil nil)) ;; Agrega un nuevo cons
      (setq ultrec (cdr ultrec)) ) ;; Apunta el último al
                                   ;; nuevo último cons
    (setcdr penrec nil) ;; Ahora que ya no se necesita elimina el
                        ;; último eslabon, haciendo al penúltimo
                        ;; apuntar a nil.
    (setcar (nthcdr 7 tabla) lrec) ;; Pon la lista de los recortes en
                                   ;; el item 7 de la lista de
                                   ;; información de la tabla
    )) ;; Listo retorna la misma tabla recibida



(defun mssql-recortar-espacios-celda-m1 (linea lcolsep lrec cab)
  "Recorta los espacios de la linea, o registro, celda por celda en
orden inverso, de la derecha, el final, hasta el principio la
izquierda."
  (let ((i (1- (length lrec))) ;; El indice de la celda, comienza al final.
        espini      ;; Espacio al inicio, a la izquierda.
        espfin      ;; Espacio al final, a la derecha.
        inicel      ;; Inicio de la celda
        fincel )    ;; Final de la celda

    (while (>= i 0)
      ;; Lee los valores que le corresponden a esta celda
      (setq espini (nth 0 (nth i lrec))
            espfin (nth 1 (nth i lrec))
            inicel (+ linea (1+ (nth i lcolsep)))
            fincel (+ linea (nth (1+ i) lcolsep)) )

      (when cab
        ;; Si estamos trabajando con una cabecera todos los espacios
        ;; que se vayan a recortar se recortan del lado derecho, del
        ;; final. Normalmente las cabeceras están alineadas a la
        ;; izquierda.
        (setq espfin (+ espini espfin)
              espini 0 ))

      ;; Elimina los espacios en cuestión
      (delete-region inicel (+ inicel espini))
      (delete-region (- fincel espfin) fincel)
      (setq i (1- i)) )
    ))



(defun mssql-recortar-espacios-m2 ()
  "Recorta los espacios de la columna usando las coordenadas del texto para ir y borrarlos."
  )



(defun mssql-recortar-espacios-m3 ()
  "Recorta los espacios de la columna usando las funciones de rectangulo para quitarlos todos de un solo golpe."
  )



(defun mssql-recortar-espacios-m1 (tabla)
  "Recorre la tabla columna por columna, de atras para alante,
haciendo recortes de los espacios en blanco. Después de terminar
de hacer los recortes no hay que actualizar la información de la
tabla porque ya cumplimos nuestro objetivo, ya no se va a usar la
información de la tabla para más nada."
  (let* ((init (nth 0 tabla))   ;; Inicio de la tabla
         (fint (nth 1 tabla))   ;; Final de la tabla
         (longr (nth 4 tabla))  ;; Longitud del registro
         (alto (/ (- fint init) longr)) ;; Cantidad de registros de la tabla
         (inicue 0)             ;; Inicio del cuerpo
         (cab (nth 2 tabla))    ;; Si hay cabecera 't, sino nil.
         (lcolsep (copy-sequence (nth 5 tabla))) ;; Lista de separadores de columnas
         (lrec (nth 7 tabla))   ;; Lista de recortes
         (linea (1- alto)) )    ;; Indice de la linea que estamos revisando actualmente

    ;; Inserta un separador en la pocisión -1 del registro en el 0 de
    ;; la lista de separadores.
    (setq lcolsep (cons -1 lcolsep))

    ;; Agrega un elemento más a la lista de separadores al final, como
    ;; si hubiera un separador en el caracter de la última posición
    ;; del registro.
    (setcdr (nthcdr (1- (length lcolsep)) lcolsep) (cons (1- longr) nil))

    ;; Cuando hay cabecera el cuerpo empieza en el 1
    (when cab (setq inicue 1))

    ;; Recorre las lineas, los registros del cuerpo, de la última a la
    ;; primera.
    (while (>= linea inicue)
      (mssql-recortar-espacios-celda-m1 (+ init (* linea longr)) lcolsep lrec nil)
      (setq linea (1- linea)) )

    (when cab
      ;; Si hay cabecera recorta los espacios también de la cabecera.
      (setq linea 0)
      (mssql-recortar-espacios-celda-m1 (+ init (* linea longr)) lcolsep lrec cab) )

    ;; Recalcula el final de la tabla, que se necesita para poder
    ;; hacer la operación de recortar los espacios del final de la
    ;; última columna.
    (dolist (rec lrec)
      (setq fint (- fint (* (+ (nth 0 rec) (nth 1 rec)) alto))) )

    ;; Recorta los últimos espacios de la última columna.
    (replace-regexp-in-region "[ \t]+$" "" init fint)
  ))



(defun mssql-asegurar-inicio (tabla)
  "Revisa que el inicio de la tabla esté al inicio de una nueva
linea de manera que la tabla no quede desalineada. A veces una
tabla puede comenzar en la misma linea en la que se encuentra el
prompt de SQL y esto causa cierta incomodidad para leerla por
parte del usuario, pero también para reformatearla."
  (let ((init (nth 0 tabla)) ;; Inicio de la tabla
        (fint (nth 1 tabla)) ;; Final de la tabla
        (llmarcas (nth 5 tabla)) ) ;; Lista de lista de marcas
    ;; Ponte en el inicio de la tablaj
    (goto-char init)
    (when (not (bolp))
      ;; Si no estas al inicio de una linea (bol beginning of line)
      ;; Inserta un fin de linea para que la tabla ahora inicie con la siguiente linea
      (insert "\n")
      (setq init (+ init (length "\n"))
            fint (+ fint (length "\n")) )
      (setcar (nthcdr 0 tabla) init)
      (setcar (nthcdr 1 tabla) fint) )
    tabla )
  )


(defun mssql-hackeo-255 (tabla)
  "Esto es un hackeo sucio para un caso especial en el que la tabla
supuestamente tiene 255 caracteres de ancho, pero en realidad
solo la cabecera tiene eso, el resto, el cuerpo tiene 254. Ojo
hablamos de la longitud del contenido del registro para la
longitud del registro total habría que sumar 1 por el fin de
linea."
  (let* ((init (nth 0 tabla))    ;; Inicio de la tabla
         (fint (nth 1 tabla))    ;; Final de la tabla
         (longr (nth 4 tabla))   ;; Longitud de un registro
         (alto (/ (- fint init) longr)) ;; Alto o sea cantidad de registros de la tabla
         (cab (nth 2 tabla))     ;; Contiene 't si la tabla tiene cabecera, sino tiene nil
         (lcolsep (nth 5 tabla)) ;; Lista de las posiciones de separadores de columnas
         ;; Estas son variables para guardar los 2 caracteres que
         ;; serán eliminados tanto el caracter como su posición.
         posact poscar1 car1 poscar2 car2
         (ttabla tabla))         ;; Temporal tabla para re-buscar la tabla.

    ;; Si esta tabla tiene registros de 256 y actulamente tiene un
    ;; alto de 3 o 2 y es de una sola columna, sin separadores
    (when (and (= longr 256) cab (or (= alto 2) (= alto 3)) (not lcolsep))
      ;; Eliminale 1 caracteres del final antes del fin de linea al
      ;; registro de la cabecera y la división vertical.

      ;; Guarda la posición desde donde estamos comenzando
      (setq posact (point))

      (goto-char (1- (+ init (* longr 2))))
      (setq car2 (char-before)
            poscar2 (1- (point)) )
      (delete-backward-char 1)

      (goto-char (- (+ init longr) 2))
      (setq car1 (char-before)
            poscar1 (1- (point)) )
      (delete-backward-char 1)

      ;; Vuelve a tratar de captar la información de la tabla.
      (goto-char posact)
      (setq ttabla (mssql-buscar-tabla))

      ;; Si se obtuvieron más registros que los 2 o 3 que teniamos
      ;; antes, entonces esta es la información correcta, sustituye la
      ;; información dentro de tabla.
      (unless (> (/ (- (nth 1 ttabla) (nth 0 ttabla)) (nth 4 ttabla)) alto)
        ;; Sino hubo exito hay que regresar esos dos caracteres que
        ;; modificamos y continuar con lo que tenemos.
        (goto-char poscar1)
        (insert-char car1)
        (goto-char poscar2)
        (insert-char car2)
        (setq ttabla tabla) ))
    ttabla ))



;; TODO: Documentar lo mejor posible como funciona esto.
;; TODO: Terminar de hacer los otros métodos para revisar y recortar los espacios de manera que podamos elegir los más eficientes.
;; TODO: Quizas fuera más fácil hacer la reparación completa en memoria y solo hacer en el buffer la parte de capturar la tabla y cuando se vuelve a poner en el buffer. Por otro lado me quedo pensando que talvez no hace mucha diferencia porque los buffer de emacs de hecho son memoria, pero me refería a usar variables string y trabajar sobre las tablas en variables string.
;; TODO: La región no se está usando realmente, no se está tomando en cuenta.
;; TODO: Esta función podría hacer recortes a las columnas para adaptarlas a un ancho de pantalla específico.
;; TODO: ¿Existe la posibilidad que un buffer sqli llame de forma automática a la función de reformat cada vez que hace un query o cada vez que hay un resultado en forma de tabla? investigar. Esta llamada automática, si se logra hacer de una forma confiable ahorraría el trabajo de tener que llamar la función de reformatear la tabla que cuando estoy trabajando en un buffer sqli hago casi siempre después de una consulta.
;; TODO: A veces en algunas columnas de algunas tablas se usa el tipo de dato datetime para almacenar una fecha, la parte de la hora queda sin uso, o sea siempre mostrando 00:00:00.000. Sería bueno que en estos casos en que todos los valores de horas de una columna datetime estuvieran en 0, eliminar esos 0 que no aportan ninguna información. Ver en los casos de prueba la "Prueba tabla grande 2".
;; TODO: En ese mismo sentido una columna con valores numéricos que muestra siempre 0 ceros a la derecha del punto que tampoco aportan información, se podría recortar para que no muestre estos ceros. Ver el caso de prueba Prueba tabla grande en la columna salario.
;; TODO: No hay ningún caso que use el trunque, incluir un par de casos y hacer que funcionen bien.
(defun mssql-reformat-table (&optional start end)
  "Reformats text tables from mssql cli as thin as posible.

Esta función es útil por sus efectos secundarios, no por su resultado. Lo que usa como entrada es su posición en el buffer y el propio texto del buffer. Lo más normal cuando se llama esta función es que el cursor esté al final del buffer, por debajo de alguna tabla y esta función busca la última tabla hacia atras para reformatearla.

El resultado de esta función es la última tabla al final del buffer ya reformateada y reducida a su expresión con el anchó más mínimo. Esta función no devuelve ningún valor útil.

El objetivo es ir de esto:
nombre               |apellido               |sueldo       |nacimiento     |puesto          
---------------------|-----------------------|-------------|---------------|----------------
Carlos Alberto       |Marcos Zapata          |     30500.00|23-03-1983     |Maestro         
Simona Bueno         |Jimenez Garcia         |      9000.00|05-12-1958     |Faenaria        
Federico Angel       |Iglesias Acosta        |     65000.00|07-01-1994     |Director        

A esto:
nombre        |apellido       |sueldo  |nacimiento|puesto
--------------|---------------|--------|----------|--------
Carlos Alberto|Marcos Zapata  |30500.00|23-03-1983|Maestro
Simona Bueno  |Jimenez Garcia | 9000.00|05-12-1958|Faenaria
Federico Angel|Iglesias Acosta|65000.00|07-01-1994|Director

Para que esta función trabaje se recomienda que se usen las
siguientes opciones para osql:

    (setq sql-ms-options (quote (\"-w\" \"300\" \"-s\" \"|\" \"-n\"))

"
  (interactive
   (list
    (if (use-region-p) (region-beginning))
    (if (use-region-p) (region-end)) ))
  (save-excursion
    (let (tabla)

      ;; Trata de encontrar una tabla buscando hacia atras.
      (when (setq tabla (mssql-buscar-tabla))
        ;; La variable tabla es una lista
        ;; 1. el primer elemento es la pocision de inicio de la tabla
        ;; 2. el segundo es la pocisión final
        ;; 3. el tercero indica si tiene cabecera o no
        ;; 4. el cuarto es el separador
        ;; 5. el quinto elemento es la longitud de un registro
        ;; 6. la sexta cosa es una sublista con las posiciones de cada
        ;;   separador
        ;; 7. el séptimo elemento es una sublista, cada elemento de esa
        ;;   sublista es una pocisión donde el registro se trunca
        ;; 8. el octavo elemento es una sublista con información de
        ;;   donde cortar los espacios de las columnas.

        ;; En el octavo elemento por cada columna hay una sublista, el
        ;; primer elemento de cada una de esas sublistas es el espacio
        ;; que se puede recortar desde la izquierda, el segundo
        ;; elemento lo que se puede recortar desde la derecha. Que se
        ;; puede recortar significa que ninguna fila tiene texto en
        ;; ese espacio.

        ;; Algunas tablas tienen condiciones especiales, este hackeo
        ;; las modifica un poco para que se adapten a la norma.
        (setq tabla (mssql-hackeo-255 tabla))

        ;; Asegura que la tabla comience al inicio de una linea.
        (mssql-asegurar-inicio tabla)

        ;; Quitale las propiedades a todo el texto de la tabla
        (set-text-properties (nth 0 tabla) (nth 1 tabla) nil)

        ;; Elimina los trunques que tenga la tabla.
        (mssql-quitar-trunques tabla)

        ;; Reemplaza todos los caracteres tab y fin de linea dentro de
        ;; cada campo de la tabla por un espacio.
        (mssql-reemplazar-fines-tabs tabla)

        ;; Revisa los espacios en blanco de cada columna, cuanto se
        ;; puede recortar y de que lado.
        (mssql-revisar-espacios tabla)

        ;; Haz el recorte de los espacio
        (mssql-recortar-espacios-m1 tabla)

        ) ;; Listo!
      )))



;; (defun sql-dentro-cadena (punto)
;;   "Dice si un punto está dentro de una cadena o no."
;;   )


;; (defun sql-dentro-comentario (punto)
;;   "Dice si un punto está dentro de un comentario."
;;   )


;; (defun limpiar-codigo-sql (&optional start end)
;;   "Una función para limpiar el código SQL y ponerlo como creo que es más legible."
;;   (interactive
;;    (list
;;     (if (use-region-p) (region-beginning))
;;     (if (use-region-p) (region-end)) ))
;;   (save-excursion
;;     (let (inir finr)
;;       (if (use-region-p)
;;           (setq inir start
;;                 finr end )
;;         (setq inir (point-min)
;;               finr (point-max) ))
;;        ;; Pon espacios alrededor de los operadores y después de una coma.

;;        ;; Ve al inicio del texto.
;;        ;; Busca el operador más cercano.
;;        ;; Si está dentro de un comentario o una cadena ignoralo.
;;        ;; Sino está dentro de un comentario o cadena pon espacio antes y espacio después
;;        "\_<\([-+=*/]\|[><]=?\|<>\)\_>"
;;        (replace-regexp-in-region "\\w\\([\\+-*/]\\)\\w" "" inir finr)
;;      )))


;; "\\([-+=*/]\\|[><]=?\\|<>\\)"



(defun sql-ms-descripcion-a-create (&optional start end)
  "Genera una sentencia create a partir de la descripción de una tabla."
  (interactive
   (list
    (if (use-region-p) (region-beginning))
    (if (use-region-p) (region-end)) ))
  (save-excursion
    (let ((orig (point)) (salida "") nombre tipo tam null
          (expcol (concat "\\([[:alnum:]_]+\\) *| *\\([[:alnum:]]+\\)"
                          " *| *\\([0-9]+\\|NULL\\) *| *\\(YES\\|NO\\)")))
      ;; Ve a la cabecera de la tabla
      (re-search-backward "columna *| *tipo *| *bytes *| *anulable")
      ;; Ve tomando de registro en registro
      (while (re-search-forward expcol nil t)
        ;; Transforma cada registro en una columna del create
        (setq nombre (match-string-no-properties 1))
        (setq tipo (match-string-no-properties 2))
        (setq tam (match-string-no-properties 3))
        (setq null (match-string-no-properties 4))
        (if (or (string= tam "NULL")
                (cl-member (downcase tipo)
                           '("text" "ntext" "image")
                           :test 'string=))
            (setq tam "")
          (progn
            (if (cl-member (downcase tipo)
                           '("nchar" "nvarchar")
                           :test 'string=)
                (setq tam (number-to-string (/ (string-to-number tam) 2))) )
            (setq tam (concat "(" tam ")"))
            ))
        (if (string= null "NO")
            (setq null " not")
          (setq null ""))
        (setq null (concat null " null,\n"))
        (setq salida (concat salida "    " nombre " " tipo tam null))
        )
      ;; Borra los últimos 2 caracteres
      (setq salida (substring salida 0 (- (length salida) 2)))

      ;; Escribe la sentencia create table.
      (goto-char orig)
      (insert "create table temptable (\n" salida "\n)\ngo\n")
      ))
  )


(defun sql-ms-descripcion-a-select (&optional start end)
  "Genera una sentencia select a partir de la descripción de una tabla."
  (interactive
   (list
    (if (use-region-p) (region-beginning))
    (if (use-region-p) (region-end)) ))
  (save-excursion
    (let ((orig (point)) (salida "") nombre tipo tam null
          (expcol (concat "\\([[:alnum:]_]+\\) *| *\\([[:alnum:]]+\\)"
                          " *| *\\([0-9]+\\|NULL\\) *| *\\(YES\\|NO\\)")))
      ;; Ve a la cabecera de la tabla
      (re-search-backward "columna *| *tipo *| *bytes *| *anulable")
      ;; Ve tomando de registro en registro
      (while (re-search-forward expcol nil t)
        ;; Transforma cada registro en una columna del select
        (setq nombre (match-string-no-properties 1))
        (setq tipo (match-string-no-properties 2))
        (setq tam (match-string-no-properties 3))
        (setq null (match-string-no-properties 4))
        (if (or (string= tam "NULL")
                (cl-member (downcase tipo)
                           '("text" "ntext" "image")
                           :test 'string=))
            (setq tam "")
          (progn
            (if (cl-member (downcase tipo)
                           '("nchar" "nvarchar")
                           :test 'string=)
                (setq tam (number-to-string (/ (string-to-number tam) 2))))
            (setq tam (concat "(" tam ")"))
            ))
        (if (string= null "NO")
            (setq null " not")
          (setq null ""))
        (setq null (concat null " null\n"))
        (setq salida (concat salida "    " nombre ", -- " tipo tam null))
        )
      ;; Borra la última coma de salida
      (string-match ", -- \\([[:alnum:]() ]+\\)\n\\'" salida)
      (setq salida (replace-match " -- \\1\n" t nil salida))

      ;; Escribe la sentencia select
      (goto-char orig)
      (insert "select top 10\n" salida "from temptable\ngo\n")
      ))
  )


(defun ruta-buffer-actual-killring ()
  "Pone la ruta del fichero actual en el killring."
  (interactive)
  (if buffer-file-name
      (progn
        (message "Tomando la ruta %s" buffer-file-name)
        (kill-new buffer-file-name) )
    (message "Este buffer no tiene un fichero asociado")
    ) )


(defun descripcion-bdatos-a-doc ()
  "Torna la linea actual, con la descripción de un campo en la base de
datos, en una descripción para usar en la documentación.

Debería tornar una linea como esta:
\" maenume          char               8 NO           \"

En una linea como esta:
\" * maenume char(8) NOT NULL:\"
"
  (interactive)
  (save-excursion
    (let (reemp nomb (tam "") (stnull "NULL"))
      (re-search-backward "^")
      (if (looking-at (concat "^[   ]*\\([[:alnum:]_]+\\)[  |]+"
                              "\\(\\w+\\)[  |]+"
                              "\\([[:digit:]]+\\|NULL\\)[   |]+"
                              "\\(YES\\|NO\\)[  ]+$") )
          (progn
            (setq nomb (string-replace "_" "\\\\_" (match-string 1)))
            (unless (or (string= (downcase (match-string 2)) "text")
                     (string= (match-string 3) "NULL"))
              (setq tam "(\\3)") )
            (when (string= (match-string 4) "NO")
              (setq stnull (concat "NOT " stnull)) )
            (setq reemp (format " * __%s \\2%s %s__:" nomb tam stnull))
            (replace-match reemp) )
          (message (concat "Hubo un error, la linea no parece una "
                           "descripción de un campo de bdatos.") )))))
