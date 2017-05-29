
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

;; Modificando de la función "open-file-at cursor" que se encuentra en:
;; http://ergoemacs.org/emacs/emacs_open_file_path_fast.html
;; Cree esta función ...
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
            ; si no, entonces se trata de un fichero.
            ('t
             (progn ; not starting 'http://"
               (if (file-exists-p texto)
                   (find-file texto)
                 (if (file-exists-p (concat texto ".el"))
                     (find-file (concat texto ".el"))
                   (when (y-or-n-p (format "El fichero \"%s\" no existe. ¿Lo creo?" texto) )
                     (find-file texto )) ) ) ))
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

(defun select-sln-csproj (posible)
  "Selects a project file from a given posible file list.
From a given posible list returns a visual studio project file,
first trys to find an sln and if its not found trys with a csproj
file. If neither is found returns nil."
  (let ((project nil)
        (file nil)
        (files (copy-tree posible)))
    (while (and (not project) files)
      (setq file (car files))
      (when (string-suffix-p ".sln" file t)
        (setq project file))
      (setq files (cdr files))
      )
    (setq files (copy-tree posible))
    (while (and (not project) files)
      (setq file (car files))
      (when (string-suffix-p ".csproj" file t)
        (setq project file))
      (setq files (cdr files))
      )
    project
    )
  )

(defun get-vsproject-here (this-file only-sln)
  "Gets the main vsproject file for a given source file."
  (let ((dir-actual (parent-directory this-file))
        posibles
        project
        (cont 4))
    (while (and (not project) (> cont 0))
      (if only-sln
          (setq posibles (directory-files dir-actual t "^.*\\.sln$"))
        (setq posibles (directory-files dir-actual t "^\\(.*\\.sln\\|.*\\.csproj\\)$")))
      (setq project (select-sln-csproj posibles))
      (setq cont (1- cont))
      (setq dir-actual (parent-directory dir-actual))
      )
    project
    )
  )
  
(defun compile-vsproject-here ()
  "Compiles the project of the current source.
This function searches for the .net project nearest to the
current path and puts it on a command to build that project."
  (interactive)
  (progn
    ;; If compile-command has its standard value, try to create a compile command for the vs-project.
    (if (equal compile-command (eval (car (get 'compile-command 'standard-value))))
        (let ((project (get-vsproject-here buffer-file-name 'nil)) )
          (when (not project)
            (setq project "(no project.csproj or project.sln found)"))
          (setq compile-command (concat "/cygdrive/d/comun/codigo/lang/bat/construirvs2013.bat " project))
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
        (setq omnisharp-vs-project (get-vsproject-here buffer-file-name 't))
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

;; TODO: una función que transforme una lista de valores separados por coma en selects con union con el fin de convertirlos en un insert.

(defun set-column-info (column-list pos type)
  "Sets the info of a column in a list of items"
  (let ((value (nth pos column-list)))
    (message (format "set-column-info %s %i %s" column-list pos type))
    (if value
        (if (or (and (equal value 'number) (equal type 'string))
                (equal value 'unknown))
            (setcar (nthcdr pos column-list) type))
      (progn
        (while (< (length column-list) (1+ pos))
          (setq column-list (reverse (cons nil (reverse column-list)))) )
        (setcar (nthcdr pos column-list) type)
        (message (format "%i %s" (length column-list) column-list))
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
        (message line)
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
          (message value)
          ;; If the value is "NULL" then we have nothing else to check
          (if (or (string-match-p "NULL" value)
                  (string-match-p numexp value))
              (setq columns (set-column-info columns column-count 'number))
            (setq columns (set-column-info columns column-count 'string)) )
          (setq column-count (1+ column-count)) )
        (setq row-count (1+ row-count))
      )
      (message (format "listo columns %s" columns))
      ;; Replace each row with a select taking into account the number of fields it should have
      (goto-char start)
      (while (re-search-forward lineexp end 't)
        (setq line (match-string-no-properties 0))
        (message line)
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
        (message line)
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
