#lang racket ;;Esta línea indica que el código se escribirá en Racket.

(require 2htdp/batch-io) ;;Esta línea indica que se necesitarán las funciones de entrada y salida de archivo de texto.

(define ruta_archivo "test.txt") ;;Se define una variable archivo que contiene el nombre del archivo de texto que se va a leer.

;;Se define una función llamada leer_archivo_como_lista_de_caracteres que recibe el nombre de un archivo, lo abre y devuelve su contenido como una lista de caracteres.
(define leer_archivo_como_lista_de_caracteres
  (lambda (filename)
    (flatten
     (map string->list
          (read-1strings ruta_archivo)))))

;;Se define una función llamada convertir_lista_de_caracteres_a_lista_de_strings que convierte una lista de caracteres en una lista de strings.
(define convertir_lista_de_caracteres_a_lista_de_strings
  (lambda (loc aux result)
    (cond
      [(empty? loc) result]
      [(char-whitespace? (car loc))
       (convertir_lista_de_caracteres_a_lista_de_strings (cdr loc)
         '()
         (cons
          (list->string aux)
          result))]
      [(char-punctuation? (car loc))
       (convertir_lista_de_caracteres_a_lista_de_strings (cdr loc)
         '()
         (cons
          (list->string
           (cons (car loc) '()))
          (cons
           (list->string aux)
           result)))]
      [else
       (convertir_lista_de_caracteres_a_lista_de_strings (cdr loc)
         (append aux (cons (car loc) '()))
         result)])))

;;Se define una función llamada leer_archivo_como_lista_de_strings que recibe el nombre de un archivo, lo lee carácter por carácter y lo devuelve como una lista de strings.
(define leer_archivo_como_lista_de_strings
  (lambda (filename)
    (reverse
     (convertir_lista_de_caracteres_a_lista_de_strings
      (leer_archivo_como_lista_de_caracteres ruta_archivo) '() '()))))

;;Se define una función llamada convertir_lista_de_strings_a_cadena que convierte una lista de strings en una cadena de caracteres.
(define convertir_lista_de_strings_a_cadena
  (lambda (strlst)
    (string-join strlst " ")))

;;Se define una variable codigo que contiene el archivo de texto leído y convertido en una lista de strings.
(define codigo_fuente (leer_archivo_como_lista_de_strings ruta_archivo))

;;Se lee el contenido del archivo.
(define contenido_archivo (read-file ruta_archivo))

;;Se reemplaza el caracter < por su entidad HTML &lt en el contenido del archivo.
(define contenido_archivo_lt (string-replace contenido_archivo "<" "&lt"))

;;Se reemplaza el caracter > por su entidad HTML &gt en el contenido del archivo.
(define contenido_archivo_gt (string-replace contenido_archivo_lt ">" "&gt"))

;;Se reemplaza el salto de línea por la etiqueta HTML <br> en el contenido del archivo.
(define contenido_archivo_br (string-replace contenido_archivo_gt "\n" " <br> "))

;;Se definen varias funciones, cada una de las cuales determina si una cadena cumple con un patrón determinado.
(define is-reservada?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"auto|bool|break|case|catch|char|class|const|continue|default|delete|do|double|else|enum|explicit|extern|false|float|for|friend|goto|if|inline|int|long|mutable|namespace|new|operator|private|protected|public|register|return|short|signed|sizeof|static|struct|switch|template|this|throw|true|try|typedef|typeid|typename|union|unsigned|using|virtual|void|volatile|wchar_t|while|False|None|True|and|as|assert|break|class|continue|def|del|elif|else|except|finally|for|from|global|if|import|in|is|lambda|nonlocal|not|or|pass|raise|return|try|while|with|yield|break|case|catch|class|const|continue|debugger|default|delete|do|else|export|extends|finally|for|function|if|import|in|instanceof|new|return|super|switch|this|throw|try|typeof|var|void|while|with|yield" palabra) #t]
            [else #f])))

(define is-integer?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"[0-9]?" palabra) #t]
            [else #f])))

(define is-decimal?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"[0-9]+[.][0-9]?" palabra) #t]
            [else #f])))

(define is-operador?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"[,]|[:]|[;]|[+][+]|[-][-]|[*][*]|[+]|[-]|[*]|[/]|[%]|[!]|[=]|&lt|&gt|&lt{1}|&gt&gt|&lt=|&gt=" palabra) #t]
            [else #f])))

(define is-identificador?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"[a-zA-Z_]+[a-zA-Z_0-9]*;?,?" palabra) #t]
            [else #f])))

(define is-delimitadorFuncion?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx".*\\(|.*\\)|.*\\{|.*\\}" palabra) #t]
            [else #f])))

(define is-comment?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"//[a-zA-Z]*" palabra) #t]
            [else #f])))

(define is-commentdos?
  (lambda(palabra)
    (cond
      [(empty? palabra) #f]
      [(regexp-match-exact? #rx"#[a-zA-Z]*" palabra) #t]
      [else #f])))

;;Se define resaltar_codigo_en_html que recibe una lista de strings y devuelve una cadena HTML que resalta código.
(define resaltar_codigo_en_html
  (lambda (lst)
    (cond
        [(null? lst) " "]
        [(is-integer? (car lst)) (string-append(string-append (string-append (string-append "<span class=integer> " (car lst)) " </span>") " ")(resaltar_codigo_en_html (cdr lst)))]
        [(is-reservada? (car lst)) (string-append(string-append (string-append (string-append "<span class=reservada> " (car lst)) " </span>") " ")(resaltar_codigo_en_html (cdr lst)))]
        [(is-operador? (car lst)) (string-append(string-append (string-append (string-append "<span class=operador> " (car lst)) " </span>") " ")(resaltar_codigo_en_html (cdr lst)))]
        [(is-delimitadorFuncion? (car lst)) (string-append(string-append (string-append (string-append "<span class=delimitadorFuncion> " (car lst)) " </span>") " ")(resaltar_codigo_en_html (cdr lst)))]
        [(is-decimal? (car lst)) (string-append(string-append (string-append (string-append "<span class=decimal> " (car lst)) " </span>") " ")(resaltar_codigo_en_html (cdr lst)))]
        [(is-identificador? (car lst)) (string-append(string-append (string-append (string-append "<span class=identificador> " (car lst)) " </span>") " ")(resaltar_codigo_en_html (cdr lst)))]
        [(is-comment? (car lst)) (string-append(string-append (string-append (string-append "<span class=comment> " (car lst)) " </span>") " ")(resaltar_codigo_en_html (cdr lst)))]
        [(is-commentdos? (car lst)) (string-append(string-append (string-append (string-append "<span class=commentdos> " (car lst)) " </span>") " ")(resaltar_codigo_en_html (cdr lst)))]
        [else (string-append " "(string-append (string-append (car lst) " ")(resaltar_codigo_en_html (cdr lst))))])))

;;Se define una variable html-head que contiene el código HTML de la página web que se va a generar.
(define encabezado_html (string-append "<!DOCTYPE html><html lang=\"es\"><head><meta charset=\"UTF-8\">
<title>Resaltador Lexico</title><link href=\"styles.css\" rel=\"stylesheet\"></head><body>" (resaltar_codigo_en_html (string-split contenido_archivo_br))))
(define html (string-append encabezado_html "</body></html>"))

;;Se define una variable que guarda el archivo HTML generado. Se utiliza la función write-file para escribir el contenido de la variable html.
(define archivo_salida (write-file "Evidencia1.html" html))