#lang racket

(define is-reservada?
  (lambda (palabra)
      (cond
          [(empty? palabra) #f] ; Comprueba si la palabra está vacía
          [(regexp-match-exact? #rx"^<span" palabra) #f] ; Comprueba si la palabra ya tiene una etiqueta HTML
          [(regexp-match-exact? #rx"break|case|class|const|continue|default|delete|do|else|for|i f|in|new|return|switch|this|throw|try|void|while" palabra) #t] ; Comprueba si la palabra es una palabra reservada
          [else #f])))

(define is-number?
  (lambda (palabra)
      (cond
          [(empty? palabra) #f] ; Comprueba si la palabra está vacía
          [(regexp-match-exact? #rx"^<span" palabra) #f] ; Comprueba si la palabra ya tiene una etiqueta HTML
          [(regexp-match-exact? #rx"^-?[0-9]+(\\.[0-9]+)?$" palabra) #t] ; Comprueba si la palabra es un número
          [else #f])))

(define is-operador?
  (lambda (palabra)
      (cond
          [(empty? palabra) #f]
          [(regexp-match-exact? #rx"[+][=]|\"\"?|[']|[[][]]|[{][}]|[(][)]|[=][!]|[>][=]|[<][=]|[<][>]|[=][=]|[|][|]|[&][&]|[<]|[>]|[$]|[(]|[)]|[{]|[}]|[[]|[]]|[&]|[|]|[,]|[:]|[;]|[+][+]|[-][-]|[][]|[+]|[-]|[*]|[/]|[%]|[!]|[=]|&lt|&gt|&lt{1}|&gt&gt|&lt=|&gt=" palabra) #t]
          [else #f])))

(define is-identificador?
  (lambda (palabra)
      (cond
          [(empty? palabra) #f] ; Comprueba si la palabra está vacía
          [(regexp-match-exact? #rx"^<span" palabra) #f] ; Comprueba si la palabra ya tiene una etiqueta HTML
          [(regexp-match-exact? #rx"[a-zA-Z_]+[a-zA-Z_0-9]*;?,?" palabra) #t] ; Comprueba si la palabra es un identificador
          [else #f])))

(define is-comment?
  (lambda (palabra)
      (cond
          [(empty? palabra) #f] ; Comprueba si la palabra está vacía
          [(regexp-match-exact? #rx"^<span" palabra) #f] ; Comprueba si la palabra ya tiene una etiqueta HTML
          [(regexp-match-exact? #rx"//[a-zA-Z]*" palabra) #t] ; Comprueba si la palabra es un comentario de una línea
          [else #f])))

(define is-commentdos?
  (lambda(palabra)
      (cond
          [(empty? palabra) #f] ; Comprueba si la palabra está vacía
          [(regexp-match-exact? #rx"^<span" palabra) #f] ; Comprueba si la palabra ya tiene una etiqueta HTML
          [(regexp-match-exact? #rx"#[a-zA-Z]*" palabra) #t] ; Comprueba si la palabra es un comentario de varias líneas
          [else #f])))

(define (resaltar_token token)
  (cond
    [(is-reservada? token) ; Comprueba si el token es una palabra reservada
     (string-append "<span class='reservada'>" token "</span>")]
    [(is-number? token) ; Comprueba si el token es un número
     (string-append "<span class='decimal'>" token "</span>")]
    [(is-operador? token) ; Comprueba si el token es un operador
     (string-append "<span class='operador'>" token "</span>")]
    [(is-identificador? token) ; Comprueba si el token es un identificador
     (string-append "<span class='identificador'>" token "</span>")]
    [(is-comment? token) ; Comprueba si el token es un comentario de una línea
     (string-append "<span class='comment'>" token "</span>")]
    [(is-commentdos? token) ; Comprueba si el token es un comentario de varias líneas
     (string-append "<span class='commentdos'>" token "</span>")]
    [else token]))

(define (resaltar_linea linea)
  (define resaltada
    (regexp-replace* #rx"[0-9]+" linea 
                     (lambda (match) (string-append " " match " "))))
  (define (aux chars token result)
    (cond
      [(null? chars) (string-append result (resaltar_token token))]
      [(char-whitespace? (car chars)) (aux (cdr chars) "" (string-append result (resaltar_token token) " "))]
      [(is-operador? (string (car chars))) (aux (cdr chars) "" (string-append result (resaltar_token token) (resaltar_token (string (car chars)))))]
      [else (aux (cdr chars) (string-append token (string (car chars))) result)]))
  (aux (string->list resaltada) "" ""))


(define (generar-archivo-de-salida lineas filename)
  (define html (string-join lineas "<pre>")) ; Une las líneas resaltadas en una cadena única con etiquetas "<pre>"
  (define html-with-nbsp (regexp-replace* #rx" " html " ")) ; Reemplaza los espacios en blanco por la entidad HTML "&nbsp;" en la cadena HTML
  (call-with-output-file (string-append "Evidencia2_" filename ".html") 
    #:exists 'replace 
    (lambda (out) 
      (display (string-append "<html><head><link rel='stylesheet' type='text/css' href='styles.css'></head><body>" html-with-nbsp "</body></html>") out)))) ; Crea un archivo de salida con el contenido HTML generado


(define (procesar-archivo filename)
  (define lineas (map resaltar_linea (file->lines filename))) ; Lee el archivo línea por línea y aplica la función resaltar_linea a cada línea
  (generar-archivo-de-salida lineas filename)) ; Genera el archivo de salida con las líneas resaltadas


(define (tiempo-ejecucion)
  (let ([inicio (current-inexact-milliseconds)])
    (define files '("p1.txt" "p2.txt" "p3.txt" "p4.txt")) 
    (define threads (map (lambda (f) (thread (lambda () (procesar-archivo f)))) files)) 
    (map thread-wait threads) 
    (let ([final (current-inexact-milliseconds)]) 
      (- final inicio))))

(display (string-append "El tiempo de ejecución fue: " (number->string (tiempo-ejecucion)) " ms"))