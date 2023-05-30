#lang racket
(require 2htdp/batch-io)

;(define ruta_archivo "python.py")
(define ruta_archivo "mix.txt")
;(define ruta_archivo "malo.txt")
;(define ruta_archivo "malo2")

(define is-reservada?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"break|case|class|const|continue|default|delete|do|else|for|if|in|new|return|switch|this|throw|try|void|while" palabra) #t]
            [else #f])))

(define is-number?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"^-?[0-9]+(\\.[0-9]+)?$" palabra) #t]
            [else #f])))

(define is-operador?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"[+][=]|\"\"?|[']|[[][]]|[{][}]|[(][)]|[=][!]|[>][=]|[<][=]|[<][>]|[=][=]|[|][|]|[&][&]|[<]|[>]|[$]|[(]|[)]|[{]|[}]|[[]|[]]|[&]|[|]|[,]|[:]|[;]|[+][+]|[-][-]|[*][*]|[+]|[-]|[*]|[/]|[%]|[!]|[=]|&lt|&gt|&lt{1}|&gt&gt|&lt=|&gt=" palabra) #t]
            [else #f])))

(define is-identificador?
    (lambda (palabra)
        (cond
            [(empty? palabra) #f]
            [(regexp-match-exact? #rx"[a-zA-Z_]+[a-zA-Z_0-9]*;?,?" palabra) #t]
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

(define (resaltar_token token)
  (cond
    [(is-reservada? token)
     (string-append "<span class='reservada'>" token "</span>")]
    [(is-number? token)
     (string-append "<span class='decimal'>" token "</span>")]
    [(is-operador? token)
     (string-append "<span class='operador'>" token "</span>")]
    [(is-identificador? token)
     (string-append "<span class='identificador'>" token "</span>")]
    [(is-comment? token)
     (string-append "<span class='comment'>" token "</span>")]
    [(is-commentdos? token)
     (string-append "<span class='commentdos'>" token "</span>")]
    [else token]))

(define (resaltar_linea linea)
  (string-join (map resaltar_token (regexp-split #rx" " (regexp-replace #rx"\t" linea "    "))) " "))

(define (leer-archivo-como-lineas filename)
  (map resaltar_linea (read-lines filename)))

(define (generar-archivo-de-salida lineas)
  (define html (string-join lineas "<pre>"))
  (define html-with-nbsp (regexp-replace* #rx" " html " "))
  (write-file "Evidencia1Final.html" (string-append "<html><head><link rel='stylesheet' type='text/css' href='styles.css'></head><body>" html-with-nbsp "</body></html>")))

(define (procesar-archivo ruta_archivo)
  (let ([lineas (leer-archivo-como-lineas ruta_archivo)])
    (generar-archivo-de-salida lineas)))

(define (medir-tiempo funcion)
  (let ([inicio (current-process-milliseconds)])
    (funcion)
    (let ([fin (current-process-milliseconds)])
      (- fin inicio))))

(define tiempo-procesamiento (medir-tiempo (lambda () (procesar-archivo ruta_archivo))))

(printf "Tiempo de procesamiento: ~a ms\n" tiempo-procesamiento)