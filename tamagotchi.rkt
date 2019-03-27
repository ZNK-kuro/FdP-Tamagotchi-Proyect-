;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tamagotchi) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
#|PROYECTO FDP|#
#|INTEGRANTES:|#
#|
-ANDRÉS PINEDA CORTEZ    -1843660
-MATEO OBANDO GUTIERREZ  -1844983
-NICOLÁS JARAMILLO MAYOR -1840558
|#

(define (tamagotchi)
  (local
    (
     ;Las acciones posibles son: comer, jugar, escuchar música, ir al baño y curar.
     ;Si el contador de alguna acción llega a 4 la mascota morirá, con excepción de curar.
     (define accs (vector "comer" "jugar" "musica" "baño" "curar"))
    
     ;Cuando se cambia de una acción A a otra B, se disminuye en 1 el conteo de las otras acciones de manera automática.
     (define contval (make-vector 5 0))
     ;Los valores de felicidad o conteo de acciones no pueden bajar de 0.
     (define accion 0)
     ;Cuando el nivel de felicidad cae a 0 la mascota muere.
     ;La felicidad inicial es 3, lo máximo es 10.
     (define felicidad 3)
     ;Cada acción afecta el nivel de felicidad de la mascota.

     (define (cerrar)
       (print "eres un mal amo"))
     #|Cuando se alimenta a la mascota 1 vez su felicidad aumenta en 1|#
     #|2 veces la aumenta en 0 y 3 veces -1|#
     (define (comer)
       (begin
              (printf (vector-ref accs 0))
              (printf "~n")
              (printf "la felicidad es de: ")
              (if
               (= (vector-ref contval 0) 0)
               (begin(set! felicidad(+ felicidad 1))
                     (display felicidad))
               
               (if (= (vector-ref contval 0) 1)
                   (display felicidad)
                   
                   (if (= (vector-ref contval 0) 2)
                       (begin(set! felicidad(- felicidad 1))
                              (display felicidad)
                              (printf "~n necesito ir al baño, rápido"))
                       
                       )))
              (vector-set! contval 0 (+ 1 (vector-ref contval 0)))
              ))
     ;En el último caso es necesario llevarlo al baño y aumentará su felicidad en 1
     ;sino lo lleva inmediatamente le disminuye en 2 la felicidad.
     
     ;Jugar una vez aumenta la felicidad en 2, jugar 2 veces le aumenta en 1 y 3 veces no le aumenta ni disminuye felicidad
     ;pero le produce hambre disminuyendo su conteo de alimento en 2.    
     (define (jugar)
       (print "incompleto"))

     ;Escuchar música una vez aumenta en 2 la felicidad, otra vez le aumenta en 1, y una tercera vez disminuye en 1, lo enferma, debe curarlo.
     (define (musica)
       (print "incompleto"))

     ;El baño le aumenta la felicidad en 2, la segunda vez no lo afecta y la tercera le disminuye en 1 enfermándolo, debe curarlo.
     (define (baño)
       (print "incompleto"))

     ;Curar una vez le aumenta la felicidad en 3 y más veces no le afecta.
    
     (define (curar)
       (print "incompleto"))

     
     (define (menu)
       (begin 
               (printf "~n")
               (printf "~n******************~n")
               (printf "****TAMAGOTCHI****~n")
               (printf "******************~n")
               (printf "*******MENU*******~n")
               (printf "******************~n")
               (printf "el estado actual de tu tamagotchi es: ~n")
               (display "comida: ")
               (display (vector-ref contval 0))
               (printf "~n")
               (display "juego: ")
               (display (vector-ref contval 1))
               (printf "~n")
               (display "musica: ")
               (display (vector-ref contval 2))
               (printf "~n")
               (display "baño: ")
               (display (vector-ref contval 3))
               (printf "~n")
               (display "felicidad: ")
               (display felicidad)
               (printf "~n")
               (if (or (=(vector-ref contval 0) 4)
                       (=(vector-ref contval 1) 4)
                       (=(vector-ref contval 2) 4)
                       (=(vector-ref contval 3) 4)
                       (= felicidad 0))
                   (cerrar)
                  
                   (begin (printf "~n1. Comer~n")
                          (printf "2. jugar~n")
                          (printf "3. Escuchar música~n")
                          (printf "4. Ir al baño~n")
                          (printf "5. Curar~n")
                          (set! accion (read))
                          (cond
                            [(= 1 accion) (begin (comer)
                                                 (menu))]
                            [(= 2 accion) (begin (jugar)
                                                 (menu))]
                            [(= 3 accion) (begin (musica)
                                                 (menu))]
                            [(= 4 accion) (begin (baño)
                                                 (menu))]
                            [(= 5 accion) (begin (curar)
                                                 (menu))]
                            [else "no entiendo que dices"]))
               ))
     )
    ;Tras cada acción se debe mostrar el nivel de felicidad.
    
    )(menu)))


(tamagotchi)