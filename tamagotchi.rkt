;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tamagotchi) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
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
     (define cuentaAcciones (make-vector 5 0))
     ;Los valores de felicidad o conteo de acciones no pueden bajar de 0.
     (define accion 0)
     ;Cuando el nivel de felicidad cae a 0 la mascota muere.
     ;La felicidad inicial es 3, lo máximo es 10.
     (define felicidad 3)
     ;Cada acción afecta el nivel de felicidad de la mascota.
     (define causaDeMuerte 0)
     (define estaEnfermo false)
     (define necesitaElBaño false)
     (define (sumaContadores num)
       (begin
         (vector-set! cuentaAcciones accion (+ 1 (vector-ref cuentaAcciones accion)))
         (if (eq? accion 0)
             (begin
               (vector-set! cuentaAcciones 1 (- (vector-ref cuentaAcciones 1) 1))
               (vector-set! cuentaAcciones 2 (- (vector-ref cuentaAcciones 2) 1))
               (vector-set! cuentaAcciones 3 (- (vector-ref cuentaAcciones 3) 1))
               )
             (if (eq? accion 1)
                 (begin
                   (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1))
                   (vector-set! cuentaAcciones 2 (- (vector-ref cuentaAcciones 2) 1))
                   (vector-set! cuentaAcciones 3 (- (vector-ref cuentaAcciones 3) 1))
                   )
                 (if (eq? accion 2)
                     (begin
                       (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1))
                       (vector-set! cuentaAcciones 1 (- (vector-ref cuentaAcciones 1) 1))
                       (vector-set! cuentaAcciones 3 (- (vector-ref cuentaAcciones 3) 1))
                       )
                     (if (eq? accion 3)
                         (begin
                           (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1))
                           (vector-set! cuentaAcciones 1 (- (vector-ref cuentaAcciones 1) 1))
                           (vector-set! cuentaAcciones 2 (- (vector-ref cuentaAcciones 2) 1)))
                         (if (eq? accion 4)
                             (begin
                               (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1))
                               (vector-set! cuentaAcciones 1 (- (vector-ref cuentaAcciones 1) 1))
                               (vector-set! cuentaAcciones 2 (- (vector-ref cuentaAcciones 2) 1))
                               (vector-set! cuentaAcciones 3 (- (vector-ref cuentaAcciones 3) 1)))
                             void)))))))
     (define (cerrar num)
       (begin
         (printf "\nEres un mal amo.")
         (cond
           [(eq? num 0) (display "\nTu mascota murió por comer demasiado.")]
           [(eq? num 1) (display "\nTu mascota murió de cansancio.")]
           )
         )
       )
     #|Cuando se alimenta a la mascota 1 vez su felicidad aumenta en 1|#
     #|2 veces la aumenta en 0 y 3 veces -1|#
     (define (comer)
       (begin
         ;(printf (vector-ref accs 0))
         (if
          (= (vector-ref cuentaAcciones accion) 0)
          (begin 
            (set! felicidad(+ felicidad 1))
            (display "\nBuena comida."))
          
          
          (if (= (vector-ref cuentaAcciones accion) 1)
              (display "\nEstoy satisfecho. ~n")
              
              (if (= (vector-ref cuentaAcciones accion) 2)
                  (begin
                    (set! felicidad(- felicidad 1))
                    (printf "~nNecesito ir al baño, no me siento muy bien.")
                    (set! necesitaElBaño true))
                  
                  (set! causaDeMuerte accion)
                  
                  )   
              )
          )
         (sumaContadores accion)))
     ;En el último caso es necesario llevarlo al baño y aumentará su felicidad en 1
     ;sino lo lleva inmediatamente le disminuye en 2 la felicidad.
     
     ;Jugar una vez aumenta la felicidad en 2, jugar 2 veces le aumenta en 1 y 3 veces no le aumenta ni disminuye felicidad
     ;pero le produce hambre disminuyendo su conteo de alimento en 2.    
     (define (jugar)
       (begin
         ;(printf (vector-ref accs 0))
         (if
          (= (vector-ref cuentaAcciones accion) 0)
          (begin 
            (set! felicidad(+ felicidad 2))
            (display "\nQue divertido."))
          
          
          (if (= (vector-ref cuentaAcciones accion) 1)
              (begin
                (set! felicidad (+ felicidad 1))
                (display "\nEstoy cansado."))
              
              (if (= (vector-ref cuentaAcciones accion) 2)
                  (begin
                    (printf "~nTengo hambre.")
                    (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1))
                    )
                  
                  (set! causaDeMuerte accion)
                  
                  )   
              )
          )
         (sumaContadores accion)
         ))
     
     ;(print "incompleto"))
     
     ;Escuchar música una vez aumenta en 2 la felicidad, otra vez le aumenta en 1, y una tercera vez disminuye en 1, lo enferma, debe curarlo.
     (define (musica)
       (print "incompleto"))
     
     ;El baño le aumenta la felicidad en 2, la segunda vez no lo afecta y la tercera le disminuye en 1 enfermándolo, debe curarlo.
     (define (baño)
       (print "incompleto"))
     
     ;Curar una vez le aumenta la felicidad en 3 y más veces no le afecta.
     
     (define (curar)
       (print "incompleto"))
     
     (define mostrarmenu #f)
     (define (menu)
       (if (or (=(vector-ref cuentaAcciones 0) 4)
               (=(vector-ref cuentaAcciones 1) 4)
               (=(vector-ref cuentaAcciones 2) 4)
               (=(vector-ref cuentaAcciones 3) 4)
               (= felicidad 0))
           (cerrar causaDeMuerte)
           (begin
             (if (boolean=? mostrarmenu #f)
                 (begin
                   (printf "~n")
                   (printf "~n******************~n")
                   (printf "****TAMAGOTCHI****~n")
                   (printf "******************~n")
                   (printf "*******MENU*******~n")
                   (printf "******************~n")
                   (set! mostrarmenu #t))
                 void)
             
             (printf "el estado actual de tu tamagotchi es: ~n")
             
             (display "felicidad: ")
             (display felicidad)
             (printf "~n")
             (display "comida: ")
             (display (vector-ref cuentaAcciones 0))
             (printf "~n")
             (display "juego: ")
             (display (vector-ref cuentaAcciones 1))
             (printf "~n")
             (display "musica: ")
             (display (vector-ref cuentaAcciones 2))
             (printf "~n")
             (display "baño: ")
             (display (vector-ref cuentaAcciones 3))
             (printf "~n")
             (display "salir")
             (display)
             
             
             (begin (printf "~n1. Comer~n")
                    (printf "2. jugar~n")
                    (printf "3. Escuchar música~n")
                    (printf "4. Ir al baño~n")
                    (printf "5. Curar~n")
                    (set! accion (- (read) 1))
                    (cond
                      [(= 0 accion) (begin (comer)
                                           (menu))]
                      [(= 1 accion) (begin (jugar)
                                           (menu))]
                      [(= 2 accion) (begin (musica)
                                           (menu))]
                      [(= 3 accion) (begin (baño)
                                           (menu))]
                      [(= 4 accion) (begin (curar)
                                           (menu))]
                      [else (begin
                              (printf "no entiendo que dices")
                              (menu))])))))
     ;Tras cada acción se debe mostrar el nivel de felicidad.
     )(menu)))
(tamagotchi)