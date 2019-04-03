;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tamagotchi) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
  PROYECTO FDP - TAMAGOTCHI
  Archivo: Tamagotchi.rkt

  Autores:                            Código

          Nicolás Jaramillo M.        1840558
          Mateo Obando G.             1844983
          Andrés Pineda C.            1843660

  Fecha creación: 2019/04/01
  Fecha última modificación: 2019/04/01
  Versión: 0.1.2
|#

(define (tamagotchi)
  (local
    (
;                         Variables
     (define accs (vector "comer" "jugar" "musica" "baño" "curar"))
     (define cuentaAcciones (make-vector 5 0))
     (define accion 0)
     (define felicidad 3)
     (define causaDeMuerte accion)
     (define estaEnfermo false)
     (define necesitaElBaño false)
     
#|                        Funciones
    PrintfNveces:
      Imprime un mensaje N numero de veces.|#
     (define (printfNVeces mensaje N)
       (cond
         [(eq? N 0) void]
         [else (begin
                 (printf mensaje)
                 (printfNVeces mensaje (- N 1))
               )
         ]
       )
     )

#|  felicidad+:
      Suma N a la felicidad pero no permite que el valor de felicidad supere a 10.|#
     (define (felicidad+ N)
       (begin
         (set! felicidad (+ felicidad N))
         (if (> felicidad 10)
             (set! felicidad 10)
             void
         )
       )
     )
     
     #|  SumaContadores:
      Suma en 1 al contador de la acción que se ejecuta y le resta 1 a las demás.|#
     (define (sumaContadores num)
       (begin
         (if (= (vector-ref cuentaAcciones 4) 4) ;Esto evita que el contador de curar sea mayor a 4.
             void
             (vector-set! cuentaAcciones accion (+ (vector-ref cuentaAcciones accion) 2)))
         (if (= (vector-ref cuentaAcciones 0) 0)
             void
             (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1)))
         (if (= (vector-ref cuentaAcciones 1) 0)
             void
             (vector-set! cuentaAcciones 1 (- (vector-ref cuentaAcciones 1) 1)))
         (if (= (vector-ref cuentaAcciones 2) 0)
             void
             (vector-set! cuentaAcciones 2 (- (vector-ref cuentaAcciones 2) 1)))
         (if (= (vector-ref cuentaAcciones 3) 0)
             void
             (vector-set! cuentaAcciones 3 (- (vector-ref cuentaAcciones 3) 1)))
         (if (= (vector-ref cuentaAcciones 4) 0)
             void
             (vector-set! cuentaAcciones 4 (- (vector-ref cuentaAcciones 4) 1)))
         )
       )
     #|-------------------------------------------------------------------------------------------------|#
     #|  Cerrar:
      Finaliza el programa, ya sea porque la mascota muere o el jugador se retira.|#
     (define (cerrar num)
       (begin
         (printf "\nEres un mal amo.")
         (cond
           [(eq? num 0) (display "\nTu mascota murió por que su estómago explotó.")]
           [(eq? num 1) (display "\nTu mascota murió de cansancio.")]
           [(eq? num 2) (display "\nTu mascota murió por derrame cerebral.")]
           [(eq? num 3) (display "\nTu mascota murió por falla renal.")]
           [(eq? num 5) (display "\nHas abandonado a tu mascota.")]
           [else void]
           )
         )
       )
     #|-------------------------------------------------------------------------------------------------|#
     (define (comer)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin 
               (felicidad+ 1)
               (printf "\nBuena comida."))
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (printf "\nEstoy satisfecho.")
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (set! felicidad(- felicidad 1))
                       (printf "\nNecesito ir al baño, no me siento muy bien.")
                       (set! necesitaElBaño true))
                     (set! causaDeMuerte accion))
                 )
             )
         (sumaContadores accion)
         )
       )
     ;si no lo lleva inmediatamente le disminuye en 2 la felicidad.
     #|-------------------------------------------------------------------------------------------------|#
     (define (jugar)
       (begin
         ;(printf (vector-ref accs 0))
         (if
          (= (vector-ref cuentaAcciones accion) 0)
          (begin 
            (felicidad+ 2)
            (display "\nQue divertido."))
          (if (= (vector-ref cuentaAcciones accion) 1)
              (begin
                (felicidad+ 1)
                (display "\nEstoy cansado."))
              (if (= (vector-ref cuentaAcciones accion) 2)
                  (begin
                    (printf "~nTengo hambre.")
                    (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1)))
                  (set! causaDeMuerte accion))))
         (sumaContadores accion)))
     #|-------------------------------------------------------------------------------------------------|#
     ;Escuchar música una vez aumenta en 2 la felicidad, otra vez le aumenta en 1, y una tercera vez disminuye en 1, lo enferma, debe curarlo.
     (define (musica)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin 
               (felicidad+ 2)
               (display "\nTiene buen ritmo."))
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (felicidad+ 1)
                   (display "\nEstoy satisfecho."))
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (set! felicidad(- felicidad 1))
                       (printf "\nNecesito curarme, me duele la cabeza."))
                  (set! causaDeMuerte accion))))
         (sumaContadores accion)
         )
       )
     #|-------------------------------------------------------------------------------------------------|#
     ;El baño le aumenta la felicidad en 2, la segunda vez no lo afecta y la tercera le disminuye en 1 enfermándolo, debe curarlo.
     (define (baño)
       (begin
         (if
          (= (vector-ref cuentaAcciones accion) 0)
          (begin
            (felicidad+ 2)
            (printf "~nYa estoy limpio"))
          (if (= (vector-ref cuentaAcciones accion) 1)
              (printf "~nEstoy satisfecho")
              (if (= (vector-ref cuentaAcciones accion)2)
                  (begin
                    (set! felicidad (- felicidad 1))
                    (set! estaEnfermo #t)
                    (printf "~nTengo gripa, cúrame"))
                  (set! causaDeMuerte accion))))
         (sumaContadores accion)))
     #|-------------------------------------------------------------------------------------------------|#
     ;Curar una vez le aumenta la felicidad en 3 y más veces no le afecta.
     (define (curar)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (felicidad+ 3)
             void)
         (display "\nEstoy sano.")
         (set! estaEnfermo false)
         (sumaContadores accion)))
     #|-------------------------------------------------------------------------------------------------|#
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
                   (printf "\n
El estado actual de tu tamagotchi es: \n")
                   (display "
felicidad:
╔══════════╗
║")
                   (printfNVeces "█" felicidad)
                   (printfNVeces " " (- 10 felicidad))
                   (display "║
╚══════════╝ ")
                   ;(display felicidad)
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
                   (display "\n \n
╔═════════════════════════╗
║                         ║
║        TAMAGOTCHI       ║
║                         ║
║          MENU           ║
║                         ║
║       1. Comer          ║
║       2. jugar          ║
║   3. Escuchar música    ║
║     4. Ir al baño       ║
║       5. Curar          ║
║       6. Salir          ║
║                         ║
╚═════════════════════════╝\n")
                   ;(set! mostrarmenu #t)
                   )
                 void
                 )
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
               [(= 5 accion) (begin (cerrar 5))]
               [else (begin
                       (printf "no entiendo que dices")
                       (menu))]
               )
             )
           )
       )
     ;Tras cada acción se debe mostrar el nivel de felicidad.
     )
    (menu)
    )
  )

(tamagotchi)