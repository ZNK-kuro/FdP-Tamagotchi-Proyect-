;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Tamagotchi) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
  PROYECTO FDP - TAMAGOTCHI
  Archivo: Tamagotchi.rkt

  Autores:                            Código

          Nicolás Jaramillo M.        1840558
          Mateo Obando G.             1844983
          Andrés Pineda C.            1843660

  Fecha creación: 2019/04/01
  Fecha última modificación: 2019/04/04
  Versión: 0.1.2.1
|#

(define (tamagotchi)
  (local
    (
;                         Variables
     (define accs (vector "comer" "jugar" "musica" "baño" "curar"))
     (define cuentaAcciones (make-vector 5 0))
     (define accion 0)
     (define felicidad 3)
     (define diversion 4)
     (define energia 7)
     (define hambre 3)
     (define higiene 7)
     (define vejiga 7)
     (define mensaje "Hola!")
     (define causaDeMuerte accion)
     (define estaEnfermo false)
     (define necesitaElBaño false)
     
#|                        Funciones
   printfNveces:
     Imprime un mensaje N numero de veces.|#
     (define (printfNVeces msj N)
       (cond
         [(eq? N 0) void]
         [else (begin
                 (printf msj)
                 (printfNVeces msj (- N 1))
               )
         ]
       )
     )

#| felicidad+:
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

#| diversion+:
     Suma N a la diversion pero no permite que el valor de diversion supere a 10.|#
     (define (diversion+ N)
       (begin
         (set! diversion (+ diversion N))
         (if (> diversion 10)
             (set! diversion 10)
             void
         )
       )
     )

#| energia+:
     Suma N a la energia pero no permite que el valor de energia supere a 10.|#
     (define (energia+ N)
       (begin
         (set! energia (+ energia N))
         (if (> energia 10)
             (set! energia 10)
             void
         )
       )
     )

#| hambre+:
     Suma N a la hambre pero no permite que el valor de hambre supere a 10.|#
     (define (hambre+ N)
       (begin
         (set! hambre (+ hambre N))
         (if (> hambre 10)
             (set! hambre 10)
             void
         )
       )
     )

#| higiene+:
     Suma N a la higiene pero no permite que el valor de higiene supere a 10.|#
     (define (higiene+ N)
       (begin
         (set! higiene (+ higiene N))
         (if (> higiene 10)
             (set! higiene 10)
             void
         )
       )
     )

#| vejiga+:
     Suma N a la vejiga pero no permite que el valor de vejiga supere a 10.|#
     (define (vejiga+ N)
       (begin
         (set! vejiga (+ vejiga N))
         (if (> vejiga 10)
             (set! vejiga 10)
             void
         )
       )
     )
     
#| SumaContadores:
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

#| Cerrar:
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
               (set! mensaje "Buena comida."))
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (set! mensaje "Estoy satisfecho.")
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (set! felicidad(- felicidad 1))
                       (set! mensaje "Necesito ir al baño, no me siento muy bien.")
                       (set! necesitaElBaño true))
                     (set! causaDeMuerte accion)
                 )
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
            (set! mensaje "Que divertido."))
          (if (= (vector-ref cuentaAcciones accion) 1)
              (begin
                (felicidad+ 1)
                (set! mensaje "Estoy cansado."))
              (if (= (vector-ref cuentaAcciones accion) 2)
                  (begin
                    (set! mensaje "Tengo hambre.")
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
               (set! mensaje "Tiene buen ritmo."))
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (felicidad+ 1)
                   (set! mensaje "Estoy satisfecho."))
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (set! felicidad(- felicidad 1))
                       (set! mensaje "Necesito curarme, me duele la cabeza."))
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
            (set! mensaje "Ya estoy limpio"))
          (if (= (vector-ref cuentaAcciones accion) 1)
              (set! mensaje "Estoy satisfecho")
              (if (= (vector-ref cuentaAcciones accion)2)
                  (begin
                    (set! felicidad (- felicidad 1))
                    (set! estaEnfermo #t)
                    (set! mensaje "Tengo gripa, cúrame"))
                  (set! causaDeMuerte accion))))
         (sumaContadores accion)))
     #|-------------------------------------------------------------------------------------------------|#
     ;Curar una vez le aumenta la felicidad en 3 y más veces no le afecta.
     (define (curar)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (felicidad+ 3)
             void)
         (set! mensaje "Estoy sano.")
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
                   (printf "\n\n\n\n\n\n\n\n\n\n\n\n\n\n
╔════════════════════════════════════════╗
║                 ESTADO                 ║
║                                        ║
║    Felicidad               Hambre      ║
║   ╔══════════╗          ╔══════════╗   ║
║   ║") (printfNVeces "█" felicidad) (printfNVeces " " (- 10 felicidad)) (display "║          ║") (printfNVeces "█" hambre) (printfNVeces " " (- 10 hambre)) (display "║   ║
║   ╚══════════╝          ╚══════════╝   ║
║                                        ║
║    Diversión               Vejiga      ║
║   ╔══════════╗          ╔══════════╗   ║
║   ║") (printfNVeces "█" diversion) (printfNVeces " " (- 10 diversion)) (display "║          ║") (printfNVeces "█" vejiga) (printfNVeces " " (- 10 vejiga)) (display "║   ║
║   ╚══════════╝          ╚══════════╝   ║
║                                        ║
║     Higiene               Energía      ║
║   ╔══════════╗          ╔══════════╗   ║
║   ║") (printfNVeces "█" higiene) (printfNVeces " " (- 10 higiene)) (display "║          ║") (printfNVeces "█" energia) (printfNVeces " " (- 10 energia)) (display "║   ║
║   ╚══════════╝          ╚══════════╝   ║
╚════════════════════════════════════════╝
")
                   (display "comida: ")
                   (display (vector-ref cuentaAcciones 0))
                   (printf "  ")
                   (display "juego: ")
                   (display (vector-ref cuentaAcciones 1))
                   (printf "  ")
                   (display "musica: ")
                   (display (vector-ref cuentaAcciones 2))
                   (printf "  ")
                   (display "baño: ")
                   (display (vector-ref cuentaAcciones 3))
                   (display"

 /\\_/\\
( o.o ) < ") (display mensaje) (display "1
 > ^ <
")
                   (display "
╔════════════════════════════════════════╗
║           TAMAGOTCHI    MENU           ║
║                                        ║
║   1. Comer        4. Ir al baño        ║
║   2. jugar        5. Curar             ║
║   3. Escuchar     6. Salir             ║
║      música                            ║
╚════════════════════════════════════════╝\n")
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