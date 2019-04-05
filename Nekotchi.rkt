;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Nekotchi) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
  PROYECTO FDP - Nekotchi
  Archivo: Nekotchi.rkt

  Autores:                            Código

          Nicolás Jaramillo M.        1840558
          Mateo Obando G.             1844983
          Andrés Pineda C.            1843660

  Fecha creación: 2019/04/01
  Fecha última modificación: 2019/04/04
  Versión: 0.2
|#

(define (nekotchi)
  (local
    (
;                         Variables
     (define cuentaAcciones (make-vector 7 0))
     (define accion 0)
     (define felicidad 3)
     (define energia 7)
     (define diversion 4)
     (define hambre 3)
     (define higiene 7)
     (define vejiga 7)
     (define mensaje1 "¡Hola!")
     (define mensaje2 "¡Gusto en conocerte!")
     (define causaDeMuerte 0)
     (define estaEnfermo false)
     (define necesitaElBaño false)
     (define bienvenida true)

;                         Gatos
     (define Gbienvenida1 "

  /)    |\\___/|  ")
     (define Gbienvenida2 "
 ((    _|  o o|. ")
     (define Gbienvenida3 "
╔═══════)))═══)))════════════════════════╗")
     
     (define Gnormal1-1 "

       /\\_/\\
      ( o.o )  ")
     (define Gnormal1-2 "
       > ^ <   ")
     
     (define Gnormal2-1 "
             /)
    /\\___/\\ ((   ")
     (define Gnormal2-2 "
    \\`*_*'/  ))  ")
     (define Gnormal2-3 "
╔═══{_}═══{_}════════════════════════════╗")
     (define Gmuerto "

       /\\_/\\
      ( x.x )  
       > ^ <   ")
     
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
     
#| salud+:
     Suma N a la salud pero no permite que el valor de salud supere a 10.|#
     (define (energia+ N)
       (begin
         (set! energia (+ energia N))
         (if (> energia 10)
             (set! energia 10)
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
         (if (= (vector-ref cuentaAcciones 6) 4) ;Esto evita que el contador de curar sea mayor a 4.
             void
             (vector-set! cuentaAcciones accion (+ (vector-ref cuentaAcciones accion) 2)))
         (if (= (vector-ref cuentaAcciones 0) 0)
             void
             (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1))
         )
         (if (= (vector-ref cuentaAcciones 1) 0)
             void
             (vector-set! cuentaAcciones 1 (- (vector-ref cuentaAcciones 1) 1))
         )
         (if (= (vector-ref cuentaAcciones 2) 0)
             void
             (vector-set! cuentaAcciones 2 (- (vector-ref cuentaAcciones 2) 1))
         )
         (if (= (vector-ref cuentaAcciones 3) 0)
             void
             (vector-set! cuentaAcciones 3 (- (vector-ref cuentaAcciones 3) 1))
         )
         (if (= (vector-ref cuentaAcciones 4) 0)
             void
             (vector-set! cuentaAcciones 4 (- (vector-ref cuentaAcciones 4) 1))
         )
         (if (= (vector-ref cuentaAcciones 5) 0)
             void
             (vector-set! cuentaAcciones 5 (- (vector-ref cuentaAcciones 5) 1))
         )
         (if (= (vector-ref cuentaAcciones 6) 0)
             void
             (vector-set! cuentaAcciones 6 (- (vector-ref cuentaAcciones 6) 1))
         )
         (if (= (vector-ref cuentaAcciones 7) 0)
             void
             (vector-set! cuentaAcciones 7 (- (vector-ref cuentaAcciones 7) 1))
         )
       )
     )



#| comer:
     |#
     (define (comer)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin 
               (felicidad+ 1)
               (set! mensaje1 "¡Buena comida!")
             )
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (set! mensaje1 "Estoy satisfecho.")
                 )
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (set! felicidad(- felicidad 1))
                       (set! mensaje1 "Necesito ir al baño,")
                       (set! mensaje2 "no me siento muy bien :S")
                       (set! necesitaElBaño true))
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores accion)
       )
     )
     ;si no lo lleva inmediatamente le disminuye en 2 la felicidad.

#| baño:
     |#
     (define (baño)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin
               (felicidad+ 2)
               (set! mensaje1 "Ya estoy limpio")
             )
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (set! mensaje1 "Estoy satisfecho")
                 (if (= (vector-ref cuentaAcciones accion)2)
                     (begin
                       (set! felicidad (- felicidad 1))
                       (set! estaEnfermo #t)
                       (set! mensaje1 "Tengo gripa, cúrame")
                     )
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores accion)
       )
     )
#| jugar:
     |#
     (define (jugar)
       (begin
         ;(printf (vector-ref accs 0))
         (if
          (= (vector-ref cuentaAcciones accion) 0)
          (begin 
            (felicidad+ 2)
            (set! mensaje1 "¡YAY!")
            (set! mensaje2 "¡Qué divertido!"))
          (if (= (vector-ref cuentaAcciones accion) 1)
              (begin
                (felicidad+ 1)
                (set! mensaje1 "Estoy cansado...")
              )
              (if (= (vector-ref cuentaAcciones accion) 2)
                  (begin
                    (set! mensaje1 "¡Tengo hambre!")
                    (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1)))
                  (set! causaDeMuerte accion))))
         (sumaContadores accion)))

#| dormir:
     |#
     (define (dormir)
       void
     )
#| bañar:
     |#
     (define (bañar)
       void
     )
     ;Escuchar música una vez aumenta en 2 la felicidad, otra vez le aumenta en 1, y una tercera vez disminuye en 1, lo enferma, debe curarlo.
     (define (musica)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin 
               (felicidad+ 2)
               (set! mensaje1 "¡Woo!")
               (set! mensaje2 "Tiene buen ritmo"))
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (felicidad+ 1)
                   (set! mensaje1 "Estuvo bien")
                 )
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (set! felicidad(- felicidad 1))
                       (set! mensaje1 "Me duele la cabeza")
                     )
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores accion)
       )
     )
#| curar:
     |#
     (define (curar)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (felicidad+ 3)
             void)
         (set! mensaje1 "Estoy sano.")
         (set! estaEnfermo false)
         (sumaContadores accion)))

#| cerrar:
     Finaliza el programa, ya sea porque la mascota muere o el jugador se retira.|#
     (define (cerrar num)
       (begin
         (printf "\nEres un mal amo.")
         (cond
           [(eq? num 0) (display "\nTu mascota murió por que su estómago explotó.")]
           [(eq? num 1) (display "\nTu mascota murió de cansancio.")]
           [(eq? num 2) (display "\nTu mascota murió por derrame cerebral.")]
           [(eq? num 3) (display "\nTu mascota murió por falla renal.")]
           [(eq? num 4) (display "\nTu mascota murió de ???")]
           [(eq? num 5) (display "\nTu mascota murió de ???")]
           [(eq? num 6) (display "\nTu mascota murió de ???")]
           [(eq? num 7) (display "\nHas abandonado a tu mascota.")]
           [else void]
         )
         (display Gmuerto)
       )
     )
#|-------------------------------------------------------------------------------------------------|#
     
     (define (menu)
       (if (or (=(vector-ref cuentaAcciones 0) 4)
               (=(vector-ref cuentaAcciones 1) 4)
               (=(vector-ref cuentaAcciones 2) 4)
               (=(vector-ref cuentaAcciones 3) 4)
               (= felicidad 0))
           (cerrar causaDeMuerte)
           (begin
             (if bienvenida
                 void
                 (begin
                   (printf "\n\n\n\n\n\n\n\n\n\n\n\n\n\n
╔════════════════════════════════════════╗
║                 ESTADO                 ║
║                                        ║
║    Felicidad              Energía      ║
║   ╔══════════╗          ╔══════════╗   ║
║   ║") (printfNVeces "█" felicidad) (printfNVeces " " (- 10 felicidad)) (display "║          ║") (printfNVeces "█" energia) (printfNVeces " " (- 10 energia)) (display "║   ║
║   ╚══════════╝          ╚══════════╝   ║
║                                        ║
║    Diversión               Hambre      ║
║   ╔══════════╗          ╔══════════╗   ║
║   ║") (printfNVeces "█" diversion) (printfNVeces " " (- 10 diversion)) (display "║          ║") (printfNVeces "█" hambre) (printfNVeces " " (- 10 hambre)) (display "║   ║
║   ╚══════════╝          ╚══════════╝   ║
║                                        ║
║     Higiene                Vejiga      ║
║   ╔══════════╗          ╔══════════╗   ║
║   ║") (printfNVeces "█" higiene) (printfNVeces " " (- 10 higiene)) (display "║          ║") (printfNVeces "█" vejiga) (printfNVeces " " (- 10 vejiga)) (display "║   ║
║   ╚══════════╝          ╚══════════╝   ║
╚════════════════════════════════════════╝
")
                   (display "comer: ")
                   (display (vector-ref cuentaAcciones 0))
                   (display " baño: ")
                   (display (vector-ref cuentaAcciones 1))
                   (display " jugar: ")
                   (display (vector-ref cuentaAcciones 2))
                   (display " dormir: ")
                   (display (vector-ref cuentaAcciones 3))
                   (printf "\n")
                   (display " bañar: ")
                   (display (vector-ref cuentaAcciones 4))
                   (display " musica: ")
                   (display (vector-ref cuentaAcciones 5))
                   (display " curar: ")
                   (display (vector-ref cuentaAcciones 6))

                 )
             )
                   (begin
                   (cond
                     [bienvenida (begin
                                   (display Gbienvenida1)
                                   (display mensaje1)
                                   (display Gbienvenida2)
                                   (display mensaje2)
                                   (set! mensaje2 " ")
                                   (display Gbienvenida3)
                                   (set! bienvenida false)
                                 )
                     ]
                     [(eq? accion 2) (begin
                                       (display Gnormal2-1)
                                       (display mensaje1)
                                       (display Gnormal2-2)
                                       (display mensaje2)
                                       (display Gnormal2-3)
                                     )
                     ]
                     [else (begin
                             (display Gnormal1-1)
                             (display mensaje1)
                             (display Gnormal1-2)
                             (display mensaje2)
                             (display "
╔════════════════════════════════════════╗")
                           )
                     ]
                   )
                   
                   (display "
║          NEKOTCHI    MENU              ║
║                                        ║
║   1. Comer        2. Ir al baño        ║
║   3. Jugar        4. Dormir            ║
║   5. Bañar        6.Escuchar música    ║
║   7. Curar        8. Salir             ║
╚════════════════════════════════════════╝\n")
                   )
             (set! accion (- (read) 1))
             (cond
               [(= 0 accion) (begin (comer)  (menu))]
               [(= 1 accion) (begin (baño)   (menu))]
               [(= 2 accion) (begin (jugar)  (menu))]
               [(= 3 accion) (begin (dormir) (menu))]
               [(= 4 accion) (begin (bañar)  (menu))]
               [(= 5 accion) (begin (musica) (menu))]
               [(= 6 accion) (begin (curar)  (menu))]
               [(= 7 accion) (cerrar 7)]
               [else (begin
                       (set! mensaje1 "No te comprendo")
                       (menu)
                     )
               ]
             )
           )
       )
     )
     ;Tras cada acción se debe mostrar el nivel de felicidad.
     )
    (menu)
  )
)

(nekotchi)