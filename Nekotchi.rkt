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
  Fecha última modificación: 2019/04/07
  Versión: 1.0
|#

(define (nekotchi)
  (local
    (  
;───────────────────────────────── Gatos ─────────────────────────────────
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
     
;───────────────────────────────── Variables ─────────────────────────────────
     (define estaEnfermo false)
     (define noHayCura 0)
     (define necesitaElBaño false)
     (define bienvenida true)
     (define numero? "c")
     (define accion 0)
     (define mensaje1 "¡Hola!")
     (define mensaje2 "¡Gusto en conocerte!")
     (define causaDeMuerte 0)
     (define cuentaAcciones (make-vector 7 0))   ;contador para la cantidad de veces que se ejecuta una acción.
     (define barrasDeEstado (vector 3 ;felicidad
                                    6 ;energia
                                    4 ;diversion
                                    4 ;comida
                                    7 ;higiene
                                    6 ;vegija
                            ) ;vector para las barras de estado
     )

#|───────────────────────────────── Funciones ─────────────────────────────────
   printfNveces:
     Imprime un mensaje N numero de veces.|#
     (define (printfNVeces msj N)
       (cond
         [(<= N 0) void]
         [else (begin
                 (printf msj)
                 (printfNVeces msj (- N 1))
               )
         ]
       )
     )
     
#| atributo+:
     Recibe la posicion de un atributo en el vector barrasDeEstado y suma N al atributo
     pero no permite que el valor supere a 10 ó baje de 0. |#
     (define (atributo+ posAtributo N)
       (begin
         (vector-set! barrasDeEstado posAtributo (+ (vector-ref barrasDeEstado posAtributo) N))
         (if (> (vector-ref barrasDeEstado posAtributo) 10)
             (vector-set! barrasDeEstado posAtributo 10)
             (if (< (vector-ref barrasDeEstado posAtributo) 0)
                 (vector-set! barrasDeEstado posAtributo 0)
                 void
             )
         )
       )
     )
     
#| SumaContadores:
     Suma en 1 al contador de la acción que se ejecuta y le resta 1 a las demás. |#
     (define (sumaContadores)
       (begin
         (if (= (vector-ref cuentaAcciones 6) 4) ;Esto evita que el contador de curar sea mayor a 4.
             void
             (vector-set! cuentaAcciones accion (+ (vector-ref cuentaAcciones accion) 2))
         )
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
       )
     )

     (define (pasaTurno) ;todos los valores de las barras disminuyen en 1 cada turno
       (begin
         (if estaEnfermo ;si está enfermo todos los valores disminuyes un total de 2 por turno
             (set! noHayCura 1)
             (set! noHayCura 0)
         )
         (if (= (vector-ref barrasDeEstado 0) 0)
             void
             (vector-set! barrasDeEstado 0 (- (vector-ref barrasDeEstado 0) (+ 1 noHayCura)))
         )
         (if necesitaElBaño ;si no lo lleva al baño disminuye la felicidad en 2 y la higiene en 2
             (begin
               (set! necesitaElBaño false)
               (vector-set! barrasDeEstado 0 (- (vector-ref barrasDeEstado 0) 2))
               (vector-set! barrasDeEstado 4 (- (vector-ref barrasDeEstado 4) 2))
             )
             void
         )
         (if (= (vector-ref barrasDeEstado 1) 0)
             void
             (vector-set! barrasDeEstado 1 (- (vector-ref barrasDeEstado 1) (+ 1 noHayCura)))
         )
         (if (= (vector-ref barrasDeEstado 2) 0)
             void
             (vector-set! barrasDeEstado 2 (- (vector-ref barrasDeEstado 2) (+ 1 noHayCura)))
         )
         (if (= (vector-ref barrasDeEstado 3) 0)
             void
             (vector-set! barrasDeEstado 3 (- (vector-ref barrasDeEstado 3) (+ 1 noHayCura)))
         )
         (if (= (vector-ref barrasDeEstado 4) 0)
             void
             (vector-set! barrasDeEstado 4 (- (vector-ref barrasDeEstado 4) (+ 1 noHayCura)))
         )
         (if (= (vector-ref barrasDeEstado 5) 0)
             void
             (vector-set! barrasDeEstado 5 (- (vector-ref barrasDeEstado 5) (+ 1 noHayCura)))
         )
       )
     )

#|────────────────────────────────────────── funciones del usuario ───────────────────────────────────────────────────────────
   comer:
     Cuando se alimenta a la mascota 1 vez su felicidad aumenta en 1, 2 veces la aumenta en 0 y 3 veces -1, lo enferma.
     En el último caso es necesario llevarlo al baño y aumentará su felicidad en 1,
     sino lo lleva inmediatamente le disminuye en 2 la felicidad. |#
     (define (comer)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin 
               (atributo+ 0 1)  ;felicidad + 1
               (atributo+ 3 6)  ;comida + 6
               (atributo+ 5 -1) ;vejiga - 1
               (atributo+ 2 1)  ;diversion + 1
               (set! mensaje1 "¡Yumi!")
             )
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (set! mensaje1 "Estoy lleno")
                   (atributo+ 3 4) ;comida + 4
                   (atributo+ 5 -1) ;vejiga - 1
                   (atributo+ 4 -1) ;higiene -1
                 )
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (atributo+ 0 -1) ;felicidad - 1
                       (atributo+ 3 2)  ;comida + 2
                       (atributo+ 5 -1) ;vejiga - 1
                       (atributo+ 4 -1) ;higiene -1
                       (set! estaEnfermo true)
                       (set! necesitaElBaño true)
                       (set! mensaje1 "Necesito ir al baño...")
                       (set! mensaje2 "No me siento muy bien :S")
                       
                     )
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores)
         (pasaTurno)
         (if (= (vector-ref cuentaAcciones accion) 2)
                     (set! necesitaElBaño true)
                     void
         )
       )
     )

#| baño:
     El baño le aumenta la felicidad en 2, la segunda vez no lo afecta
     y la tercera le disminuye en 1 enfermándolo, debe curarlo.|#
     (define (baño)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin
               (atributo+ 0 2) ;felicidad + 2
               (atributo+ 5 6) ;vejiga + 6
               (atributo+ 4 -1) ;higiene -1
               (set! mensaje1 "Listo!")
             )
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (set! mensaje1 "Acabé")
                   (atributo+ 5 4) ;vejiga + 4
                   (atributo+ 4 -1) ;higiene -1
                 )
                 (if (= (vector-ref cuentaAcciones accion)2)
                     (begin
                       (atributo+ 0 -1) ;felicidad - 1
                       (atributo+ 5 2)  ;vejiga + 2
                       (atributo+ 4 -2) ;higiene -2
                       (set! estaEnfermo true)
                       (set! mensaje1 "No me siento bien...")
                     )
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores)
         (pasaTurno)
       )
     )
#| jugar:
     Jugar una vez aumenta la felicidad en 2, jugar 2 veces le aumenta en 1 y 3 veces no le aumenta
     ni disminuye felicidad pero le produce hambre disminuyendo su conteo de alimento en 2. |#
     (define (jugar)
       (begin
         ;(printf (vector-ref accs 0))
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin 
               (atributo+ 0 2)  ;felicidad + 2
               (atributo+ 2 6)  ;diversion + 6
               (atributo+ 1 -1) ;energia - 1
               (atributo+ 4 -1) ;higiene - 1
               (set! mensaje1 "¡YAY!")
               (set! mensaje2 "¡Qué divertido!")
             )
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (atributo+ 0 2)  ;felicidad + 1
                   (atributo+ 2 4)  ;diversion + 4
                   (atributo+ 1 -1) ;energia - 1
                   (atributo+ 3 -1) ;comida - 1
                   (atributo+ 4 -1) ;higiene - 1
                   (set! mensaje1 "Eso fue divertido")
                   (set! mensaje2 "Estoy cansado...")
                 )
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (atributo+ 0 1)  ;felicidad + 1
                       (atributo+ 2 2)  ;diversion + 2
                       (atributo+ 1 -2) ;energia - 2
                       (atributo+ 3 -2) ;comida - 2
                       (atributo+ 4 -1) ;higiene - 1
                       (set! mensaje1 "¡Tengo hambre!")
                       (vector-set! cuentaAcciones 0 (- (vector-ref cuentaAcciones 0) 1))
                     )
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores)
         (pasaTurno)
       )
     )
     
#| dormir:
     |#
     (define (dormir)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin
               (atributo+ 0 2) ;felicidad + 2
               (atributo+ 1 6) ;energia + 6
               (set! mensaje1 "¡Qué buena siesta!"))
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (atributo+ 0 1)  ;felicidad + 1
                   (atributo+ 1 4)  ;energia + 4
                   (atributo+ 3 -1) ;comida - 1
                   (set! mensaje1 "¡Qué bueno descansar!")
                 )
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (atributo+ 0 -1) ;felicidad - 1
                       (atributo+ 1 1)  ;energia + 1
                       (atributo+ 3 -1) ;comida - 1
                       (atributo+ 2 -2) ;diversion - 2
                       (set! mensaje1 "¡No quiero dormir más!")
                     )
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores)
         (pasaTurno)
       )
     )
#| bañar:
     |#
     (define (bañar)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin
               (atributo+ 4 6)  ;higiene + 6
               (atributo+ 0 -1) ;felicidad - 1
               (atributo+ 2 -2) ;diversion -2
               (set! mensaje1 "¡No me gusta el agua!")
             )
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                    (atributo+ 4 4)  ;higiene + 4
                    (atributo+ 0 -2) ;felicidad -2
                    (atributo+ 2 -2) ;diversion -2
                    (set! mensaje1 "¡Achú!")
                  )
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (atributo+ 4 2)  ;higiene + 2
                       (atributo+ 0 -2) ;felicidad -2
                       (atributo+ 2 -2) ;diversion -2
                       (set! estaEnfermo true)
                       (set! mensaje1 "No me siento bien...")
                     )
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores)
         (pasaTurno)
       )
     )

#| musica:
     Escuchar música una vez aumenta en 2 la felicidad, otra vez le aumenta en 1,
     y una tercera vez disminuye en 1, lo enferma, debe curarlo. |#
     (define (musica)
       (begin
         (if (= (vector-ref cuentaAcciones accion) 0)
             (begin 
               (atributo+ 0 3) ;felicidad + 3
               (atributo+ 2 4) ;diversion + 4
               (set! mensaje1 "¡Woo!")
               (set! mensaje2 "Tiene buen ritmo"))
             (if (= (vector-ref cuentaAcciones accion) 1)
                 (begin
                   (atributo+ 0 2) ;felicidad + 2
                   (atributo+ 2 2) ;diversion + 2
                   (set! mensaje1 "Estuvo bien")
                 )
                 (if (= (vector-ref cuentaAcciones accion) 2)
                     (begin
                       (atributo+ 0 -1) ;felicidad - 1
                       (set! estaEnfermo true)
                       (set! mensaje1 "Me duele la cabeza")
                     )
                     (set! causaDeMuerte accion)
                 )
             )
         )
         (sumaContadores)
         (pasaTurno)
       )
     )
#| curar:
     Curar una vez le aumenta la felicidad en 3 y más veces no le afecta.|#
     (define (curar)
       (begin
         (if estaEnfermo
             (atributo+ 0 3) ;felicidad + 3
             void)
         (set! mensaje1 "Estoy sano")
         (set! estaEnfermo false)
         (sumaContadores)
         (pasaTurno)
       )
     )

#| cerrar:
     Finaliza el programa, ya sea porque la mascota muere o el jugador se retira.|#
     (define (cerrar num)
       (begin
         (printf "\nEres un mal amo.")
         (cond
           ;muertes por realizar mucho una acción:
           [(eq? num 0) (display "\nTu mascota murió de gastritis")]
           [(eq? num 1) (display "\nTu mascota murió de falla renal")]
           [(eq? num 2) (display "\nTu mascota murió de cansancio")]
           [(eq? num 3) (display "\nTu mascota murió en coma")]
           [(eq? num 4) (display "\nTu mascota murió de hipotermia")]
           [(eq? num 5) (display "\nTu mascota murió de derrame cerebral.")]
           [(eq? num 6) (display "\nTu mascota murió de aburrimiento")]
           [(eq? num 7) (display "\nHas abandonado a tu mascota")]

           ;muertes porque una barra se vacíe
           [(eq? num 10) (display "\nTu mascota murió de tristeza")]
           [(eq? num 11) (display "\nTu mascota murió de agotamiento")]
           [(eq? num 12) (display "\nTu mascota murió de aburrimiento")]
           [(eq? num 13) (display "\nTu mascota murió de hambre")]
           [(eq? num 14) (display "\nTu mascota murió de suciedad")]
           [(eq? num 15) (display "\nA tu mascota le explotó la vejiga y murió")]
           [else void]
         )
         (display Gmuerto)
       )
     )
#|──────────────────────────────────── menu ──────────────────────────────────────────────────────
     |#
     
     (define (menu)
       (begin
         (cond
           [(< (vector-ref barrasDeEstado 0) 1) (set! causaDeMuerte 10)]
           [(< (vector-ref barrasDeEstado 1) 1) (set! causaDeMuerte 11)]
           [(< (vector-ref barrasDeEstado 2) 1) (set! causaDeMuerte 12)]
           [(< (vector-ref barrasDeEstado 3) 1) (set! causaDeMuerte 13)]
           [(< (vector-ref barrasDeEstado 4) 1) (set! causaDeMuerte 14)]
           [(< (vector-ref barrasDeEstado 5) 1) (set! causaDeMuerte 15)]
           [else void]
         )
       (if (or (> (vector-ref cuentaAcciones 0) 3)
               (> (vector-ref cuentaAcciones 1) 3)
               (> (vector-ref cuentaAcciones 2) 3)
               (> (vector-ref cuentaAcciones 3) 3)
               (> (vector-ref cuentaAcciones 4) 3)
               (> (vector-ref cuentaAcciones 5) 3)
               
               (< (vector-ref barrasDeEstado 0) 1)
               (< (vector-ref barrasDeEstado 1) 1)
               (< (vector-ref barrasDeEstado 2) 1)
               (< (vector-ref barrasDeEstado 3) 1)
               (< (vector-ref barrasDeEstado 4) 1)
               (< (vector-ref barrasDeEstado 5) 1)
           )
           (cerrar causaDeMuerte)
           (begin
             (if bienvenida
                 void
                 (begin
                   (display "\n\n\n\n\n\n\n\n\n\n\n\n\n\n
╔════════════════════════════════════════╗
║            ESTADO   ")(if estaEnfermo (printf "ENFERMO") (printf " BIEN  ")) (display "            ║
║                                        ║
║     Felicidad            Energía       ║
║    ╔══════════╗        ╔══════════╗    ║
║    ║") (printfNVeces "█" (vector-ref barrasDeEstado 0)) (printfNVeces " " (- 10 (vector-ref barrasDeEstado 0)))
      (display "║        ║") (printfNVeces "█" (vector-ref barrasDeEstado 1)) (printfNVeces " " (- 10 (vector-ref barrasDeEstado 1))) (display "║    ║
║    ╚══════════╝        ╚══════════╝    ║
║                                        ║
║     Diversión             Comida       ║
║    ╔══════════╗        ╔══════════╗    ║
║    ║") (printfNVeces "█" (vector-ref barrasDeEstado 2)) (printfNVeces " " (- 10 (vector-ref barrasDeEstado 2)))
      (display "║        ║") (printfNVeces "█" (vector-ref barrasDeEstado 3)) (printfNVeces " " (- 10 (vector-ref barrasDeEstado 3))) (display "║    ║
║    ╚══════════╝        ╚══════════╝    ║
║                                        ║
║      Higiene              Vejiga       ║
║    ╔══════════╗        ╔══════════╗    ║
║    ║") (printfNVeces "█" (vector-ref barrasDeEstado 4)) (printfNVeces " " (- 10 (vector-ref barrasDeEstado 4)))
      (display "║        ║") (printfNVeces "█" (vector-ref barrasDeEstado 5)) (printfNVeces " " (- 10 (vector-ref barrasDeEstado 5))) (display "║    ║
║    ╚══════════╝        ╚══════════╝    ║
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
                 [(or (eq? accion 0) (eq? accion 2) (eq? accion 5))
                      (begin
                        (display Gnormal2-1)
                        (display mensaje1)
                        (display Gnormal2-2)
                        (display mensaje2)
                        (set! mensaje2 " ")
                        (display Gnormal2-3)
                      )
                 ]
                 [else (begin
                         (display Gnormal1-1)
                         (display mensaje1)
                         (display Gnormal1-2)
                         (display mensaje2)
                         (set! mensaje2 " ")
                         (display "
╔════════════════════════════════════════╗"))]) (display "
║           MENU   NEKOTCHI              ║
║                                        ║
║     1. Comer      2. Ir al baño        ║
║     3. Jugar      4. Dormir            ║
║     5. Bañar      6. Escuchar música   ║
║     7. Curar      8. Abandonar         ║
╚════════════════════════════════════════╝\n"   )
             )
             (set! numero? (read))
             (if (number? numero?)
                 (begin
                   (set! accion (- numero? 1))
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
                 (begin
                   (set! mensaje1 "No te comprendo")
                   (menu)
                 )
             )
           )
       )
       )
     )
    )
    (menu)
  )
)

(define (jugar?)
  (begin
    (printf "\n¿Volver a jugar?
1. Sí
... o pulsa cualquier tecla para salir\n")
    (if (eq? (read) 1)
        (begin
          (nekotchi)
          (jugar?)
        )
        (printf "Saliste")
    )
  )
)

(nekotchi)
(jugar?)
