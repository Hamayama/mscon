;;
;; snake game - sample for raw console manipulation
;;
;; modified for windows
;;

(cond-expand
 [gauche.os.windows
  (add-load-path "." :relative)
  (display #\cr)(flush) ; allocate console
  ]
 [else])
(use gauche.termios)
(use gauche.parameter)
(use gauche.array)
(use util.match)
(use srfi-27)
(cond-expand
 [gauche.os.windows
  (use mscon)
  (use mscontext)]
 [else
  (use text.console)])

(define *wait*    #e100e6) ; 100msec
(define *waitmin* #e20e6)  ;  20msec
(define *waitnow* 0)
(define *speedup* 5)
(define *extbody* 1)

(define (main args)
  (cond-expand
   [gauche.os.windows
    ;(exit 1 "Windows support is coming - stay tuned!")]
    (cond [(not (mscon-all-available?))
           (exit 1 "This program requires Gauche v0.9.4 or later.")]
          [else
           (call-with-console (make <vt100>) game)])]
   [else
    (cond [(not (and (sys-isatty (current-input-port))
                     (sys-isatty (current-output-port))))
           (exit 1 "You need to run this program on terminal.")]
          [(not (#/(vt100|xterm)/i (or (sys-getenv "TERM") "")))
           (exit 1 "TERM is not defined or unknown terminal.")]
          [else
           (call-with-console (make <vt100>) game)])]))

;; snake : (<dir> <head> <tail> ...)
;; <head> : <pt>
;; <tail> : <pt>
;; <pt> : (<x> . <y>)
;; <dir> : N | E | W | S
;; the last tail is used to erase a character on display
(define (snake-dir snake) (car snake))
(define (snake-body snake) (cdr snake))
(define (snake-head snake) (car (snake-body snake)))
(define (snake-tail snake) (cdr (snake-body snake)))

(define (game con)
  (hide-cursor con)
  (clear-screen con)
  (random-source-randomize! default-random-source)
  (receive (row col) (query-screen-size con)
    (let* ([field (new-field row col)]
           [snake (new-snake field)])
      (set! *waitnow* *wait*)
      (set-character-attribute con '(cyan blue))
      (render-field con field)
      (run-game con field snake (new-food field snake) 0))))

(define (collide-wall? field x y) (array-ref field y x))

(define (collide-snake? snake x y)
  ;(any (^p (and (eqv? (car p) x) (eqv? (cdr p) y))) (cdr snake)))
  (any (^p (and (eqv? (car p) x) (eqv? (cdr p) y))) (drop-right (cdr snake) 1)))

(define (new-field row col)
  (rlet1 field (make-array (shape 0 row 0 col) #f)
    (dotimes [k col]
      (array-set! field 0 k #t)
      (array-set! field (- row 1) k #t))
    (dotimes [k row]
      (array-set! field k 0 #t)
      (array-set! field k (- col 1) #t))
    (dotimes [k (* (quotient row 5) 2)]
      (array-set! field k (ash col -1) #t)
      (array-set! field (- row k 1) (ash col -1) #t))))

(define (new-snake field)
  (let ([row (array-end field 0)]
        [col (array-end field 1)])
    ;(let* ([t (cons (random-integer col) (random-integer row))]
    ;       [dir (~ '(N E W S) (random-integer 4))]
    ;       [h (next-point t dir)])
    ;  (if (or (collide-wall? field (car t) (cdr t))
    ;          (collide-wall? field (car h) (cdr h)))
    ;    (new-snake field)
    ;    `(,dir ,h ,t)))))
    (let* ([t (cons (ash col -1) (ash row -1))]
           [dir 'E]
           [h (next-point t dir)])
      `(,dir ,h ,t ,t))))

(define (update-snake snake dir new-head extend?)
  `(,dir ,new-head
         ;,@(if extend? (snake-body snake) (drop-right (snake-body snake) 1))))
         ,@(if extend?
             (append! (snake-body snake) (make-list *extbody* (last snake)))
             (drop-right (snake-body snake) 1))))

(define (new-food field snake) ; returns food location (x . y)
  (let ([row (array-end field 0)]
        [col (array-end field 1)])
    (let ([x (random-integer col)]
          [y (random-integer row)])
      (if (or (collide-wall? field x y)
              (collide-snake? snake x y))
        (new-food field snake)
        (cons x y)))))

(define (next-point point dir)
  (match-let1 (x . y) point
    (cons (case dir [(E) (+ x 1)] [(W) (- x 1)] [else x])
          (case dir [(S) (+ y 1)] [(N) (- y 1)] [else y]))))

(define (find-food? snake dir food)
  (equal? food (next-point (snake-head snake) dir)))

(define (run-game con field snake food score)
  (render con field snake food)
  ;; trick - since height of character is larger than width, if we use the
  ;; same interval, it would look like the snake runs faster in N-S direction
  ;; than E-W direction.
  (case (snake-dir snake)
    ;[(N S) (sys-nanosleep #e18e7)]
    ;[(E W) (sys-nanosleep #e9e7)])
    [(N S) (sys-nanosleep (* *waitnow* 2))]
    [(E W) (sys-nanosleep *waitnow*)])
  (let1 dir (or (and-let* ([newdir (get-dir con)])
                  (case (snake-dir snake) ; do now allow to turn 180 deg.
                    [(N) (and (not (eq? newdir 'S)) newdir)]
                    [(E) (and (not (eq? newdir 'W)) newdir)]
                    [(S) (and (not (eq? newdir 'N)) newdir)]
                    [(W) (and (not (eq? newdir 'E)) newdir)]))
                (snake-dir snake))
    (match-let1 (and (x . y) hd) (next-point (snake-head snake) dir)
      (cond [(or (collide-wall? field x y)
                 (collide-snake? snake x y))
             (render con field (update-snake snake dir hd #f) food)
             ;(game-over con)]
             (game-over con score)]
            [(find-food? snake dir food)
             (if (= (modulo (+ score 1) *speedup*) 0)
               (set! *waitnow* (max (- *waitnow* #e20e6) *waitmin*)))
             (let1 snake. (update-snake snake dir hd #t)
               (run-game con field snake. (new-food field snake.) (+ score 1)))]
            [else
             (run-game con field (update-snake snake dir hd #f) food score)]))))

(define (get-dir con) ;returns W, S, N, E or #f
  (and (chready? con)
       ;(case (getch con) [(#\h) 'W] [(#\j) 'S] [(#\k) 'N] [(#\l) 'E] [else #f])))
       (case (getch con) [(#\h #\x11) 'W] [(#\j #\x14) 'S] [(#\k #\x12) 'N] [(#\l #\x13) 'E] [else #f])))

(define (render con field snake food)
  ;(clear-screen con)
  ;(set-character-attribute con '(cyan blue))
  ;(render-field con field)
  (set-character-attribute con '(green black bright))
  (render-snake con snake)
  ;(set-character-attribute con '(magenta black bright))
  ;(render-point con food #\o))
  (set-character-attribute con '(yellow black bright))
  (render-point con food #\%))

(define (render-field con field)
  ($ array-for-each-index field
     (^[y x]
       (when (array-ref field y x)
         (move-cursor-to con y x)
         (putch con #\#))))
  (move-cursor-to con 0 0)) ; for scroll recovery

(define (render-snake con snake)
  ;(render-point con (snake-head snake) #\@)
  ;(dolist [pt (snake-tail snake)]
  ;  (render-point con pt #\*)))
  (dolist [pt (drop-right (snake-tail snake) 1)]
    (render-point con pt #\*))
  (render-point con (last (snake-tail snake)) #\space)
  (render-point con (snake-head snake) #\@))

(define (render-point con pt ch)
  (match-let1 (x . y) pt
    (move-cursor-to con y x)
    (putch con ch)))

;(define (game-over con)
(define (game-over con score)
  (receive (row col) (query-screen-size con)
    (move-cursor-to con (ash row -1) (- (ash col -1) 5))
    (set-character-attribute con '(white black reverse))
    (putstr con "Game over!")
    (move-cursor-to con (+ (ash row -1) 1) (- (ash col -1) 4))
    (putstr con (format #f "Score:~d" score))
    ;; Exit game with any keypress; however, we don't want to pick
    ;; the keypress right after game over, so wait for a sec, discard
    ;; whatever keys pressed during that, then wait for input.
    (sys-sleep 1)
    (while (chready? con) (getch con))
    (getch con)))

