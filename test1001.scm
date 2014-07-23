;; -*- coding: utf-8 -*-
;;
;; Windowsコンソールで花火の表示 (msconモジュールのテスト用)
;;
(add-load-path "." :relative)
(use mscon)
(use math.const)
(use math.mt-random)

;; 乱数
;;   (randint n1 n2)でn1以上n2以下の整数の乱数を取得する(n1,n2は整数でn1<n2であること)
(define m (make <mersenne-twister> :seed (sys-time)))
(define (randint n1 n2)
  (+ (mt-random-integer m (+ (- n2 n1) 1)) n1))

;; 花火情報
;;   act   行動モード(=0:なし,=1:上昇中,=2:爆発後)
;;   tim   生存カウンタ(0になったら消える)
;;   x     X座標
;;   y     Y座標
;;   rad   角度
;;   spd   速度
;;   divx  X方向の速度の補正係数(例えば2にするとX方向の速度が半分になる)
;;   divy  Y方向の速度の補正係数(例えば2にするとY方向の速度が半分になる)
;;   col   色
(define-class <hanabi> () (act tim x y rad spd divx divy col))

;; 各種初期化
(define wait     100)             ; ウェイト時間(msec)
(define wd       (screen-width))  ; 画面の幅(単位:文字)
(define ht       (screen-height)) ; 画面の高さ(単位:文字)
(define hanalist '())             ; 花火のリスト
(define done     #f)              ; ループフラグ
(define sc       0)               ; カウンタ(タイミング調整用)

;; メインループ
(if (mscon-all-available?) (keyclear))
(while (not done)
  (if (mscon-all-available?) (cls2) (cls))
  (color COL_YELLOW)
  (locate 0 0)
  (if (mscon-all-available?)
    (print "HIT ANY KEY TO EXIT")
    (print "HIT CTRL-C CTRL-C KEY TO EXIT"))
  (color COL_WHITE)

  ;; 花火の生成
  (inc! sc)
  ;(print sc)
  (if (> sc 1000000) (set! sc 1))
  (if (and (= (modulo sc 2) 0) (<= (randint 1 6) 3))
    (let1 hb (make <hanabi>)
      (slot-set! hb 'act  1)
      (slot-set! hb 'tim  (randint 4 8))
      (slot-set! hb 'x    (randint 5 (- wd 5 1)))
      (slot-set! hb 'y    (- ht 1))
      (slot-set! hb 'rad  (- pi/2))
      (slot-set! hb 'spd  5)
      (slot-set! hb 'divx 1)
      (slot-set! hb 'divy 2.2)
      (slot-set! hb 'col  (randint 10 15))
      (push! hanalist hb)))

  ;; 花火の移動と爆発
  (for-each
    (lambda (hb)
      (let ((act  (slot-ref hb 'act))
            (tim  (slot-ref hb 'tim))
            (x    (slot-ref hb 'x))
            (y    (slot-ref hb 'y))
            (rad  (slot-ref hb 'rad))
            (spd  (slot-ref hb 'spd))
            (divx (slot-ref hb 'divx))
            (divy (slot-ref hb 'divy))
            (col  (slot-ref hb 'col))
            (vx   0)
            (vy   0))
        (when (> act 0)
          ;; 移動
          (set! vx  (/. (*. spd (cos rad)) divx))
          (set! vy  (/. (*. spd (sin rad)) divy))
          (set! x   (+ x vx))
          (set! y   (+ y vy))
          (set! spd (- spd 0.2))
          (dec! tim)
          (slot-set! hb 'x   x)
          (slot-set! hb 'y   y)
          (slot-set! hb 'spd spd)
          (slot-set! hb 'tim tim)
          (when (or (< x 0) (>= x wd) (< y 0) (>= y ht))
            (set! act 0)
            (slot-set! hb 'act 0))
          (when (<= tim 0)
            (if (= act 1)
              ;; 爆発
              (do ((i     0 (+ i 1))
                   (rad1  0 (+ rad1 pi/4))
                   (cycle 0))
                  ((>= i 16) #f)
                (let1 hb2 (make <hanabi>)
                  (if (= (modulo i 8) 0) (inc! cycle))
                  (slot-set! hb2 'act  2)
                  (slot-set! hb2 'tim  5)
                  (slot-set! hb2 'x    x)
                  (slot-set! hb2 'y    y)
                  (slot-set! hb2 'rad  rad1)
                  (slot-set! hb2 'spd  (* cycle 2.3))
                  (slot-set! hb2 'divx divx)
                  (slot-set! hb2 'divy divy)
                  (slot-set! hb2 'col  col)
                  (push! hanalist hb2))))
            (slot-set! hb 'act 0)))))
    hanalist)

  ;; 花火の表示
  (for-each
    (lambda (hb)
      (let ((act (slot-ref hb 'act))
            (x   (slot-ref hb 'x))
            (y   (slot-ref hb 'y))
            (col (slot-ref hb 'col)))
        (when (> act 0)
          (set! x (x->integer (floor x)))
          (set! y (x->integer (floor y)))
          (if (mscon-all-available?)
            (begin
              (putcolor 2 x y col)
              (puttext "★" x y))
            (begin
              ;(color 15)
              ;(locate x y)
              ;(display "*")
              ;(flush)
              (puttext "*" x y))))))
    hanalist)

  ;; 花火の消去
  (set! hanalist
    (remove!
      (lambda (hb)
        (let1 act (slot-ref hb 'act)
          (if (<= act 0) #t #f)))
      hanalist))
  ;(print hanalist)

  ;; キー入力のチェックとウェイト
  (if (mscon-all-available?)
    (let1 ks (keywait2 wait)
      (when (not (null? ks))
        ;(print ks)
        (set! done #t)))
    (sys-nanosleep (* wait 1000000)))
  )

