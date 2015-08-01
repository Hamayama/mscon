;; -*- coding: utf-8 -*-
;;
;; mscontext.scm
;; 2015-8-1 v1.00
;;
;; ＜内容＞
;;   Gauche の text.console モジュールの動作を、
;;   Windows のコマンドプロンプト上でシミュレートするためのモジュールです。
;;   内部で mscon モジュールを使用しています。
;;
;; ＜インストール方法＞
;;   mscontext.scm と mscon.scm を Gauche でロード可能なフォルダにコピーします。
;;   (例えば (gauche-site-library-directory) で表示されるフォルダ等)
;;
;; ＜使い方＞
;;   (use mscontext)             ; モジュールをロードします
;;   (define con (make <vt100>)) ; コンソールオブジェクトを生成します
;;   (query-screen-size con)     ; 画面のサイズ(幅w,高さh)を取得します
;;                               ; 戻り値は、h w の多値で返ります(順番に注意)
;;   (query-cursor-position con) ; カーソルの座標(x,y)を取得します
;;                               ; 戻り値は、y x の多値で返ります(順番に注意)
;;   (clear-screen con)          ; 画面をクリアします
;;   (move-cursor-to con 5 10)   ; カーソルの座標を移動します
;;                               ; (座標指定が y x の順なので注意)
;;   (set-character-attribute con '(green black bright))
;;                               ; 色を指定します
;;                               ; 色はリストで前景色、背景色、オプションの順に指定します
;;                               ; 前景色と背景色には以下のシンボルを指定できます
;;                               ;   black red green yellow blue magenta cyan white
;;                               ; また、オプションには以下のシンボルを指定できます
;;                               ;   bright (輝度)
;;   (putch con #\space)         ; 1文字出力します
;;   (putstr con "HIT ANY KEY!") ; 文字列を出力します
;;   (chready? con)              ; 入力バッファに文字があるかチェックします
;;                               ; 文字があれば #t を、なければ #f を返します
;;   (getch con)                 ; 1文字入力します
;;                               ; 入力バッファに文字がなければ、入力されるまで待ちます
;;                               ; 暫定的にカーソルキーには以下の文字を割り当てています
;;                               ;   左:#\x11  上:#\x12  右:#\x13  下:#\x14
;;   (hide-cursor con)           ; カーソルを非表示にします
;;   (show-cursor con)           ; カーソルを表示します
;;   (reset-terminal con)        ; 端末をリセットします
;;                               ; (現状は色指定とカーソル表示が初期状態に戻るのみです)
;;
(define-module mscontext
  (use util.match)
  (use mscon)
  (export
    <vt100>
    call-with-console
    putch putstr getch chready?
    query-screen-size query-cursor-position move-cursor-to
    hide-cursor show-cursor
    reset-terminal clear-screen clear-to-eol
    set-character-attribute with-character-attribute))
(select-module mscontext)

(define-class <vt100> ()
  ((iport :init-keyword :iport :initform (standard-input-port)) ; not used
   (oport :init-keyword :oport :initform (standard-output-port))
   ;; private
   (keybuf :init-value '())))

(define-method call-with-console ((con <vt100>) proc)
  (unwind-protect
   (proc con)
   (reset-terminal con)))

(define-method putch ((con <vt100>) c)
  (display c (~ con'oport)) (flush (~ con'oport)))
(define-method putstr ((con <vt100>) s)
  (display s (~ con'oport)) (flush (~ con'oport)))
(define (%getch-sub con)
  (let ((done   #f)
        (ks     '())
        (kslist '()))
    (set! kslist (keystate))
    (while (not (null? kslist))
      (set! ks     (car kslist))
      (set! kslist (cdr kslist))
      (receive (kdown ch vk sft ctl alt) (apply values ks)
        (if (= kdown 1)
          (if (= ch 0)
            (push! (~ con 'keybuf) (case vk
                                     ;; temporary support of cursor keys
                                     ((37) 17) ; left
                                     ((38) 18) ; up
                                     ((39) 19) ; right
                                     ((40) 20) ; down
                                     (else  0)))
            (push! (~ con 'keybuf) ch)))
        ))))
(define-method getch ((con <vt100>))
  (while (<= (length (~ con 'keybuf)) 0)
    (sys-nanosleep #e100e6) ; 100msec
    (%getch-sub con))
  (integer->char (pop! (~ con 'keybuf))))
(define-method chready? ((con <vt100>))
  (%getch-sub con)
  ;(print (~ con 'keybuf))
  (if (> (length (~ con 'keybuf)) 0) #t #f))

(define-method query-cursor-position ((con <vt100>))
  (values (cursor-y) (cursor-x)))

(define-method move-cursor-to ((con <vt100>) y x)
  (locate x y))

(define-method reset-terminal ((con <vt100>))
  (color COL_GRAY COL_BLACK)
  (cursor-on))
(define-method clear-screen ((con <vt100>))
  (cls2))
(define-method clear-to-eol ((con <vt100>))
  (display (make-string (- (screen-width) (cursor-x))) (~ con'oport)) (flush (~ con'oport)))

(define-method hide-cursor ((con <vt100>))
  (cursor-off))
(define-method show-cursor ((con <vt100>))
  (cursor-on))

(define-method query-screen-size ((con <vt100>))
  (values (screen-height) (screen-width)))

(define-method set-character-attribute ((con <vt100>) spec)
  (define (get-color-code color)
    (case color
      ((black)      COL_BLACK)
      ((red)        COL_DARK_RED)
      ((green)      COL_DARK_GREEN)
      ((yellow)     COL_DARK_YELLOW)
      ((blue)       COL_DARK_BLUE)
      ((magenta)    COL_DARK_VIOLET)
      ((cyan)       COL_DARK_CYAN)
      ((white)      COL_GRAY)
      (else         COL_BLACK)
      ))
  (define (get-optional-code opt)
    (case opt
      ((bright)     COL_INTENSITY)
      ((reverse)    0) ; not supported
      ((underscore) 0) ; not supported
      (else         0)
      ))
  (match spec
    ((fgcolor bgcolor . opts)
     (let ((fc (get-color-code fgcolor))
           (bc (get-color-code bgcolor)))
       (for-each (lambda (opt)
                   (set! fc (logior fc (get-optional-code opt))))
                 opts)
       (color fc bc)))
    ))

(define-method reset-character-attribute ((con <vt100>))
  (color COL_GRAY COL_BLACK))

(define-method with-character-attribute ((con <vt100>) attrs thunk)
  (unwind-protect
   (begin
     (set-character-attribute con attrs)
     (thunk))
   (reset-character-attribute con)))
