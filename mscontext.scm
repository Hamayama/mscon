;; -*- coding: utf-8 -*-
;;
;; mscontext.scm
;; 2015-10-15 v1.13
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
;;                               ; カーソルキー等の特殊なキーを入力した場合は、
;;                               ; KEY_UP 等のシンボルが返ります
;;                               ; Ctrl+xキーを入力した場合は、
;;                               ; #\x00～#\x1f の制御コード文字が返ります
;;                               ; Alt+xキーを入力した場合は、(ALT #\x) のようなリストが
;;                               ; 返ります(詳細はソースコード参照)
;;   (hide-cursor con)           ; カーソルを非表示にします
;;   (show-cursor con)           ; カーソルを表示します
;;   (reset-terminal con)        ; 端末をリセットします
;;                               ; (現状は色指定とカーソル表示が初期状態に戻るのみです)
;;
(define-module mscontext
  (use util.match)
  (use util.queue)
  (use mscon)
  (export
    <vt100>
    call-with-console
    putch putstr getch chready? beep
    query-screen-size query-cursor-position move-cursor-to
    hide-cursor show-cursor cursor-down/scroll-up cursor-up/scroll-down
    reset-terminal clear-screen clear-to-eol clear-to-eos
    set-character-attribute with-character-attribute
    make-default-console
    ))
(select-module mscontext)

(define-class <vt100> ()
  ((iport :init-keyword :iport :init-form (standard-input-port)) ; not used
   (oport :init-keyword :oport :init-form (standard-output-port))
   (input-delay :init-keyword :input-delay :init-value 1000) ; not used
   ;; private
   (keybuf :initform (make-queue))))

(define-method call-with-console ((con <vt100>) proc)
  (unwind-protect
   (proc con)
   (reset-terminal con)))

(define *virtual-key-table*
  (apply
   hash-table
   'eqv?
   '([38  . KEY_UP]
     [40  . KEY_DOWN]
     [39  . KEY_RIGHT]
     [37  . KEY_LEFT]
     [35  . KEY_END]
     [36  . KEY_HOME]
     [45  . KEY_INS]
     [46  . KEY_DEL]
     [34  . KEY_PGDN]
     [33  . KEY_PGUP]
     [112 . KEY_F1]
     [113 . KEY_F2]
     [114 . KEY_F3]
     [115 . KEY_F4]
     [116 . KEY_F5]
     [117 . KEY_F6]
     [118 . KEY_F7]
     [119 . KEY_F8]
     [120 . KEY_F9]
     [121 . KEY_F10]
     [122 . KEY_F11]
     [123 . KEY_F12]
     )))

(define-method putch ((con <vt100>) c)
  (display c (~ con'oport)) (flush (~ con'oport)))
(define-method putstr ((con <vt100>) s)
  (display s (~ con'oport)) (flush (~ con'oport)))
(define (%getch-sub con)
  (define ignorevk (list VK_SHIFT    VK_CONTROL  VK_MENU   VK_LWIN
                         VK_RWIN     VK_APPS     VK_LSHIFT VK_RSHIFT
                         VK_LCONTROL VK_RCONTROL VK_LMENU  VK_RMENU))
  (define (get-ctrl-char vk)
    (cond
     ((or (and (>= vk #x41) (<= vk #x5a)) ; #\A-#\Z
          (= vk 32))  ; #\space
      (integer->char (- (logand vk (lognot #x20)) #x40)))
     (else
      (case vk
        ((192) #\x00) ; #\@
        ((219) #\x1b) ; #\[
        ((220) #\x1c) ; #\\
        ((221) #\x1d) ; #\]
        ((222) #\x1e) ; #\^
        ((226) #\x1f) ; #\_
        (else  #\x00)))))
  (for-each
   (lambda (ks)
     (receive (kdown ch vk sft ctl alt) (apply values ks)
       ;(if (= ch 3) (enqueue! (~ con 'keybuf) (eof-object))) ; Ctrl-C
       (if (and (= kdown 1) (not (memv vk ignorevk)))
         (cond
          ((hash-table-get *virtual-key-table* vk #f)
           (enqueue! (~ con 'keybuf) (hash-table-get *virtual-key-table* vk)))
          ((and (= alt 1) (= ctl 1))
           (enqueue! (~ con 'keybuf) `(ALT ,(get-ctrl-char vk))))
          ((= alt 1)
           (enqueue! (~ con 'keybuf) `(ALT ,(integer->char ch))))
          ((= ctl 1)
           (enqueue! (~ con 'keybuf) (get-ctrl-char vk)))
          (else
           (enqueue! (~ con 'keybuf) (integer->char ch)))))))
   (keystate)))
(define-method getch ((con <vt100>))
  (while (queue-empty? (~ con 'keybuf))
    (sys-nanosleep #e10e6) ; 10msec
    (%getch-sub con))
  (dequeue! (~ con 'keybuf)))
(define-method chready? ((con <vt100>))
  (%getch-sub con)
  ;(print (~ con 'keybuf))
  (if (not (queue-empty? (~ con 'keybuf))) #t #f))

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
  (let ((x   (cursor-x))
        (y   (cursor-y))
        (sbw (screen-buffer-width)))
    ;(display (make-string (- sbw x)) (~ con'oport))
    ;(flush (~ con'oport))
    ;(locate x y)
    (let1 n (- sbw x)
      (putcolor n x y COL_GRAY COL_BLACK)
      (puttext (make-string n) x y ))
    ))
(define-method clear-to-eos ((con <vt100>))
  (let ((x   (cursor-x))
        (y   (cursor-y))
        (sl  (screen-left))
        (st  (screen-top))
        (sw  (screen-width))
        (sh  (screen-height))
        (sbw (screen-buffer-width)))
    ;(display (make-string (+ (* (+ st sh (- y) -1) sbw) (- x) sl sw -1)) (~ con'oport))
    ;(flush (~ con'oport))
    ;(locate x y)
    (let1 n (+ (* (+ st sh (- y) -1) sbw) (- x) sl sw)
      (putcolor n x y COL_GRAY COL_BLACK)
      (puttext (make-string n) x y ))
    ))

(define-method hide-cursor ((con <vt100>))
  (cursor-off))
(define-method show-cursor ((con <vt100>))
  (cursor-on))

(define-method cursor-down/scroll-up ((con <vt100>))
  (locate (cursor-x) (+ (cursor-y) 1)))
(define-method cursor-up/scroll-down ((con <vt100>))
  (locate (cursor-x) (- (cursor-y) 1)))

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

(define-method beep ((con <vt100>))
  (putch con #\alarm))

(define (make-default-console)
  (cond [(not (mscon-all-available?))
         (error "mscontext module requires Gauche v0.9.4 or later.")]
        [else
         (make <vt100>)]))

