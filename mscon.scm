;; -*- coding: utf-8 -*-
;;
;; mscon.scm
;; 2015-7-31 v1.22
;;
;; ＜内容＞
;;   Windows のコマンドプロンプトで Gauche(gosh.exe) を使うときに、
;;   コンソールの制御を可能とするモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/mscon
;;
(define-module mscon
  (use gauche.uvector)
  (use os.windows)
  (export
    mscon-all-available? cls cls2 screen-size screen-area
    screen-left screen-top screen-width screen-height
    screen-buffer-width screen-buffer-height
    cursor-x cursor-y cursor-off cursor-on locate color
    keywait keystate keystate-test keywait2 keyclear
    puttext putcolor set-console-title get-console-title
    COL_BLACK       COL_DARK_BLUE   COL_DARK_GREEN  COL_DARK_CYAN
    COL_DARK_RED    COL_DARK_VIOLET COL_DARK_YELLOW COL_GRAY
    COL_DARK_GRAY   COL_BLUE        COL_GREEN       COL_CYAN
    COL_RED         COL_VIOLET      COL_YELLOW      COL_WHITE
    COL_BLUE_MASK   COL_GREEN_MASK  COL_RED_MASK    COL_INTENSITY
    VK_SHIFT        VK_CONTROL      VK_MENU))
(select-module mscon)


;; 標準入出力のハンドルの保持
;; (保持しておかないとエラーになる。Gauche v0.9.4-rc2では修正ずみ)
(define stdin-handle  (sys-get-std-handle STD_INPUT_HANDLE))
(define stdout-handle (sys-get-std-handle STD_OUTPUT_HANDLE))
(define stderr-handle (sys-get-std-handle STD_ERROR_HANDLE))


;; 色の定数
(define-values
  (COL_BLACK       COL_DARK_BLUE   COL_DARK_GREEN  COL_DARK_CYAN
   COL_DARK_RED    COL_DARK_VIOLET COL_DARK_YELLOW COL_GRAY
   COL_DARK_GRAY   COL_BLUE        COL_GREEN       COL_CYAN
   COL_RED         COL_VIOLET      COL_YELLOW      COL_WHITE)
  (apply values (iota 16)))
(define COL_BLUE_MASK  #x1)
(define COL_GREEN_MASK #x2)
(define COL_RED_MASK   #x4)
(define COL_INTENSITY  #x8)

;; イベントの定数
(define KEY_EVENT 1)

;; コントロールキーの定数
(define RIGHT_ALT_PRESSED  #x01)
(define LEFT_ALT_PRESSED   #x02)
(define RIGHT_CTRL_PRESSED #x04)
(define LEFT_CTRL_PRESSED  #x08)
(define SHIFT_PRESSED      #x10)

;; 仮想キーコードの定数
(define VK_SHIFT    #x10)
(define VK_CONTROL  #x11)
(define VK_MENU     #x12)
(define VK_LWIN     #x5B)
(define VK_RWIN     #x5C)
(define VK_APPS     #x5D)
(define VK_LSHIFT   #xA0)
(define VK_RSHIFT   #xA1)
(define VK_LCONTROL #xA2)
(define VK_RCONTROL #xA3)
(define VK_LMENU    #xA4)
(define VK_RMENU    #xA5)


;; 全機能利用可能か
(define (mscon-all-available?)
  (guard (exc ((<error> exc) #f))
    (and
     (procedure? sys-fill-console-output-character)
     (procedure? sys-fill-console-output-attribute)
     (procedure? sys-flush-console-input-buffer))))

;; 画面クリア
(define (cls)
  (sys-system "cls")
  ;(undefined))
  (values))

;; 画面クリア2
(define (cls2 :optional (fc COL_GRAY) (bc COL_BLACK))
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl))
         (bw    (slot-ref cinfo 'size.x))
         (bh    (slot-ref cinfo 'size.y))
         (cattr (get-color-attr fc bc)))
    (sys-fill-console-output-attribute hdl cattr   (* bw bh) 0 0)
    (sys-fill-console-output-character hdl #\space (* bw bh) 0 0)
    (sys-set-console-cursor-position hdl 0 0)))

;; 画面のバッファのサイズを設定
(define (screen-size w h)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (sys-set-screen-buffer-size hdl w h)))

;; 画面の表示エリアを設定
(define (screen-area x1 y1 x2 y2)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (sys-set-console-window-info hdl #t (s16vector x1 y1 x2 y2))))

;; 画面の左上のx座標を取得
(define (screen-left)
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl)))
    (slot-ref cinfo 'window.left)))

;; 画面の左上のy座標を取得
(define (screen-top)
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl)))
    (slot-ref cinfo 'window.top)))

;; 画面の幅を取得
(define (screen-width)
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl))
         (wl    (slot-ref cinfo 'window.left))
         (wr    (slot-ref cinfo 'window.right)))
    (+ (- wr wl) 1)))

;; 画面の高さを取得
(define (screen-height)
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl))
         (wt    (slot-ref cinfo 'window.top))
         (wb    (slot-ref cinfo 'window.bottom)))
    (+ (- wb wt) 1)))

;; 画面のバッファの幅を取得
(define (screen-buffer-width)
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl)))
    (slot-ref cinfo 'size.x)))

;; 画面のバッファの高さを取得
(define (screen-buffer-height)
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl)))
    (slot-ref cinfo 'size.y)))

;; カーソルのx座標を取得
(define (cursor-x)
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl)))
    (slot-ref cinfo 'cursor-position.x)))

;; カーソルのy座標を取得
(define (cursor-y)
  (let* ((hdl   (sys-get-std-handle STD_OUTPUT_HANDLE))
         (cinfo (sys-get-console-screen-buffer-info hdl)))
    (slot-ref cinfo 'cursor-position.y)))

;; カーソル非表示
(define (cursor-off)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #f))))

;; カーソル表示
(define (cursor-on)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #t))))

;; カーソル移動
(define (locate x y)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (sys-set-console-cursor-position hdl x y)))

;; 色属性の取得(内部処理用)
(define (get-color-attr fc bc)
  (rlet1 cattr 0
    (if (logtest fc COL_BLUE_MASK)  (set! cattr (logior cattr FOREGROUND_BLUE)))
    (if (logtest fc COL_GREEN_MASK) (set! cattr (logior cattr FOREGROUND_GREEN)))
    (if (logtest fc COL_RED_MASK)   (set! cattr (logior cattr FOREGROUND_RED)))
    (if (logtest fc COL_INTENSITY)  (set! cattr (logior cattr FOREGROUND_INTENSITY)))
    (if (logtest bc COL_BLUE_MASK)  (set! cattr (logior cattr BACKGROUND_BLUE)))
    (if (logtest bc COL_GREEN_MASK) (set! cattr (logior cattr BACKGROUND_GREEN)))
    (if (logtest bc COL_RED_MASK)   (set! cattr (logior cattr BACKGROUND_RED)))
    (if (logtest bc COL_INTENSITY)  (set! cattr (logior cattr BACKGROUND_INTENSITY)))))

;; 色設定
(define (color :optional (fc COL_GRAY) (bc COL_BLACK))
  (let ((hdl     (sys-get-std-handle STD_OUTPUT_HANDLE))
        (cattr   (get-color-attr fc bc)))
    (sys-set-console-text-attribute hdl cattr)))

;; キーボード入力待ち
(define (keywait)
  (let ((hdl     (sys-get-std-handle STD_INPUT_HANDLE))
        (cmode   0)
        (ch      0))
    (set! cmode (sys-get-console-mode hdl))
    (sys-set-console-mode hdl 0)
    (set! ch (read-char))
    (sys-set-console-mode hdl cmode)
    ch))

;; キーボード状態取得
(define (keystate)
  (let ((hdl     (sys-get-std-handle STD_INPUT_HANDLE))
        (cmode   0)
        (done    #f)
        (ir      '())
        (irlist  '())
        (retlist '()))
    (set! cmode (sys-get-console-mode hdl))
    (sys-set-console-mode hdl 0)
    (while (not done)
      (set! irlist (sys-peek-console-input hdl))
      (if (null? irlist)
        (set! done #t)
        (begin
          (sys-read-console-input hdl)
          (while (not (null? irlist))
            (set! ir     (car irlist))
            (set! irlist (cdr irlist))
            (let1 evt (slot-ref ir 'event-type)
              (if (= evt KEY_EVENT)
                (let* ((kdown (if (slot-ref ir 'key.down) 1 0))
                       (ch    (slot-ref ir 'key.unicode-char))
                       (vk    (slot-ref ir 'key.virtual-key-code))
                       (ctls  (slot-ref ir 'key.control-key-state))
                       (sft   (if (logtest ctls SHIFT_PRESSED) 1 0))
                       (ctl   (if (logtest ctls (logior RIGHT_CTRL_PRESSED LEFT_CTRL_PRESSED)) 1 0))
                       (alt   (if (logtest ctls (logior RIGHT_ALT_PRESSED  LEFT_ALT_PRESSED )) 1 0)))
                  ;(set! retlist (append! retlist (list (list kdown ch vk sft ctl alt))))
                  ;(set! retlist (cons (list kdown ch vk sft ctl alt) retlist)) ; 最後にリバースする必要あり
                  (push! retlist (list kdown ch vk sft ctl alt)) ; 最後にリバースする必要あり
                  )))))))
    (sys-set-console-mode hdl cmode)
    (reverse retlist)))

;; キーボード状態取得のテスト
(define (keystate-test)
  (print "HIT ANY KEY! ([ESC] TO EXIT)")
  (let ((done    #f)
        (ks      '())
        (kslist  '()))
    (while (not done)
      (set! kslist (keystate))
      ;(print kslist)
      (while (not (null? kslist))
        (set! ks     (car kslist))
        (set! kslist (cdr kslist))
        (receive (kdown ch vk sft ctl alt) (apply values ks)
          (cond
           ((and (= kdown 1) (= vk 27))
            (set! done #t)
            (set! kslist '()))
           (else
            (print " keydown=" kdown " unicode-char=" ch " virtual-key-code=" vk " shift=" sft " ctrl=" ctl " alt=" alt)))))
      (sys-nanosleep (* 100 1000000)))) ; 100msec
  (undefined))

;; キーボード入力待ち2
(define (keywait2 :optional (timeout 0) (interval 100))
  (let ((done    #f)
        (ks      '())
        (kslist  '())
        (timecount 0)
        ;; [shift]と[ctrl]と[alt]は除外。Windowsキーとアプリキーも除外
        (ignorevk  (list VK_SHIFT    VK_CONTROL  VK_MENU     VK_LWIN
                         VK_RWIN     VK_APPS     VK_LSHIFT   VK_RSHIFT
                         VK_LCONTROL VK_RCONTROL VK_LMENU    VK_RMENU)))
    (if (<= interval 0) (set! interval 100))
    (if (and (> timeout 0) (< timeout interval)) (set! interval timeout))
    (while (not done)
      (set! kslist (keystate))
      ;(print kslist)
      (while (not (null? kslist))
        (set! ks     (car kslist))
        (set! kslist (cdr kslist))
        (receive (kdown ch vk sft ctl alt) (apply values ks)
          (when (and (= kdown 1) (not (memv vk ignorevk)))
            (set! done #t)
            (set! kslist '()))))
      (when (not done)
        (sys-nanosleep (* interval 1000000))
        (when (> timeout 0)
          (set! timecount (+ timecount interval))
          (when (>= timecount timeout)
            (set! done #t)
            (set! ks '())))))
    ks))

;; キーボード入力クリア
(define (keyclear)
  (let1 hdl (sys-get-std-handle STD_INPUT_HANDLE)
    (sys-flush-console-input-buffer hdl)))

;; 文字列表示
(define (puttext str :optional (x 0) (y 0))
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (sys-write-console-output-character hdl str x y)))

;; 文字数と座標を指定して色設定
(define (putcolor n :optional (x 0) (y 0) (fc COL_GRAY) (bc COL_BLACK))
  (let ((hdl     (sys-get-std-handle STD_OUTPUT_HANDLE))
        (cattr   (get-color-attr fc bc)))
    (sys-fill-console-output-attribute hdl cattr n x y)))

;; タイトル設定
(define (set-console-title str)
  (sys-set-console-title str))

;; タイトル取得
(define (get-console-title)
  (sys-get-console-title))

