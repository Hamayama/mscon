;; -*- coding: utf-8 -*-
;;
;; mscon.scm
;; 2014-6-22 v1.01
;;
;; ＜内容＞
;;   Windows のコマンドプロンプトで Gauche(gosh.exe) を使うときに、
;;   コンソールの制御を可能とするモジュールです。
;;
;; ＜インストール方法＞
;;   mscon.scm を Gauche でロード可能なフォルダにコピーします。
;;   (例えば (gauche-site-library-directory) で表示されるフォルダ等)
;;
;; ＜使い方＞
;;   (use mscon)                  ; モジュールをロードします
;;   (cls)                        ; 画面をクリアします(コマンドのため遅い)
;;   (cls2)                       ; 画面をクリアします2(*)
;;   (screen-width)               ; 画面の幅を取得します(文字数)
;;   (screen-height)              ; 画面の高さを取得します(文字数)
;;   (cursor-on)                  ; カーソルを表示します
;;   (cursor-off)                 ; カーソルを非表示にします
;;   (color COL_GREEN)            ; 色を設定します
;;   (locate 10 10)               ; カーソルを座標(x,y)に移動します
;;   (print "HIT ANY KEY!")       ;
;;   (keywait)                    ; キーボードの入力を待ちます
;;   (keystate)                   ; キーボードの状態を取得します(*)
;;   (keystate-test)              ; キーボード状態取得テスト用です(*)
;;   (keywait2)                   ; キーボードの入力を待ちます2(タイムアウト設定可)(*)
;;   (keyclear)                   ; キーボードの入力をクリアします(*)
;;   (puttext "ABCDE" 10 10)      ; 座標(x,y)に文字列を表示します
;;   (putcolor 5 10 10 COL_GREEN) ; 座標(x,y)からn文字分に色を設定します(*)
;;
;;   (*)マークがある命令は Gauche v0.9.3.3 では使用できません。
;;      使用可能かどうかは (mscon-all-available?) でチェックできます。
;;
;; ＜注意事項＞
;;   (1)writeやdisplayが、指定したカーソル位置に表示しない(原因不明)。
;;
;;   (2)最下行にprintで表示すると、改行のためスクロールしてしまう。
;;
;;   (3)puttextが、colorで指定した色では表示しない。
;;      (色をつけるにはputcolorと併用する必要がある)
;;
;;   (4)リダイレクトには非対応(エラーになる)。
;;
;;   (5)Windows8上のGauche v0.9.3.3では、しばらく動いた後に実行時エラーが出る。
;;
(define-module mscon
  (use gauche.uvector)
  (use os.windows)
  (export
    mscon-all-available?
    cls cls2 screen-width screen-height cursor-on cursor-off
    color locate keywait keystate keystate-test keywait2 keyclear
    puttext putcolor
    COL_BLACK       COL_DARK_BLUE   COL_DARK_GREEN  COL_DARK_CYAN
    COL_DARK_RED    COL_DARK_VIOLET COL_DARK_YELLOW COL_GRAY
    COL_DARK_GRAY   COL_BLUE        COL_GREEN       COL_CYAN
    COL_RED         COL_VIOLET      COL_YELLOW      COL_WHITE
    COL_BLUE_MASK   COL_GREEN_MASK  COL_RED_MASK    COL_INTENSITY
    VK_SHIFT        VK_CONTROL      VK_MENU))
(select-module mscon)


;; 標準入出力のハンドルの保持
;; (保持しておかないとエラーになる。Gaucheの開発最新版では修正ずみ)
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
(define VK_SHIFT   16)
(define VK_CONTROL 17)
(define VK_MENU    18)


;; 全機能利用可能か
(define (mscon-all-available?)
  (guard (exc
          ((<error> exc) #f))
         (procedure? sys-fill-console-output-character)
         (procedure? sys-fill-console-output-attribute)
         (procedure? sys-flush-console-input-buffer)
         #t))

;; 画面クリア
(define (cls)
  (sys-system "cls")
  (undefined))

;; 画面クリア2
(define (cls2 :optional (fc COL_GRAY) (bc COL_BLACK))
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (let1 cinfo (sys-get-console-screen-buffer-info hdl)
      (let ((bw (slot-ref cinfo 'size.x))
            (bh (slot-ref cinfo 'size.y))
            (cattr (get-color-attr fc bc)))
        (sys-fill-console-output-attribute hdl cattr (* bw bh) 0 0)
        (sys-fill-console-output-character hdl " "   (* bw bh) 0 0)
        (sys-set-console-cursor-position hdl 0 0)))))

;; 画面の幅を取得
(define (screen-width)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (let1 cinfo (sys-get-console-screen-buffer-info hdl)
      (let ((wl (slot-ref cinfo 'window.left))
            (wr (slot-ref cinfo 'window.right)))
        (+ (- wr wl) 1)))))

;; 画面の高さを取得
(define (screen-height)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (let1 cinfo (sys-get-console-screen-buffer-info hdl)
      (let ((wt (slot-ref cinfo 'window.top))
            (wb (slot-ref cinfo 'window.bottom)))
        (+ (- wb wt) 1)))))

;; カーソル表示
(define (cursor-on)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #t))))

;; カーソル非表示
(define (cursor-off)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #f))))

;; 色属性の取得(内部処理用)
(define (get-color-attr fc bc)
  (let1 cattr 0
    (if (logtest fc COL_BLUE_MASK)  (set! cattr (logior cattr FOREGROUND_BLUE)))
    (if (logtest fc COL_GREEN_MASK) (set! cattr (logior cattr FOREGROUND_GREEN)))
    (if (logtest fc COL_RED_MASK)   (set! cattr (logior cattr FOREGROUND_RED)))
    (if (logtest fc COL_INTENSITY)  (set! cattr (logior cattr FOREGROUND_INTENSITY)))
    (if (logtest bc COL_BLUE_MASK)  (set! cattr (logior cattr BACKGROUND_BLUE)))
    (if (logtest bc COL_GREEN_MASK) (set! cattr (logior cattr BACKGROUND_GREEN)))
    (if (logtest bc COL_RED_MASK)   (set! cattr (logior cattr BACKGROUND_RED)))
    (if (logtest bc COL_INTENSITY)  (set! cattr (logior cattr BACKGROUND_INTENSITY)))
    cattr))

;; 色設定
(define (color :optional (fc COL_GRAY) (bc COL_BLACK))
  (let ((hdl (sys-get-std-handle STD_OUTPUT_HANDLE))
        (cattr (get-color-attr fc bc)))
    (sys-set-console-text-attribute hdl cattr)))

;; カーソル移動
(define (locate x y)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (sys-set-console-cursor-position hdl x y)))

;; キーボード入力待ち
(define (keywait)
  (let ((hdl     (sys-get-std-handle STD_INPUT_HANDLE))
        (cmode   0))
    (set! cmode (sys-get-console-mode hdl))
    (sys-set-console-mode hdl 0)
    (let1 ch (read-char)
      (sys-set-console-mode hdl cmode)
      ch)))

;; キーボード状態取得
(define (keystate)
  (let ((hdl     (sys-get-std-handle STD_INPUT_HANDLE))
        (cmode   0)
        (done    #f)
        (retlist '()))
    (set! cmode (sys-get-console-mode hdl))
    (sys-set-console-mode hdl 0)
    (while (not done)
      (let1 irlist (sys-peek-console-input hdl)
        (if (null? irlist)
          (set! done #t)
          (let ((ir  (car irlist))
                (evt 0))
            (set! evt (slot-ref ir 'event-type))
            (if (= evt KEY_EVENT)
              (let ((kdown (if (slot-ref ir 'key.down) 1 0))
                    (ch    (slot-ref ir 'key.unicode-char))
                    (vk    (slot-ref ir 'key.virtual-key-code))
                    (ctls  (slot-ref ir 'key.control-key-state))
                    (sft   0)
                    (ctl   0)
                    (alt   0))
                (if (logtest ctls SHIFT_PRESSED) (set! sft 1))
                (if (logtest ctls (logior RIGHT_CTRL_PRESSED LEFT_CTRL_PRESSED)) (set! ctl 1))
                (if (logtest ctls (logior RIGHT_ALT_PRESSED  LEFT_ALT_PRESSED )) (set! alt 1))
                ;(set! retlist (append! retlist (list (list kdown ch vk sft ctl alt))))
                ;(set! retlist (cons (list kdown ch vk sft ctl alt) retlist)) ; 最後にリバースする必要あり
                (push! retlist (list kdown ch vk sft ctl alt)) ; 最後にリバースする必要あり
                ))
            (sys-read-console-input hdl)))))
    (sys-set-console-mode hdl cmode)
    (reverse retlist)))

;; キーボード状態取得のテスト
(define (keystate-test)
  (print "HIT ANY KEY! ([ESC] TO EXIT)")
  (let1 done #f
    (while (not done)
      (let1 kslist (keystate)
        ;(print kslist)
        (while (not (null? kslist))
          (receive (kdown ch vk sft ctl alt) (apply values (car kslist))
            (cond
              ((and (= kdown 1) (= vk 27)) (set! done #t))
              ((not (and (= kdown 0) (= ch 0) (= vk 0) (= sft 0) (= ctl 0) (= alt 0)))
                (print " keydown=" kdown " unicode-char=" ch " virtual-key-code=" vk " shift=" sft " ctrl=" ctl " alt=" alt))))
          (set! kslist (cdr kslist)))
        (sys-nanosleep (* 100 1000000))))) ; 100msec
  (undefined))

;; キーボード入力待ち2
(define (keywait2 :optional (timeout 0) (interval 100))
  (let ((done      #f)
        (ks        '())
        (timecount 0))
    (while (not done)
      (let1 kslist (keystate)
        ;(print kslist)
        (while (not (null? kslist))
          (set! ks (car kslist))
          (receive (kdown ch vk sft ctl alt) (apply values ks)
            ;; [shift]と[ctrl]と[alt]は除外
            (if (and (= kdown 1) (not (or (= vk VK_SHIFT) (= vk VK_CONTROL) (= vk VK_MENU) (and (>= vk #xA0) (<= vk #xA5)))))
              (set! done #t)))
          (set! kslist (cdr kslist)))
        (sys-nanosleep (* interval 1000000))
        (if (> timeout 0)
          (begin
            (set! timecount (+ timecount interval))
            (if (>= timecount timeout)
              (set! done #t))))))
    ks))

;; キーボード入力クリア
(define (keyclear)
  (let1 hdl (sys-get-std-handle STD_INPUT_HANDLE)
    (sys-flush-console-input-buffer hdl)))

;; 文字列表示
(define (puttext str x y)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (sys-write-console-output-character hdl str x y)))

;; 文字数と座標を指定して色設定
(define (putcolor n x y :optional (fc COL_GRAY) (bc COL_BLACK))
  (let ((hdl (sys-get-std-handle STD_OUTPUT_HANDLE))
        (cattr (get-color-attr fc bc)))
    (sys-fill-console-output-attribute hdl cattr n x y)))

