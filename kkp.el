;;; kkp --- Enable emacs support for the Kitty Keyboard Protocol (one flavor of the CSI u encoding)  -*- lexical-binding: t -*-

;; Copyright (C) 2022  Benjamin Orthen

;; Author: Benjamin Orthen <contact@orthen.net>
;; Maintainer: Benjamin Orthen <contact@orthen.net>
;; Keywords: terminal
;; Version: 0.1
;; Homepage: https://github.com/benjaminor/kkp
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; The Kitty Keyboard Protocol (KKP) is documented here: https://sw.kovidgoyal.net/kitty/keyboard-protocol

;; kitty modifier encoding
;; shift     0b1         (1)
;; alt       0b10        (2)
;; ctrl      0b100       (4)
;; super     0b1000      (8)
;; hyper     0b10000     (16)
;; meta      0b100000    (32)
;; caps_lock 0b1000000   (64)
;; num_lock  0b10000000  (128)

;; Possible format of escape sequences sent to Emacs.
;; - CSI keycode u
;; - CSI keycode; modifier u
;; - CSI number ; modifier ~
;; - CSI {ABCDEFHPQRS}
;; - CSI 1; modifier {ABCDEFHPQRS}


;;; Code:

(require 'cl-lib)

(defvar kkp--printable-key-modifiers
  '(("C-" 5)
    ("C-M-" 7)
    ("C-M-" 37)
    ("M-S-" 4)
    ("M-S-" 34)
    ("M-" 3)
    ("M-" 33)
    ;; the second half is just the modifiers but with the num_lock encoding (128) added
    ("C-" 133)
    ("C-M-" 125)
    ("C-M-" 165)
    ("M-S-" 132)
    ("M-S-" 162)
    ("M-" 131)
    ("M-" 161))
  "List of combination of kbd modifiers for printable keys with their encoding.")

(defvar kkp--key-modifiers
  '(("M-" 3)
    ("M-" 33)
    ("C-" 5)
    ("S-" 2)
    ("C-S-" 6)
    ("C-M-" 7)
    ("C-M-" 37)
    ("M-S-" 4)
    ("M-S-" 34)
    ("C-M-S-" 8)
    ("C-M-S-" 38)
    ;; the second half is just the modifiers but with the numlock encoding (128) added
    ("M-" 131)
    ("M-" 161)
    ("C-" 133)
    ("S-" 130)
    ("C-S-" 134)
    ("C-M-" 125)
    ("C-M-" 165)
    ("M-S-" 132)
    ("M-S-" 162)
    ("C-M-S-" 136)
    ("C-M-S-" 166))
  "List of combination of kbd modifiers for non-printable keys with their encoding.")

(defvar kkp--printable-keys
  (let ((ascii-1 (cl-loop for c from 32 to 64 collect c))
        (ascii-2 (cl-loop for c from 91 to 126 collect c)))
    (cl-concatenate 'list ascii-1 ascii-2))
  "List of all printable ascii chars excluding upper case letters.")


;; when not found in emacs source code, taken from http://xahlee.info/linux/linux_show_keycode_keysym.html
(defvar kkp--non-printable-keys-with-u-terminator
  '(("<escape>" 27)
    ("<return>" 13)
    ("<tab>" 9)
    ("<backspace>" 127)
    ("<Caps_Lock>" 57358)
    ("<Scroll_Lock>" 57359)
    ("<kp-numlock>" 57360)
    ("<print>" 57361)
    ("<pause>" 57362)
    ("<menu>" 57363)
    ("<f13>" 57376)
    ("<f14>" 57377)
    ("<f15>" 57378)
    ("<f16>" 57379)
    ("<f17>" 57380)
    ("<f18>" 57381)
    ("<f19>" 57382)
    ("<f20>" 57383)
    ("<f21>" 57384)
    ("<f22>" 57385)
    ("<f23>" 57386)
    ("<f24>" 57387)
    ("<f25>" 57388)
    ("<f26>" 57389)
    ("<f27>" 57390)
    ("<f28>" 57391)
    ("<f29>" 57392)
    ("<f30>" 57393)
    ("<f31>" 57394)
    ("<f32>" 57395)
    ("<f33>" 57396)
    ("<f34>" 57397)
    ("<f35>" 57398)
    ("<kp-0>" 57399)
    ("<kp-1>" 57400)
    ("<kp-2>" 57401)
    ("<kp-3>" 57402)
    ("<kp-4>" 57403)
    ("<kp-5>" 57404)
    ("<kp-6>" 57405)
    ("<kp-7>" 57406)
    ("<kp-8>" 57407)
    ("<kp-9>" 57408)
    ("<kp-decimal>" 57409)
    ("<kp-divide>" 57410)
    ("<kp-multiply>" 57411)
    ("<kp-subtract>" 57412)
    ("<kp-add>" 57413)
    ("<kp-enter>" 57414)
    ("<kp-equal>" 57415)
    ("<kp-separator>" 57416)
    ("<kp-left>" 57417)
    ("<kp-right>" 57418)
    ("<kp-up>" 57419)
    ("<kp-down>" 57420)
    ("<kp-prior>" 57421) ;; KP_PAGE_UP
    ("<kp-next>" 57422) ;; KP_PAGE_DOWN
    ("<kp-home>" 57423)
    ("<kp-end>" 57424)
    ("<kp-insert>" 57425)
    ("<kp-delete>" 57426)
    ("<media-play>" 57428)
    ("<media-pause>" 57429)
    ("<media-play-pause>" 57430)
    ("<media-reverse>" 57431)
    ("<media-stop>" 57432)
    ("<media-fast-forward>" 57433)
    ("<media-rewind>" 57434)
    ("<media-next>" 57435) ;; MEDIA_TRACK_NEXT
    ("<media-previous>" 57436) ;; MEDIA_TRACK_PREVIOUS
    ("<media-record>" 57437)
    ("<volume-down>" 57438)
    ("<volume-up>" 57439)
    ("<volume-mute>" 57440)

    ;; it is rather unlikely Emacs gets this keysequence directly from the terminal
    ;; but just for the case...
    ("<SHIFT_L>" 57441)
    ("<Control_L>" 57442)
    ("<Alt_L>" 57443)
    ("<Super_L>" 57444)
    ("<Hyper_L>" 57445)
    ("<Meta_L>" 57446)
    ("<Shift_R>" 57447)
    ("<Control_R>" 57448)
    ("<Alt_R>" 57449)
    ("<Super_R>" 57450)
    ("<Hyper_R>" 57451)
    ("<Meta_R>" 57452)
    ("<ISO_Level3_Shift>" 57453)
    ("<ISO_Level5_Shift>" 57454)))

(defvar kkp--non-printable-keys-with-tilde-terminator
  '(("<insert>" 2)
    ("<delete>" 3)
    ("<prior>" 5)
    ("<next>" 6)
    ("<home>" 7)
    ("<end>" 8)
    ("<f1>" 11)
    ("<f2>" 12)
    ("<f3>" 13)
    ("<f4>" 14)
    ("<f5>" 15)
    ("<f6>" 17)
    ("<f7>" 18)
    ("<f8>" 19)
    ("<f9>" 20)
    ("<f10>" 21)
    ("<f11>" 23)
    ("<f12>" 24)
    ("<kp-begin>" 57427)))


(defvar kkp--non-printable-keys-with-letter-terminator
  '(("<left>" "D")
    ("<right>" "C")
    ("<up>" "A")
    ("<down>" "B")
    ("<home>" "H")
    ("<end>" "F")
    ("<f1>" "P")
    ("<f2>" "Q")
    ("<f3>" "R")
    ("<f4>" "S")
    ("<kp-begin>" "E")))

(defun kkp--csi-escape (&rest args)
  "Prepend the CSI bytes before the ARGS."
  (format "\e[%s" (apply 'concat args)))


(defun kkp--add-modified-printable-keys (map keychar)
  "Add the ascii KEYCHAR with each possible modifier combination to the keymap MAP."
  (dolist (bind kkp--key-modifiers)
    (let*
        ((keysequence (kkp--csi-escape (format "%s;%su" keychar (nth 1 bind))))
         (kbd-encoded-char (if (eql keychar 32) ;; space character
                               "<SPC>"
                             (encode-coding-char keychar 'ascii)))
         (keybinding (kbd (concat (nth 0 bind) kbd-encoded-char))))
      (define-key map keysequence keybinding))))


(defun kkp--add-modified-non-printable-keys (map keystring keycode terminator)
  "Add the KEYSTRING with each possible modifier combination to the keymap MAP.
Each KEYSTRING has a protocol-defined associated KEYCODE and TERMINATOR
for its escape sequence which is sent by the terminal to Emacs.
Examples of this include <escape> with keycode 27 and terminator u."

  ;; without any modifiers
  (define-key map (kkp--csi-escape (format "%s%s" keycode terminator)) (kbd keystring))

  ;; with modifiers
  (dolist (bind kkp--key-modifiers)
    (let
        ((keysequence (kkp--csi-escape (format "%s;%s%s" keycode (nth 1 bind) terminator)))
         (keybinding (kbd (concat (nth 0 bind) keystring))))
      (define-key map keysequence keybinding))))


(defun kkp--add-modified-letter-terminated-keys (map keystring terminator)
  "Add the KEYSTRING with each possible modifier combination to the keymap MAP.
Each KEYSTRING has a protocol-defined TERMINATOR
for its escape sequence which is sent by the terminal to Emacs."

  (define-key map (kkp--csi-escape (format "%s" terminator)) (kbd keystring))
  (dolist (bind kkp--key-modifiers)
    (let
        ((keysequence (kkp--csi-escape (format "1;%s%s" (nth 1 bind) terminator)))
         (keybinding (kbd (concat (nth 0 bind) keystring))))
      (define-key map keysequence keybinding))))

(defvar kkp--disambiguate-escape-code-decode-map
  (let ((map (make-sparse-keymap "kkp-translation-map")))
    (dolist (elt kkp--printable-keys)
      (kkp--add-modified-printable-keys map elt))
    (dolist (elt kkp--non-printable-keys-with-u-terminator)
      (kkp--add-modified-non-printable-keys map (nth 0 elt) (nth 1 elt) "u"))
    (dolist (elt kkp--non-printable-keys-with-tilde-terminator)
      (kkp--add-modified-non-printable-keys map (nth 0 elt) (nth 1 elt) "~"))
    (dolist (elt kkp--non-printable-keys-with-letter-terminator)
      (kkp--add-modified-letter-terminated-keys map (nth 0 elt) (nth 1 elt)))

    map))

(defvar kkp-terminal-query-timeout 0.1
  "Seconds to wait for an answer from the terminal. Nil means no timeout.")

(defvar kkp--progressive-enhancement-flags
  '((disambiguate-escape-codes . (:bit 1 :activation-fn 'kkp--enable-disambiguate-escape-codes))))

(defvar kkp-active-enhancements
  '(disambiguate-escape-codes)
  "List of enhancements which should be enabled.
Possible values are the keys in `kkp--progressive-enhancement-flags`.")

(defun kkp--get-enhancement-bit (enhancement)
  "Get the bitflag which enables the ENHANCEMENT."
  (plist-get (cdr enhancement) :bit))


(defun kkp--query-terminal-sync (query &optional terminal)
  "Send QUERY to TERMINAL (to current if nil) and return response (if any)."
  (discard-input)
  (send-string-to-terminal (kkp--csi-escape query) terminal)
  (let ((cond t)
        (terminal-input nil))
    (while cond
      (let ((evt (read-event nil nil kkp-terminal-query-timeout)))
        (if (null evt)
            (setq cond nil)
          (push evt terminal-input)
          )))
    (nreverse terminal-input) ))

(defun kkp--query-terminal-async (query handlers)
  "Send QUERY string to the terminal and watch for a response.
HANDLERS is an alist with elements of the form (STRING . FUNCTION).
We run the first FUNCTION whose STRING matches the input events.
This function code is copied from `xterm--query`."
  (let ((register
         (lambda (handlers)
           (dolist (handler handlers)
             (define-key input-decode-map (car handler)
                         (lambda (&optional _prompt)
                           ;; Unregister the handler, since we don't expect
                           ;; further answers.
                           (dolist (handler handlers)
                             (define-key input-decode-map (car handler) nil))
                           (funcall (cdr handler))
                           []))))))

    (funcall register handlers)
    (send-string-to-terminal (kkp--csi-escape query))))


(defun kkp--enabled-terminal-enhancements ()
  "Query the terminal and return list of currently enabled enhancements."
  (let ((reply (kkp--query-terminal-sync "?u")))
    (when (not reply)
      (error "Terminal did not reply correctly to query"))

    (let ((intflag (- (nth 3 reply) ?0))
          (enabled-enhancements nil))

      (dolist (bind kkp--progressive-enhancement-flags)
        (when (> (logand intflag (kkp--get-enhancement-bit bind)) 0)
          (push (car bind) enabled-enhancements)))
      enabled-enhancements)))


(defun kkp--check-terminal-supports-kkp ()
  "Check if the terminal supports the Kitty Keyboard Protocol."
  (let ((reply (kkp--query-terminal-sync "?u")))
    (and
     (eql (length reply) 5) ;; TODO response can be 6 bytes long
     (equal '(27 91 63) (cl-subseq reply 0 3))
     (eql 117 (nth 4 reply)))))



(defun kkp--enable-disambiguate-escape-codes ()
  "Add new kkp keymap as parent of terminal-local input-decode-map."
  (set-keymap-parent kkp--disambiguate-escape-code-decode-map (keymap-parent input-decode-map))
  (set-keymap-parent input-decode-map kkp--disambiguate-escape-code-decode-map))

(defun kkp--calculate-flags-integer ()
  "Calculate the flag integer to send to the terminal to activate the enhancemets."
  (cl-reduce #'(lambda (sum elt) (+ sum
                               (kkp--get-enhancement-bit (assoc elt kkp--progressive-enhancement-flags))))
             kkp-active-enhancements :initial-value 0))

(defun kkp--pop-terminal-flag (&optional terminal)
  "Send query to TERMINAL (if nil, the current one) to pop previously pushed flags."
  (unless (display-graphic-p)
    (kkp--query-terminal-sync "<u") terminal))

(defun kkp-check-progressive-enhancement-flags ()
  "Message, if terminal supports kkp, currently enabled enhancements."
  (interactive)
  (when (kkp--check-terminal-supports-kkp)
    (message "%s" (kkp--enabled-terminal-enhancements))))

(defun kkp-reset-flags ()
  "Disable in this terminal where command is executed, the activated enhancements."
  (interactive)
  (kkp--query-terminal-sync "=0;1u"))

;; TODO
;; (defun kkp-disable-completely()
;;   "Disable in emacs the activated keyboard enhancements."
;;   )

(defun kkp-enable ()
  "Enable support for the Kitty Keyboard Protocol.
This activates, if a terminal supports it, the protocol enhancements
specified in `kkp-active-enhancements`."

  (push 'kkp--pop-terminal-flag delete-frame-functions)
  ;; call setup for future terminals to be opened
  (add-hook 'tty-setup-hook 'kkp-terminal-setup)

  ;; call setup for this terminal (if it is a terminal, else this does nothing)
  (kkp-terminal-setup))


(defun kkp--terminal-setup ()
  "Run setup to enable KKP support."
  (let ((a (read-event))
        (b (read-event)))
    (when (and (<= ?0 a ?9) ;; TODO: flag can be two bytes
               (eql b ?u))

      (message "Enabled KKP support in this terminal.")
      (let ((intflag (kkp--calculate-flags-integer)))
        (unless (eq intflag 0)
          (kkp--query-terminal-sync (format ">%su" intflag))

          (mapc #'(lambda (elt) (funcall (eval (plist-get (alist-get elt kkp--progressive-enhancement-flags) :activation-fn))))
                kkp-active-enhancements)

          (let ((frame (selected-frame)))
            (add-hook 'kill-emacs-hook `(lambda () (kkp--pop-terminal-flag ',frame)))))))))

;; this function can be called more than one time, as each time we call it, we also add an pop-flag function to the exit hooks
(defun kkp-terminal-setup ()
  "Enable KKP support in Emacs running in the terminal."
  (unless
      (display-graphic-p)
    (kkp--query-terminal-async "?u"
                               '(("\e[?" . kkp--terminal-setup)))))

(provide 'kkp)
;;; kkp.el ends here
