;;; kkp.el --- Enable support for the Kitty Keyboard Protocol -*- lexical-binding: t -*-

;; Copyright (C) 2023  Benjamin Orthen

;; Author: Benjamin Orthen <contact@orthen.net>
;; Maintainer: Benjamin Orthen <contact@orthen.net>
;; Keywords: terminals
;; Version: 0.2
;; URL: https://github.com/benjaminor/kkp
;; Package-Requires: ((emacs "27.1") (compat "29.1.3.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; kkp.el enables support for the Kitty Keyboard Protocol in Emacs.
;; This protocol is documented here:
;; https://sw.kovidgoyal.net/kitty/keyboard-protocol. It provides an
;; alternative, improved way to transmit keyboard input from a
;; terminal to Emacs running in that terminal.

;;; Code:

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


(require 'cl-lib)
(require 'compat)
(require 'term/xterm)

(defgroup kkp nil
  "Kitty Keyboard Protocol (KKP) support."
  :group 'convenience
  :prefix "kkp-")

(defcustom kkp-terminal-query-timeout 0.1
  "Seconds to wait for an answer from the terminal. Nil means no timeout."
  :type 'float)

(defcustom kkp-active-enhancements
  '(disambiguate-escape-codes report-alternate-keys)
  "List of enhancements which should be enabled.
Possible values are the keys in `kkp--progressive-enhancement-flags'."
  :type '(repeat (choice (const disambiguate-escape-codes) (const report-alternate-keys))))

(defvar kkp--progressive-enhancement-flags
  '((disambiguate-escape-codes . (:bit 1))
    (report-alternate-keys . (:bit 4))))

(defconst kkp--modifiers
  '(choice (const shift) (const alt) (const control)
           (const super) (const hyper) (const meta)
           (const caps-lock) (const num-lock)))

;; These mirror the behavior of `mac-command-modifier' and friends.
;; They specify which virtual key the physical key maps to.
(defcustom kkp-shift-modifier 'shift
  "This variable describes the behavior of the shift key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-alt-modifier 'meta
  "This variable describes the behavior of the alt key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-control-modifier 'control
  "This variable describes the behavior of the ctrl key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-super-modifier 'super
  "This variable describes the behavior of the super key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-hyper-modifier 'hyper
  "This variable describes the behavior of the hyper key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-meta-modifier 'meta
  "This variable describes the behavior of the meta key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-caps-lock-modifier 'caps-lock
  "This variable describes the behavior of the caps key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

(defcustom kkp-num-lock-modifier 'num-lock
  "This variable describes the behavior of the num key.

It is one of the symbols `shift', `alt', `control', `super',
`hyper', `meta', `caps-lock' or `num-lock'."
  :type kkp--modifiers)

;; kitty modifier encoding
(put 'kkp-shift-modifier :encoding 1)
(put 'kkp-alt-modifier :encoding 2)
(put 'kkp-control-modifier :encoding 4)
(put 'kkp-super-modifier :encoding 8)
(put 'kkp-hyper-modifier :encoding 16)
(put 'kkp-meta-modifier :encoding 32)
(put 'kkp-caps-lock-modifier :encoding 64)
(put 'kkp-num-lock-modifier :encoding 128)

(defvar kkp--printable-ascii-letters
  (cl-loop for c from ?a to ?z collect c))

;; when not found in emacs source code, taken from http://xahlee.info/linux/linux_show_keycode_keysym.html
(defvar kkp--non-printable-keys-with-u-terminator
  '((27 . "<escape>")
    (13 . "<return>")
    (?\s . "SPC")
    (9 . "<tab>")
    (127 . "<backspace>")
    (57358 . "<Caps_Lock>")
    (57359 . "<Scroll_Lock>")
    (57360 . "<kp-numlock>")
    (57361 . "<print>")
    (57362 . "<pause>")
    (57363 . "<menu>")
    (57376 . "<f13>")
    (57377 . "<f14>")
    (57378 . "<f15>")
    (57379 . "<f16>")
    (57380 . "<f17>")
    (57381 . "<f18>")
    (57382 . "<f19>")
    (57383 . "<f20>")
    (57384 . "<f21>")
    (57385 . "<f22>")
    (57386 . "<f23>")
    (57387 . "<f24>")
    (57388 . "<f25>")
    (57389 . "<f26>")
    (57390 . "<f27>")
    (57391 . "<f28>")
    (57392 . "<f29>")
    (57393 . "<f30>")
    (57394 . "<f31>")
    (57395 . "<f32>")
    (57396 . "<f33>")
    (57397 . "<f34>")
    (57398 . "<f35>")
    (57399 . "<kp-0>")
    (57400 . "<kp-1>")
    (57401 . "<kp-2>")
    (57402 . "<kp-3>")
    (57403 . "<kp-4>")
    (57404 . "<kp-5>")
    (57405 . "<kp-6>")
    (57406 . "<kp-7>")
    (57407 . "<kp-8>")
    (57408 . "<kp-9>")
    (57409 . "<kp-decimal>")
    (57410 . "<kp-divide>")
    (57411 . "<kp-multiply>")
    (57412 . "<kp-subtract>")
    (57413 . "<kp-add>")
    (57414 . "<kp-enter>")
    (57415 . "<kp-equal>")
    (57416 . "<kp-separator>")
    (57417 . "<kp-left>")
    (57418 . "<kp-right>")
    (57419 . "<kp-up>")
    (57420 . "<kp-down>")
    (57421 . "<kp-prior>") ;; KP_PAGE_UP
    (57422 . "<kp-next>") ;; KP_PAGE_DOWN
    (57423 . "<kp-home>")
    (57424 . "<kp-end>")
    (57425 . "<kp-insert>")
    (57426 . "<kp-delete>")
    (57428 . "<media-play>")
    (57429 . "<media-pause>")
    (57430 . "<media-play-pause>")
    (57431 . "<media-reverse>")
    (57432 . "<media-stop>")
    (57433 . "<media-fast-forward>")
    (57434 . "<media-rewind>")
    (57435 . "<media-next>") ;; MEDIA_TRACK_NEXT
    (57436 . "<media-previous>") ;; MEDIA_TRACK_PREVIOUS
    (57437 . "<media-record>")
    (57438 . "<volume-down>")
    (57439 . "<volume-up>")
    (57440 . "<volume-mute>")

    ;; it is rather unlikely Emacs gets this keysequence directly from the terminal
    ;; but just for the case...
    (57441 . "<SHIFT_L>")
    (57442 . "<Control_L>")
    (57443 . "<Alt_L>")
    (57444 . "<Super_L>")
    (57445 . "<Hyper_L>")
    (57446 . "<Meta_L>")
    (57447 . "<Shift_R>")
    (57448 . "<Control_R>")
    (57449 . "<Alt_R>")
    (57450 . "<Super_R>")
    (57451 . "<Hyper_R>")
    (57452 . "<Meta_R>")
    (57453 . "<ISO_Level3_Shift>")
    (57454 . "<ISO_Level5_Shift>")))

(defvar kkp--non-printable-keys-with-tilde-terminator
  '((2 . "<insert>")
    (3 . "<delete>")
    (5 . "<prior>")
    (6 . "<next>")
    (7 . "<home>")
    (8 . "<end>")
    (11 . "<f1>")
    (12 . "<f2>")
    (13 . "<f3>")
    (14 . "<f4>")
    (15 . "<f5>")
    (17 . "<f6>")
    (18 . "<f7>")
    (19 . "<f8>")
    (20 . "<f9>")
    (21 . "<f10>")
    (23 . "<f11>")
    (24 . "<f12>")
    (57427 . "<kp-begin>")))

(defvar kkp--keycode-mapping
  (cl-concatenate 'list kkp--non-printable-keys-with-u-terminator kkp--non-printable-keys-with-tilde-terminator))

(defvar kkp--non-printable-keys-with-letter-terminator
  '((?A . "<up>")
    (?B . "<down>")
    (?C . "<right>")
    (?D . "<left>")
    (?E . "<kp-begin>")
    (?F . "<end>")
    (?H . "<home>")
    (?P . "<f1>")
    (?Q . "<f2>")
    (?R . "<f3>") ;; TODO remove in the future as this conflicts with *Cursor Position Report* (https://github.com/kovidgoyal/kitty/commit/cd92d50a0d34b7c3525377770fba47ce75cf1cfc)
    (?S . "<f4>")))

(defvar kkp--letter-terminators
  '(?A ?B ?C ?D ?E ?F ?H ?P ?Q ?R ?S))

(defvar kkp--acceptable-terminators
  (cl-concatenate 'list '(?u ?~) kkp--letter-terminators))

(defvar kkp--key-prefixes
  (cl-concatenate 'list
                  (cl-loop for c from ?1 to ?9 collect c)
                  kkp--letter-terminators))

(defvar kkp--active-terminal-list
  nil "Internal variable to track terminals which have enabled KKP.")

(defvar kkp--setup-initial-terminal-list
  nil "Internal variable to track initial terminals when enabling `global-kkp-mode??.")

(defvar kkp--setup-visited-terminal-list
  nil "Internal variable to track visited terminals after enabling `global-kkp-mode??.")


(defun kkp--mod-bits (modifier)
  "Return the KKP encoding bits that should be interpreted as MODIFIER.

MODIFIER is one of the symbols `shift', `alt', `control',
`super', `hyper', `meta', `caps-lock' or `num-lock'."
  (apply #'logior
         (cl-map 'sequence
                 (lambda (sym)
                   (get sym :encoding))
                 (cl-remove-if-not
                  (lambda (sym) (eq (symbol-value sym) modifier))
                  '(kkp-shift-modifier
                    kkp-alt-modifier
                    kkp-control-modifier
                    kkp-super-modifier
                    kkp-hyper-modifier
                    kkp-meta-modifier
                    kkp-caps-lock-modifier
                    kkp-num-lock-modifier)))))

(defun kkp--csi-escape (&rest args)
  "Prepend the CSI bytes before the ARGS."
  (concat "\e[" (apply #'concat args)))

(defun kkp--selected-terminal ()
  "Get the terminal that is now selected."
  (frame-terminal (selected-frame)))

(defun kkp--cl-split (separator seq)
  "Split SEQ along SEPARATOR and return all subsequences."
  (let ((pos 0)
        (last-pos 0)
        (len (length seq))
        (subseqs nil))
    (while (< pos len)
      (setq pos (cl-position separator seq :start last-pos))
      (unless pos
        (setq pos len))
      (let ((subseq (cl-subseq seq last-pos pos)))
        (push subseq subseqs))
      (setq last-pos (1+ pos)))
    (nreverse subseqs)))

(defun kkp--ascii-chars-to-number (seq)
  "Calculate an integer from a sequence SEQ of decimal ascii bytes."
  (string-to-number (apply #'string seq)))

(defun kkp--bit-set-p (num bit)
  "Check if BIT is set in NUM."
  (not (eql (logand num bit) 0)))

(defun kkp--create-modifiers-string (modifier-num)
  "Create a string of Emacs key modifiers according to MODIFIER-NUM."

  ;; add modifiers as defined in key-valid-p
  ;; Modifiers have to be specified in this order:
  ;;    A-C-H-M-S-s which is
  ;;    Alt-Control-Hyper-Meta-Shift-super

  (let ((key-str ""))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'alt))
      (setq key-str (concat key-str "A-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'control))
      (setq key-str (concat key-str "C-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'hyper))
      (setq key-str (concat key-str "H-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'meta))
      (setq key-str (concat key-str "M-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'shift))
      (setq key-str (concat key-str "S-")))
    (when (kkp--bit-set-p modifier-num (kkp--mod-bits 'super))
      (setq key-str (concat key-str "s-")))
    key-str))


(defun kkp--get-keycode-representation (keycode)
  "Try to lookup the Emacs key representation for KEYCODE.
This is either in the mapping or it is the string representation of the
key codepoint."
  (let ((rep (alist-get keycode kkp--keycode-mapping)))
    (if rep
        rep
      (string keycode))))


(defun kkp--process-keys (first-byte)
  "Read input from terminal to parse key events to an Emacs keybinding.
FIRST-BYTE is the byte read before this function is called.
This function returns the Emacs keybinding associated with the sequence read."

  (let ((terminal-input (list first-byte)))

    ;; read in events from stdin until we have found a terminator
    (while (not (member (cl-first terminal-input) kkp--acceptable-terminators))
      (let ((evt (read-event)))
        (push evt terminal-input)))

    ;; reverse input has we pushed events to the front of the list
    (setq terminal-input (nreverse terminal-input))

    ;; three different terminator types: u, ~, and letters
    ;; u und tilde have same structure
    ;; letter terminated sequences have a different structure
    (let
        ((terminator (car (last terminal-input))))
      (cond

       ;; this protocol covers all keys with a prefix in `kkp--key-prefixes' except for this external one
       ((equal terminal-input "\e[200~")
        #'xterm-translate-bracketed-paste)

       ;; input has this form: keycode[:[shifted-key][:base-layout-key]];[modifiers[:event-type]][;text-as-codepoints]{u~}
       ((member terminator '(?u ?~))
        (let* ((splitted-terminal-input (kkp--cl-split ?\; (remq terminator terminal-input)))
               (splitted-keycodes (kkp--cl-split ?: (cl-first splitted-terminal-input))) ;; get keycodes from sequence
               (splitted-modifiers-events (kkp--cl-split ?: (cl-second splitted-terminal-input))) ;; list of modifiers and event types
               ;; (text-as-codepoints (cl-third splitted-terminal-input))
               (primary-key-code (cl-first splitted-keycodes))
               (shifted-key-code (cl-second splitted-keycodes))
               (modifiers (cl-first splitted-modifiers-events))
               (key-code nil)
               (modifier-num
                (if modifiers
                    (1-
                     (kkp--ascii-chars-to-number modifiers))
                  0)))

          ;; check if keycode has shifted key:
          ;; set key-code to shifted key-code & remove shift from modifiers
          (if
              (and
               shifted-key-code
               (not (member (kkp--ascii-chars-to-number primary-key-code) kkp--printable-ascii-letters)))
              (progn
                (setq key-code shifted-key-code)
                (setq modifier-num (logand modifier-num (lognot 1))))
            (setq key-code primary-key-code))

          ;; create keybinding by concatenating the modifier string with the key-name
          (let
              ((modifier-str (kkp--create-modifiers-string modifier-num))
               (key-name (kkp--get-keycode-representation (kkp--ascii-chars-to-number key-code))))
            (kbd (concat modifier-str key-name)))))

       ;; terminal input has this form [1;modifier[:event-type]]{letter}
       ((member terminator kkp--letter-terminators)
        (let* ((splitted-terminal-input (kkp--cl-split ?\; (remq terminator terminal-input)))
               (splitted-modifiers-events (kkp--cl-split ?: (cl-second splitted-terminal-input))) ;; list of modifiers and event types
               (modifiers (cl-first splitted-modifiers-events)) ;; get modifiers
               (modifier-num
                (if modifiers
                    (1-
                     (kkp--ascii-chars-to-number modifiers))
                  0)))
          (let
              ((modifier-str (kkp--create-modifiers-string modifier-num))
               (key-name (alist-get terminator kkp--non-printable-keys-with-letter-terminator)))
            (kbd (concat modifier-str key-name)))))))))


(defun kkp--get-enhancement-bit (enhancement)
  "Get the bitflag which enables the ENHANCEMENT."
  (plist-get (cdr enhancement) :bit))


(defun kkp--query-terminal-sync (query)
  "Send QUERY to TERMINAL (to current if nil) and return response (if any)."
  (discard-input)
  (send-string-to-terminal (kkp--csi-escape query))
  (let ((loop-cond t)
        (terminal-input nil))
    (while loop-cond
      (let ((evt (read-event nil nil kkp-terminal-query-timeout)))
        (if (null evt)
            (setq loop-cond nil)
          (push evt terminal-input))))
    (nreverse terminal-input)))


(defun kkp--query-terminal-async (query handlers terminal)
  "Send QUERY string to TERMINAL and register HANDLERS for a response.
HANDLERS is an alist with elements of the form (STRING . FUNCTION).
We run the first FUNCTION whose STRING matches the input events.
This function code is copied from `xterm--query'."
  (with-selected-frame (car (frames-on-display-list terminal))
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
      (send-string-to-terminal (kkp--csi-escape query) terminal))))


(defun kkp--enabled-terminal-enhancements ()
  "Query the current terminal and return list of currently enabled enhancements."
  (let ((reply (kkp--query-terminal-sync "?u")))
    (when (not reply)
      (error "Terminal did not reply correctly to query"))

    (let ((enhancement-flag (- (nth 3 reply) ?0))
          (enabled-enhancements nil))

      (dolist (bind kkp--progressive-enhancement-flags)
        (when (> (logand enhancement-flag (kkp--get-enhancement-bit bind)) 0)
          (push (car bind) enabled-enhancements)))
      enabled-enhancements)))


(defun kkp--terminal-supports-kkp-p ()
  "Check if the current terminal supports the Kitty Keyboard Protocol.
This does not work well if checking for another terminal which
does not have focus, as input from this terminal cannot be reliably read."
  (let ((reply (kkp--query-terminal-sync "?u")))
    (and
     (member (length reply) '(5 6))
     (equal '(27 91 63) (cl-subseq reply 0 3))
     (eql 117 (car (last reply))))))


(defun kkp--calculate-flags-integer ()
  "Calculate the flag integer to send to the terminal to activate the enhancements."
  (cl-reduce (lambda (sum elt)
               (+
                sum
                (kkp--get-enhancement-bit (assoc elt kkp--progressive-enhancement-flags))))
             kkp-active-enhancements :initial-value 0))


(defun kkp--terminal-teardown (terminal)
  "Run procedures to disable KKP in TERMINAL."
  (when
      (member terminal kkp--active-terminal-list)
    (send-string-to-terminal (kkp--csi-escape "<u") terminal)
    (setq kkp--active-terminal-list (delete terminal kkp--active-terminal-list))
    (with-selected-frame (car (frames-on-display-list terminal))
      (dolist (prefix kkp--key-prefixes)
        (compat-call define-key input-decode-map (kkp--csi-escape (string prefix)) nil t)))))


(defun kkp--terminal-read-and-ignore (terminator)
  "Read from terminal until TERMINATOR, ignore input."
  (let ((inhibit-redisplay t)
        chr)
    (while (and (setq chr (read-event)) (not (equal chr terminator)))
      (ignore))))

(defun kkp--terminal-setup ()
  "Run setup to enable KKP support in current terminal.
This does not work well if checking for another terminal which
does not have focus, as input from this terminal cannot be reliably read."
  ;; we do not really care about the response, but if we would, we could check
  ;; the response matches a positive integer:
  ;; (string-match-p "^\\(?:0\\|[1-9][0-9]*\\)$" response-from-terminal)
  (kkp--terminal-read-and-ignore ?u)
  (let ((terminal (kkp--selected-terminal)))
    (unless (member terminal kkp--active-terminal-list)
      (let ((enhancement-flag (kkp--calculate-flags-integer)))
        (unless (eq enhancement-flag 0)

          (send-string-to-terminal (kkp--csi-escape (format ">%su" enhancement-flag)) terminal)

          (push terminal kkp--active-terminal-list)

          ;; we register functions for each prefix to not interfere with e.g., M-[ I
          (with-selected-frame (car (frames-on-display-list terminal))
            (dolist (prefix kkp--key-prefixes)
              (define-key input-decode-map (kkp--csi-escape (string prefix))
                          (lambda (_prompt) (kkp--process-keys prefix))))))))))


(defun kkp--disable-in-active-terminals()
  "In all terminals with active KKP, pop the previously pushed enhancement flag."
  (dolist (terminal kkp--active-terminal-list)
    (kkp--terminal-teardown terminal)))

(cl-defun kkp-enable-in-terminal (&optional (terminal (kkp--selected-terminal)))
  "Enable KKP support in Emacs running in the TERMINAL."
  (interactive)
  (when
      (eq t (terminal-live-p terminal))
    (push terminal kkp--setup-visited-terminal-list)
    (unless
        (member terminal kkp--active-terminal-list)
      (kkp--query-terminal-async "?u"
                                 '(("\e[?" . kkp--terminal-setup)) terminal))))

;;;###autoload
(defun kkp-disable-in-terminal ()
  "Disable in this terminal where command is executed, the activated enhancements."
  (interactive)
  (kkp--terminal-teardown (kkp--selected-terminal)))


(defun kkp-focus-change (&rest _)
  "Enable KKP when focus on terminal which has not yet enabled it once."
  (let* ((frame (selected-frame))
         (terminal (kkp--selected-terminal)))
    (when
        (and (not (member terminal kkp--setup-visited-terminal-list))
             (frame-focus-state frame))
      (kkp-enable-in-terminal terminal))
    (when
        (cl-subsetp
         kkp--setup-initial-terminal-list kkp--setup-visited-terminal-list)
      (remove-function after-focus-change-function #'kkp-focus-change))))


;;;###autoload
(define-minor-mode global-kkp-mode
  "Toggle KKP support in all terminal frames."
  :global t
  :lighter nil
  :group 'kkp
  (cond
   (global-kkp-mode
    ;; call setup for future terminals to be opened
    (add-hook 'tty-setup-hook #'kkp-enable-in-terminal)
    ;; call teardown for terminals to be closed
    (add-hook 'kill-emacs-hook #'kkp--disable-in-active-terminals)
    ;; we call this on each frame teardown, this has no effects if kkp is not enabled
    (add-to-list 'delete-terminal-functions #'kkp--terminal-teardown)

    ;; this is by far the most reliable method to enable kkp in all associated terminals
    ;; trying to switch to each terminal with `with-selected-frame' does not work very well
    ;; as input from `read-event' cannot be reliably read from the corresponding terminal
    (add-function :after after-focus-change-function #'kkp-focus-change)
    (setq kkp--setup-initial-terminal-list
          (cl-remove-if-not (lambda (elt) (eq t (terminal-live-p elt)))
                            (terminal-list)))
    (setq kkp--setup-visited-terminal-list nil)

    ;; At startup, this global mode might be called before the 'tty-setup-hook'.
    ;; To avoid running 'kkp-enable-in-terminal' before, we only run it if
    ;; the 'tty-run-terminal-initialization' already ran.
    (when (terminal-parameter (kkp--selected-terminal) 'terminal-initted)
      (kkp-enable-in-terminal)))
   (t
    (kkp--disable-in-active-terminals)
    (remove-hook 'tty-setup-hook #'kkp-enable-in-terminal)
    (remove-hook 'kill-emacs-hook #'kkp--disable-in-active-terminals)
    (remove-function after-focus-change-function #'kkp-focus-change)
    (setq delete-terminal-functions (delete #'kkp--terminal-teardown kkp--active-terminal-list)))))

;;;###autoload
(defun kkp-print-terminal-support ()
  "Message if terminal supports KKP."
  (interactive)
  (message "KKP%s supported in this terminal"
           (if (kkp--terminal-supports-kkp-p)
               "" " not")))


;;;###autoload
(defun kkp-print-enabled-progressive-enhancement-flags ()
  "Message, if terminal supports KKP, currently enabled enhancements."
  (interactive)
  (if (kkp--terminal-supports-kkp-p)
      (message "%s" (kkp--enabled-terminal-enhancements))
    (message "KKP not supported in this terminal.")))

(provide 'kkp)
;;; kkp.el ends here
