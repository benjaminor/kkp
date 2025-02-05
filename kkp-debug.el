;;; kkp-debug.el --- Debugging helpers for Kitty Keyboard Protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Benjamin Orthen
;; Author: Benjamin Orthen <contact@orthen.net>
;; Maintainer: Benjamin Orthen <contact@orthen.net>
;; Version: 0.3
;; URL: https://github.com/benjaminor/kkp
;; Package-Requires: ((emacs "27.1") (compat "29.1.3.4"))
;; Keywords: terminals, debugging

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file provides helper functions for debugging the key translation
;; of the kitty keyboard protocol. The main interactive command it provides is
;; `kkp-debug-describe-key-translation-chain'.
;;
;; The key translation process is broken into three stages:
;;
;;  1. Input decoding via `input-decode-map` (with a fallback to KKP helper functions if needed).
;;     This step converts raw terminal events into a canonical key sequence.
;;
;;  2. Normal binding lookup: the input-decoded sequence is checked against the
;;     active keymaps (local, minor, or global). If a normal binding exists,
;;     remapping via `local-function-key-map` is skipped.
;;
;;  3. If no normal binding is found, remapping via `local-function-key-map` is
;;     applied, followed by further translation via `key-translation-map`.
;;
;; The final key sequence and its associated command binding are then displayed,
;; with each stage output in aligned columns for clarity.
;;
;; To use these debugging helpers, you can either:
;;
;;   M-x load-library RET kkp-debug RET
;;
;; or add (require 'kkp-debug) to your Emacs init file.
;;
;;; Code:

(require 'cl-lib)
(require 'kkp)

(defun kkp-debug--menu-item-binding (binding)
  "Return the \"real\" binding behind a menu-item BINDING.
If BINDING is a menu-item form, extract its REAL-BINDING.
If that binding is itself a menu-item, keep unwrapping.
Otherwise, return BINDING unchanged."
  (cond
   ((and (consp binding)
         (eq (car binding) 'menu-item))
    ;; A menu-item looks like (menu-item NAME REAL-BINDING . PROPS)
    ;; so the third element is the real binding.
    (kkp-debug--menu-item-binding (nth 2 binding)))
   (t
    binding)))

(defun kkp-debug--lookup-key-sequence (keymap keyseq)
  "Lookup KEYSEQ in KEYMAP using `map-keymap' logic, including menu-item submaps.
Return the \"raw\" binding (which may be a command, a keymap, or a menu-item),
or nil if KEYSEQ is not bound in KEYMAP.
Replicates prefix descent like `lookup-key' but does not actually call it."
  (let ((current keymap)
        (keys (append keyseq nil)))      ; convert vector/string to list of events
    (catch 'done
      (while t
        (when (null keys)
          ;; If we ran out of events, then `current` might be the final submap if
          ;; we consider a bare submap as a final binding.  Return it here.
          (throw 'done current))
        (let ((evt (pop keys))
              found-binding)
          ;; Search CURRENT for EVT.
          (catch 'found
            (map-keymap
             (lambda (map-key map-val)
               (when (equal evt map-key)
                 (setq found-binding map-val)
                 (throw 'found nil)))
             current))
          (unless found-binding
            ;; Nothing matched this event, fail.
            (throw 'done nil))
          ;; If found-binding is a menu-item, unwrap it to get the real binding.
          (setq found-binding (kkp-debug--menu-item-binding found-binding))

          (cond
           ;; If it's a keymap and we still have leftover events, descend.
           ((and (keymapp found-binding) keys)
            (setq current found-binding))

           ;; If it's a keymap but no leftover keys, treat the submap as final.
           ((and (keymapp found-binding) (null keys))
            (throw 'done found-binding))

           ;; If it's not a keymap but we do have leftover events, can't descend.
           ((and (not (keymapp found-binding)) keys)
            (throw 'done nil))

           ;; If it's not a keymap and no leftover keys, this is our final binding.
           (t
            (throw 'done found-binding))))))))



(defun kkp-debug--get-key-events-from-terminal ()
  "Read a full key sequence from the terminal.
This function uses `read-event' with a short timeout to gather all parts
of the sequence and returns them as a list of events."
  (let ((events (list (read-event "Input your key: ")))
        (timeout 0.1)
        evt)
    ;; Gather additional events with a short timeout.
    (while (setq evt (read-event nil nil timeout))
      (push evt events))
    (nreverse events)))

(defun kkp-debug--translate-events-with-kkp-fallback (events)
  "Translate terminal input EVENTS using `input-decode-map` with a fallback.
This function works as follows:
  1. It builds a key sequence from EVENTS and looks it up in `input-decode-map`.
  2. If `lookup-key` returns a non-number, that value is returned.
  3. If `lookup-key` returns a number (indicating a prefix) and the first two
     events are 27 and 91 (ESC and '[') and the third event is an ASCII digit
     between ?1 and ?9, then it calls `kkp--translate-terminal-input' on the
     remaining events.
  4. Otherwise, it returns nil."
  (let* ((key-seq (vconcat events))
         (lookup-result (lookup-key input-decode-map key-seq)))
    (if (not (numberp lookup-result))
        (progn
          (message "Mapping found in input-decode-map: %s" lookup-result)
          lookup-result)
      (if (and (>= (length events) 3)
               (eq (nth 0 events) 27)
               (eq (nth 1 events) 91)
               (memq (nth 2 events) kkp--key-prefixes))
          (let ((rest (nthcdr 2 events)))
            (message "Prefix detected (ESC [ and a digit). Calling kkp--translate-terminal-input on: %s"
                     (key-description (vconcat rest)))
            (kkp--translate-terminal-input rest))
        (progn
          (message "lookup-key returned %s (prefix), but conditions not met." lookup-result)
          nil)))))

;;;###autoload
(defun kkp-debug-describe-key-translation-chain ()
  "Display the key translation chain for a terminal key sequence.
The translation process is performed in the following stages:
  1. Input decoding via `input-decode-map` (with fallback to KKP if needed).
  2. Normal binding lookup: if the input-decoded sequence has a normal binding,
     then remapping via `local-function-key-map` is skipped.
  3. If no normal binding is found, remap via `local-function-key-map`.
  4. Finally, `key-translation-map` is applied unconditionally.
The resulting key sequence and its final command binding are displayed.
Output is arranged in aligned columns for clarity."
  (interactive)
  (with-help-window "*Key Translation Chain*"
    (let* ((kkp-is-active (kkp--this-terminal-has-active-kkp-p))
           (events (kkp-debug--get-key-events-from-terminal))
           (raw-key (vconcat events))
           ;; Stage 1: Input decoding (with KKP fallback).
           (fallback-result
            (if kkp-is-active
                (kkp-debug--translate-events-with-kkp-fallback events)
              ;; somehow lookup-key does not always find the correct mapping in
              ;; input-decode-map after we have deactived global-kkp-mode
              (kkp-debug--lookup-key-sequence input-decode-map events)))
           (input-decode-output (if (and fallback-result (not (numberp fallback-result)))
                                    fallback-result
                                  raw-key))
           ;; Check if the decoded key sequence already has a normal binding.
           (normal-binding (key-binding input-decode-output))
           ;; Stage 2: Apply local-function-key-map only if no normal binding exists.
           (local-result (if normal-binding
                             nil
                           (let ((temp (lookup-key local-function-key-map input-decode-output)))
                             (and (not (numberp temp)) temp))))
           (local-output (or local-result input-decode-output))
           ;; Stage 3: Always apply key-translation-map.
           (translation-result (let ((temp (lookup-key key-translation-map local-output)))
                                 (and (not (numberp temp)) temp)))
           (final-output (or translation-result local-output))
           (final-binding (key-binding final-output)))
      (princ (format "%-40s %s\n" "KKP is active:" (if kkp-is-active "YES" "NO")))
      (princ (format "%-40s %s\n" "Raw key events:" (key-description raw-key)))
      (princ (format "%-40s %s\n" "After input-decode-map:" (key-description input-decode-output)))
      (princ (format "%-40s %s\n" "Normal binding (if any):" (or (and normal-binding (prin1-to-string normal-binding))
                                                                 "none")))
      (princ (format "%-40s %s => %s\n" "After local-function-key-map:"
                     (if normal-binding "not considered" (if local-result "found" "not found"))
                     (key-description local-output)))
      (princ (format "%-40s %s => %s\n" "After key-translation-map:"
                     (if translation-result "found" "not found")
                     (key-description final-output)))
      (princ (format "%-40s %s\n" "Final command binding:" (or final-binding "undefined"))))))

(provide 'kkp-debug)
;;; kkp-debug.el ends here
