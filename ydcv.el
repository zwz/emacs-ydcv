;;; ydcv.el --- Interface for YouDao console version.

;; Author: Zhang Weize (zwz)
;; Keywords: dict Chinese
;; Copyright (C) 2014, Zhang Weize, all rights reserved.
;; Created: 2014-03-15

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Translate word by ydcv (console version of YouDao Dict), and display
;; translation use popup or buffer.
;;
;; Below are commands you can use:
;;
;; `ydcv-show-popup'
;; Search around word and display with popup.
;; `ydcv-show-buffer'
;; Search around word and display with buffer.
;; `ydcv-search-word'
;; Search input word and display with buffer.
;;
;; Tips:
;;
;; If current mark is active, ydcv commands will translate
;; region string, otherwise translate word around point.
;;

;;; Installation:
;;
;; To use this extension, you have to install ydcv.
;; And make sure have install `popup.el', which comes along with auto-complete

;; Put ydcv.el to your load-path.
;; And the following to your ~/.emacs startup file.
;;
;; (require 'ydcv)

(eval-when-compile
  (require 'cl))
(require 'popup)
(require 'ansi-color)

(defgroup ydcv nil
  "Interface for ydcv (有道词典 console version)."
  :group 'processes)

(defcustom ydcv-buffer-name "*YDCV*"
  "The name of the buffer of ydcv."
  :type 'string
  :group 'ydcv)

(defcustom ydcv-program-name "ydcv"
  "The name of the ydcv program."
  :type 'string
  :group 'ydcv)

(defcustom ydcv-simple-options "-s"
  "The options for simple results."
  :type 'string
  :group 'ydcv)

(defcustom ydcv-full-options "--color always"
  "The options for full results."
  :type 'string
  :group 'ydcv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ydcv-previous-window-configuration nil
  "Window configuration before switching to ydcv buffer.")

(defvar ydcv-mode-map                   ;key map
  (let ((map (make-sparse-keymap)))
    ;; Ydcv command.
    (define-key map "q" 'ydcv-quit)
    (define-key map "n" 'ydcv-next-item)
    (define-key map "p" 'ydcv-previous-item)
    (define-key map "w" 'ydcv-copy-item)
    (define-key map "i" 'ydcv-search-word)
    (define-key map "d" 'ydcv-show-popup)
    ;; Isearch.
    (define-key map "S" 'isearch-forward-regexp)
    (define-key map "R" 'isearch-backward-regexp)
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)

    ;; Misc.
    (define-key map "DEL" 'scroll-down)
    (define-key map " " 'scroll-up)
    (define-key map "l" 'forward-char)
    (define-key map "h" 'backward-char)
    (define-key map "?" 'describe-mode)
    map)
  "Keymap for `ydcv-mode'.")

(define-derived-mode ydcv-mode nil "ydcv"
  "Major mode to look up word through ydcv.
\\{ydcv-mode-map}
Turning on Text mode runs the normal hook `ydcv-mode-hook'."
  (setq buffer-read-only t))

(defun ydcv-quit ()
  "Bury ydcv buffer and restore the previous window configuration."
  (interactive)
  (if (window-configuration-p ydcv-previous-window-configuration)
      (progn
        (set-window-configuration ydcv-previous-window-configuration)
        (setq ydcv-previous-window-configuration nil)
        (bury-buffer (ydcv-get-buffer)))
    (bury-buffer)))

(defun ydcv-next-item ()
  "Jump to next item."
  (interactive)
  (if (search-forward-regexp "^\s*\\* " nil t) ;don't show error when search failed
      (recenter 0)
    (message "Have reach last item.")))

(defun ydcv-previous-item ()
  "Jump to previous item."
  (interactive)
  (beginning-of-line)
  (if (search-backward-regexp "^\s*\\* " nil t) ;don't show error when search failed
      (progn
        (goto-char (match-end 0))
        (recenter 0))                   ;adjust position
    (message "Have reach first item.")))

(defun ydcv-copy-item ()
  "Copy the current item."
  (interactive)
  (save-excursion
    (if (search-backward-regexp "^\s*\\* " nil t) ;don't show error when search failed
        (let ((begin (match-end 0)))
          (if (search-forward-regexp "\n\s*\\* \\|\n\n" nil t)
              (goto-char (match-beginning 0))
            (end-of-buffer))
          (copy-region-as-kill begin (point))
          (message "The ydcv item is copied"))
      (message "No ydcv item here"))))

(defun ydcv-encoding-string (string)
  "convert the string into utf-8"
  (string-as-multibyte (decode-coding-string
                        (encode-coding-string (string-as-unibyte string)
                                              (car (find-coding-systems-string string)));buffer-file-coding-system)
                                              'utf-8)))

(defun ydcv-search (word simple)
  "Search some WORD."
  (setq word (ydcv-encoding-string word))
  ;; Return translate result.
  (let ((old-default-process-coding-system default-process-coding-system)
        (result))
    (setq default-process-coding-system '(utf-8 . utf-8))
    (if simple
        (setq result (shell-command-to-string
                      (format "%s %s %s" ydcv-program-name ydcv-simple-options word)))
        (setq result
              (ansi-color-apply             ; filter the colors ansi-color-filter-apply
               (shell-command-to-string
                (format "%s %s %s" ydcv-program-name ydcv-full-options word)))))
    (setq default-process-coding-system old-default-process-coding-system)
    result))

(defun ydcv-goto-ydcv ()
  "Switch to ydcv buffer in other window."
  (setq ydcv-previous-window-configuration (current-window-configuration))
  (let* ((buffer (ydcv-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))

(defun ydcv-get-buffer ()
  "Get the ydcv buffer.  Create one if there's none."
  (let ((buffer (get-buffer-create ydcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'ydcv-mode)
        (ydcv-mode)))
    buffer))

(defun ydcv-mode-reinit ()
  "Re-initialize buffer.
Hide all entry but the first one and goto
the beginning of the buffer."
  (ignore-errors
    (setq buffer-read-only t)
    (goto-char (point-min))
    (ydcv-next-item)
    (message "YDCV has search finished with `%s'." ydcv-current-translate-object)))

(defun ydcv-prompt-input ()
  "Prompt input object for translation."
  (read-string (format "Word (%s): " (or (ydcv-region-or-word) ""))
               nil nil
               (ydcv-region-or-word)))

(defun ydcv-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

;;;###autoload
(defun ydcv-search-word ()
  "Prompt for input WORD.
And show translation in other buffer."
  (interactive)
  (let ((word (ydcv-prompt-input)))
    (with-current-buffer (get-buffer-create ydcv-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (ydcv-search word nil))
      (ydcv-goto-ydcv)
      (ydcv-mode-reinit))))

;;;###autoload
(defun ydcv-show-buffer (&optional word)
  "Translate current WORD.
And show information in other buffer."
  (interactive)
  (or word (setq word (ydcv-region-or-word)))
  (with-current-buffer (get-buffer-create ydcv-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (ydcv-search word nil))
    (ydcv-goto-ydcv)
    (ydcv-mode-reinit)))

;;;###autoload
(defun ydcv-show-popup (&optional word)
  "Translate current WORD.
And show information in popup."
  (interactive)
  (or word (setq word (ydcv-region-or-word)))
  (popup-tip (ydcv-search word t)))

(provide 'ydcv)

;;; ydcv.el ends here
