;;; exwm-modeline.el ---  A modeline segment for EXWM workspaces -*- lexical-binding: t -*-

;; Copyright (C) 2021 Korytov Pavel
;; Copyright (C) 2021 Ellis Kenyő
;; Copyright (C) 2008-2020 Natalie Weizenbaum <nex342@gmail.com>

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.4
;; Package-Requires: ((emacs "27.1") (exwm "0.26"))
;; Homepage: https://github.com/SqrtMinusOne/pomm.el

;; This file is NOT part of GNU Emacs.

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

;; TODO

;;; Code
(require 'exwm-workspace)

(defgroup exwm-modeline nil
  "A modeline segment to show EXWM workspaces"
  :group 'mode-line)

(defcustom exwm-modeline-dividers '("[" "]" "|")
  "Plist of strings used to create the string shown in the modeline.
First string is the start of the modestring, second is the
closing of the mode string, and the last is the divider between
workspaces."
  :group 'exwm-modeline
  :type '(list (string :tag "Open")
               (string :tag "Close")
               (string :tag "Divider")))

(defcustom exwm-modeline-short nil
  "When t, show a shortened modeline string.

TODO."
  :group 'exwm-modeline
  :type 'boolean)

(defface exwm-modeline-current-workspace
  '((t (:inherit warning :weight bold)))
  "Face for the current workspace."
  :group 'exwm-modeline)

(defface exwm-modeline-populated-workspace
  '((t (:inherit success)))
  "Face for any workspace populated with an X window."
  :group 'exwm-modeline)

(defface exwm-modeline-empty-workspace
  `((t (:foreground ,(face-foreground 'mode-line))))
  "Face for any workspace without an X window."
  :group 'exwm-modeline)

(defface exwm-modeline-urgent-workspace
  '((t (:inherit error :weight bold)))
  "Face for any workspace that is tagged as urgent by X."
  :group 'exwm-modeline)

(defun exwm-modeline--urgent-p (frame)
  (frame-parameter frame 'exwm-urgency))

(defun exwm-modeline--populated-p (frame)
  (cl-loop for item in exwm--id-buffer-alist
           if (eq frame (buffer-local-value 'exwm--frame (cdr item)))
           return t))

(defun exwm-modeline--click (event)
  (interactive "e")
  (when-let
      (target
       (cl-loop with name = (format "%s" (car (posn-string (event-start event))))
                for i from 0 to (1- (length exwm-workspace--list))
                if (string-equal (funcall exwm-workspace-index-map i) name)
                return i))
    (exwm-workspace-switch target)))

(defconst exwm-modeline-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'exwm-modeline--click)
    map))

(defun exwm-modeline--format-list (workspace-list index-list)
  "Format the modestring for the current frame."
  (cl-loop for index in index-list
           for frame in workspace-list
           for i from 0 to (length workspace-list)
           for workspace-name = (funcall exwm-workspace-index-map index)
           with current-frame = (selected-frame)
           if (= i 0) collect (nth 0 exwm-modeline-dividers)
           collect
           (propertize workspace-name
                       'face
                       (cond ((exwm-modeline--urgent-p frame)
                              'exwm-modeline-urgent-workspace)
                             ((eq frame current-frame)
                              'exwm-modeline-current-workspace)
                             ((exwm-modeline--populated-p frame)
                              'exwm-modeline-populated-workspace)
                             (t 'exwm-modeline-empty-workspace))
                       'local-map (unless (eq frame current-frame)
                                    exwm-modeline-line-map)
                       'mouse-face (unless (eq frame current-frame)
                                     'mode-line-highlight))
           if (= i (1- (length workspace-list)))
           collect (nth 1 exwm-modeline-dividers)
           else collect (nth 2 exwm-modeline-dividers)))

(defun exwm-modeline--format ()
  (exwm-modeline--format-list
   (if exwm-modeline-short (list (selected-frame))
     exwm-workspace--list)
   (if exwm-modeline-short (list exwm-workspace-current-index)
     (cl-loop for i from 0 to (1- (length exwm-workspace--list))
              collect i))))

(defun exwm-modeline-update ()
  (interactive)
  (cl-loop for frame in exwm-workspace--list
           do (with-selected-frame frame
                (set-frame-parameter nil 'exwm-modeline--string
                                     (exwm-modeline--format)))))

(defun exwm-modeline-segment ()
  (frame-parameter nil 'exwm-modeline--string))

(defun exwm-modeline--unmanage-advice (&rest _)
  (exwm-modeline-update))

;;;###autoload
(define-minor-mode exwm-modeline-mode ()
  :global t
  (if exwm-modeline-mode
      (progn
        (add-to-list 'global-mode-string '(:eval (exwm-modeline-segment)))
        (add-hook 'exwm-workspace-list-change-hook #'exwm-modeline-update)
        (add-hook 'exwm-randr-refresh-hook #'exwm-modeline-update)
        (add-hook 'exwm-manage-finish-hook #'exwm-modeline-update)
        (advice-add #'exwm-manage--unmanage-window
                    :after #'exwm-modeline--unmanage-advice))
    (setq global-mode-string (delete '(:eval (exwm-modeline-segment))
                                     global-mode-string))
    (remove-hook 'exwm-workspace-list-change-hook #'exwm-modeline-update)
    (remove-hook 'exwm-randr-refresh-hook #'exwm-modeline-update)
    (remove-hook 'exwm-manage-finish-hook #'exwm-modeline-update)
    (advice-remove #'exwm-manage--unmanage-window
                   #'exwm-modeline--unmanage-advice)))

(provide 'exwm-modeline)
;;; exwm-modeline.el ends here
