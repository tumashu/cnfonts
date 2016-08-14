;;; cfs-fonts-manager.el --- A fonts manager of chinese-fonts-setup

;; * Header
;; Copyright (c) 2011-2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-fonts-setup
;; Version: 0.6
;; Keywords: convenience, Chinese, font

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

;; * 代码                                                                 :code:
;; #+BEGIN_SRC emacs-lisp

(defvar cfs-fonts-manager-settings
  '(nil nil nil nil))

(defconst cfs-fonts-manager--buffers
  '((0 . " *owp-fonts-manager 1*")
    (1 . " *owp-fonts-manager 2*")
    (2 . " *owp-fonts-manager 3*")
    (3 . " *owp-fonts-manager 4*")))

(defun cfs-fonts-manager--switch-buffer (index)
  (switch-to-buffer
   (cdr (assoc index cfs-fonts-manager--buffers))))

(defun cfs-fonts-manager (&optional buffer)
  (interactive)
  (dolist (buffer cfs-fonts-manager--buffers)
    (switch-to-buffer (get-buffer-create (cdr buffer)))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (cfs-fonts-manager-mode)
    (set (make-local-variable 'cfs-fonts-manager--listed-fonts) nil)
    (setq truncate-lines t)
    (widget-insert "Chinese-font-setup Manager\n\n")
    (widget-create 'push-button
                   :tag " 英文字体设置 "
                   :action '(lambda (widget &optional event)
                              (interactive)
                              (cfs-fonts-manager--switch-buffer 0)))
    (widget-insert "  ")
    (widget-create 'push-button
                   :tag " 中文字体设置 "
                   :action '(lambda (widget &optional event)
                              (interactive)
                              (cfs-fonts-manager--switch-buffer 1)))
    (widget-insert "  ")
    (widget-create 'push-button
                   :tag " EXT-B 字体设置 "
                   :action '(lambda (widget &optional event)
                              (interactive)
                              (cfs-fonts-manager--switch-buffer 2)))
    (widget-insert "  ")
    (widget-create 'push-button
                   :tag " 等宽对齐设置 "
                   :action '(lambda (widget &optional event)
                              (interactive)
                              (cfs-fonts-manager--switch-buffer 3)))
    (widget-insert "  ")
    (widget-create 'push-button
                   :tag " 保存字体设置 "
                   :help-echo "Save the selected font for future sessions."
                   :action 'cfs-fonts-manager-save-setting)
    (widget-insert "\n\n")
    (let ((index (car buffer)) widget)
      (if (= index 3)
          (progn (widget-insert cfs--test-string)
                 (widget-insert "\n")
                 (dolist (fontsize-list cfs--fontsizes-fallback)
                   (let ((i 0))
                     (dolist (fontsize fontsize-list)
                       (widget-insert (format " %-10s" fontsize))
                       (when (> i 0)
                         (widget-create 'push-button :tag "[+]")
                         (widget-insert " ")
                         (widget-create 'push-button :tag "[-]"))
                       (setq i (+ i 1))
                       (widget-insert " "))
                     (widget-insert "\n"))))
        (dolist (font (nth index cfs--fontnames-fallback))
          (setq widget
                (widget-create 'checkbox
                               :value (equal font (nth index cfs-fonts-manager-settings))
                               :font-name font
                               :index (car buffer)
                               :action 'cfs-fonts-manager-checkbox-toggle))
          (push (cons font widget) cfs-fonts-manager--listed-fonts)
          (widget-create-child-and-convert widget 'push-button
                                           :button-face-get 'ignore
                                           :mouse-face-get 'ignore
                                           :value (format " %-25s" font)
                                           :action 'widget-parent-action)
          (widget-insert "\n" ))))
    (goto-char (point-min))
    (widget-setup))
  (cfs-fonts-manager--switch-buffer 0))

(defun cfs-fonts-manager-save-setting (&rest _ignore)
  (interactive)
  (customize-save-variable
   'cfs-fonts-manager-settings cfs-fonts-manager-settings)
  (message "cfs-fonts-manager's setting is saved."))

(defun cfs-fonts-manager-checkbox-toggle (widget &optional event)
  (let ((this-font (widget-get widget :font-name))
        (index (widget-get widget :index)))
    (widget-toggle-action widget event)
	(dolist (font cfs-fonts-manager--listed-fonts)
	  (unless (eq (car font) this-font)
	    (widget-value-set (cdr font) nil)
	    (widget-apply (cdr font) :notify (cdr font) event)))
    (when (widget-value widget)
      (setf (nth index cfs-fonts-manager-settings) this-font))))

(defvar cfs-fonts-manager-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "\C-x\C-s" 'cfs-fonts-manager-save-setting)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `cfs-fonts-manager-mode'.")

(define-derived-mode cfs-fonts-manager-mode special-mode "CFS-FONTS-MANAGER"
  "Major mode for selecting org-chinese-font.
Do not call this mode function yourself.  It is meant for internal use."
  (use-local-map cfs-fonts-manager-mode-map)
  (custom--initialize-widget-variables)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (_ignore-auto noconfirm)
         (when (or noconfirm (y-or-n-p "Discard current choices? "))
           (cfs-fonts-manager (current-buffer))))))
(put 'cfs-fonts-manager-mode 'mode-class 'special)

;; #+END_SRC
;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'cfs-fonts-manager)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; cfs-fonts-manager.el ends here
;; #+END_SRC
