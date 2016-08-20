;;; cfs-ui.el --- A chinese-fonts-setup profile editor with beautiful interface

;; * Header
;; Copyright (c) 2016, Feng Shu

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
;;  A chinese-fonts-setup profile editor which has a beautiful interface.

;;; Code:

;; * 代码                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'cl-lib)
(require 'cus-edit)

(defconst cfs-ui--buffers
  '((0 " *cfs-ui-buffer 1*")
    (1 " *cfs-ui-buffer 2*")
    (2 " *cfs-ui-buffer 3*")
    (3 " *cfs-ui-buffer 4-1*" (9 10 11.5 12.5 14 16 18))
    (4 " *cfs-ui-buffer 4-2*" (20 22 24))
    (5 " *cfs-ui-buffer 4-3*" (26 28))
    (6 " *cfs-ui-buffer 4-4*" (30))
    (7 " *cfs-ui-buffer 4-5*" (32))))

(defvar cfs-ui--fontname-widgets nil)
(defvar cfs-ui--fontsize-widgets nil)
(defvar cfs-personal-fontnames) ;Deal with compile warn.

(declare-function cfs--get-xlfd "chinese-fonts-setup" (fontname &optional uncheck))
(declare-function cfs--get-valid-fonts "chinese-fonts-setup" (&optional prefer-shortname))
(declare-function cfs--read-profile "chinese-fonts-setup" ())
(declare-function cfs--font-exists-p "chinese-fonts-setup" (font))
(declare-function cfs--save-profile "chinese-fonts-setup" (fontnames fontsizes &optional profile-name))
(declare-function cfs--set-font "chinese-fonts-setup" (fontsizes-list))
(declare-function cfs-set-font-with-saved-step "chinese-fonts-setup" (&optional frame))

(defun cfs-ui--switch-buffer (index)
  (switch-to-buffer
   (nth 1 (assoc index cfs-ui--buffers))))

(defun cfs-ui--create-main-navigation ()
    (widget-create 'push-button
                   :tag " 英文字体 "
                   :action '(lambda (widget &optional event)
                              (interactive)
                              (cfs-ui--switch-buffer 0)))
    (widget-insert "   ")
    (widget-create 'push-button
                   :tag " 中文字体 "
                   :action '(lambda (widget &optional event)
                              (interactive)
                              (cfs-ui--switch-buffer 1)))
    (widget-insert "   ")
    (widget-create 'push-button
                   :tag " EXT-B字体 "
                   :action '(lambda (widget &optional event)
                              (interactive)
                              (cfs-ui--switch-buffer 2)))
    (widget-insert "   ")
    (widget-create 'push-button
                   :tag " 等宽对齐 "
                   :action '(lambda (widget &optional event)
                              (interactive)
                              (cfs-ui--switch-buffer 3))))

(defun cfs-ui--create-fontsize-navigation ()
  (widget-insert "+--------------------------------------------------+\n")
  (widget-insert "| ")
  (widget-create 'push-button
                 :tag "[09--18]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action '(lambda (widget &optional event)
                            (interactive)
                            (cfs-ui--switch-buffer 3)))
  (widget-insert "  ")
  (widget-create 'push-button
                 :tag "[20--24]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action '(lambda (widget &optional event)
                            (interactive)
                            (cfs-ui--switch-buffer 4)))
  (widget-insert "  ")
  (widget-create 'push-button
                 :tag "[26--28]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action '(lambda (widget &optional event)
                            (interactive)
                            (cfs-ui--switch-buffer 5)))
  (widget-insert "  ")
  (widget-create 'push-button
                 :tag "[  30  ]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action '(lambda (widget &optional event)
                            (interactive)
                            (cfs-ui--switch-buffer 6)))
  (widget-insert "  ")
  (widget-create 'link
                 :tag "[  32  ]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action '(lambda (widget &optional event)
                            (interactive)
                            (cfs-ui--switch-buffer 7)))
  (widget-insert " |")
  (widget-insert "
| 如果此表格无法对齐，请按下面的加号减号按钮来调整 |
| abcdefjhijklmnoprqstuvwxwyABCDEFJHIJkLMNOPQRSTUV |
| 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄄𠄅𠄆𠄇𠄇 |
| 英文字号  中文字号调整    EXTB字号调整    测试   |
+--------------------------------------------------+"))

(defun cfs-ui--create-warning-board ()
  (when (and (nth 2 (cfs--get-valid-fonts))
             (eq system-type 'darwin))
    (widget-insert "
+----------------------------------------------------------------------------------+
| 注意：由于某些未知原因，未安装 EXT-B 字体的苹果系统，chinese-fonts-setup 会导致  |
| emacs 卡顿甚至崩溃，建议安装 HanaMinB 字体来解决这个问题，这个字体的下载地址：   |
| https://osdn.jp/projects/hanazono-font/downloads/62072/hanazono-20141012.zip/    |
+----------------------------------------------------------------------------------+

")))

(defun cfs-ui--create-fontsize-operate-buttons (fontsize key index)
  (let (widget1 widget2 widget3)
    (widget-insert " ")
    (setq widget1 (widget-create 'push-button
                                 :value (format "%-4s" fontsize)
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore))
    (widget-insert " ")
    (setq widget2 (widget-create 'push-button
                                 :tag "[-]"
                                 :key key
                                 :index index
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cfs-ui-decrease-fontsize))
    (setq widget3 (widget-create 'push-button
                                 :tag "[+]"
                                 :key key
                                 :index index
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cfs-ui-increase-fontsize))
    (widget-insert " ")
    (push (cons widget2 widget1) cfs-ui--fontsize-widgets)
    (push (cons widget3 widget1) cfs-ui--fontsize-widgets)))

(defun cfs-ui--create-fontsize-test-buttons (key index)
  (widget-create 'push-button
                 :tag " 测试 "
                 :key key
                 :index index
                 :action 'cfs-ui-test-fontsize))

(defun cfs-ui--return-status-string (font index)
  (format "%-2s %-2s"
          (if (cfs--get-xlfd font t) "" "NA")
          (if (member font (nth index cfs-personal-fontnames)) "P" "")))

(defun cfs-ui ()
  (interactive)
  (dolist (buffer-list cfs-ui--buffers)
    (switch-to-buffer (get-buffer-create (nth 1 buffer-list)))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (cfs-ui-mode)
    (set (make-local-variable 'cfs-ui--fontname-widgets) nil)
    (set (make-local-variable 'cfs-ui--fontsize-widgets) nil)
    (setq truncate-lines t)
    (widget-insert "\n")
    (cfs-ui--create-main-navigation)
    (let ((index (car buffer-list))
          (fontname-alist (car (cfs--read-profile)))
          (fontsize-alist (car (cdr (cfs--read-profile))))
          widget)
      (if (> index 2)
          (progn (widget-insert "\n")
                 (cfs-ui--create-fontsize-navigation)
                 (widget-insert "\n" )
                 (dolist (fontsize-list fontsize-alist)
                   (when (member (car fontsize-list) (nth 2 buffer-list))
                     (let ((i 0))
                       (dolist (fontsize fontsize-list)
                         (if (= i 0)
                             (widget-insert (format "  %-6s" fontsize))
                           (cfs-ui--create-fontsize-operate-buttons
                            (number-to-string fontsize)
                            (car fontsize-list) i))
                         (setq i (+ i 1))
                         (widget-insert "   "))
                       (widget-insert " ")
                       (cfs-ui--create-fontsize-test-buttons (car fontsize-list) i))
                     (widget-insert "\n"))))
        (widget-insert "\n\n")
        (let ((fonts (delete-dups
                      `(,@(nth index fontname-alist)
                        ,@(when (= index 1)
                            (cl-remove-if #'(lambda (font)
                                              (not (string-match-p "\\cc" font)))
                                          (font-family-list)))))))
          (cfs-ui--create-warning-board)
          (widget-insert "状态  字体名称\n")
          (widget-insert "----  -----------------------------------------------\n")
          (dolist (font fonts)
            (widget-insert (format "%-6s" (cfs-ui--return-status-string font index)))
            (setq widget
                  (widget-create 'checkbox
                                 :value (equal font (car (nth index fontname-alist)))
                                 :font-name font
                                 :index index
                                 :action 'cfs-ui-checkbox-toggle))
            (push (cons font widget) cfs-ui--fontname-widgets)
            (widget-create-child-and-convert widget 'push-button
                                             :button-face-get 'ignore
                                             :mouse-face-get 'ignore
                                             :value (format " %s" font)
                                             :action 'widget-parent-action)
            (widget-insert "\n" )))))
    (goto-char (point-min))
    (widget-setup))
  (cfs-ui--switch-buffer 0))

(defun cfs-ui-checkbox-toggle (widget &optional event)
  (let ((this-font (widget-get widget :font-name))
        (index (widget-get widget :index))
        (fontname-alist (car (cfs--read-profile)))
        (fontsize-alist (car (cdr (cfs--read-profile))))
        fonts-list)
    (widget-toggle-action widget event)
    (dolist (font cfs-ui--fontname-widgets)
      (unless (eq (car font) this-font)
        (widget-value-set (cdr font) nil)
        (widget-apply (cdr font) :notify (cdr font) event)))
    (if (not (cfs--font-exists-p this-font))
        (message "Chinese-fonts-setup UI: 系统没有安装字体: %S ." this-font)
      (when (widget-value widget)
        (setf (nth index fontname-alist)
              (delete-dups
               `(,this-font ,@(nth index fontname-alist))))
        (cfs--save-profile fontname-alist fontsize-alist)
        (cfs-set-font-with-saved-step)))))

(defun cfs-ui-operate-fontsize (widget &optional event n)
  (let ((key (widget-get widget :key))
        (index (widget-get widget :index))
        (widget-show-fontsize (cdr (assoc widget cfs-ui--fontsize-widgets)))
        (fontname-alist (car (cfs--read-profile)))
        (fontsize-alist (car (cdr (cfs--read-profile)))))
    (when (numberp n)
      (cl-incf (nth index (assoc key fontsize-alist)) n)
      ;; 更新加号按钮和减号按钮前面的数字标签
      (widget-value-set
       widget-show-fontsize
       (format "%-4s" (nth index (assoc key fontsize-alist)))))
    (let ((fontsizes-list (assoc key fontsize-alist)))
      (cfs--save-profile fontname-alist fontsize-alist)
      (cfs--set-font fontsizes-list))))

(defun cfs-ui-test-fontsize (widget &optional event)
  (cfs-ui-operate-fontsize widget event))

(defun cfs-ui-increase-fontsize (widget &optional event)
  (cfs-ui-operate-fontsize widget event 0.5))

(defun cfs-ui-decrease-fontsize (widget &optional event)
  (cfs-ui-operate-fontsize widget event -0.5))

(defvar cfs-ui-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `cfs-ui-mode'.")

(define-derived-mode cfs-ui-mode special-mode "CFS-UI"
  "Major mode for cfs-ui. Do not call this mode function yourself.
It is meant for internal use."
  (use-local-map cfs-ui-mode-map)
  (custom--initialize-widget-variables))
(put 'cfs-ui-mode 'mode-class 'special)

;; #+END_SRC
;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'cfs-ui)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; cfs-ui.el ends here
;; #+END_SRC
