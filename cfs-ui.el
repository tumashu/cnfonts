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

(defconst cfs-ui--pages
  '((english-fonts-page
     :index 0
     :page-builder cfs-ui--create-fonts-page)
    (chinese-fonts-page
     :index 1
     :page-builder cfs-ui--create-fonts-page)
    (extb-fonts-page
     :index 2
     :page-builder cfs-ui--create-fonts-page)
    (fontsize-page-1
     :fontsizes (9 10 11.5 12.5 14 16 18)
     :page-builder cfs-ui--create-fontsize-page)
    (fontsize-page-2
     :fontsizes (20 22 24)
     :page-builder cfs-ui--create-fontsize-page)
    (fontsize-page-3
     :fontsizes (26 28)
     :page-builder cfs-ui--create-fontsize-page)
    (fontsize-page-4
     :fontsizes (30)
     :page-builder cfs-ui--create-fontsize-page)
    (fontsize-page-5
     :fontsizes (32)
     :page-builder cfs-ui--create-fontsize-page)
    (help-page
     :page-builder cfs-ui--create-help-page)))

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

(defun cfs-ui--switch-to-page (page-name)
  (switch-to-buffer (format " *%S*" page-name)))

(defun cfs-ui-switch-to-page:english-fonts-page (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'english-fonts-page))

(defun cfs-ui-switch-to-page:chinese-fonts-page (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'chinese-fonts-page))

(defun cfs-ui-switch-to-page:extb-fonts-page (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'extb-fonts-page))

(defun cfs-ui-switch-to-page:fontsize-page-1 (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'fontsize-page-1))

(defun cfs-ui-switch-to-page:fontsize-page-2 (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'fontsize-page-2))

(defun cfs-ui-switch-to-page:fontsize-page-3 (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'fontsize-page-3))

(defun cfs-ui-switch-to-page:fontsize-page-4 (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'fontsize-page-4))

(defun cfs-ui-switch-to-page:fontsize-page-5 (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'fontsize-page-5))

(defun cfs-ui-switch-to-page:help-page (&optional widget event)
  (interactive)
  (cfs-ui--switch-to-page 'help-page))

(defun cfs-ui--create-main-navigation ()
  (widget-create 'push-button
                 :tag " 英文 "
                 :action 'cfs-ui-switch-to-page:english-fonts-page)
  (widget-insert " ")
  (widget-create 'push-button
                 :tag " 中文 "
                 :action 'cfs-ui-switch-to-page:chinese-fonts-page)
  (widget-insert " ")
  (widget-create 'push-button
                 :tag " EXT-B "
                 :action 'cfs-ui-switch-to-page:extb-fonts-page)
  (widget-insert " ")
  (widget-create 'push-button
                 :tag " 字号 "
                 :action 'cfs-ui-switch-to-page:fontsize-page-1)
  (widget-insert " ")
  (widget-create 'push-button
                 :tag " ---- ")

  (widget-insert " ")
  (widget-create 'push-button
                 :tag " ---- ")

  (widget-insert " ")
  (widget-create 'push-button
                 :tag " 帮助 "
                 :action 'cfs-ui-switch-to-page:help-page))

(defun cfs-ui--create-fontsize-navigation ()
  (widget-insert "+--------------------------------------------------+\n")
  (widget-insert "| ")
  (widget-create 'push-button
                 :tag "[09--18]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action 'cfs-ui-switch-to-page:fontsize-page-1)
  (widget-insert "  ")
  (widget-create 'push-button
                 :tag "[20--24]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action 'cfs-ui-switch-to-page:fontsize-page-2)
  (widget-insert "  ")
  (widget-create 'push-button
                 :tag "[26--28]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action 'cfs-ui-switch-to-page:fontsize-page-3)
  (widget-insert "  ")
  (widget-create 'push-button
                 :tag "[  30  ]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action 'cfs-ui-switch-to-page:fontsize-page-4)
  (widget-insert "  ")
  (widget-create 'push-button
                 :tag "[  32  ]"
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action 'cfs-ui-switch-to-page:fontsize-page-5)
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
                                 :key key
                                 :index index
                                 :flag t
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cfs-ui-test-fontsize))
    (widget-insert " ")
    (setq widget2 (widget-create 'push-button
                                 :tag "[-]"
                                 :key key
                                 :index index
                                 :flag t
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cfs-ui-decrease-fontsize))
    (setq widget3 (widget-create 'push-button
                                 :tag "[+]"
                                 :key key
                                 :index index
                                 :flag t
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cfs-ui-increase-fontsize))
    (widget-insert " ")
    (push (cons widget1 widget1) cfs-ui--fontsize-widgets)
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
  (dolist (page-info cfs-ui--pages)
    (switch-to-buffer (get-buffer-create (format " *%S*" (car page-info))))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (cfs-ui-mode)
    (set (make-local-variable 'cfs-ui--fontname-widgets) nil)
    (set (make-local-variable 'cfs-ui--fontsize-widgets) nil)
    (setq truncate-lines t)
    (let ((page-builder (plist-get (cdr page-info) :page-builder)))
      (funcall page-builder page-info))
    (goto-char (point-min))
    (widget-setup))
  (cfs-ui-switch-to-page:english-fonts-page))

(defun cfs-ui--create-fontsize-page (page-info)
  (let ((page-name (car page-info))
        (index (plist-get (cdr page-info) :index))
        (fontsize-alist (car (cdr (cfs--read-profile))))
        (page-fontsizes (plist-get (cdr page-info) :fontsizes)))
    (widget-insert "\n")
    (cfs-ui--create-main-navigation)
    (widget-insert "\n")
    (cfs-ui--create-fontsize-navigation)
    (widget-insert "\n" )
    (dolist (fontsize-list fontsize-alist)
      (when (member (car fontsize-list) page-fontsizes)
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
        (widget-insert "\n")))))

(defun cfs-ui--create-fonts-page (page-info)
  (let ((page-name (car page-info))
        (index (plist-get (cdr page-info) :index))
        (fontname-alist (car (cfs--read-profile)))
        widget)
    (widget-insert "\n")
    (cfs-ui--create-main-navigation)
    (widget-insert "\n\n")
    (let ((fonts (delete-dups
                  `(,@(nth index fontname-alist)
                    ,@(when (equal page-name 'chinese-fonts-page)
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

(defun cfs-ui--create-help-page (page-info)
  (widget-insert "\n")
  (cfs-ui--create-main-navigation)
  (widget-insert "\n")
  (widget-insert
   (substitute-command-keys "
** 标签切换快捷键

 功能                  按键
 --------------------  --------
 切换到 '帮助' 标签    \\[cfs-ui-switch-to-page:help-page]
 切换到 '中文' 标签    \\[cfs-ui-switch-to-page:chinese-fonts-page]
 切换到 ’英文' 标签    \\[cfs-ui-switch-to-page:english-fonts-page]
 切换到 'EXTB' 标签    \\[cfs-ui-switch-to-page:extb-fonts-page]
 切换到 [09--18] 标签  \\[cfs-ui-switch-to-page:fontsize-page-1]
 切换到 [20--24] 标签  \\[cfs-ui-switch-to-page:fontsize-page-2]
 切换到 [26--28] 标签  \\[cfs-ui-switch-to-page:fontsize-page-3]
 切换到 [  30  ] 标签  \\[cfs-ui-switch-to-page:fontsize-page-4]
 切换到 [  32  ] 标签  \\[cfs-ui-switch-to-page:fontsize-page-5]

** 字体选择快捷键

 功能                  按键
 --------------------  --------
 选择/不选择当前字体   \\[widget-button-press]


** 字号调整快捷键

 功能                 按键
 -------------------  --------
 增大光标处的字号     \\[cfs-ui-increase-fontsize]
 减小光标处的字号     \\[cfs-ui-decrease-fontsize]
 测试字体显示效果     \\[cfs-ui-test-fontsize]
"))
  (widget-insert "\n")
  (widget-insert "\n" ))

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

(defun cfs-ui-operate-fontsize (&optional widget event n)
  (let* ((widget (or widget (widget-at)))
         (key (widget-get widget :key))
         (index (widget-get widget :index))
         (flag (widget-get widget :flag))
         (widget-show-fontsize (cdr (assoc widget cfs-ui--fontsize-widgets)))
         (fontname-alist (car (cfs--read-profile)))
         (fontsize-alist (car (cdr (cfs--read-profile)))))
    (if (not flag)
        (message "当前光标所在位置不对，请将光标移动到 ‘中文字号’ 或者 ‘EXT-B字体字号’ 对应的数字上。")
      (when (numberp n)
        (cl-incf (nth index (assoc key fontsize-alist)) n)
        ;; 更新加号按钮和减号按钮前面的数字标签
        (widget-value-set
         widget-show-fontsize
         (format "%-4s" (nth index (assoc key fontsize-alist)))))
      (let ((fontsizes-list (assoc key fontsize-alist)))
        (cfs--save-profile fontname-alist fontsize-alist)
        (cfs--set-font fontsizes-list)))))

(defun cfs-ui-test-fontsize (&optional widget event)
  (interactive)
  (cfs-ui-operate-fontsize widget event))

(defun cfs-ui-increase-fontsize (&optional widget event)
  (interactive)
  (cfs-ui-operate-fontsize widget event 0.5))

(defun cfs-ui-decrease-fontsize (&optional widget event)
  (interactive)
  (cfs-ui-operate-fontsize widget event -0.5))

(defvar cfs-ui-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map (kbd "C-c C-c") 'cfs-ui-test-fontsize)
    (define-key map (kbd "C-<up>") 'cfs-ui-increase-fontsize)
    (define-key map (kbd "C-<down>") 'cfs-ui-decrease-fontsize)
    (define-key map "h" 'cfs-ui-switch-to-page:help-page)
    (define-key map "e" 'cfs-ui-switch-to-page:english-fonts-page)
    (define-key map "c" 'cfs-ui-switch-to-page:chinese-fonts-page)
    (define-key map "x" 'cfs-ui-switch-to-page:extb-fonts-page)
    (define-key map "s" 'cfs-ui-switch-to-page:fontsize-page-1)
    (define-key map "1" 'cfs-ui-switch-to-page:fontsize-page-1)
    (define-key map "2" 'cfs-ui-switch-to-page:fontsize-page-2)
    (define-key map "3" 'cfs-ui-switch-to-page:fontsize-page-3)
    (define-key map "4" 'cfs-ui-switch-to-page:fontsize-page-4)
    (define-key map "5" 'cfs-ui-switch-to-page:fontsize-page-5)
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
