;;; cnfonts-ui.el --- A cnfonts profile editor with beautiful interface.

;; * Header
;; Copyright (c) 2016, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; Package-Requires: ((emacs "24"))
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
;;  A cnfonts profile editor which has a beautiful interface.

;;; Code:

;; * 代码                                                                 :code:
(require 'cl-lib)
(require 'cus-edit)

(defconst cnfonts-ui--pages
  '((english-fonts-page
     :index 0
     :main-page t
     :keybinding "e"
     :button-name "英文")
    (chinese-fonts-page
     :index 1
     :main-page t
     :keybinding "c"
     :button-name "中文")
    (extb-fonts-page
     :index 2
     :main-page t
     :keybinding "x"
     :button-name "EXT-B")
    (align-page-1
     :main-page t
     :align-page t
     :keybinding "1"
     :button-name "对齐"
     :alter-button-name "9.0-18"
     :group-pages (align-page-1 align-page-2 align-page-3
                                align-page-4 align-page-5)
     :fontsizes (9 10 11.5 12.5 14 15 16 18))
    (align-page-2
     :fontsizes (20 22 24)
     :align-page t
     :keybinding "2"
     :button-name "20-24")
    (align-page-3
     :fontsizes (26 28)
     :keybinding "3"
     :align-page t
     :button-name "26-28")
    (align-page-4
     :fontsizes (30)
     :keybinding "4"
     :align-page t
     :button-name "-30-")
    (align-page-5
     :fontsizes (32)
     :keybinding "5"
     :align-page t
     :button-name "-32-")
    (other-features-page
     :keybinding "o"
     :main-page t
     :button-name "其它")
    (key-page
     :keybinding "k"
     :main-page t
     :button-name "快捷键")
    (help-page
     :keybinding "h"
     :main-page t
     :button-name "帮助")))

(defvar cnfonts-ui-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "f" 'cnfonts-ui-next-main-page)
    (define-key map "b" 'cnfonts-ui-previous-main-page)
    (define-key map "F" 'cnfonts-ui-next-page)
    (define-key map "B" 'cnfonts-ui-previous-page)
    (define-key map "R" 'cnfonts-ui-restart)
    (define-key map " " 'cnfonts-ui-toggle-select-font)
    (define-key map "\t" 'cnfonts-ui-forward)
    (define-key map "\e\t" 'cnfonts-ui-backward)
    (define-key map [backtab] 'cnfonts-ui-backward)
    (define-key map "=" 'cnfonts-ui-increase-align)
    (define-key map "-" 'cnfonts-ui-decrease-align)
    (define-key map (kbd "C-c C-c") 'cnfonts-ui-test-align)
    (define-key map (kbd "C-c C-r") 'cnfonts-ui-quit-align)
    (define-key map (kbd "C-<up>") 'cnfonts-ui-increase-align)
    (define-key map (kbd "C-<down>") 'cnfonts-ui-decrease-align)
    map)
  "Keymap for `cnfonts-ui-mode'.")

(defvar cnfonts-ui--widgets-alist nil)
(defvar cnfonts-ui--current-page nil)
(defvar cnfonts-ui--widgets-main-navigation nil)
(defvar cnfonts-ui--widgets-align-navigation nil)
(defvar cnfonts-ui--widgets-elisp-snippet nil)

;; Deal with compile warn.
(defvar cnfonts-personal-fontnames)
(defvar cnfonts--enabled-p)
(defvar cnfonts-verbose)

(declare-function cnfonts--get-xlfd "cnfonts" (fontname &optional uncheck))
(declare-function cnfonts--get-valid-fonts "cnfonts" (&optional prefer-shortname))
(declare-function cnfonts--read-profile "cnfonts" ())
(declare-function cnfonts--font-exists-p "cnfonts" (font))
(declare-function cnfonts--save-profile "cnfonts" (fontnames fontsizes &optional profile-name))
(declare-function cnfonts--set-font "cnfonts" (fontsizes-list))
(declare-function cnfonts--step-fontsize "cnfonts" (num))
(declare-function cnfonts--get-current-profile "cnfonts" (&optional return-profile-name))
(declare-function cnfonts--get-current-fontsizes "cnfonts" ())
(declare-function cnfonts-set-font-with-saved-step "cnfonts" (&optional frame))
(declare-function cnfonts--return-fonts-configure-string "cnfonts" ())
(declare-function cnfonts-message "cnfonts" (force-show &rest args))
(declare-function cnfonts-decrease-fontsize "cnfonts" ())
(declare-function cnfonts-increase-fontsize "cnfonts" ())
(declare-function cnfonts--upgrade-profile-need-p "cnfonts" ())

(defun cnfonts-ui--switch-to-page (page-name)
  "Switch to page which name is PAGE-NAME."
  (switch-to-buffer (format " *%S*" page-name))
  (dolist (widget cnfonts-ui--widgets-main-navigation)
    (let ((orig-value (widget-value widget))
          (widget-page (widget-get widget :page-name))
          (widget-group-pages (widget-get widget :group-pages)))
      (if (if widget-group-pages
              (memq cnfonts-ui--current-page widget-group-pages)
            (eq cnfonts-ui--current-page widget-page))
          (widget-value-set
           widget (replace-regexp-in-string " " "*" orig-value))
        (widget-value-set
         widget (replace-regexp-in-string "*" " " orig-value)))))
  (dolist (widget cnfonts-ui--widgets-align-navigation)
    (let ((orig-value (widget-value widget))
          (widget-page (widget-get widget :page-name)))
      (if (eq cnfonts-ui--current-page widget-page)
          (widget-value-set
           widget (replace-regexp-in-string " " "*" orig-value))
        (widget-value-set
         widget (replace-regexp-in-string "*" " " orig-value))))))

(defun cnfonts-ui--create-page-switch-button (page-name &optional ignore-face)
  "Create a button which used to switch page named PAGE-NAME.
TODO: IGNORE-FACE."
  (let ((button-name (cnfonts-ui--get-page-info page-name :button-name))
        (alter-button-name (cnfonts-ui--get-page-info page-name :alter-button-name))
        (group-pages (cnfonts-ui--get-page-info page-name :group-pages))
        (action (cnfonts-ui--get-page-function page-name)))
    (if ignore-face
        (widget-create 'push-button
                       :value (format "[ %s ]" (or alter-button-name button-name))
                       :button-face-get 'ignore
                       :mouse-face-get 'ignore
                       :group-pages group-pages
                       :page-name page-name
                       :action action)
      (widget-create 'push-button
                     :value (format " %s " button-name)
                     :page-name page-name
                     :group-pages group-pages
                     :action action))))

(defun cnfonts-ui--filter-page (arg &optional all-page)
  (remove nil
          (mapcar #'(lambda (x)
                      (when (or all-page
                                (plist-get (cdr x) arg))
                        (car x)))
                  cnfonts-ui--pages)))

(defun cnfonts-ui--create-main-navigation ()
  (dolist (page-name (cnfonts-ui--filter-page :main-page))
    (push (cnfonts-ui--create-page-switch-button page-name)
          cnfonts-ui--widgets-main-navigation)
    (widget-insert " ")))

(defun cnfonts-ui--create-align-navigation ()
  (widget-insert "+----------------------------------------------------+\n")
  (widget-insert "| ")
  (dolist (page-name (cnfonts-ui--filter-page :align-page))
    (push (cnfonts-ui--create-page-switch-button page-name t)
          cnfonts-ui--widgets-align-navigation)
    (widget-insert " "))
  (widget-insert "  |")
  (widget-insert "
| 中英文等宽对齐设置：按加号或减号按钮直至此表格对齐 |
| abcdefjhijklmnoprqstuvwxwyABCDEFJHIJkLMNOPQRSTUVXW |
| 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄄𠄅𠄆𠄇𠄇𠄆 |
| 英文字号   中文对齐设置    EXT-B 对齐设置    测试  |
+----------------------------------------------------+"))

(defun cnfonts-ui--create-warning-board ()
  (cond
   (cnfonts-verbose
    (widget-insert "
+----------------------------------------------------+
| 如果需要 emacs 启动时激活 cnfonts，请在 emacs 配置 |
| 文件中添加一行代码：                               |
|                                                    |
|                (cnfonts-enable)                    |
|                                                    |
| 常用命令                      功能                 |
| ----------------------        -------------        |
| cnfonts-edit-profile          调整字体设置         |
| cnfonts-increase-fontsize     增大字号             |
| cnfonts-decrease-fontsize     减小字号             |
|                                                    |
| 注: 设置 cnfonts-verbose 为 nil , 可以隐藏这个消息 |
+----------------------------------------------------+
"))
   ((not (nth 2 (cnfonts--get-valid-fonts)))
    (widget-insert "
+----------------------------------------------------+
| 注：如果安装 cnfonts 后，发现 emacs 卡顿甚至崩溃， |
| 可以安装 HanaMinB 字体试试，这个字体的下载地址可   |
| 以从 [ 帮助 ] 页面中找到。                         |
+----------------------------------------------------+
"))))

(defun cnfonts-ui--create-align-operate-buttons (fontsize key index)
  (let (widget1 widget2 widget3 widget4 widget5)
    (if (= index 0)
        (progn (setq widget1 (widget-create 'push-button
                                            :value (format "  %-6s" fontsize)
                                            :flag t
                                            :key key
                                            :button-face-get 'ignore
                                            :mouse-face-get 'ignore
                                            :action 'cnfonts-ui-test-align))
               (push (cons widget1 widget1) cnfonts-ui--widgets-alist))
      (setq widget2 (widget-create 'push-button
                                   :value (format "%-5s" fontsize)
                                   :key key
                                   :index index
                                   :flag t
                                   :tab-stop-point t
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore
                                   :action 'cnfonts-ui-test-align))
      (setq widget3 (widget-create 'push-button
                                   :tag "[-]"
                                   :key key
                                   :index index
                                   :flag t
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore
                                   :action 'cnfonts-ui-decrease-align))
      (setq widget4 (widget-create 'push-button
                                   :tag "[+]"
                                   :key key
                                   :index index
                                   :flag t
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore
                                   :action 'cnfonts-ui-increase-align))
      (push (cons widget2 widget2) cnfonts-ui--widgets-alist)
      (push (cons widget3 widget2) cnfonts-ui--widgets-alist)
      (push (cons widget4 widget2) cnfonts-ui--widgets-alist))
    (setq widget5 (widget-create 'push-button
                                 :value "     "
                                 :flag t
                                 :key key
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cnfonts-ui-test-align))
    (push (cons widget5 widget5) cnfonts-ui--widgets-alist)))

(defun cnfonts-ui--create-align-test-buttons (key index)
  (let (widget1 widget2)
    (setq widget1 (widget-create 'push-button
                                 :value "  "
                                 :flag t
                                 :key key
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cnfonts-ui-test-align))
    (setq widget2 (widget-create 'push-button
                                 :tag " 测试 "
                                 :key key
                                 :flag t
                                 :action 'cnfonts-ui-test-align))
    (push (cons widget1 widget1) cnfonts-ui--widgets-alist)
    (push (cons widget2 widget2) cnfonts-ui--widgets-alist)))

(defun cnfonts-ui--return-status-string (font index)
  (format "%-2s %-2s"
          (if (cnfonts--get-xlfd font t) "" "NA")
          (if (member font (nth index cnfonts-personal-fontnames)) "P" "")))

(defun cnfonts-ui--get-page-function (page-name)
  (intern (concat "cnfonts-ui-page-" (symbol-name page-name))))

(defmacro cnfonts-ui-create-page (page-name &rest body)
  (declare (indent 1) (debug t))
  (let ((func-name (intern (concat "cnfonts-ui-page-" (symbol-name page-name))))
        (buffer-name (make-symbol "buffer-name")))
    `(defun ,func-name (&optional widget event create-buffer)
       (interactive)
       (let ((,buffer-name (format " *%S*" ',page-name)))
         (if create-buffer
             (with-current-buffer (get-buffer-create ,buffer-name)
               (let ((inhibit-read-only t))
                 (erase-buffer))
               (cnfonts-ui-mode)
               (define-key cnfonts-ui-mode-map (cnfonts-ui--get-page-info ',page-name :keybinding) ',func-name)
               (set (make-local-variable 'cnfonts-ui--widgets-alist) nil)
               (set (make-local-variable 'cnfonts-ui--current-page) ',page-name)
               (set (make-local-variable 'cnfonts-ui--widgets-main-navigation) nil)
               (set (make-local-variable 'cnfonts-ui--widgets-align-navigation) nil)
               (set (make-local-variable 'cnfonts-ui--widgets-elisp-snippet) nil)
               (setq truncate-lines t)
               ,@body
               (goto-char (point-min))
               (widget-setup))
           (cnfonts-ui--switch-to-page ',page-name))))))

(defun cnfonts-ui--get-page-info (page-name key)
  (let ((page-info (cdr (assq page-name cnfonts-ui--pages))))
    (plist-get page-info key)))

(defun cnfonts-ui--create-align-page (page-name)
  (let ((index (cnfonts-ui--get-page-info page-name :index))
        (fontsize-alist (car (cdr (cnfonts--read-profile))))
        (page-fontsizes (cnfonts-ui--get-page-info page-name :fontsizes)))
    (widget-insert "\n")
    (cnfonts-ui--create-main-navigation)
    (widget-insert "\n")
    (cnfonts-ui--create-align-navigation)
    (widget-insert "\n" )
    (dolist (fontsize-list fontsize-alist)
      (when (member (car fontsize-list) page-fontsizes)
        (let ((i 0))
          (dolist (fontsize fontsize-list)
            (cnfonts-ui--create-align-operate-buttons
             (number-to-string fontsize) (car fontsize-list) i)
            (setq i (+ i 1)))
          (cnfonts-ui--create-align-test-buttons (car fontsize-list) i))
        (widget-insert "\n")))
    (widget-insert "\n")
    (widget-insert (format "%-38s" (format "( %s )" (cnfonts--get-current-profile t))))
    (widget-create 'push-button
                   :tag "[ 对齐设置完成 ]"
                   :tab-stop-point t
                   :button-face-get 'ignore
                   :mouse-face-get 'ignore
                   :action 'cnfonts-ui-quit-align)))

(defun cnfonts-ui--create-fonts-page (page-name)
  (let ((index (cnfonts-ui--get-page-info page-name :index))
        (fontname-alist (car (cnfonts--read-profile))))
    (widget-insert "\n")
    (cnfonts-ui--create-main-navigation)
    (widget-insert "\n")
    (cnfonts-ui--create-warning-board)
    (widget-insert "\n")
    (let ((fonts (nth index fontname-alist))
          widget1 widget2 widget3)
      (widget-insert "状态  当前字体")
      (widget-create 'push-button
                     :button-face-get 'ignore
                     :mouse-face-get 'ignore
                     :tag "[-]"
                     :action '(lambda (widget event)
                                (cnfonts-decrease-fontsize)))
      (widget-create 'push-button
                     :button-face-get 'ignore
                     :mouse-face-get 'ignore
                     :tag "[+]"
                     :action '(lambda (widget event)
                                (cnfonts-increase-fontsize)))
      (widget-insert (format "%33s\n" (format "( %s )" (cnfonts--get-current-profile t))))
      (widget-insert "----  -----------------------------------------------\n")
      (dolist (font fonts)
        (setq widget1
              (widget-create 'push-button
                             :font-name font
                             :index index
                             :button-face-get 'ignore
                             :mouse-face-get 'ignore
                             :value (format "%-6s" (cnfonts-ui--return-status-string font index))))
        (setq widget2
              (widget-create 'checkbox
                             :value (equal font (car (nth index fontname-alist)))
                             :font-name font
                             :flag t
                             :tab-stop-point t
                             :index index
                             :action 'cnfonts-ui-toggle-select-font))
        (setq widget3
              (widget-create 'push-button
                             :button-face-get 'ignore
                             :mouse-face-get 'ignore
                             :value (format " %-50s" font)
                             :action 'cnfonts-ui-toggle-select-font))
        (push (cons widget1 widget2) cnfonts-ui--widgets-alist)
        (push (cons widget2 widget2) cnfonts-ui--widgets-alist)
        (push (cons widget3 widget2) cnfonts-ui--widgets-alist)
        (widget-insert "\n" ))
      (widget-insert "
注1: \"P\"  表示当前字体包含在 `cnfonts-personal-fontnames' 中
     \"NA\" 表示系统没有安装当前字体\n")
      (when (cnfonts--upgrade-profile-need-p)
        (widget-insert "
注2: profile 的格式已经更新，用户可以点击 ")
        (widget-create 'push-button
                       :tag "[ 这里 ]"
                       :tab-stop-point t
                       :button-face-get 'ignore
                       :mouse-face-get 'ignore
                       :action '(lambda (widget event)
                                  (cnfonts--save-profile cnfonts--fontnames-fallback
                                                     cnfonts--fontsizes-fallback
                                                     cnfonts--current-profile)
                                  (cnfonts-set-font-with-saved-step)
                                  (cnfonts-ui-restart)))
        (widget-insert " 强制
     *重置* 当前 profile。")))))

(cnfonts-ui-create-page english-fonts-page
  (cnfonts-ui--create-fonts-page 'english-fonts-page))

(cnfonts-ui-create-page chinese-fonts-page
  (cnfonts-ui--create-fonts-page 'chinese-fonts-page))

(cnfonts-ui-create-page extb-fonts-page
  (cnfonts-ui--create-fonts-page 'extb-fonts-page))

(cnfonts-ui-create-page align-page-1
  (cnfonts-ui--create-align-page 'align-page-1))

(cnfonts-ui-create-page align-page-2
  (cnfonts-ui--create-align-page 'align-page-2))

(cnfonts-ui-create-page align-page-3
  (cnfonts-ui--create-align-page 'align-page-3))

(cnfonts-ui-create-page align-page-4
  (cnfonts-ui--create-align-page 'align-page-4))

(cnfonts-ui-create-page align-page-5
  (cnfonts-ui--create-align-page 'align-page-5))

(cnfonts-ui-create-page help-page
  (cnfonts-ui--create-tab-stop-point)
  (cnfonts-ui--create-main-navigation)
  (widget-insert "\n\n")
  (let ((file (concat (file-name-directory (locate-library "cnfonts"))
                      "cnfonts.el"))
        begin end string)
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^;;; Commentary:$" nil t)
          (setq begin (line-beginning-position 2))
          (when (re-search-forward "^;;; Code:$")
            (setq end (line-beginning-position))))
        (when (and begin end)
          (setq string (replace-regexp-in-string
                        ":README:" ""
                        (replace-regexp-in-string
                         "^;; " ""
                         (buffer-substring-no-properties begin end)))))))
    (widget-insert (or string "")))
  (cnfonts-ui--create-tab-stop-point))

(cnfonts-ui-create-page other-features-page
  (cnfonts-ui--create-tab-stop-point)
  (cnfonts-ui--create-main-navigation)
  (widget-insert "

------------------------------------------------------
** 根据 cnfonts 的设置，自动生成一个 elisp 字体配置片断

如果用户觉得 cnfonts *太厚重*, 可以将下面
一段 elisp 粘贴到 ~/.emacs 文件，然后保存，就不需要启
动 cnfonts 来配置字体了。

-------
")
  (setq cnfonts-ui--widgets-elisp-snippet
        (widget-create 'push-button
                       :value (cnfonts--return-fonts-configure-string)
                       :tab-stop-point t
                       :button-face-get 'ignore
                       :mouse-face-get 'ignore))
  (widget-insert "\n")
  (widget-create 'push-button
                 :tag "[ 重新生成 ]"
                 :tab-stop-point t
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore
                 :action (lambda (widget event)
                           (widget-value-set
                            cnfonts-ui--widgets-elisp-snippet
                            (cnfonts--return-fonts-configure-string))))
  (widget-insert "
------------------------------------------------------\n")
  (cnfonts-ui--create-tab-stop-point))

;; key-page *must* create at the end, make sure other page's
;; keybinding are defined.
(cnfonts-ui-create-page key-page
  (cnfonts-ui--create-tab-stop-point)
  (cnfonts-ui--create-main-navigation)
  (widget-insert
   (substitute-command-keys "

** 标签切换快捷键

 功能                    按键
 ----------------------  --------
 切换到下一个主标签      \\[cnfonts-ui-next-main-page]
 切换到上一个主标签      \\[cnfonts-ui-previous-main-page]
 切换到下一个标签        \\[cnfonts-ui-next-page]
 切换到上一个标签        \\[cnfonts-ui-previous-page]
 切换到 [ 英文 ] 标签    \\[cnfonts-ui-page-english-fonts-page]
 切换到 [ 中文 ] 标签    \\[cnfonts-ui-page-chinese-fonts-page]
 切换到 [ EXT-B ] 标签   \\[cnfonts-ui-page-extb-fonts-page]
 切换到 [ 对齐 ] 标签    \\[cnfonts-ui-page-align-page-1]
 切换到 [09--18] 标签    \\[cnfonts-ui-page-align-page-1]
 切换到 [20--24] 标签    \\[cnfonts-ui-page-align-page-2]
 切换到 [26--28] 标签    \\[cnfonts-ui-page-align-page-3]
 切换到 [  30  ] 标签    \\[cnfonts-ui-page-align-page-4]
 切换到 [  32  ] 标签    \\[cnfonts-ui-page-align-page-5]
 切换到 [ 其他 ] 标签    \\[cnfonts-ui-page-other-features-page]
 切换到 [ 快捷键 ] 标签  \\[cnfonts-ui-page-key-page]
 切换到 [ 帮助 ] 标签    \\[cnfonts-ui-page-help-page]

** 字体选择快捷键

 功能                    按键
 ----------------------  --------
 选择/不选择当前字体     \\[cnfonts-ui-toggle-select-font]


** 中英文等宽对齐快捷键

 功能                    按键
 ----------------------  --------
 增大光标处的字号来对齐  \\[cnfonts-ui-increase-align]
 减小光标处的字号来对齐  \\[cnfonts-ui-decrease-align]
 测试字体对齐效果        \\[cnfonts-ui-test-align]
 对齐设置完成            \\[cnfonts-ui-quit-align]

** 其它快捷键

 功能                    按键
 ----------------------  --------
 重启UI                  \\[cnfonts-ui-restart]
"))
  (cnfonts-ui--create-tab-stop-point))

(defun cnfonts-ui-toggle-select-font (&optional widget event)
  (interactive)
  (let* ((widget (or widget (widget-at)))
         (widget1 (cdr (assoc widget cnfonts-ui--widgets-alist)))
         (widgets (mapcar #'cdr cnfonts-ui--widgets-alist))
         (font (widget-get widget1 :font-name))
         (index (widget-get widget1 :index))
         (flag (widget-get widget1 :flag))
         (fontname-alist (car (cnfonts--read-profile)))
         (fontsize-alist (car (cdr (cnfonts--read-profile))))
         fonts-list)
    (if (not flag)
        (cnfonts-message t "当前光标所在位置不对，请将光标移动到字体所在的行上面。")
      (widget-toggle-action widget1 event)
      (dolist (w widgets)
        (unless (equal (widget-get w :font-name) font)
          (widget-value-set w nil)
          (widget-apply w :notify w event)))
      (if (not (cnfonts--font-exists-p font))
          (cnfonts-message t "cnfonts UI: 系统没有安装字体: %S ." font)
        (when (widget-value widget1)
          (setf (nth index fontname-alist)
                (cl-remove-duplicates
                 `(,font ,@(nth index fontname-alist))
                 :from-end t :test 'equal))
          (cnfonts--save-profile fontname-alist fontsize-alist)
          (cnfonts-set-font-with-saved-step))))))

(defun cnfonts-ui--operate-align (&optional widget event n)
  (let* ((widget (or widget (widget-at)))
         (key (widget-get widget :key))
         (index (widget-get widget :index))
         (flag (widget-get widget :flag))
         (widget-show-fontsize (cdr (assoc widget cnfonts-ui--widgets-alist)))
         (fontname-alist (car (cnfonts--read-profile)))
         (fontsize-alist (car (cdr (cnfonts--read-profile)))))
    (if (not flag)
        (cnfonts-message t "当前光标所在位置不对，请将光标移动到 ‘中文字号’ 或者 ‘EXT-B字体字号’ 对应的数字上。")
      (when (and index key (numberp n))
        (cl-incf (nth index (assoc key fontsize-alist)) n)
        ;; 更新加号按钮和减号按钮前面的数字标签
        (widget-value-set
         widget-show-fontsize
         (format "%-5s" (nth index (assoc key fontsize-alist)))))
      (when key
        (let ((fontsizes-list (assoc key fontsize-alist)))
          (cnfonts--save-profile fontname-alist fontsize-alist)
          (cnfonts--set-font fontsizes-list))))))

(defun cnfonts-ui--create-tab-stop-point ()
  "Create a widget, the curse will stop to this widget when forward/backward widget."
  (widget-create 'push-button
                 :tag "\n"
                 :tab-stop-point t
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore))

(defun cnfonts-ui-quit-align (&optional widget event)
  "Quit align."
  (interactive)
  (cnfonts--step-fontsize 0))

(defun cnfonts-ui-test-align (&optional widget event)
  "Align test command."
  (interactive)
  (cnfonts-ui--operate-align widget event))

(defun cnfonts-ui-increase-align (&optional widget event)
  (interactive)
  (cnfonts-ui--operate-align widget event 0.5))

(defun cnfonts-ui-decrease-align (&optional widget event)
  (interactive)
  (cnfonts-ui--operate-align widget event -0.5))

(defun cnfonts-ui-forward (&optional backward)
  "Switch to next widget of current page.
If BACKWARD is non-nil, switch to previous widget."
  (interactive)
  (run-hooks 'widget-forward-hook)
  (let ((step (if backward -1 1))
        (forward t))
    (widget-move step)
    (while forward
      (if (widget-get (widget-at) :tab-stop-point)
          (setq forward nil)
        (widget-move step)))))

(defun cnfonts-ui-backward ()
  "Switch to previous widget of current page."
  (interactive)
  (cnfonts-ui-forward t))

(defun cnfonts-ui--operate-page (step &optional operate-all-page)
  "Internal function, which used to cnfonts-ui page switch."
  (let* ((pages (cnfonts-ui--filter-page :main-page operate-all-page))
         (pos-max (- (length pages) 1))
         (cur-page-pos
          (cl-position cnfonts-ui--current-page pages))
         (next-page-pos
          (if cur-page-pos
              (if (> step 0)
                  (if (> (+ step cur-page-pos) pos-max)
                      0
                    (+ step cur-page-pos))
                (if (< (+ step cur-page-pos) 0)
                    pos-max
                  (+ step cur-page-pos)))
            0))
         (next-page (nth next-page-pos pages)))
    (cnfonts-ui--switch-to-page next-page)))

(defun cnfonts-ui-next-main-page ()
  "Switch to next main page of cnfonts-ui."
  (interactive)
  (cnfonts-ui--operate-page 1))

(defun cnfonts-ui-previous-main-page ()
  "Switch to previous main page of cnfonts-ui."
  (interactive)
  (cnfonts-ui--operate-page -1))

(defun cnfonts-ui-next-page ()
  "Switch to next page of cnfonts-ui."
  (interactive)
  (cnfonts-ui--operate-page 1 t))

(defun cnfonts-ui-previous-page ()
  "Switch to previous page of cnfonts-ui."
  (interactive)
  (cnfonts-ui--operate-page -1 t))

(defun cnfonts-ui-restart ()
  "Restart cnfonts-ui."
  (interactive)
  (let ((current-page cnfonts-ui--current-page)
        (point (point)))
    (cnfonts-ui)
    (cnfonts-ui--switch-to-page current-page)
    (goto-char point)))

(define-derived-mode cnfonts-ui-mode special-mode "CNFONTS-UI"
  "Major mode for cnfonts-ui. Do not call this mode function yourself.
It is meant for internal use."
  (use-local-map cnfonts-ui-mode-map)
  (custom--initialize-widget-variables))
(put 'cnfonts-ui-mode 'mode-class 'special)

(defun cnfonts-ui ()
  (interactive)
  (if (not (display-graphic-p))
      (cnfonts-message t "cnfonts 不支持 emacs 终端模式！")
    ;; "cus-edit" 不能很好的在 emacs daemon 下工作，hack!
    (setq custom-raised-buttons
          (not (equal (face-valid-attribute-values :box)
                      '(("unspecified" . unspecified)))))
    (load-library "cus-edit")
    (dolist (page-info cnfonts-ui--pages)
      (let ((page-name (car page-info)))
        (funcall (cnfonts-ui--get-page-function page-name) nil nil t)))
    (funcall (cnfonts-ui--get-page-function 'english-fonts-page))))

;; * Footer
(provide 'cnfonts-ui)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; cnfonts-ui.el ends here
