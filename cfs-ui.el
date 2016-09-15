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

(defvar cfs-ui--widgets-alist nil)
(defvar cfs-ui--current-page nil)
(defvar cfs-ui--widgets:main-navigation nil)
(defvar cfs-ui--widgets:align-navigation nil)
(defvar cfs-ui--widgets:elisp-snippet nil)

;; Deal with compile warn.
(defvar cfs-personal-fontnames)
(defvar cfs--enabled-p)
(defvar cfs-verbose)

(declare-function cfs--get-xlfd "chinese-fonts-setup" (fontname &optional uncheck))
(declare-function cfs--get-valid-fonts "chinese-fonts-setup" (&optional prefer-shortname))
(declare-function cfs--read-profile "chinese-fonts-setup" ())
(declare-function cfs--font-exists-p "chinese-fonts-setup" (font))
(declare-function cfs--save-profile "chinese-fonts-setup" (fontnames fontsizes &optional profile-name))
(declare-function cfs--set-font "chinese-fonts-setup" (fontsizes-list))
(declare-function cfs--step-fontsize "chinese-fonts-setup" (num))
(declare-function cfs--get-current-profile "chinese-fonts-setup" (&optional return-profile-name))
(declare-function cfs--get-current-fontsizes "chinese-fonts-setup" ())
(declare-function cfs-set-font-with-saved-step "chinese-fonts-setup" (&optional frame))
(declare-function cfs--return-fonts-configure-string "chinese-fonts-setup" ())
(declare-function cfs-message "chinese-fonts-setup" (force-show &rest args))
(declare-function cfs-decrease-fontsize "chinese-fonts-setup" ())
(declare-function cfs-increase-fontsize "chinese-fonts-setup" ())
(declare-function cfs--upgrade-profile-need-p "chinese-fonts-setup" ())

(defun cfs-ui--switch-to-page (page-name)
  (switch-to-buffer (format " *%S*" page-name))
  (dolist (widget cfs-ui--widgets:main-navigation)
    (let ((orig-value (widget-value widget))
          (widget-page (widget-get widget :page-name))
          (widget-group-pages (widget-get widget :group-pages)))
      (if (if widget-group-pages
              (memq cfs-ui--current-page widget-group-pages)
            (eq cfs-ui--current-page widget-page))
          (widget-value-set
           widget (replace-regexp-in-string " " "*" orig-value))
        (widget-value-set
         widget (replace-regexp-in-string "*" " " orig-value)))))
  (dolist (widget cfs-ui--widgets:align-navigation)
    (let ((orig-value (widget-value widget))
          (widget-page (widget-get widget :page-name)))
      (if (eq cfs-ui--current-page widget-page)
          (widget-value-set
           widget (replace-regexp-in-string " " "*" orig-value))
        (widget-value-set
         widget (replace-regexp-in-string "*" " " orig-value))))))

(defun cfs-ui--create-page-switch-button (page-name &optional ignore-face)
  (let ((button-name (cfs-ui--get-page-info page-name :button-name))
        (alter-button-name (cfs-ui--get-page-info page-name :alter-button-name))
        (group-pages (cfs-ui--get-page-info page-name :group-pages))
        (action (cfs-ui--get-page-function page-name)))
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

(defun cfs-ui--filter-page (arg &optional all-page)
  (remove nil
          (mapcar #'(lambda (x)
                      (when (or all-page
                                (plist-get (cdr x) arg))
                        (car x)))
                  cfs-ui--pages)))

(defun cfs-ui--create-main-navigation ()
  (dolist (page-name (cfs-ui--filter-page :main-page))
    (push (cfs-ui--create-page-switch-button page-name)
          cfs-ui--widgets:main-navigation)
    (widget-insert " ")))

(defun cfs-ui--create-align-navigation ()
  (widget-insert "+----------------------------------------------------+\n")
  (widget-insert "| ")
  (dolist (page-name (cfs-ui--filter-page :align-page))
    (push (cfs-ui--create-page-switch-button page-name t)
          cfs-ui--widgets:align-navigation)
    (widget-insert " "))
  (widget-insert "  |")
  (widget-insert "
| 中英文等宽对齐设置：按加号或减号按钮直至此表格对齐 |
| abcdefjhijklmnoprqstuvwxwyABCDEFJHIJkLMNOPQRSTUVXW |
| 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄄𠄅𠄆𠄇𠄇𠄆 |
| 英文字号   中文对齐设置    EXT-B 对齐设置    测试  |
+----------------------------------------------------+"))

(defun cfs-ui--create-warning-board ()
  (cond
   (cfs-verbose
    (widget-insert "
+----------------------------------------------------+
| 如果需要 emacs 启动时激活 chinese-fonts-setup，请  |
| 在 emacs 配置文件中添加一行代码：                  |
|                                                    |
|          (chinese-fonts-setup-enable)              |
|                                                    |
| 常用命令                  功能                     |
| ----------------------    -------------            |
| cfs-edit-profile          调整字体设置             |
| cfs-increase-fontsize     增大字号                 |
| cfs-decrease-fontsize     减小字号                 |
|                                                    |
| 注: 设置 cfs-verbose 为 nil , 可以隐藏这个消息。   |
+----------------------------------------------------+
"))
   ((not (nth 2 (cfs--get-valid-fonts)))
    (widget-insert "
+----------------------------------------------------+
| 注：如果安装 chinese-fonts-setup 后，发现 emacs 卡 |
| 顿甚至崩溃，可以安装 HanaMinB 字体试试，这个字体的 |
| 下载地址可以从 [ 帮助 ] 页面中找到。               |
+----------------------------------------------------+
"))))

(defun cfs-ui--create-align-operate-buttons (fontsize key index)
  (let (widget1 widget2 widget3 widget4 widget5)
    (if (= index 0)
        (progn (setq widget1 (widget-create 'push-button
                                            :value (format "  %-6s" fontsize)
                                            :flag t
                                            :key key
                                            :button-face-get 'ignore
                                            :mouse-face-get 'ignore
                                            :action 'cfs-ui-test-align))
               (push (cons widget1 widget1) cfs-ui--widgets-alist))
      (setq widget2 (widget-create 'push-button
                                   :value (format "%-5s" fontsize)
                                   :key key
                                   :index index
                                   :flag t
                                   :tab-stop-point t
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore
                                   :action 'cfs-ui-test-align))
      (setq widget3 (widget-create 'push-button
                                   :tag "[-]"
                                   :key key
                                   :index index
                                   :flag t
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore
                                   :action 'cfs-ui-decrease-align))
      (setq widget4 (widget-create 'push-button
                                   :tag "[+]"
                                   :key key
                                   :index index
                                   :flag t
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore
                                   :action 'cfs-ui-increase-align))
      (push (cons widget2 widget2) cfs-ui--widgets-alist)
      (push (cons widget3 widget2) cfs-ui--widgets-alist)
      (push (cons widget4 widget2) cfs-ui--widgets-alist))
    (setq widget5 (widget-create 'push-button
                                 :value "     "
                                 :flag t
                                 :key key
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cfs-ui-test-align))
    (push (cons widget5 widget5) cfs-ui--widgets-alist)))

(defun cfs-ui--create-align-test-buttons (key index)
  (let (widget1 widget2)
    (setq widget1 (widget-create 'push-button
                                 :value "  "
                                 :flag t
                                 :key key
                                 :button-face-get 'ignore
                                 :mouse-face-get 'ignore
                                 :action 'cfs-ui-test-align))
    (setq widget2 (widget-create 'push-button
                                 :tag " 测试 "
                                 :key key
                                 :flag t
                                 :action 'cfs-ui-test-align))
    (push (cons widget1 widget1) cfs-ui--widgets-alist)
    (push (cons widget2 widget2) cfs-ui--widgets-alist)))

(defun cfs-ui--return-status-string (font index)
  (format "%-2s %-2s"
          (if (cfs--get-xlfd font t) "" "NA")
          (if (member font (nth index cfs-personal-fontnames)) "P" "")))

(defun cfs-ui--get-page-function (page-name)
  (intern (concat "cfs-ui:" (symbol-name page-name))))

(defmacro cfs-ui-create-page (page-name &rest body)
  (declare (indent 1) (debug t))
  (let ((func-name (intern (concat "cfs-ui:" (symbol-name page-name))))
        (buffer-name (make-symbol "buffer-name")))
    `(defun ,func-name (&optional widget event create-buffer)
       (interactive)
       (let ((,buffer-name (format " *%S*" ',page-name)))
         (if create-buffer
             (with-current-buffer (get-buffer-create ,buffer-name)
               (let ((inhibit-read-only t))
                 (erase-buffer))
               (cfs-ui-mode)
               (define-key cfs-ui-mode-map (cfs-ui--get-page-info ',page-name :keybinding) ',func-name)
               (set (make-local-variable 'cfs-ui--widgets-alist) nil)
               (set (make-local-variable 'cfs-ui--current-page) ',page-name)
               (set (make-local-variable 'cfs-ui--widgets:main-navigation) nil)
               (set (make-local-variable 'cfs-ui--widgets:align-navigation) nil)
               (set (make-local-variable 'cfs-ui--widgets:elisp-snippet) nil)
               (setq truncate-lines t)
               ,@body
               (goto-char (point-min))
               (widget-setup))
           (cfs-ui--switch-to-page ',page-name))))))

(defun cfs-ui--get-page-info (page-name key)
  (let ((page-info (cdr (assq page-name cfs-ui--pages))))
    (plist-get page-info key)))

(defun cfs-ui--create-align-page (page-name)
  (let ((index (cfs-ui--get-page-info page-name :index))
        (fontsize-alist (car (cdr (cfs--read-profile))))
        (page-fontsizes (cfs-ui--get-page-info page-name :fontsizes)))
    (widget-insert "\n")
    (cfs-ui--create-main-navigation)
    (widget-insert "\n")
    (cfs-ui--create-align-navigation)
    (widget-insert "\n" )
    (dolist (fontsize-list fontsize-alist)
      (when (member (car fontsize-list) page-fontsizes)
        (let ((i 0))
          (dolist (fontsize fontsize-list)
            (cfs-ui--create-align-operate-buttons
             (number-to-string fontsize) (car fontsize-list) i)
            (setq i (+ i 1)))
          (cfs-ui--create-align-test-buttons (car fontsize-list) i))
        (widget-insert "\n")))
    (widget-insert "\n")
    (widget-insert (format "%-38s" (format "( %s )" (cfs--get-current-profile t))))
    (widget-create 'push-button
                   :tag "[ 对齐设置完成 ]"
                   :tab-stop-point t
                   :button-face-get 'ignore
                   :mouse-face-get 'ignore
                   :action 'cfs-ui-quit-align)))

(defun cfs-ui--create-fonts-page (page-name)
  (let ((index (cfs-ui--get-page-info page-name :index))
        (fontname-alist (car (cfs--read-profile))))
    (widget-insert "\n")
    (cfs-ui--create-main-navigation)
    (widget-insert "\n")
    (cfs-ui--create-warning-board)
    (widget-insert "\n")
    (let ((fonts (nth index fontname-alist))
          widget1 widget2 widget3)
      (widget-insert "状态  当前字体")
      (widget-create 'push-button
                     :button-face-get 'ignore
                     :mouse-face-get 'ignore
                     :tag "[-]"
                     :action '(lambda (widget event)
                                (cfs-decrease-fontsize)))
      (widget-create 'push-button
                     :button-face-get 'ignore
                     :mouse-face-get 'ignore
                     :tag "[+]"
                     :action '(lambda (widget event)
                                (cfs-increase-fontsize)))
      (widget-insert (format "%33s\n" (format "( %s )" (cfs--get-current-profile t))))
      (widget-insert "----  -----------------------------------------------\n")
      (dolist (font fonts)
        (setq widget1
              (widget-create 'push-button
                             :font-name font
                             :index index
                             :button-face-get 'ignore
                             :mouse-face-get 'ignore
                             :value (format "%-6s" (cfs-ui--return-status-string font index))))
        (setq widget2
              (widget-create 'checkbox
                             :value (equal font (car (nth index fontname-alist)))
                             :font-name font
                             :flag t
                             :tab-stop-point t
                             :index index
                             :action 'cfs-ui-toggle-select-font))
        (setq widget3
              (widget-create 'push-button
                             :button-face-get 'ignore
                             :mouse-face-get 'ignore
                             :value (format " %-50s" font)
                             :action 'cfs-ui-toggle-select-font))
        (push (cons widget1 widget2) cfs-ui--widgets-alist)
        (push (cons widget2 widget2) cfs-ui--widgets-alist)
        (push (cons widget3 widget2) cfs-ui--widgets-alist)
        (widget-insert "\n" ))
      (widget-insert "
注1: \"P\"  表示当前字体包含在 `cfs-personal-fontnames' 中
     \"NA\" 表示系统没有安装当前字体\n")
      (when (cfs--upgrade-profile-need-p)
        (widget-insert "
注2: profile 的格式已经更新，用户可以点击 ")
        (widget-create 'push-button
                       :tag "[ 这里 ]"
                       :tab-stop-point t
                       :button-face-get 'ignore
                       :mouse-face-get 'ignore
                       :action '(lambda (widget event)
                                  (cfs--save-profile cfs--fontnames-fallback
                                                     cfs--fontsizes-fallback
                                                     cfs--current-profile)
                                  (cfs-set-font-with-saved-step)
                                  (cfs-ui-restart)))
        (widget-insert " 强制
     *重置* 当前 profile。")))))

(cfs-ui-create-page english-fonts-page
  (cfs-ui--create-fonts-page 'english-fonts-page))

(cfs-ui-create-page chinese-fonts-page
  (cfs-ui--create-fonts-page 'chinese-fonts-page))

(cfs-ui-create-page extb-fonts-page
  (cfs-ui--create-fonts-page 'extb-fonts-page))

(cfs-ui-create-page align-page-1
  (cfs-ui--create-align-page 'align-page-1))

(cfs-ui-create-page align-page-2
  (cfs-ui--create-align-page 'align-page-2))

(cfs-ui-create-page align-page-3
  (cfs-ui--create-align-page 'align-page-3))

(cfs-ui-create-page align-page-4
  (cfs-ui--create-align-page 'align-page-4))

(cfs-ui-create-page align-page-5
  (cfs-ui--create-align-page 'align-page-5))

(cfs-ui-create-page key-page
  (cfs-ui--create-tab-stop-point)
  (cfs-ui--create-main-navigation)
  (widget-insert
   (substitute-command-keys "

** 标签切换快捷键

 功能                    按键
 ----------------------  --------
 切换到下一个主标签      \\[cfs-ui-next-main-page]
 切换到上一个主标签      \\[cfs-ui-previous-main-page]
 切换到下一个标签        \\[cfs-ui-next-page]
 切换到上一个标签        \\[cfs-ui-previous-page]
 切换到 [ 英文 ] 标签    \\[cfs-ui:english-fonts-page]
 切换到 [ 中文 ] 标签    \\[cfs-ui:chinese-fonts-page]
 切换到 [ EXT-B ] 标签   \\[cfs-ui:extb-fonts-page]
 切换到 [ 对齐 ] 标签    \\[cfs-ui:align-page-1]
 切换到 [09--18] 标签    \\[cfs-ui:align-page-1]
 切换到 [20--24] 标签    \\[cfs-ui:align-page-2]
 切换到 [26--28] 标签    \\[cfs-ui:align-page-3]
 切换到 [  30  ] 标签    \\[cfs-ui:align-page-4]
 切换到 [  32  ] 标签    \\[cfs-ui:align-page-5]
 切换到 [ 其他 ] 标签    \\[cfs-ui:other-features-page]
 切换到 [ 快捷键 ] 标签  \\[cfs-ui:key-page]
 切换到 [ 帮助 ] 标签    \\[cfs-ui:help-page]

** 字体选择快捷键

 功能                    按键
 ----------------------  --------
 选择/不选择当前字体     \\[cfs-ui-toggle-select-font]


** 中英文等宽对齐快捷键

 功能                    按键
 ----------------------  --------
 增大光标处的字号来对齐  \\[cfs-ui-increase-align]
 减小光标处的字号来对齐  \\[cfs-ui-decrease-align]
 测试字体对齐效果        \\[cfs-ui-test-align]
 对齐设置完成            \\[cfs-ui-quit-align]

** 其它快捷键

 功能                    按键
 ----------------------  --------
 重启UI                  \\[cfs-ui-restart]
"))
  (cfs-ui--create-tab-stop-point))

(cfs-ui-create-page help-page
  (cfs-ui--create-tab-stop-point)
  (cfs-ui--create-main-navigation)
  (widget-insert "\n\n")
  (let ((file (concat (file-name-directory (locate-library "chinese-fonts-setup"))
                      "chinese-fonts-setup.el"))
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
  (cfs-ui--create-tab-stop-point))

(cfs-ui-create-page other-features-page
  (cfs-ui--create-tab-stop-point)
  (cfs-ui--create-main-navigation)
  (widget-insert "

------------------------------------------------------
** 根据 cfs 的设置，自动生成一个 elisp 字体配置片断

如果用户觉得 chinese-fonts-setup *太厚重*, 可以将下面
一段 elisp 粘贴到 ~/.emacs 文件，然后保存，就不需要启
动 chinese-fonts-setup 来配置字体了。

-------
")
  (setq cfs-ui--widgets:elisp-snippet
        (widget-create 'push-button
                       :value (cfs--return-fonts-configure-string)
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
                            cfs-ui--widgets:elisp-snippet
                            (cfs--return-fonts-configure-string))))
  (widget-insert "
------------------------------------------------------\n")
  (cfs-ui--create-tab-stop-point))

(defun cfs-ui-toggle-select-font (&optional widget event)
  (interactive)
  (let* ((widget (or widget (widget-at)))
         (widget1 (cdr (assoc widget cfs-ui--widgets-alist)))
         (widgets (mapcar #'cdr cfs-ui--widgets-alist))
         (font (widget-get widget1 :font-name))
         (index (widget-get widget1 :index))
         (flag (widget-get widget1 :flag))
         (fontname-alist (car (cfs--read-profile)))
         (fontsize-alist (car (cdr (cfs--read-profile))))
         fonts-list)
    (if (not flag)
        (cfs-message t "当前光标所在位置不对，请将光标移动到字体所在的行上面。")
      (widget-toggle-action widget1 event)
      (dolist (w widgets)
        (unless (equal (widget-get w :font-name) font)
          (widget-value-set w nil)
          (widget-apply w :notify w event)))
      (if (not (cfs--font-exists-p font))
          (cfs-message t "Chinese-fonts-setup UI: 系统没有安装字体: %S ." font)
        (when (widget-value widget1)
          (setf (nth index fontname-alist)
                (cl-remove-duplicates
                 `(,font ,@(nth index fontname-alist))
                 :from-end t :test 'equal))
          (cfs--save-profile fontname-alist fontsize-alist)
          (cfs-set-font-with-saved-step))))))

(defun cfs-ui--operate-align (&optional widget event n)
  (let* ((widget (or widget (widget-at)))
         (key (widget-get widget :key))
         (index (widget-get widget :index))
         (flag (widget-get widget :flag))
         (widget-show-fontsize (cdr (assoc widget cfs-ui--widgets-alist)))
         (fontname-alist (car (cfs--read-profile)))
         (fontsize-alist (car (cdr (cfs--read-profile)))))
    (if (not flag)
        (cfs-message t "当前光标所在位置不对，请将光标移动到 ‘中文字号’ 或者 ‘EXT-B字体字号’ 对应的数字上。")
      (when (and index key (numberp n))
        (cl-incf (nth index (assoc key fontsize-alist)) n)
        ;; 更新加号按钮和减号按钮前面的数字标签
        (widget-value-set
         widget-show-fontsize
         (format "%-5s" (nth index (assoc key fontsize-alist)))))
      (when key
        (let ((fontsizes-list (assoc key fontsize-alist)))
          (cfs--save-profile fontname-alist fontsize-alist)
          (cfs--set-font fontsizes-list))))))

(defun cfs-ui--create-tab-stop-point ()
  (widget-create 'push-button
                 :tag "\n"
                 :tab-stop-point t
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore))

(defun cfs-ui-quit-align (&optional widget event)
  (interactive)
  (cfs--step-fontsize 0))

(defun cfs-ui-test-align (&optional widget event)
  (interactive)
  (cfs-ui--operate-align widget event))

(defun cfs-ui-increase-align (&optional widget event)
  (interactive)
  (cfs-ui--operate-align widget event 0.5))

(defun cfs-ui-decrease-align (&optional widget event)
  (interactive)
  (cfs-ui--operate-align widget event -0.5))

(defun cfs-ui-forward (&optional backward)
  (interactive)
  (run-hooks 'widget-forward-hook)
  (let ((step (if backward -1 1))
        (forward t))
    (widget-move step)
    (while forward
      (if (widget-get (widget-at) :tab-stop-point)
          (setq forward nil)
        (widget-move step)))))

(defun cfs-ui-backward ()
  (interactive)
  (cfs-ui-forward t))

(defun cfs-ui--operate-page (step &optional operate-all-page)
  (let* ((pages (cfs-ui--filter-page :main-page operate-all-page))
         (pos-max (- (length pages) 1))
         (cur-page-pos
          (cl-position cfs-ui--current-page pages))
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
    (cfs-ui--switch-to-page next-page)))

(defun cfs-ui-next-main-page ()
  (interactive)
  (cfs-ui--operate-page 1))

(defun cfs-ui-previous-main-page ()
  (interactive)
  (cfs-ui--operate-page -1))

(defun cfs-ui-next-page ()
  (interactive)
  (cfs-ui--operate-page 1 t))

(defun cfs-ui-previous-page ()
  (interactive)
  (cfs-ui--operate-page -1 t))

(defun cfs-ui-restart ()
  (interactive)
  (let ((current-page cfs-ui--current-page)
        (point (point)))
    (cfs-ui)
    (cfs-ui--switch-to-page current-page)
    (goto-char point)))

(defvar cfs-ui-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "f" 'cfs-ui-next-main-page)
    (define-key map "b" 'cfs-ui-previous-main-page)
    (define-key map "F" 'cfs-ui-next-page)
    (define-key map "B" 'cfs-ui-previous-page)
    (define-key map "R" 'cfs-ui-restart)
    (define-key map " " 'cfs-ui-toggle-select-font)
    (define-key map "\t" 'cfs-ui-forward)
    (define-key map "\e\t" 'cfs-ui-backward)
    (define-key map [backtab] 'cfs-ui-backward)
    (define-key map "=" 'cfs-ui-increase-align)
    (define-key map "-" 'cfs-ui-decrease-align)
    (define-key map (kbd "C-c C-c") 'cfs-ui-test-align)
    (define-key map (kbd "C-c C-r") 'cfs-ui-quit-align)
    (define-key map (kbd "C-<up>") 'cfs-ui-increase-align)
    (define-key map (kbd "C-<down>") 'cfs-ui-decrease-align)
    map)
  "Keymap for `cfs-ui-mode'.")

(define-derived-mode cfs-ui-mode special-mode "CFS-UI"
  "Major mode for cfs-ui. Do not call this mode function yourself.
It is meant for internal use."
  (use-local-map cfs-ui-mode-map)
  (custom--initialize-widget-variables))
(put 'cfs-ui-mode 'mode-class 'special)

(defun cfs-ui ()
  (interactive)
  (if (not (display-graphic-p))
      (cfs-message t "Chinese-fonts-setup 不支持 emacs 终端模式！")
    ;; "cus-edit" 不能很好的在 emacs daemon 下工作，hack!
    (setq custom-raised-buttons
          (not (equal (face-valid-attribute-values :box)
                      '(("unspecified" . unspecified)))))
    (load-library "cus-edit")
    (dolist (page-info cfs-ui--pages)
      (let ((page-name (car page-info)))
        (funcall (cfs-ui--get-page-function page-name) nil nil t)))
    (funcall (cfs-ui--get-page-function 'english-fonts-page))))

;; #+END_SRC
;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'cfs-ui)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; cfs-ui.el ends here
;; #+END_SRC
