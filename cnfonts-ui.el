;;; cnfonts-ui.el --- A cnfonts profile editor with beautiful interface.  -*- lexical-binding: t; -*-

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
(require 'cnfonts)

(defconst cnfonts-ui--pages
  '((start-page
     :keybinding "t"
     :button-name "开始")
    (english-fonts-page
     :index 0
     :keybinding "e"
     :button-name "英文")
    (chinese-fonts-page
     :index 1
     :keybinding "c"
     :button-name "中文")
    (extb-fonts-page
     :index 2
     :keybinding "x"
     :button-name "EXT-B")
    (symbol-fonts-page
     :index 3
     :keybinding "s"
     :note "
注意：字符等宽对齐往往不容易设置，涉及字体混用, char-width, 字体
优先级等多种原因，有兴趣的同学可以看看这个链接：

    https://github.com/tumashu/cnfonts/issues/64

Emacs 25.2 以后，当 default font 有某个字符的时候，优先使用这个
字体，可以将 use-default-font-for-symbols 设置为 nil 来关闭。"
     :button-name "字符")
    (ornament-fonts-page
     :index 4
     :keybinding "d"
     :note (lambda ()
             (format "
某些 Emacs 社区配置（比如：spacemacs）使用某些特殊字符或者符号做
为装饰或者点缀，这个页面用于设置这些特殊字符的字体。
比如: %S"
                     (mapconcat (lambda (x)
                                  (when (ignore-errors (consp x))
                                    (concat (char-to-string (car x))
                                            (char-to-string (cdr x)))))
                                cnfonts-ornaments "")))
     :button-name "点缀")
    (align-page
     :align-page t
     :keybinding "1"
     :button-name "对齐")
    (key-page
     :keybinding "k"
     :button-name "快捷键")
    (help-page
     :keybinding "h"
     :button-name "帮助")))

(defvar cnfonts-ui-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "f" 'cnfonts-ui-next-page)
    (define-key map "b" 'cnfonts-ui-previous-page)
    (define-key map "R" 'cnfonts-ui-restart)
    (define-key map " " 'cnfonts-ui-toggle-select-font)
    (define-key map "\t" 'cnfonts-ui-forward)
    (define-key map "\e\t" 'cnfonts-ui-backward)
    (define-key map [backtab] 'cnfonts-ui-backward)
    (define-key map "=" 'cnfonts-ui-increase-align)
    (define-key map "-" 'cnfonts-ui-decrease-align)
    (define-key map (kbd "C-<up>") 'cnfonts-ui-increase-align)
    (define-key map (kbd "C-<down>") 'cnfonts-ui-decrease-align)
    (define-key map [drag-mouse-1] 'ignore)
    (define-key map [mouse-1]      'ignore)
    map)
  "Keymap for `cnfonts-ui-mode'.")

(defvar cnfonts-ui--widgets-alist nil)
(defvar cnfonts-ui--current-page nil)
(defvar cnfonts-ui--widgets-navigation nil)
(defvar cnfonts-ui--widgets-elisp-snippet nil)
(defvar cnfonts-ui--verbose nil)
(defvar cnfonts-ui--move-mouse nil)

(defun cnfonts-ui--move-mouse ()
  "Move mouse to current point."
  (let ((x-y (posn-x-y (posn-at-point (+ (point) 1)))))
    (when (and (car x-y) (cdr x-y))
      (set-mouse-pixel-position
       (window-frame)
       (+ (car x-y) (/ (default-font-width) 2))
       (+ (cdr x-y) (/ (default-font-height) 2))))))

(defun cnfonts-ui--switch-to-page (page-name)
  "Switch to page which name is PAGE-NAME."
  (let ((point (point)))
    (switch-to-buffer (format " *cnfonts: %S*" page-name))
    (dolist (widget cnfonts-ui--widgets-navigation)
      (let ((orig-value (widget-value widget))
            (widget-page (widget-get widget :page-name)))
        (if (eq cnfonts-ui--current-page widget-page)
            (widget-value-set
             widget (replace-regexp-in-string " " "*" orig-value))
          (widget-value-set
           widget (replace-regexp-in-string "*" " " orig-value)))))
    (goto-char point)))

(defun cnfonts-ui--create-page-switch-button (page-name &optional ignore-face)
  "Create a button which used to switch page named PAGE-NAME.
TODO: IGNORE-FACE."
  (let ((button-name (cnfonts-ui--get-page-info page-name :button-name))
        (alter-button-name (cnfonts-ui--get-page-info page-name :alter-button-name))
        (action (cnfonts-ui--get-page-function page-name)))
    (if ignore-face
        (widget-create 'push-button
                       :value (format "[ %s ]" (or alter-button-name button-name))
                       :button-face-get 'ignore
                       :mouse-face-get 'ignore
                       :page-name page-name
                       :action action)
      (widget-create 'push-button
                     :value (format " %s " button-name)
                     :page-name page-name
                     :action action))))

(defun cnfonts-ui--create-navigation ()
  (dolist (page-name (mapcar #'car cnfonts-ui--pages))
    (push (cnfonts-ui--create-page-switch-button page-name)
          cnfonts-ui--widgets-navigation)
    (widget-insert " ")))

(defun cnfonts-ui--create-align-line (index label fontsize-list align-string)
  (let ((fontsize (number-to-string (nth index fontsize-list)))
        widget1 widget2 widget3 widget4)
    (widget-insert (format "%s. " (+ index 1)))
    (widget-insert (format "%-5s " label))
    (if (= index 0)
        (progn (setq widget1 (widget-create 'push-button
                                            :value (format "%-6s     " fontsize)
                                            :flag t
                                            :key (car fontsize-list)
                                            :button-face-get 'ignore
                                            :mouse-face-get 'ignore))
               (push (cons widget1 widget1) cnfonts-ui--widgets-alist))
      (setq widget2 (widget-create 'push-button
                                   :value (format "%-5s" fontsize)
                                   :index index
                                   :flag t
                                   :key (car fontsize-list)
                                   :tab-stop-point t
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore))
      (setq widget3 (widget-create 'push-button
                                   :tag "[-]"
                                   :index index
                                   :flag t
                                   :key (car fontsize-list)
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore
                                   :action 'cnfonts-ui-decrease-align))
      (setq widget4 (widget-create 'push-button
                                   :tag "[+]"
                                   :index index
                                   :flag t
                                   :key (car fontsize-list)
                                   :button-face-get 'ignore
                                   :mouse-face-get 'ignore
                                   :action 'cnfonts-ui-increase-align))
      (push (cons widget2 widget2) cnfonts-ui--widgets-alist)
      (push (cons widget3 widget2) cnfonts-ui--widgets-alist)
      (push (cons widget4 widget2) cnfonts-ui--widgets-alist))
    (widget-insert "                  ")
    (widget-insert align-string)
    (widget-insert "\n")))

(defun cnfonts-ui--return-status-string (font index)
  (format "%-2s %-2s"
          (if (cnfonts--font-exists-p font t) "" "NA")
          (if (member font (nth index cnfonts-personal-fontnames)) "P" "")))

(defun cnfonts-ui--get-page-function (page-name)
  (intern (concat "cnfonts-ui-page-" (symbol-name page-name))))

(defmacro cnfonts-ui-create-page (page-name &rest body)
  (declare (indent 1) (debug t))
  (let ((func-name (intern (concat "cnfonts-ui-page-" (symbol-name page-name))))
        (buffer-name (make-symbol "buffer-name"))
        (point (make-symbol "point")))
    `(defun ,func-name (&optional _widget _event create-buffer)
       (interactive)
       (let ((,buffer-name (format " *cnfonts: %S*" ',page-name))
             (,point (point)))
         ;; 创建或者切换 page 时，强制重新读取 profile.
         (cnfonts--read-profile t)
         (if create-buffer
             (with-current-buffer (get-buffer-create ,buffer-name)
               (let ((inhibit-read-only t))
                 (erase-buffer))
               (cnfonts-ui-mode)
               (define-key cnfonts-ui-mode-map (cnfonts-ui--get-page-info ',page-name :keybinding) ',func-name)
               (set (make-local-variable 'cnfonts-ui--widgets-alist) nil)
               (set (make-local-variable 'cnfonts-ui--current-page) ',page-name)
               (set (make-local-variable 'cnfonts-ui--widgets-navigation) nil)
               (set (make-local-variable 'cnfonts-ui--widgets-elisp-snippet) nil)
               (setq truncate-lines t)
               ,@body
               (widget-setup)
               (goto-char ,point))
           (cnfonts-ui--switch-to-page ',page-name))
         (when cnfonts-ui--move-mouse
           (cnfonts-ui--move-mouse))))))

(defun cnfonts-ui--get-page-info (page-name key)
  (let ((page-info (cdr (assq page-name cnfonts-ui--pages))))
    (plist-get page-info key)))

(defun cnfonts-ui--create-align-page (_page-name)
  (let* ((profile-name (cnfonts--get-current-profile t))
         (profile-fontsize (cnfonts--get-profile-fontsize profile-name))
         (fontsize-list (cnfonts--get-fontsizes profile-fontsize)))

    (widget-insert "\n")
    (cnfonts-ui--create-navigation)
    (widget-insert "\n\n")

    (widget-insert "字体类别    字号 ")
    (widget-insert (format "%51s\n" (format "( %s )" (cnfonts--get-current-profile t))))
    (widget-insert "---------- ---------------------------------------------------------\n")

    (cnfonts-ui--create-align-line 0 "ASCII   " fontsize-list "| More haste, less speed. |")
    (cnfonts-ui--create-align-line 1 "CJKV    " fontsize-list "| 为天地立心，为生民立命；|")
    (cnfonts-ui--create-align-line 2 "EXT-B   " fontsize-list "| 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀。|")
    (cnfonts-ui--create-align-line 3 "Symbol  " fontsize-list "> αβχδεφγηιϕκλνοπθρστυ")
    (cnfonts-ui--create-align-line 4 "Ornament" fontsize-list
                                   (concat "> "
                                           (mapconcat (lambda (x)
                                                        (when (ignore-errors (consp x))
                                                          (concat (char-to-string (car x))
                                                                  (char-to-string (cdr x)))))
                                                      cnfonts-ornaments "")))

    (widget-insert "\n")

    (widget-create 'push-button
                   :button-face-get 'ignore
                   :mouse-face-get 'ignore
                   :tag "[设置上一个字号]"
                   :action '(lambda (widget event)
                              (let ((cnfonts-ui--move-mouse t))
                                (cnfonts-decrease-fontsize)
                                (cnfonts-ui-page-align-page nil nil t))))
    (widget-insert "                                    ")
    (widget-create 'push-button
                   :button-face-get 'ignore
                   :mouse-face-get 'ignore
                   :tag "[设置下一个字号]"
                   :action '(lambda (widget event)
                              (let ((cnfonts-ui--move-mouse t))
                                (cnfonts-increase-fontsize)
                                (cnfonts-ui-page-align-page nil nil t))))))

(defun cnfonts-ui--create-fonts-page (page-name)
  (let ((index (cnfonts-ui--get-page-info page-name :index))
        (note (cnfonts-ui--get-page-info page-name :note) )
        (fontname-alist cnfonts--custom-set-fontnames))
    (widget-insert "\n")
    (cnfonts-ui--create-navigation)
    (widget-insert "\n")
    (when note
      (if (functionp note)
          (widget-insert (funcall note) "\n")
        (widget-insert note "\n")))
    (widget-insert "
P:    表示当前字体包含在变量 `cnfonts-personal-fontnames' 中。
NA:   表示系统没有安装当前字体。\n\n")
    (let ((fonts (nth index fontname-alist))
          widget1 widget2 widget3)
      (widget-insert "状态  当前字体")
      (widget-insert (format "%53s\n" (format "( %s )" (cnfonts--get-current-profile t))))
      (widget-insert "----  -------------------------------------------------------------\n")
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
        (widget-insert "\n" )))))

(cnfonts-ui-create-page english-fonts-page
  (cnfonts-ui--create-fonts-page 'english-fonts-page))

(cnfonts-ui-create-page chinese-fonts-page
  (cnfonts-ui--create-fonts-page 'chinese-fonts-page))

(cnfonts-ui-create-page extb-fonts-page
  (cnfonts-ui--create-fonts-page 'extb-fonts-page))

(cnfonts-ui-create-page symbol-fonts-page
  (cnfonts-ui--create-fonts-page 'symbol-fonts-page))

(cnfonts-ui-create-page ornament-fonts-page
  (cnfonts-ui--create-fonts-page 'ornament-fonts-page))

(cnfonts-ui-create-page align-page
  (cnfonts-ui--create-align-page 'align-page))

(cnfonts-ui-create-page help-page
  (cnfonts-ui--create-tab-stop-point)
  (cnfonts-ui--create-navigation)
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

(cnfonts-ui-create-page start-page
  (cnfonts-ui--create-tab-stop-point)
  (cnfonts-ui--create-navigation)
  (widget-insert "

** 注意事项

如果需要 Emacs 启动时激活 cnfonts，请在 Emacs 配置文件中添加一行
代码：

                     (cnfonts-mode 1)

常用命令                                        功能
----------------------------------------------  ----------------
cnfonts-edit-profile                            调整字体设置
cnfonts-increase-fontsize                       增大字号
cnfonts-decrease-fontsize                       减小字号


注意：如果安装 cnfonts 后，发现 Emacs 卡顿甚至崩溃，可以安装
HanaMinB 字体试试，这个字体的下载地址可以从 [ 帮助 ] 页面中找到。
" ))

;; key-page *must* create at the end, make sure other page's
;; keybinding are defined.
(cnfonts-ui-create-page key-page
  (cnfonts-ui--create-tab-stop-point)
  (cnfonts-ui--create-navigation)
  (widget-insert
   (substitute-command-keys "

** 标签切换快捷键

 功能                    按键
 ----------------------  --------
 切换到下一个标签        \\[cnfonts-ui-next-page]
 切换到上一个标签        \\[cnfonts-ui-previous-page]
 切换到 [ 开始 ] 标签    \\[cnfonts-ui-page-start-page]
 切换到 [ 英文 ] 标签    \\[cnfonts-ui-page-english-fonts-page]
 切换到 [ 中文 ] 标签    \\[cnfonts-ui-page-chinese-fonts-page]
 切换到 [ EXT-B ] 标签   \\[cnfonts-ui-page-extb-fonts-page]
 切换到 [ 对齐 ] 标签    \\[cnfonts-ui-page-align-page]
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
         (flag (widget-get widget1 :flag)))
    (if (not flag)
        (message "[cnfonts]: 当前光标所在位置不对，请将光标移动到字体所在的行上面。")
      (widget-toggle-action widget1 event)
      (dolist (w widgets)
        (unless (equal (widget-get w :font-name) font)
          (widget-value-set w nil)
          (widget-apply w :notify w event)))
      (if (not (cnfonts--font-exists-p font))
          (message "[cnfonts]: 系统没有安装字体: %S ." font)
        (when (widget-value widget1)
          (cnfonts--update-profile-fontnames index font)
          (cnfonts--save-profile)
          (cnfonts-set-font))))))

(defun cnfonts-ui--operate-align (&optional widget _event n)
  (let* ((widget (or widget (widget-at)))
         (key (widget-get widget :key))
         (index (widget-get widget :index))
         (flag (widget-get widget :flag))
         (widget-show-fontsize (cdr (assoc widget cnfonts-ui--widgets-alist))))
    (if (not flag)
        (message "[cnfonts]: 当前光标所在位置不对，请将光标移动到 ‘中文字号’ 或者 ‘EXT-B字体字号’ 对应的数字上。")
      (when (and index key (numberp n))
        (cnfonts--update-profile-fontsizes key index n)
        ;; 更新加号按钮和减号按钮前面的数字标签
        (widget-value-set
         widget-show-fontsize
         (format "%-5s" (nth index (cnfonts--get-fontsizes key)))))
      (when key
        (cnfonts--save-profile)
        (cnfonts--set-font (cnfonts--get-fontsizes key)))
      (dotimes (i 5)
        (sit-for 0.3)
        (message "[cnfonts]: 测试 Minibuffer 是否抖动 (%s/%s)" (+ i 1) 5)
        (sit-for 0.3)
        (message nil)))))

(defun cnfonts-ui--create-tab-stop-point ()
  "Create a widget.
the curse will stop to this widget when forward/backward widget."
  (widget-create 'push-button
                 :tag "\n"
                 :tab-stop-point t
                 :button-face-get 'ignore
                 :mouse-face-get 'ignore))

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

(defun cnfonts-ui--operate-page (step)
  "Internal function, which used to cnfonts-ui page switch."
  (let* ((pages (mapcar #'car cnfonts-ui--pages))
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

(defun cnfonts-ui-next-page ()
  "Switch to next page of cnfonts-ui."
  (interactive)
  (cnfonts-ui--operate-page 1))

(defun cnfonts-ui-previous-page ()
  "Switch to previous page of cnfonts-ui."
  (interactive)
  (cnfonts-ui--operate-page -1))

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
      (message "[cnfonts]: 不支持 Emacs 终端模式！")
    ;; "cus-edit" 不能很好的在 Emacs daemon 下工作，hack!
    (setq custom-raised-buttons
          (not (equal (face-valid-attribute-values :box)
                      '(("unspecified" . unspecified)))))
    (load-library "cus-edit")
    (dolist (page-info cnfonts-ui--pages)
      (let ((page-name (car page-info)))
        (funcall (cnfonts-ui--get-page-function page-name) nil nil t)))
    (funcall (cnfonts-ui--get-page-function 'start-page))))

;; * Footer
(provide 'cnfonts-ui)

;;; cnfonts-ui.el ends here
