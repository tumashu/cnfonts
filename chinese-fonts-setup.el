;;; chinese-fonts-setup.el --- 实现中英文字体等宽对齐的字体配置工具。

;; Copyright (c) 2011-2014, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/chinese-fonts-setup
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `chinese-fonts-setup' 是一个emacs中文字体配置工具。可以比较方便的
;; 的实现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。
;;
;; 这个package特别适用于需要处理中英文混合表格的中文org-mode用户。
;;
;; ### 过程展示 ###
;;
;;     http://www.tudou.com/programs/view/v7Kr0_a9INw/
;;
;; ### 下载 ###
;;
;;     https://github.com/tumashu/chinese-fonts-setup
;;
;; ### 安装 ###
;; 将这个文件放到任意一个emacs搜索目录之下，然后在~/.emacs中添加：
;;
;;     (require 'chinese-fonts-setup)
;;
;; ### 配置 ###
;; chinese-fonts-setup 使用profile的概念，来实现特定的环境使用特定的
;; 字体配置，比如：在编程时使用 “Consolas + 微米黑”，在阅读文章时使用
;; “PragmataPro + 黑体”。
;;
;; 每一个profile都是一个emacs-lisp文件。其中包括了英文字体设置，中文字体设置
;; 以及中文字体调整系数（scale）。

;; chinese-fonts-setup 默认使用三个profile: profile1, profile2 和 profile3,
;; 如果想使用其他有意义的名称，可以使用下面类似的方式配置:
;;
;;      (setq cfs--profiles
;;            '("program" "org-mode" "read-book"))
;;
;; profile文件保存在`cfs--profiles-directory'对应的目录中。如果文件不存在，
;; chinese-fonts-setup 在切换 profile 时通过自带的falback信息创建一个。
;;
;; 切换 profile 的命令有：
;; 1. `cfs--switch-profile' (选择profile)
;; 2. `cfs--next-profile'   (直接切换到下一个profile)
;;
;; 如果当前的profile不适合时，可以通过`cfs--edit-profile'来编辑当前
;; 的profile文件。chinese-fonts-setup自带一个profile-edit编辑模式。
;;
;; 1.  C-c C-c     `cfs--test-scale-at-point'
;;                  察看字体显示效果
;; 2.  C-<up>      `cfs--increment-font-scale-at-point'
;;                  增大光标下的scale数字，同时显示增加后的字体对齐效果
;; 3.  C-<down>    `cfs--decrement-font-scale-at-point'
;;                  减小光标下的scale数字，同时显示减小后的字体对齐效果
;;

;;; Code:
(require 'cl)
(require 'ido)

(defcustom cfs--profiles '("profile1" "profile2" "profile3")
  "Lists chinese-fonts-setup profiles"
  :group 'chinese-fonts-setup
  :type 'list)

(defcustom cfs--profiles-directory "~/.emacs.d/cfs-profiles.d/"
  "*Directory variable from which all other chinese-fonts-setup profiles are derived."
  :group 'chinese-fonts-setup
  :type 'directory)

(defcustom cfs--ignore-italic t
  "使用正常代替斜体。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defcustom cfs--ignore-bold-italic t
  "使用粗体代替粗斜体。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defvar cfs--current-profile-name (car cfs--profiles)
  "Current profile name used by chinese-fonts-setup")

(defvar cfs--current-size 12.5
  "Current font size used by chinese-fonts-setup.")

(defconst cfs--size-steps
  '(9 10.5 11.5 12.5 14 16 18 20 22))

(defconst cfs--size-fallback 12.5)

(defconst cfs--scale-fallback
  '(1.05 1.05 1.10 1.10 1.10 1.05 1.00 1.05 1.05))

(defconst cfs--names-fallback
  '(("PragmataPro" "Monaco" "Consolas" "Menlof" "DejaVu Sans Mono"
     "Droid Sans Mono Pro" "Droid Sans Mono" "Inconsolata" "Source Code Pro"
     "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter"
     "monoOne" "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono"
     "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Courier New"
     "Courier" "Cousine" "Fira Mono" "Lekton" "Ubuntu Mono" "Liberation Mono"
     "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows"
     "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
    ("黑体" "文泉驿等宽微米黑" "Microsoft Yahei" "Microsoft_Yahei" "微软雅黑"
     "Hiragino Sans GB" "文泉驿等宽正黑" "文泉驿正黑" "文泉驿点阵正黑"
     "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书"
     "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文彩云"
     "华文新魏" "华文细黑" "华文行楷")
    ("PragmataPro" "Courier New")))

(defconst cfs--test-string "
;; 请看下面中文和英文能否对齐.
;; +----------------------------------+
;; |  天生我材必有用，千金散尽还复来。|  (^_/)
;; |  abcdefghigklmnopqrstuvwxyz,.?!  | (='.'=)
;; | *abcdefghigklmnopqrstuvwxyz,.?!* | (0)_(0)
;; | /abcdefghigklmnopqrstuvwxyz,.?!/ |
;; +----------------------------------+
")

(defun cfs--get-current-profile ()
  (let ((directory-name
	 (expand-file-name
	  (file-name-as-directory cfs--profiles-directory))))
    (make-directory directory-name t)
    (concat directory-name
	    cfs--current-profile-name ".el")))

(defun cfs--dump-variable (variable-name value)
  "Insert a \"(setq VARIABLE value)\" in the current buffer."
  (cond ((atom value)
	 (insert (format "\n(setq %s %S)\n" variable-name value)))
	((atom (car value))
	 (insert (format "\n(setq %s\n       '%S)\n" variable-name value)))
	(t (insert (format "\n(setq %s\n      '(" variable-name))
	   (dolist (e value)
	     (insert (format "\n        %S" e)))
	   (insert "\n       ))\n"))))

(defun cfs--save-profile (fonts-names fonts-scales)
  "Save fonts names and scales to current profile"
  (interactive)
  (let ((variable-fonts-names "cfs--custom-set-fonts-names")
	(variable-fonts-scales "cfs--custom-set-fonts-scales"))
    (with-temp-buffer
      (erase-buffer)
      (insert ";;; 设置默认字体列表，按`C-c C-c'测试字体显示效果")
      (cfs--dump-variable variable-fonts-names  fonts-names)
      (insert (format "\n;;; 为每个字号%s设置中文调整系数，使中英文等宽度。" cfs--size-steps))
      (cfs--dump-variable variable-fonts-scales fonts-scales)
      (write-file (cfs--get-current-profile)))))

(defun cfs--read-profile ()
  "Get previously saved fonts names and scales from current profile"
  (interactive)
  (let ((file (cfs--get-current-profile)))
    (when (file-readable-p file)
      (load-file file))
    (list (if (boundp 'cfs--custom-set-fonts-names)
	      cfs--custom-set-fonts-names
	    cfs--names-fallback)
	  (if (boundp 'cfs--custom-set-fonts-scales)
	      cfs--custom-set-fonts-scales
	    cfs--scale-fallback))))

(defun cfs--font-exists-p (font)
  (if (null (x-list-fonts font))
      nil t))

(defun cfs--get-valid-fonts ()
  (mapcar (lambda (x)
	    (find-if #'cfs--font-exists-p x))
	  (car (cfs--read-profile))))

(defun cfs--make-font-string (font-name font-size)
  (if (and (stringp font-size)
	   (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s-%s" font-name font-size)))

(defun cfs--get-scale (&optional size)
  (let* ((scale-list (car (cdr (cfs--read-profile))))
	 (index (or (position size cfs--size-steps) 1)))
    (unless (file-exists-p (cfs--get-current-profile))
      (message "如果中英文不能对齐，请运行`cfs--edit-profile'编辑当前profile。"))
    (or (nth index scale-list) 1)))

(defun cfs--set-font (font-size &optional font-scale)
  (setq face-font-rescale-alist
	(mapcar (lambda (x)
		  (cons x (or font-scale 1.25)))
		(nth 1 (car (cfs--read-profile)))))
  (cfs--set-font-internal font-size))

(defun cfs--set-font-internal (font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (let* ((valid-fonts (cfs--get-valid-fonts))
	 (english-main-font (cfs--make-font-string (nth 0 valid-fonts) font-size))
	 (chinese-main-font (font-spec :family (nth 1 valid-fonts)))
	 (english-bold-font
	  (font-spec :slant 'normal :weight 'bold
		     :size font-size
		     :family (nth 0 valid-fonts)))
	 (english-italic-font
	  (font-spec :slant 'italic :weight 'normal
		     :size font-size
		     :family (nth 0 valid-fonts)))
	 (english-bold-italic-font
	  (font-spec :slant 'italic :weight 'bold
		     :size font-size
		     :family (nth 0 valid-fonts)))
	 (english-symbol-font (font-spec :family (nth 3 valid-fonts))))
    (set-face-attribute 'default nil :font english-main-font)
    (set-face-font 'italic
		   (if cfs--ignore-italic
		       english-main-font
		     english-italic-font))
    (set-face-font 'bold-italic
		   (if cfs--ignore-bold-italic
		       english-bold-font
		     english-bold-italic-font))
    (set-fontset-font t 'symbol english-symbol-font)
    (set-fontset-font t nil (font-spec :family "DejaVu Sans"))

    ;; Set Chinese font and don't not use 'unicode charset,
    ;; it will cause the english font setting invalid.
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset chinese-main-font))))

(defun cfs--step-font-size (step)
  (let ((steps cfs--size-steps)
	(current-size cfs--current-size)
	next-size)
    (when (< step 0)
      (setq steps (reverse cfs--size-steps)))
    (setq next-size
	  (cadr (member current-size steps)))
    (when next-size
      (cfs--set-font next-size (cfs--get-scale next-size))
      (customize-save-variable 'cfs--current-size next-size)
      (message "Your font size is set to %.1f" next-size))))

(defun cfs--set-font-with-saved-size ()
  (let* ((font-size cfs--current-size)
	 (font-size-scale (cfs--get-scale font-size)))
    (when (display-graphic-p)
      (cfs--set-font font-size font-size-scale))))

;; 正常启动emacs时设置字体
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (cfs--set-font-with-saved-size))))
  (cfs--set-font-with-saved-size))

(defun cfs--decrease-font-size ()
  (interactive)
  (cfs--step-font-size -1))

(defun cfs--increase-font-size ()
  (interactive)
  (cfs--step-font-size 1))

(defvar cfs--profile-edit-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'cfs--test-scale-at-point)
    (define-key keymap (kbd "C-<up>") 'cfs--increment-font-scale-at-point)
    (define-key keymap (kbd "C-<down>") 'cfs--decrement-font-scale-at-point)
    (define-key keymap (kbd "C-<right>") 'cfs--increment-font-scale-at-point)
    (define-key keymap (kbd "C-<left>") 'cfs--decrement-font-scale-at-point)
    keymap)
  "Keymap for `cfs--profile-edit-mode', a minor mode used to setup fonts names and scales")

(define-minor-mode cfs--profile-edit-mode
  "Minor for setup fonts names and scales"
  nil " Rem" cfs--profile-edit-mode-map)

(defun cfs--switch-profile ()
  (interactive)
  (let ((profile (ido-completing-read "Set chinese-fonts-setup profile to:" cfs--profiles)))
    (setq cfs--current-profile-name profile)
    (customize-save-variable 'cfs--current-profile-name profile)
    (cfs--set-font-with-saved-size)))

(defun cfs--next-profile (&optional step)
  (interactive)
  (let ((profiles cfs--profiles)
	(current-profile cfs--current-profile-name)
	next-profile)
    (setq next-profile
	  (or (cadr (member current-profile profiles))
	      (car profiles)))
    (when next-profile
      (setq cfs--current-profile-name next-profile)
      (customize-save-variable 'cfs--current-profile-name next-profile))
    (when (display-graphic-p)
      (cfs--set-font-with-saved-size))
    (message "Current chinese-fonts-setup profile is set to: \"%s\"" next-profile)))

(defun cfs--edit-profile ()
  (interactive)
  (let ((file (cfs--get-current-profile)))
    (unless (file-readable-p file)
      (cfs--save-profile cfs--names-fallback
			      cfs--scale-fallback))
    (find-file file)
    (cfs--profile-edit-mode 1)
    (goto-char (point-min))))

(defun cfs--test-scale-at-point ()
  "Test scale list at point, which is usd to write font scale list"
  (interactive)
  (let (scale size index)
    (setq scale (sexp-at-point))
    (if (and scale (numberp scale))
	(progn
	  (setq index
		(save-excursion
		  (let* ((point1 (point))
			 (point2 (progn (search-backward "(")
					(point))))
		    (length (split-string
			     (buffer-substring-no-properties point1 point2)
			     " ")))))
	  (setq size (nth (1- index) cfs--size-steps))
	  (cfs--set-font size scale)
	  (cfs--show-font-effect size scale))
      (cfs--set-font 14 1.25)
      (cfs--show-font-effect 14 1.25 t))))

(defun cfs--change-font-scale-at-point (step)
  (interactive)
  (skip-chars-backward "0123456789\\.")
  (or (looking-at "[0123456789.]+")
      (error "No number at point"))
  (replace-match
   (format "%.5s"
	   (number-to-string
	    (+ step (string-to-number (match-string 0))))))
  (backward-char 1)
  (cfs--test-scale-at-point))

(defun cfs--increment-font-scale-at-point ()
  (interactive)
  (cfs--change-font-scale-at-point 0.01))

(defun cfs--decrement-font-scale-at-point ()
  (interactive)
  (cfs--change-font-scale-at-point -0.01))

(defun cfs--show-font-effect (&optional size scale info)
  "show font and its size in a new buffer"
  (interactive)
  (let ((buffer-name "*Show-font-effect*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
	(org-mode))
      (setq truncate-lines 1)
      (when size
	(insert (format "# 英文字体大小设置为: %s ; " size)))
      (when scale
	(insert (format "中文字体调整系数(scale)设置为: %s 。\n" scale)))
      (when info
	(insert
	 (concat
	  "# 将光标移动到`cfs--custom-set-fonts-scales‘中各个数字上，"
	  "C-<up> 增大 scale 的值，C-<down> 减小 scale 的值。")))
      (insert
       (replace-regexp-in-string
	"\\^"  "\\\\"
	(replace-regexp-in-string
	 "@@"  "   "
	 cfs--test-string)))
      (when (and size scale)
	(cfs--set-font size scale)))))

;;;###autoload(require 'chinese-fonts-setup)
(provide 'chinese-fonts-setup)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-fonts-setup.el ends here
