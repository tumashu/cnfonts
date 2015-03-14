;;; chinese-fonts-setup.el --- A fonts config tool enforcing double-width Chinese character display

;; Copyright (c) 2011-2014, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/chinese-fonts-setup
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.0.1
;; Keywords: convenience, Chinese, font

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

;; `chinese-fonts-setup' 是一个emacs中文字体配置工具。可以比较方便地
;; 实现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。
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
;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET chinese-fonts-setup RET
;; 3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：
;;
;; ```lisp
;; (require 'chinese-fonts-setup)
;; ```
;;
;; ### 配置 ###
;; chinese-fonts-setup 使用profile的概念，来实现特定的环境使用特定的
;; 字体配置，比如：在编程时使用 “Consolas + 微米黑”，在阅读文章时使用
;; “PragmataPro + 黑体”。
;;
;; 每一个profile都是一个emacs-lisp文件。其中包括了英文字体设置，中文字体设置
;; 以及中文字体大小。

;; chinese-fonts-setup 默认使用三个profile: profile1, profile2 和 profile3,
;; 如果想使用其他有意义的名称，可以使用下面类似的方式配置:
;;
;; ```lisp
;; (setq cfs-profiles
;;     '("program" "org-mode" "read-book"))
;; ```
;;
;; 所有的profile文件都保存在`cfs-profiles-directory'对应的目录中。
;; 如果profile文件不存在，`chinese-fonts-setup' 将使用其自带的fallback
;; 信息来配置字体。
;;
;; 在运行profile编辑命令`cfs-edit-profile'的时候，缺失的falback文件
;; 将会自动创建，其原始内容为软件包自带的fallback信息。
;;
;; `chinese-fonts-setup' 默认不会覆盖已经存在的profile文件。当需要重置
;; 某个profile文件时，可以使用命令：`cfs-regenerate-profile'。这个命令
;; 会强制覆盖profile文件，请做好备份。
;;
;; 切换 profile 的命令有：
;;
;; 1. `cfs-switch-profile' (使用ido切换profile)
;; 2. `cfs-next-profile'   (直接切换到下一个profile)
;;
;; 如果当前的profile不适合时，可以通过`cfs-edit-profile'来编辑当前
;; 的profile文件。chinese-fonts-setup自带一个profile-edit编辑模式。
;;
;; 1.  C-c C-c     `cfs-test-chinese-fontsize-at-point'
;;                  察看字体显示效果
;; 2.  C-up        `cfs-increment-chinese-fontsize-at-point'
;;                  增大光标下的中文字号，同时显示增加后的字体对齐效果
;; 3.  C-down      `cfs-decrement-chinese-fontsize-at-point'
;;                  减小光标下的中文字号，同时显示减小后的字体对齐效果
;;
;; 注意配置完成后，可能需要重启 Emacs。(参考： http://debbugs.gnu.org/db/17/1785.html)
;;
;; ### 调整字体大小 ###
;; `chinese-fonts-setup' 使用下述两个命令调整字体大小:
;;
;; 1.  `cfs-increase-english-fontsize' 增大字体大小
;; 2.  `cfs-decrease-english-fontsize' 减小字体大小
;;
;; 在调整字体大小的同时，字号信息也通过customize-save-variable函数保存到~/.emacs中了。
;;
;; ### 使用斜体和粗斜体 ###
;; `chinese-fonts-setup' 默认使用正常字体代替斜体，粗体代替粗斜体。这样设置的原因是：
;; 大多数英文等宽字体包含的斜体不能将(9 10.5 11.5 12.5 14 16 18 20 22)这几个字号完全覆盖。
;; 如果想使用斜体和粗斜体，请使用下面的设置：
;;
;; ```lisp
;; (setq cfs-enable-italic t)
;; (setq cfs-enable-bold-italic t)
;; ```
;;
;; 与此同时，你要使用一个包含粗体和粗斜体的英文等宽字体。

;; ### Tips ###
;;
;; 1. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
;; 2. 运行`(print (font-family-list))' 可以获得当前可用的字体的名称列表。
;;
;; ### 参考文章 ###
;;
;; 1. http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; 2. http://zhuoqiang.me/torture-emacs.html

;;; Code:
(require 'cl-lib)
(require 'ido)

(defcustom cfs-profiles '("profile1" "profile2" "profile3")
  "Lists chinese-fonts-setup profiles"
  :group 'chinese-fonts-setup
  :type 'list)

(defcustom cfs-profiles-directory "~/.emacs.d/cfs-profiles.d/"
  "*Directory variable from which all other chinese-fonts-setup profiles are derived."
  :group 'chinese-fonts-setup
  :type 'directory)

(defcustom cfs-enable-bold t
  "Enable 英文粗体字体。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defcustom cfs-enable-italic t
  "Enable 英文斜体字体。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defcustom cfs-enable-bold-italic t
  "Enable 英文粗斜体字体。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defvar cfs--current-profile-name (car cfs-profiles)
  "Current profile name used by chinese-fonts-setup")

(defvar cfs--profiles-english-fontsizes
  (mapcar (lambda (x) 12.5) cfs-profiles)
  "fontsizes list of all profiles.")

(defconst cfs--english-fontsizes-steps-fallback
  '(9 10.5 11.5 12.5 14 16 18 20 22))

(defconst cfs--chinese-fontsizes-steps-fallback
  '(9 10.5 11.5 12.5 14 16 18 20 22))

(defconst cfs--fontnames-fallback
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
    ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB")))

(defconst cfs--test-string "
| 正常字体    | 粗体        | 粗斜体        |
|-------------+-------------+---------------|
| 堂堂正正    | *五大三粗*  | /东倒西歪/    |
| I'm normal. | *I'm bold!* | /I'm italic?/ |
;; 请看上面表格线能否对齐, 如果没有对齐，请调整profile文件
;; 中变量 `cfs--custom-set-chinese-fontsizes-steps' 列表各个数字的大小。
")

(defconst cfs--profile-comment-1 "
;;; `cfs--custom-set-fonts-names' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *系统存在* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。")

(defconst cfs--profile-comment-2 "
;;; 为每个英文字体字号 (9 10.5 11.5 12.5 14 16 18 20 22) 设置对应的中文字体字号，使中英文等宽。
;;; 将光标移动到 `cfs--custom-set-chinese-fontsizes-steps' 列表中各个数字上：
;;; 1. C-c C-c 查看光标处中文字号的对齐效果。
;;; 2. C-<up> 增大光标处中文字号的大小，同时显示对齐效果。
;;; 3. C-<down> 减小光标处中文字号大小, 同时显示对齐效果。")

(defvar cfs--minibuffer-echo-string nil)

(defun cfs--get-profile (profile-name)
  (let ((directory-name
         (expand-file-name
          (file-name-as-directory cfs-profiles-directory))))
    (make-directory directory-name t)
    (concat directory-name
            (replace-regexp-in-string
             "/" "-"
             profile-name) ".el")))

(defun cfs--get-current-profile ()
  (cfs--get-profile cfs--current-profile-name))

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

(defun cfs--save-english-fontsize (profile-name size)
  (let* ((profiles-names cfs-profiles)
         (profiles-english-fontsizes cfs--profiles-english-fontsizes)
         (length1 (length profiles-names))
         (length2 (length profiles-english-fontsizes))
         (index (cl-position profile-name cfs-profiles :test #'string=)))
    (if (= length1 length2)
        (setf (nth index profiles-english-fontsizes) size)
      (setq profiles-english-fontsizes
            (mapcar (lambda (x) 12.5) profiles-names)))
    (setq cfs--profiles-english-fontsizes profiles-english-fontsizes)
    (customize-save-variable 'cfs--profiles-english-fontsizes profiles-english-fontsizes)))

(defun cfs--read-english-fontsize (profile-name)
  (let ((index (cl-position profile-name cfs-profiles :test #'string=)))
    (nth index cfs--profiles-english-fontsizes)))

(defun cfs--save-profile (fonts-names chinese-fontsizes-steps &optional profile-name)
  "Save fonts names and chinese fontsizes to current profile"
  (let ((variable-fonts-names "cfs--custom-set-fonts-names")
        (variable-chinese-fontsizes-steps "cfs--custom-set-chinese-fontsizes-steps"))
    (with-temp-buffer
      (erase-buffer)
      (insert (replace-regexp-in-string
               "^ *\n" ""
               cfs--profile-comment-1))
      (cfs--dump-variable variable-fonts-names  fonts-names)
      (insert cfs--profile-comment-2)
      (cfs--dump-variable variable-chinese-fontsizes-steps chinese-fontsizes-steps)
      (write-file (cfs--get-profile
                   (or profile-name cfs--current-profile-name))))))

(defun cfs--read-profile ()
  "Get previously saved fonts names and Chinese fontsizes from current profile"
  (interactive)
  (let ((file (cfs--get-current-profile)))
    (if (file-readable-p file)
        (progn (load-file file)
               (list
                (if (boundp 'cfs--custom-set-fonts-names)
                    cfs--custom-set-fonts-names
                  cfs--fontnames-fallback)
                (if (boundp 'cfs--custom-set-chinese-fontsizes-steps)
                    cfs--custom-set-chinese-fontsizes-steps
                  cfs--chinese-fontsizes-steps-fallback)))
      (list cfs--fontnames-fallback
            cfs--chinese-fontsizes-steps-fallback))))

(defun cfs--font-exists-p (font)
  (if (null (x-list-fonts font))
      nil t))

(defun cfs--get-valid-fonts ()
  (mapcar (lambda (x)
            (cl-find-if #'cfs--font-exists-p x))
          (car (cfs--read-profile))))

(defun cfs--make-font-string (fontname fontsize &optional type)
  (if (and (stringp fontsize)
           (equal ":" (string (elt fontsize 0))))
      (format "%s%s" fontname fontsize)
    (cond
     ((eq type 'bold) (format "%s-%s:weight=bold:slant=normal" fontname fontsize))
     ((eq type 'italic) (format "%s-%s::weight=normal:slant=italic" fontname fontsize))
     ((eq type 'bold-italic) (format "%s-%s:weight=bold:slant=italic" fontname fontsize))
     (t (format "%s-%s:weight=normal:slant=normal" fontname fontsize)))))

;; (cfs--get-fontset "courier" 10 'italic)

(defun cfs--get-chinese-fontsize (&optional size)
  (let* ((chinese-fontsizes-steps (car (cdr (cfs--read-profile))))
         (index (or (cl-position size cfs--english-fontsizes-steps-fallback) 1)))
    (unless (file-exists-p (cfs--get-current-profile))
      (message "如果中英文不能对齐，请运行`cfs-edit-profile'编辑当前profile。"))
    (or (nth index chinese-fontsizes-steps) 1)))

(defun cfs--get-fontset (fontname fontsize &optional type)
  "返回 fontname 对应的 fontset"
  (car (x-list-fonts (cfs--make-font-string fontname fontsize type)
                     nil nil 1)))

(defun cfs--set-font (english-fontsize chinese-fontsize)
  "english-fontsize could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-fontsize to nil, it will follow english-fontsize"
  (let* ((valid-fonts (cfs--get-valid-fonts))
         (english-main-fontset
          (cfs--get-fontset (nth 0 valid-fonts) english-fontsize))

         (english-bold-fontset
          (cfs--get-fontset (nth 0 valid-fonts) english-fontsize 'bold))

         (english-italic-fontset
          (cfs--get-fontset (nth 0 valid-fonts) english-fontsize 'italic))

         (english-bold-italic-fontset
          (cfs--get-fontset (nth 0 valid-fonts) english-fontsize 'bold-italic))

         (chinese-main-fontset
          (cfs--get-fontset (nth 1 valid-fonts) chinese-fontsize))

         (chinese-extra-fontset
          (cfs--get-fontset (nth 2 valid-fonts) chinese-fontsize)))

    (set-face-attribute 'default nil :font english-main-fontset)

    (if (and english-bold-fontset
             cfs-enable-bold)
        (set-face-font 'bold english-bold-fontset)
      (set-face-font 'bold english-main-fontset))

    (if (and english-italic-fontset
             cfs-enable-italic)
        (set-face-font 'italic english-italic-fontset)
      (set-face-font 'italic english-main-fontset))

    (if (and english-bold-italic-fontset
             cfs-enable-bold-italic)
        (set-face-font 'bold-italic english-bold-italic-fontset)
      (if (and english-bold-fontset
               cfs-enable-bold)
          (set-face-font 'bold-italic english-bold-fontset)
        (set-face-font 'bold-italic english-main-fontset)))

    ;; Set Chinese font and don't not use 'unicode charset,
    ;; it will cause the english font setting invalid.
    (dolist (charset '(kana han cjk-misc bopomofo gb18030))
      (set-fontset-font t charset chinese-main-fontset))

    ;; Set symbol fonts
    (set-fontset-font t 'symbol english-main-fontset)
    (set-fontset-font t 'symbol chinese-main-fontset nil 'prepend)

    ;; Set font of chars which is not covered above.
    (set-fontset-font t nil chinese-extra-fontset nil 'prepend)

    (setq cfs--minibuffer-echo-string
          (format "英文字体: %s, 字号: %.1f, 中文字体: %s, 字号：%.2f"
                  (nth 0 valid-fonts) english-fontsize (nth 1 valid-fonts) chinese-fontsize))))

(defun cfs--step-fontsize (step)
  (let* ((profile-name cfs--current-profile-name)
         (steps cfs--english-fontsizes-steps-fallback)
         (current-english-fontsize (cfs--read-english-fontsize profile-name))
         next-english-fontsize)
    (when (< step 0)
      (setq steps (reverse cfs--english-fontsizes-steps-fallback)))
    (setq next-english-fontsize
          (cadr (member current-english-fontsize steps)))
    (when next-english-fontsize
      (cfs--set-font next-english-fontsize (cfs--get-chinese-fontsize next-english-fontsize))
      (cfs--save-english-fontsize profile-name next-english-fontsize)
      (message cfs--minibuffer-echo-string))))

(defun cfs-set-font-with-saved-size ()
  (let* ((profile-name cfs--current-profile-name)
         (english-fontsize (cfs--read-english-fontsize profile-name))
         (chinese-fontsize (cfs--get-chinese-fontsize english-fontsize)))
    (when (display-graphic-p)
      (cfs--set-font english-fontsize chinese-fontsize))))

;; emacs启动的时候激活chinese-fonts-setup。
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (cfs-set-font-with-saved-size))))
  (add-hook 'window-setup-hook
            'cfs-set-font-with-saved-size))

(defun cfs-decrease-english-fontsize ()
  (interactive)
  (cfs--step-fontsize -1))

(defun cfs-increase-english-fontsize ()
  (interactive)
  (cfs--step-fontsize 1))

(defvar cfs-profile-edit-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'cfs-test-chinese-fontsize-at-point)
    (define-key keymap (kbd "C-<up>") 'cfs-increment-chinese-fontsize-at-point)
    (define-key keymap (kbd "C-<down>") 'cfs-decrement-chinese-fontsize-at-point)
    (define-key keymap (kbd "C-<right>") 'cfs-increment-chinese-fontsize-at-point)
    (define-key keymap (kbd "C-<left>") 'cfs-decrement-chinese-fontsize-at-point)
    keymap)
  "Keymap for `cfs-profile-edit-mode', a minor mode used to setup fonts names and chinese fontsize")

(define-minor-mode cfs-profile-edit-mode
  "Minor for setup fonts names and chinese fontsize"
  nil " Rem" cfs-profile-edit-mode-map)

(defun cfs--select-profile (profile-name)
  (if (member profile-name cfs-profiles)
      (progn (setq cfs--current-profile-name profile-name)
             (customize-save-variable 'cfs--current-profile-name profile-name)
             (cfs-set-font-with-saved-size))
    (message "%s doesn't exist." profile-name)))

(defun cfs-switch-profile ()
  (interactive)
  (let ((profile
         (ido-completing-read "Set chinese-fonts-setup profile to:" cfs-profiles)))
    (cfs--select-profile profile)))

(defun cfs-next-profile (&optional step)
  (interactive)
  (let ((profiles cfs-profiles)
        (current-profile cfs--current-profile-name)
        next-profile)
    (setq next-profile
          (or (cadr (member current-profile profiles))
              (car profiles)))
    (when next-profile
      (setq cfs--current-profile-name next-profile)
      (customize-save-variable 'cfs--current-profile-name next-profile))
    (when (display-graphic-p)
      (cfs-set-font-with-saved-size))
    (message "Current chinese-fonts-setup profile is set to: \"%s\"" next-profile)))

(defun cfs-edit-profile ()
  (interactive)
  (let ((file (cfs--get-current-profile)))
    (unless (file-readable-p file)
      (cfs--save-profile cfs--fontnames-fallback
                         cfs--chinese-fontsizes-steps-fallback))
    (find-file file)
    (cfs-profile-edit-mode 1)
    (goto-char (point-min))))

(defun cfs-regenerate-profile ()
  (interactive)
  (let ((profile-name
         (ido-completing-read "Regenerate profile: " cfs-profiles)))
    (if (yes-or-no-p (format "Regenerate (%s)? " profile-name))
        (cfs--save-profile cfs--fontnames-fallback
                           cfs--chinese-fontsizes-steps-fallback profile-name)
      (message "Ignore regenerate profile!"))))

(defun cfs-test-chinese-fontsize-at-point ()
  "Test chinese fontsizes list at point, which is usd to write chinese fontsizes list"
  (interactive)
  (let (chinese-fontsize chinese-fontsize index)
    (setq chinese-fontsize (sexp-at-point))
    (if (and chinese-fontsize (numberp chinese-fontsize))
        (progn
          (setq index
                (save-excursion
                  (let* ((point1 (point))
                         (point2 (progn (search-backward "(")
                                        (point))))
                    (length (split-string
                             (buffer-substring-no-properties point1 point2)
                             " ")))))
          (setq english-fontsize (nth (1- index) cfs--english-fontsizes-steps-fallback))
          (cfs--set-font english-fontsize chinese-fontsize)
          (cfs--show-font-effect english-fontsize chinese-fontsize))
      (cfs--set-font 14 15)
      (cfs--show-font-effect 14 15))))

(defun cfs-change-chinese-fontsize-at-point (step)
  (interactive)
  (skip-chars-backward "0123456789\\.")
  (or (looking-at "[0123456789.]+")
      (error "No number at point"))
  (replace-match
   (format "%.5s"
           (number-to-string
            (+ step (string-to-number (match-string 0))))))
  (backward-char 1)
  (cfs-test-chinese-fontsize-at-point))

(defun cfs-increment-chinese-fontsize-at-point ()
  (interactive)
  (cfs-change-chinese-fontsize-at-point 0.5))

(defun cfs-decrement-chinese-fontsize-at-point ()
  (interactive)
  (cfs-change-chinese-fontsize-at-point -0.5))

(defun cfs--show-font-effect (&optional english-fontsize chinese-fontsize)
  "show font and its size in a new buffer"
  (interactive)
  (let ((buffer-name "*Show-font-effect*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
        (org-mode))
      (setq truncate-lines 1)
      (insert (replace-regexp-in-string "^ *\n" "" cfs--test-string)))
    (when (and english-fontsize chinese-fontsize)
      (cfs--set-font english-fontsize chinese-fontsize))
    (message cfs--minibuffer-echo-string)))

(provide 'chinese-fonts-setup)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-fonts-setup.el ends here
