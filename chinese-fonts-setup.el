;;; chinese-fonts-setup.el --- A fonts config tool enforcing double-width Chinese character display

;; Copyright (c) 2011-2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-fonts-setup
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.6
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
;; 所有的profile文件都保存在 `cfs-profiles-directory' 对应的目录中。
;; 如果profile文件不存在， `chinese-fonts-setup' 将使用其自带的fallback
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
;; 1.  C-c C-c     `cfs-test-fontsizes-at-point'
;;                  查看字体显示效果
;; 2.  C-up        `cfs-increment-fontsize-at-point'
;;                  增大光标下字号的大小，同时显示对齐效果
;; 3.  C-down      `cfs-decrement-fontsize-at-point'
;;                  减小光标下字号的大小，同时显示对齐效果
;;
;; 配置完成后，有可能需要重启 Emacs。(参考： http://debbugs.gnu.org/db/17/1785.html)
;;
;; ### 调整字体大小 ###
;; `chinese-fonts-setup' 使用下述两个命令调整字体大小:
;;
;; 1.  `cfs-increase-fontsize' 增大字体大小
;; 2.  `cfs-decrease-fontsize' 减小字体大小
;;
;; 在调整字体大小的同时，字号信息也通过customize-save-variable函数保存到~/.emacs中了。
;;
;; ### Tips ###
;;
;; 1. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
;; 2. 运行 `(print (font-family-list))' 可以获得当前可用的字体的名称列表。
;; 3. Windows 用户 (特别是 Windows XP 用户) 可以安装 MacType 软件来优化
;;    字体显示效果，推荐使用。
;;
;; ### 参考文章 ###
;;
;; 1. http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; 2. http://zhuoqiang.me/torture-emacs.html

;;; Code:
(require 'cl-lib)
(require 'ido)
(require 'thingatpt)

(defcustom cfs-profiles '("profile1" "profile2" "profile3")
  "Lists chinese-fonts-setup profiles"
  :group 'chinese-fonts-setup
  :type 'list)

(defcustom cfs-profiles-directory (locate-user-emacs-file "chinese-fonts-setup/")
  "*Directory variable from which all other chinese-fonts-setup profiles are derived."
  :group 'chinese-fonts-setup
  :type 'directory)

(defcustom cfs-keep-frame-size t
  "在调整字体的时候，是否保持当前 frame 大小不变。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defcustom cfs-use-face-font-rescale (eq system-type 'gnu/linux)
  "是否通过设定 `face-font-rescale-alist' 来达到中英文对齐。

在 window 平台下，将这个变量设置为 t 会导致 chinese-fonts-setup
字体对齐预览功能失效，在 linux 平台下可以正常使用。"
  :group 'chinese-fonts-setup
  :type 'boolean)

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

(defvar cfs--fontsize-steps
  (mapcar #'(lambda (x) 4) cfs-profiles)
  "用来保存每一个 profile 使用 `cfs--fontsizes-fallback' 中第几个字号组合。")

(defconst cfs--fontsizes-fallback
  '((9     9.0   9.0)
    (10   10.5  10.5)
    (11.5 12.0  12.0)
    (12.5 13.5  13.5)
    (14   15.0  15.0)
    (16   16.5  16.5)
    (18   18.0  18.0)
    (20   21.0  21.0)
    (22   22.5  22.5))
  "一个列表，每一个元素都有类似结构：(英文字号 中文字号 EXT-B字体字号)")

(defconst cfs--fontnames-fallback
  '(("PragmataPro" "Monaco" "Consolas" "Menlof" "DejaVu Sans Mono"
     "Droid Sans Mono Pro" "Droid Sans Mono" "Inconsolata" "Source Code Pro"
     "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter"
     "monoOne" "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono"
     "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Courier New"
     "Courier" "Cousine" "Fira Mono" "Lekton" "Ubuntu Mono" "Liberation Mono"
     "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows"
     "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
    ("黑体" "文泉驿等宽微米黑" "微软雅黑" "Microsoft Yahei" "Microsoft_Yahei"
     "Hiragino Sans GB" "文泉驿等宽正黑" "文泉驿正黑" "文泉驿点阵正黑"
     "SimHei" "SimSun" "NSimSun" "FangSong" "KaiTi" "FangSong_GB2312" "KaiTi_GB2312"
     "LiSu" "YouYuan" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书"
     "STXihei" "STKaiti" "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti"
     "STCaiyun" "STHupo" "STLiti" "STXingkai" "STXinwei" "方正姚体" "方正舒体"
     "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文彩云" "华文新魏" "华文细黑" "华文行楷")
    ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB")))

(defconst cfs--test-string "
| 正常字体    | 粗体        | 粗斜体        |
|-------------+-------------+---------------|
| 堂堂正正    | *五大三粗*  | /东倒西歪/    |
| I'm normal. | *I'm bold!* | /I'm italic?/ |
| 𠄀𠄁𠄂𠄃    | *𠄄𠄅𠄆𠄇*  | /𠄈𠄉𠄊𠄋/    |
;; 请看上面表格线能否对齐, 如果没有对齐，请调整profile文件
;; 中变量 `cfs--custom-set-fontsizes' 列表各个数字的大小。
")

(defconst cfs--profile-comment-1 "
;;; `cfs--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *系统存在* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。")

(defconst cfs--profile-comment-2 "
;;; `cfs--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。")

(defvar cfs--minibuffer-echo-string nil)

(defun cfs--get-profile (profile-name)
  (let* ((cfs-profile-version "v3") ;; 升级 profile 格式时改变版本号
         (directory-name
          (expand-file-name
           (file-name-as-directory
            (concat (file-name-as-directory cfs-profiles-directory)
                    cfs-profile-version)))))
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
             (insert (concat "\n        ("
                             (mapconcat #'(lambda (x)
                                            (format "%-4S" x)) e  " ") ")")))
           (insert "\n       ))\n"))))

(defun cfs--save-fontsize-step (profile-name step)
  (let* ((profiles-names cfs-profiles)
         (profiles-fontsize-steps cfs--fontsize-steps)
         (length1 (length profiles-names))
         (length2 (length profiles-fontsize-steps))
         (index (cl-position profile-name cfs-profiles :test #'string=)))
    (if (= length1 length2)
        (setf (nth index profiles-fontsize-steps) step)
      (setq profiles-fontsize-steps
            (mapcar #'(lambda (x) 4) profiles-names)))
    (setq cfs--fontsize-steps profiles-fontsize-steps)
    (customize-save-variable 'cfs--fontsize-steps profiles-fontsize-steps)))

(defun cfs--read-fontsize-step (profile-name)
  (let ((index (cl-position profile-name cfs-profiles :test #'string=)))
    (nth index cfs--fontsize-steps)))

(defun cfs--save-profile (fontnames fontsizes &optional profile-name)
  "Save `fontnames' and `fontsizes' to current profile"
  (let ((variable-fontnames "cfs--custom-set-fontnames")
        (variable-fontsizes "cfs--custom-set-fontsizes"))
    (with-temp-buffer
      (erase-buffer)
      (insert (replace-regexp-in-string
               "^ *\n" ""
               cfs--profile-comment-1))
      (cfs--dump-variable variable-fontnames fontnames)
      (insert cfs--profile-comment-2)
      (cfs--dump-variable variable-fontsizes fontsizes)
      (write-file (cfs--get-profile
                   (or profile-name cfs--current-profile-name))))))

(defun cfs--read-profile ()
  "Get previously saved fontnames and fontsizes from current profile"
  (interactive)
  (let ((file (cfs--get-current-profile)))
    (if (file-readable-p file)
        (progn (load-file file)
               (list
                (if (boundp 'cfs--custom-set-fontnames)
                    cfs--custom-set-fontnames
                  cfs--fontnames-fallback)
                (if (boundp 'cfs--custom-set-fontsizes)
                    cfs--custom-set-fontsizes
                  cfs--fontsizes-fallback)))
      (list cfs--fontnames-fallback
            cfs--fontsizes-fallback))))

(defun cfs--font-exists-p (font)
  (if (null (x-list-fonts font))
      nil t))

(defun cfs--get-valid-fonts ()
  (mapcar #'(lambda (x)
              (cl-find-if #'cfs--font-exists-p x))
          (car (cfs--read-profile))))

(defun cfs--make-font-string (fontname fontsize &optional type)
  (if (and (stringp fontsize)
           (equal ":" (string (elt fontsize 0))))
      (format "%s%s" fontname fontsize)
    (if fontsize
        (cond
         ((eq type 'bold) (format "%s-%s:weight=bold:slant=normal" fontname fontsize))
         ((eq type 'italic) (format "%s-%s:weight=normal:slant=italic" fontname fontsize))
         ((eq type 'bold-italic) (format "%s-%s:weight=bold:slant=italic" fontname fontsize))
         (t (format "%s-%s:weight=normal:slant=normal" fontname fontsize)))
      (cond
       ((eq type 'bold) (format "%s:weight=bold:slant=normal" fontname))
       ((eq type 'italic) (format "%s:weight=normal:slant=italic" fontname))
       ((eq type 'bold-italic) (format "%s:weight=bold:slant=italic" fontname))
       (t (format "%s:weight=normal:slant=normal" fontname))))))

;; (cfs--get-fontset "courier" 10 'italic)

(defun cfs--get-fontsizes (&optional step)
  (let* ((fontsizes-list (car (cdr (cfs--read-profile)))))
    (unless (file-exists-p (cfs--get-current-profile))
      (message "如果中英文不能对齐，请运行`cfs-edit-profile'编辑当前profile。"))
    (or (nth (- step 1) fontsizes-list) 12.5)))

(defun cfs--get-fontset (fontname fontsize &optional type)
  "返回 fontname 对应的 fontset"
  (let ((font-xlfd (car (x-list-fonts (cfs--make-font-string fontname fontsize type)
                                      nil nil 1))))
    (when (and font-xlfd
               ;; 当字体名称中包含 "-" 时，`x-list-fonts'
               ;; 返回无效的 XLFD 字符串，具体细节请参考 emacs bug#17457 。
               ;; 忽略无效 XLFD 字符串。
               (x-decompose-font-name font-xlfd))
      font-xlfd)))

(defun cfs--set-font (fontsizes-list)
  "调整当前 frame 使用的字体，当全局变量 `cfs-keep-frame-size'
设置为 t 时，调整字体时保持当前 frame 大小不变。"
  (let ((frame (selected-frame))
        height width)

    (when cfs-use-face-font-rescale
      (cfs--set-face-font-rescale fontsizes-list)
      ;; 通过设定 `face-font-rescale-alist' 来实现中英文对齐时，
      ;; 只设定英文字体字号，中文等字体字号不设定。
      (setq fontsizes-list
            (list (car fontsizes-list))))

    (when (display-multi-font-p frame)
      (when cfs-keep-frame-size
        (setq height (* (frame-parameter frame 'height)
                        (frame-char-height frame))
              width  (* (frame-parameter frame 'width)
                        (frame-char-width frame))))
      (cfs--set-font-1 fontsizes-list)
      (when cfs-keep-frame-size
        (modify-frame-parameters
         frame
         (list (cons 'height (round height (frame-char-height frame)))
               (cons 'width  (round width  (frame-char-width frame)))))))))

(defun cfs--set-face-font-rescale (fontsizes-list)
  "设定 `face-font-rescale-alist' 系数。"
  (setq face-font-rescale-alist
        (cl-loop for font in (cfs--get-valid-fonts)
                 for size in fontsizes-list
                 collect (cons font (/ (float size)
                                       (car fontsizes-list))))))

(defun cfs--set-font-1 (fontsizes-list)
  "核心函数，用于设置字体，参数 `fontsizes-list' 是一个列表，其结构类似：

    (英文字体字号 中文字体字号 EXT-B字体字号 英文symbol字体字号 中文symbol字体字号)

其中，英文字体字号必须设定，其余字体字号可以设定，也可以省略。"
  (let* ((valid-fonts (cfs--get-valid-fonts))

         (english-main-fontsize (nth 0 fontsizes-list))
         (chinese-main-fontsize (nth 1 fontsizes-list))
         (chinese-extra-fontsize (nth 2 fontsizes-list))

         (english-symbol-fontsize (nth 3 fontsizes-list))
         (chinese-symbol-fontsize (nth 4 fontsizes-list))

         (english-main-fontset
          (cfs--get-fontset (nth 0 valid-fonts)
                            english-main-fontsize))
         (english-bold-fontset
          (cfs--get-fontset (nth 0 valid-fonts)
                            english-main-fontsize 'bold))
         (english-italic-fontset
          (cfs--get-fontset (nth 0 valid-fonts)
                            english-main-fontsize 'italic))

         (english-bold-italic-fontset
          (cfs--get-fontset (nth 0 valid-fonts)
                            english-main-fontsize 'bold-italic))

         (english-symbol-fontset
          (cfs--get-fontset (nth 0 valid-fonts)
                            (or english-symbol-fontsize
                                english-main-fontsize)))
         (chinese-main-fontset
          (cfs--get-fontset (nth 1 valid-fonts)
                            chinese-main-fontsize))

         (chinese-symbol-fontset
          (cfs--get-fontset (nth 1 valid-fonts)
                            (or chinese-symbol-fontsize
                                chinese-main-fontsize)))
         (chinese-extra-fontset
          (cfs--get-fontset (nth 2 valid-fonts)
                            (or chinese-extra-fontsize
                                chinese-main-fontsize))))

    (when english-main-fontset
      ;; 设置英文字体。
      (set-face-attribute 'default nil
                          :font english-main-fontset)
      ;; 设置英文粗体。
      (if (and english-bold-fontset
               cfs-enable-bold)
          (set-face-font 'bold english-bold-fontset)
        (set-face-font 'bold english-main-fontset))

      ;; 设置英文斜体。
      (if (and english-italic-fontset
               cfs-enable-italic)
          (set-face-font 'italic english-italic-fontset)
        (set-face-font 'italic english-main-fontset))

      ;; 设置英文粗斜体。
      (if (and english-bold-italic-fontset
               cfs-enable-bold-italic)
          (set-face-font 'bold-italic english-bold-italic-fontset)
        (if (and english-bold-fontset
                 cfs-enable-bold)
            (set-face-font 'bold-italic english-bold-fontset)
          (set-face-font 'bold-italic english-main-fontset))))

    ;; 设置中文字体，注意，不要使用 'unicode charset,
    ;; 否则上面的英文字体设置将会失效。
    (when chinese-main-fontset
      (dolist (charset '(kana han cjk-misc bopomofo gb18030))
        (set-fontset-font t charset chinese-main-fontset)))

    ;; 设置 symbol 字体。
    (when english-main-fontset
      (set-fontset-font t 'symbol english-symbol-fontset))

    (when chinese-main-fontset
      (set-fontset-font t 'symbol chinese-symbol-fontset nil 'prepend))

    ;; 设置 fallback 字体，用于显示不常用的字符。
    (when chinese-extra-fontset
      (set-fontset-font t nil chinese-extra-fontset nil 'prepend))

    (setq cfs--minibuffer-echo-string
          (format "英文字体: %s %.1f，中文字体: %s"
                  (nth 0 valid-fonts) english-main-fontsize (nth 1 valid-fonts)))))

(defun cfs--step-fontsize (num)
  (let* ((profile-name cfs--current-profile-name)
         (current-step
          (max 1 (min (+ num (cfs--read-fontsize-step profile-name))
                      (length cfs--fontsizes-fallback))))
         (fontsizes-list (cfs--get-fontsizes current-step)))
    (cfs--set-font fontsizes-list)
    (cfs--save-fontsize-step profile-name current-step)
    (message cfs--minibuffer-echo-string)))

(defun cfs-set-font-with-saved-step ()
  (let* ((profile-name cfs--current-profile-name)
         (current-step (cfs--read-fontsize-step profile-name))
         (fontsizes-list (cfs--get-fontsizes current-step)))
    (when (display-graphic-p)
      (cfs--set-font fontsizes-list))))

;; emacs启动的时候激活chinese-fonts-setup。
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              #'(lambda (frame)
                  (with-selected-frame frame
                    (cfs-set-font-with-saved-step))))
  (add-hook 'window-setup-hook
            'cfs-set-font-with-saved-step))

(defun cfs-decrease-fontsize ()
  (interactive)
  (cfs--step-fontsize -1))

(defun cfs-increase-fontsize ()
  (interactive)
  (cfs--step-fontsize 1))

(defvar cfs-profile-edit-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'cfs-test-fontsize-at-point)
    (define-key keymap (kbd "C-<up>") 'cfs-increment-fontsize-at-point)
    (define-key keymap (kbd "C-<down>") 'cfs-decrement-fontsize-at-point)
    (define-key keymap (kbd "C-<right>") 'cfs-increment-fontsize-at-point)
    (define-key keymap (kbd "C-<left>") 'cfs-decrement-fontsize-at-point)
    keymap)
  "Keymap for `cfs-profile-edit-mode', a minor mode used to setup fonts names and sizes")

(define-minor-mode cfs-profile-edit-mode
  "Minor for setup fonts names and sizes"
  nil " Rem" cfs-profile-edit-mode-map)

(defun cfs--select-profile (profile-name)
  (if (member profile-name cfs-profiles)
      (progn (setq cfs--current-profile-name profile-name)
             (customize-save-variable 'cfs--current-profile-name profile-name)
             (cfs-set-font-with-saved-step))
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
      (cfs-set-font-with-saved-step))
    (message "Current chinese-fonts-setup profile is set to: \"%s\"" next-profile)))

(defun cfs-edit-profile ()
  (interactive)
  (let ((file (cfs--get-current-profile)))
    (unless (file-readable-p file)
      (cfs--save-profile cfs--fontnames-fallback
                         cfs--fontsizes-fallback))
    (find-file file)
    (cfs-profile-edit-mode 1)
    (goto-char (point-min))))

(defun cfs-regenerate-profile ()
  (interactive)
  (let ((profile-name
         (ido-completing-read "Regenerate profile: " cfs-profiles)))
    (if (yes-or-no-p (format "Regenerate (%s)? " profile-name))
        (cfs--save-profile cfs--fontnames-fallback
                           cfs--fontsizes-fallback profile-name)
      (message "Ignore regenerate profile!"))))

(defun cfs-test-fontsize-at-point ()
  "Test fontsizes list at point, which is usd to edit fontsizes list"
  (interactive)
  (let* ((fontsizes-list (form-at-point 'list)))
    (if (numberp (car fontsizes-list))
        (progn
          (cfs--set-font fontsizes-list)
          (cfs--show-font-effect fontsizes-list))
      (cfs--set-font '(14 15 15))
      (cfs--show-font-effect '(14 15 15)))))

(defun cfs-change-fontsize-at-point (step)
  (interactive)
  (skip-chars-backward "0123456789\\.")
  (or (looking-at "[0123456789.]+")
      (error "No number at point"))
  (replace-match
   (format "%.5s"
           (number-to-string
            (min 30 (max 5 (+ step (string-to-number (match-string 0))))))))
  (backward-char 1)
  (cfs-test-fontsize-at-point))

(defun cfs-increment-fontsize-at-point ()
  (interactive)
  (cfs-change-fontsize-at-point 0.5))

(defun cfs-decrement-fontsize-at-point ()
  (interactive)
  (cfs-change-fontsize-at-point -0.5))

(defun cfs--show-font-effect (&optional fontsizes-list)
  "show font and its size in a new buffer"
  (interactive)
  (let ((buffer-name "*Show-font-effect*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
        (org-mode))
      (setq truncate-lines 1)
      (insert (replace-regexp-in-string "^ *\n" "" cfs--test-string)))
    (when (and (nth 0 fontsizes-list)
               (nth 1 fontsizes-list))
      (cfs--set-font fontsizes-list))
    (message cfs--minibuffer-echo-string)))

(provide 'chinese-fonts-setup)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-fonts-setup.el ends here
