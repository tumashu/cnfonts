;;; chinese-fonts-setup.el --- Emacs fonts config tool enforcing double-width Chinese character display

;; * Header
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

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * Chinese-fonts-setup README                                         :README:

;; ** 简介
;; Chinese-fonts-setup 是一个 emacs 中英文字体配置工具。可以比较方便地
;; 实现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。

;; 注： 这个 package 特别适用于需要处理中英文混合表格的中文 org-mode 用户。
;; ** 基本原理
;; Chinese-fonts-setup 的核心很简单，就是让中文字体和英文字体使用不同的字号，
;; 从而实现中英文对齐，它和下面的样例代码原理是一样的：

;; #+BEGIN_EXAMPLE
;; (set-frame-font "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1")
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "Microsoft Yahei" :size 16)))
;; #+END_EXAMPLE

;; ** 使用特点
;; Chinese-fonts-setup 添加了许多辅助工具，使配置和调节字体和字号的工作更加简便快捷，
;; 它有几个优点：

;; 1. 安装即用：Chinese-fonts-setup 内置字体 fallback 功能，只需安装，就能够配置中文字体和英文字体，
;;    让中文可以 *正确* 显示（但未必完美），不会因为 emacs 配置中指定的字体不存在而报错。
;; 2. 设置方便：Chinese-fonts-setup 自带一个 profile 文件编辑工具，可以让用户动态调整字体名称和字体
;;    大小，分分钟实现中文字体和英文字体的等宽对齐。

;; ** 下载安装
;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET chinese-fonts-setup RET
;; 3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：

;; #+BEGIN_EXAMPLE
;; (require 'chinese-fonts-setup)
;; #+END_EXAMPLE

;; ** 配置使用
;; *** 编辑使用 profile
;; 一个 profile 代表了一套字体配置，chinese-fonts-setup 使用 profile 的概念，
;; 来维护多套字体配置，从而实现特定的环境使用特定的字体配置，
;; 比如：在编程时使用 “Consolas + 微米黑”，在阅读文章时使用 “PragmataPro + 黑体”，等等。

;; 调整 profile 最简单的方式是使用 chinese-fonts-setup 自带的 profile 调整工具，用户只
;; 需要在运行 `cfs-edit-profile' 时，选择 “使用图形化工具来调整当前 profile 文件” 就可以了，
;; 其界面类似：

;; [[./snapshots/cfs-ui-1.png]]
;; [[./snapshots/cfs-ui-2.png]]
;; [[./snapshots/cfs-ui-3.png]]
;; [[./snapshots/cfs-ui-4.png]]

;; 除了用上述图形化工具，用户也可以选择直接编辑 profile 文件，两种方式的效果是一样的。

;; 在 `cfs-profiles-directory' 目录中, 每一个 profile 都对应一个 emacs-lisp 文件,
;; 这些文件包含了英文字体设置，中文字体设置以及中文字体大小，其内容类似：

;; #+BEGIN_EXAMPLE
;;; `cfs--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
;;; `cfs-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。
;; (setq cfs--custom-set-fontnames
;;       '(
;;         ("PragmataPro" "Monaco" "Consolas" "DejaVu Sans Mono" "Droid Sans Mono" "Courier" "Courier New" "Liberation Mono" "Ubuntu Mono" "Droid Sans Mono Pro" "Inconsolata" "Source Code Pro" "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Hack" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
;;         ("文泉驿等宽微米黑" "Noto Sans S Chinese Regular" "微软雅黑" "Microsoft Yahei" "Microsoft_Yahei" "文泉驿等宽微米黑" "文泉驿等宽正黑" "黑体" "Hiragino Sans GB" "文泉驿正黑" "文泉驿点阵正黑" "SimHei" "SimSun" "NSimSun" "FangSong" "KaiTi" "FangSong_GB2312" "KaiTi_GB2312" "LiSu" "YouYuan" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "STXihei" "STKaiti" "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti" "STXingkai" "STXinwei" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文彩云" "华文新魏" "华文细黑" "华文行楷")
;;         ("HanaMinB")
;;         ))

;;; `cfs--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。
;; (setq cfs--custom-set-fontsizes
;;       '(
;;         (9    9.0 11.0)
;;         (10   10.5 12.5)
;;         (11.5 12.0 14.0)
;;         (12.5 13.5 15.0)
;;         (14   15.0 16.5)
;;         (16   16.5 20.0)
;;         (18   18.0 21.0)
;;         (20   21.0 24.0)
;;         (22   22.5 26.0)
;;         ))
;; #+END_EXAMPLE

;; Chinese-fonts-setup 默认使用三个 profile: profile1, profile2 和 profile3,
;; 如果想使用其它有意义的名称，可以设置:

;; #+BEGIN_EXAMPLE
;; (setq cfs-profiles
;;     '("program" "org-mode" "read-book"))
;; #+END_EXAMPLE

;; 用户可以使用下面两个命令快速切换 profile：

;; | Command            | Help                    |
;; |--------------------+-------------------------|
;; | cfs-switch-profile | 选择并切换 profile      |
;; | cfs-next-profile   | 直接切换到下一个profile |

;; 如果用户觉得 *当前使用* 的 profile 不符合个人使用习惯，可以使用 `cfs-edit-profile'
;; 命令来编辑当前 profile 文件（如果 profile 文件不存在，chinese-fonts-setup
;; 会在编辑之前自动新建一个, *不需要用户手动创建 profile 文件* ），用户可以使用命令：
;; `cfs-regenerate-profile' 强制覆盖一个 *已经存在* 的 profile 文件！

;; `cfs-edit-profile' 命令会打开当前 profile 文件，并激活内置的 profile 编辑模式，
;; 在编辑的过程中，用户可以使用下面三个命令 *快速* 的了解测试编辑效果：

;; | Key     | Command                         | Help                                   |
;; |---------+---------------------------------+----------------------------------------|
;; | C-c C-c | cfs-test-fontsizes-at-point     | 查看字体显示效果                       |
;; | C-up    | cfs-increment-fontsize-at-point | 增大光标下字号的大小，同时显示对齐效果 |
;; | C-down  | cfs-decrement-fontsize-at-point | 减小光标下字号的大小，同时显示对齐效果 |

;; 配置完成后，有可能需要重启 Emacs。(参考： http://debbugs.gnu.org/db/17/1785.html)

;; [[./snapshots/cfs-edit-fontnames.gif]]

;; [[./snapshots/cfs-edit-fontsizes.gif]]

;; *** 调整字体大小
;; `chinese-fonts-setup' 使用下述两个命令调整字体大小:

;; | Command               | Help         |
;; |-----------------------+--------------|
;; | cfs-increase-fontsize | 增大字体大小 |
;; | cfs-decrease-fontsize | 减小字体大小 |

;; 注意：在调整字体大小的同时，字号信息也会保存 ~/.emacs 中。

;; [[./snapshots/cfs-increase-and-decrease-fontsize.gif]]

;; ** Tips

;; 1. 如果用户需要在自己的 emacs 配置中管理一些个人字体，可以使用
;;    变量 `cfs-personal-fontnames' , 其结构与 `cfs--fontnames-fallback'
;;    一样。
;; 2. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
;; 3. 在 scratch 中写一行 elisp 代码： (cl-prettyprint (font-family-list)),
;;    执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很有用的技巧。
;; 4. 命令：`cfs-insert-fontname', 可以让用户选择一个可用字体插入到当前光标处。
;; 5. Windows 用户 (特别是 Windows XP 用户) 可以安装 MacType 软件来优化
;;    字体显示效果，推荐使用。
;; 5. Mac 用户配置 profile 文件的时候，偶尔会遇到 'C-c C-c' 刷新缓慢的问题，这可能
;;    是 ext-b 字体缺失引起的，建议安装 ext-b 字体试试。
;;    1. Ext-B字符列表: https://cdo.wikipedia.org/wiki/Wikipedia:Unicode%E6%93%B4%E5%B1%95%E6%BC%A2%E5%AD%97
;;    2. HanaMinB 下载地址: https://osdn.jp/projects/hanazono-font/downloads/62072/hanazono-20141012.zip/
;; 6. chinese-fonts-setup 默认会随着 emacs 启动，出现问题时，用户可以检查下面两个 hook 的取值：

;;    1. `after-make-frame-functions'
;;    2. `window-setup-hook'

;;    如果两个 hook *都不包含* `cfs-set-font-with-saved-step', 说明有其它 emacs 包
;;    覆盖了 chinese-fonts-setup 的设置，另外，用户也可以手动运行 `cfs-set-font-with-saved-step'
;;    这个命令来测试字体效果。


;; ** 参考文章
;; 1. http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; 2. http://zhuoqiang.me/torture-emacs.html

;;; Code:

;; * 代码                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'cl-lib)
(require 'thingatpt)
(require 'cfs-ui)

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

(defcustom cfs-disable-bold nil
  "是否禁用英文粗体。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defcustom cfs-disable-italic nil
  "是否禁用英文斜体。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defcustom cfs-disable-bold-italic nil
  "是否禁用英文粗斜体。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defcustom cfs-use-face-font-rescale (eq system-type 'gnu/linux)
  "是否通过设定 `face-font-rescale-alist' 来达到中英文对齐。

在 window 平台下，将这个变量设置为 t 会导致 chinese-fonts-setup
字体对齐预览功能失效，在 linux 平台下可以正常使用。"
  :group 'chinese-fonts-setup
  :type 'boolean)

(defcustom cfs-set-font-finish-hook nil
  "A hook, by which user can set additional fonts.
The below is an example which is used to set symbol fonts:

(defun cfs-set-symbol-fonts (fontsizes-list)
  (set-fontset-font t 'symbol \"Inconsolata\" nil 'append)
  (set-fontset-font t 'symbol \"Symbola\" nil 'append)
  (set-fontset-font t 'unicode \"Segoe UI Emoji\" nil 'append)
  (set-fontset-font t 'unicode \"STIX\" nil 'append))
(add-hook 'cfs-set-font-finish-hook 'cfs-set-symbol-fonts)"
  :group 'chinese-fonts-setup
  :type 'hook)

(defvar cfs--current-profile nil
  "Current profile name used by chinese-fonts-setup")

(defvar cfs--profiles-steps nil
  "用来保存每一个 profile 使用 `cfs--fontsizes-fallback' 中第几个字号组合。")

(defconst cfs--fontsizes-fallback
  '((9    10.5 10.5)
    (10   12.0 12.0)
    (11.5 13.5 13.5)
    (12.5 15.0 15.0)
    (14   16.5 16.5)
    (16   19.5 19.5)
    (18   21.0 21.0)
    (20   24.0 24.0)
    (22   25.5 25.5)
    (24   28.5 28.5)
    (26   31.5 31.5)
    (28   33.0 33.0)
    (30   36.0 36.0)
    (32   39.0 39.0))
  "一个列表，每一个元素都有类似结构：(英文字号 中文字号 EXT-B字体字号)")

(defcustom cfs-personal-fontnames nil
  "用户自己维护的字体列表，其结构与 `cfs--fontnames-fallback' 一致。"
  :group 'chinese-fonts-setup)

(defconst cfs--fontnames-fallback
  '(("Monaco" "Consolas" "DejaVu Sans Mono" "Droid Sans Mono" "PragmataPro"
     "Courier" "Courier New" "Ubuntu Mono" "Liberation Mono" "MonacoB" "MonacoB2"
     "MonacoBSemi" "Droid Sans Mono Pro" "Inconsolata" "Source Code Pro" "Lucida Console"
     "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter"
     "Panic Sans" "Hack" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc"
     "Excalibur Monospace" "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono"
     "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002"
     "ProggyCleanTT" "ProggyTinyTT")
    ("微软雅黑" "Noto Sans S Chinese Regular" "Microsoft Yahei" "Microsoft_Yahei"
     "文泉驿等宽微米黑" "文泉驿等宽正黑" "黑体" "Hiragino Sans GB"  "文泉驿正黑" "文泉驿点阵正黑"
     "SimHei" "SimSun" "NSimSun" "FangSong" "KaiTi" "FangSong_GB2312" "KaiTi_GB2312" "LiSu"
     "YouYuan" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "STXihei" "STKaiti"
     "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti"
     "STXingkai" "STXinwei" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋"
     "华文彩云" "华文新魏" "华文细黑" "华文行楷")
    ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB")))

(defconst cfs--test-string "
| 如果此表格无法对齐，请调整下面变量中的数字 |
|        `cfs--custom-set-fontsizes'         |
| 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄇 |
")

(defconst cfs--profile-comment-1 "
;;; `cfs--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
;;; `cfs-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。")

(defconst cfs--profile-comment-2 "
;;; `cfs--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。")

(defvar cfs--minibuffer-echo-string nil)

(defvar cfs--custom-set-fontnames nil
  "这个变量是一个 *专用* 变量，只用与 chinese-fonts-setup 的 profile 文件，
这些 profile 文件保存在 `cfs-profiles-directory' 对应的目录中。在其它地方
设置这个变量没有任何用处！")

(defvar cfs--custom-set-fontsizes nil
  "这个变量是一个 *专用* 变量，只用与 chinese-fonts-setup 的 profile 文件，
这些 profile 文件保存在 `cfs-profiles-directory' 对应的目录中。在其它地方
设置这个变量没有任何用处！")

(defun cfs--get-profile (profile-name)
  (let* ((cfs-profile-version "v4") ;; 升级 profile 格式时改变版本号
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

(defun cfs--get-current-profile (&optional return-profile-name)
  (let ((profile-name
         (if (member cfs--current-profile cfs-profiles)
             cfs--current-profile
           (car cfs-profiles))))
    (if return-profile-name
        profile-name
      (cfs--get-profile profile-name))))

(defun cfs--dump-variable (variable value)
  "Insert a \"(setq VARIABLE value)\" in the current buffer."
  (cond ((atom value)
         (insert (format "\n(setq %S %S)\n" variable value)))
        ((atom (car value))
         (insert (format "\n(setq %S\n      '%S)\n" variable value)))
        (t (insert (format "\n(setq %S\n      '(" variable))
           (dolist (e value)
             (insert (concat "\n        ("
                             (mapconcat #'(lambda (x)
                                            (format "%-4S" x)) e  " ") ")")))
           (insert "\n        ))\n"))))

(defun cfs--save-profile-step (profile-name step)
  (if (assoc profile-name cfs--profiles-steps)
      (setf (cdr (assoc profile-name cfs--profiles-steps)) step)
    (push `(,profile-name . ,step) cfs--profiles-steps))
  (customize-save-variable 'cfs--current-profile profile-name)
  (customize-save-variable 'cfs--profiles-steps cfs--profiles-steps))

(defun cfs--get-profile-step (profile-name)
  (or (cdr (assoc profile-name cfs--profiles-steps)) 4))

(defun cfs--save-profile (fontnames fontsizes &optional profile-name)
  "Save `fontnames' and `fontsizes' to current profile"
  (with-temp-buffer
    (erase-buffer)
    (insert (replace-regexp-in-string
             "^ *\n" ""
             cfs--profile-comment-1))
    (cfs--dump-variable 'cfs--custom-set-fontnames fontnames)
    (insert cfs--profile-comment-2)
    (cfs--dump-variable 'cfs--custom-set-fontsizes fontsizes)
    (write-file (cfs--get-profile
                 (or profile-name (cfs--get-current-profile t))))))

(defun cfs--read-profile ()
  "Get previously saved fontnames and fontsizes from current profile"
  (interactive)
  (let ((file (cfs--get-current-profile)))
    (if (file-readable-p file)
        (progn (when (load (expand-file-name file) nil t)
                 (message "Chinese-fonts-setup: load %S successfully." (cfs--get-current-profile t)))
               (list
                (if cfs--custom-set-fontnames
                    (cfs--merge-fontname-list cfs--custom-set-fontnames
                                              cfs-personal-fontnames
                                              cfs--fontnames-fallback)
                  (cfs--merge-fontname-list cfs-personal-fontnames
                                            cfs--fontnames-fallback))
                (or cfs--custom-set-fontsizes
                    cfs--fontsizes-fallback)))
      (list cfs--fontnames-fallback
            cfs--fontsizes-fallback))))

(defun cfs--merge-fontname-list (list1 list2 &optional list3)
  (mapcar #'delete-dups
          `((,@(nth 0 list1) ,@(nth 0 list2) ,@(nth 0 list3))
            (,@(nth 1 list1) ,@(nth 1 list2) ,@(nth 1 list3))
            (,@(nth 2 list1) ,@(nth 2 list2) ,@(nth 2 list3)))))

(defun cfs--font-exists-p (font)
  (or (cfs--get-xlfd font)
      (let ((all-fonts (font-family-list)))
        (or (member font all-fonts)
            (member (encode-coding-string font 'gbk) all-fonts)
            (member (encode-coding-string font 'utf-8) all-fonts)))))

(defun cfs--get-valid-fonts (&optional prefer-shortname)
  (mapcar #'(lambda (x)
              (let ((font (cl-find-if #'cfs--font-exists-p x)))
                (when font
                  (if prefer-shortname
                      font
                    (or (cfs--get-xlfd font) font)))))
          (car (cfs--read-profile))))

(defun cfs--get-xlfd (fontname)
  "返回 fontname 对应的 fontset"
  (when fontname
    (let* ((font-xlfd
            (car (or (x-list-fonts fontname nil nil 1)
                     (x-list-fonts (encode-coding-string fontname 'gbk) nil nil 1)
                     (x-list-fonts (encode-coding-string fontname 'utf-8) nil nil 1)))))
      (when (and font-xlfd
                 ;; 当字体名称中包含 "-" 时，`x-list-fonts'
                 ;; 返回无效的 XLFD 字符串，具体细节请参考 emacs bug#17457 。
                 ;; 忽略无效 XLFD 字符串。
                 (x-decompose-font-name font-xlfd))
        font-xlfd))))

;; (cfs--get-fontset "courier" 10 'italic)

(defun cfs--get-fontsizes (&optional step)
  (let* ((fontsizes-list (car (cdr (cfs--read-profile)))))
    (unless (file-exists-p (cfs--get-current-profile))
      (message "如果中英文不能对齐，请运行`cfs-edit-profile'编辑当前profile。"))
    (or (nth (- step 1) fontsizes-list) 12.5)))

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
      (run-hook-with-args 'cfs-set-font-finish-hook fontsizes-list)
      (when cfs-keep-frame-size
        (modify-frame-parameters
         frame
         (list (cons 'height (round height (frame-char-height frame)))
               (cons 'width  (round width  (frame-char-width frame)))))))))

(defun cfs--set-face-font-rescale (fontsizes-list)
  "设定 `face-font-rescale-alist' 系数。"
  (setq face-font-rescale-alist
        (cl-loop for font in (cfs--get-valid-fonts t)
                 for size in fontsizes-list
                 collect (cons font (/ (float size)
                                       (car fontsizes-list))))))

(defun cfs--fontspec-valid-p (fontspec)
  (and fontspec (list-fonts fontspec)))

(defun cfs--float (num)
  (when (numberp num)
    (float num)))

(defun cfs--set-font-1 (fontsizes-list)
  "核心函数，用于设置字体，参数 `fontsizes-list' 是一个列表，其结构类似：

    (英文字体字号 中文字体字号 EXT-B字体字号 英文symbol字体字号 中文symbol字体字号)

其中，英文字体字号必须设定，其余字体字号可以设定，也可以省略。"
  (let* ((valid-fonts (cfs--get-valid-fonts))
         (valid-fontnames (cfs--get-valid-fonts t))

         (english-main-fontname (nth 0 valid-fonts))
         (chinese-main-fontname (nth 1 valid-fonts))
         (chinese-extra-fontname (nth 2 valid-fonts))

         (english-main-fontsize (cfs--float (nth 0 fontsizes-list)))
         (chinese-main-fontsize (cfs--float (nth 1 fontsizes-list)))
         (chinese-extra-fontsize (cfs--float (nth 2 fontsizes-list)))

         (english-symbol-fontsize (cfs--float (nth 0 fontsizes-list)))
         (chinese-symbol-fontsize (cfs--float (nth 1 fontsizes-list)))

         (english-main-fontspec
          (when english-main-fontname
            (font-spec :name english-main-fontname
                       :size english-main-fontsize
                       :weight 'normal
                       :slant 'normal)))
         (english-bold-fontspec
          (when english-main-fontname
            (font-spec :name english-main-fontname
                       :size english-main-fontsize
                       :weight 'bold
                       :slant 'normal)))
         (english-italic-fontspec
          (when english-main-fontname
            (font-spec :name  english-main-fontname
                       :size english-main-fontsize
                       :weight 'normal
                       :slant 'italic)))
         (english-bold-italic-fontspec
          (when english-main-fontname
            (font-spec :name english-main-fontname
                       :size english-main-fontsize
                       :weight 'bold
                       :slant 'italic)))
         (english-symbol-fontspec
          (when english-main-fontname
            (font-spec :name english-main-fontname
                       :size (or english-symbol-fontsize
                                 english-main-fontsize)
                       :weight 'normal
                       :slant 'normal)))
         (chinese-main-fontspec
          (when chinese-main-fontname
            (font-spec :name chinese-main-fontname
                       :size chinese-main-fontsize
                       :weight 'normal
                       :slant 'normal)))
         (chinese-symbol-fontspec
          (when chinese-main-fontname
            (font-spec :name chinese-main-fontname
                       :size (or chinese-symbol-fontsize
                                 chinese-main-fontsize)
                       :weight 'normal
                       :slant 'normal)))
         (chinese-extra-fontspec
          (when chinese-extra-fontname
            (font-spec :name chinese-extra-fontname
                       :size (or chinese-extra-fontsize
                                 chinese-main-fontsize)
                       :weight 'normal
                       :slant 'normal))))

    (when (cfs--fontspec-valid-p english-main-fontspec)
      ;; 设置英文字体。
      (set-face-attribute
       'default nil :font english-main-fontspec)
      ;; 设置英文粗体。
      (if cfs-disable-bold
          (set-face-font 'bold english-main-fontspec)
        (if (cfs--fontspec-valid-p english-bold-fontspec)
            (set-face-font 'bold english-bold-fontspec)
          (message "Chinese-fonts-setup: 字体 %S 对应的粗体没有找到，不作处理！" english-main-fontname)))

      ;; 设置英文斜体。
      (if cfs-disable-italic
          (set-face-font 'italic english-main-fontspec)
        (if (cfs--fontspec-valid-p english-italic-fontspec)
            (set-face-font 'italic english-italic-fontspec)
          (message "Chinese-fonts-setup: 字体 %S 对应的斜体没有找到，不作处理！" english-main-fontname)))

      ;; 设置英文粗斜体。
      (if cfs-disable-bold-italic
          (set-face-font 'bold-italic english-main-fontspec)
        (if (cfs--fontspec-valid-p english-bold-italic-fontspec)
            (set-face-font 'bold-italic english-bold-italic-fontspec)
          (message "Chinese-fonts-setup: 字体 %S 对应的粗斜体没有找到，不作处理！" english-main-fontname))))

    ;; 设置中文字体，注意，不要使用 'unicode charset,
    ;; 否则上面的英文字体设置将会失效。
    (when (cfs--fontspec-valid-p chinese-main-fontspec)
      (dolist (charset '(kana han cjk-misc bopomofo gb18030))
        (set-fontset-font "fontset-default" charset chinese-main-fontspec)))

    ;; 设置 symbol 字体。
    (when (cfs--fontspec-valid-p english-main-fontspec)
      (set-fontset-font "fontset-default" 'symbol english-symbol-fontspec))

    ;; (when (cfs--fontspec-valid-p chinese-main-fontset)
    ;;   (set-fontset-font t 'symbol chinese-symbol-fontspec nil 'append))

    ;; 设置 fallback 字体，用于显示不常用的字符。
    (when (cfs--fontspec-valid-p chinese-extra-fontspec)
      (set-fontset-font "fontset-default" nil chinese-extra-fontspec nil 'prepend))

    (setq cfs--minibuffer-echo-string
          (format "[%s]: 英文字体: %s-%.1f，中文字体: %s, EXTB字体：%s"
                  (cfs--get-current-profile t)
                  (or (nth 0 valid-fontnames) "无") english-main-fontsize
                  (or (nth 1 valid-fontnames) "无")
                  (or (nth 2 valid-fontnames) "无")))))

(defun cfs--step-fontsize (num)
  (let* ((profile-name (cfs--get-current-profile t))
         (profile-step
          (max 1 (min (+ num (cfs--get-profile-step profile-name))
                      (length cfs--fontsizes-fallback))))
         (fontsizes-list (cfs--get-fontsizes profile-step)))
    (cfs--set-font fontsizes-list)
    (cfs--save-profile-step profile-name profile-step)
    (message cfs--minibuffer-echo-string)))

(defun cfs-set-font-with-saved-step (&optional frame)
  (interactive)
  (let* ((profile-name (cfs--get-current-profile t))
         (profile-step (cfs--get-profile-step profile-name))
         (fontsizes-list (cfs--get-fontsizes profile-step)))
    (when (display-graphic-p)
      (if frame
          (with-selected-frame frame
            (cfs--set-font fontsizes-list))
        (cfs--set-font fontsizes-list)))))

(defun chinese-fonts-setup-enable ()
  "运行这个函数，可以让 emacs 启动的时候就激活 chinese-fonts-setup."
  (interactive)
  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions #'cfs-set-font-with-saved-step)
    (add-hook 'window-setup-hook #'cfs-set-font-with-saved-step)))

(defun chinese-fonts-setup-disable ()
  "清除与 chinese-fonts-setup 相关的 hook 设定。"
  (interactive)
  (remove-hook 'after-make-frame-functions #'cfs-set-font-with-saved-step)
  (remove-hook 'window-setup-hook #'cfs-set-font-with-saved-step))

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
      (progn (setq cfs--current-profile profile-name)
             (customize-save-variable 'cfs--current-profile profile-name)
             (cfs-set-font-with-saved-step))
    (message "%s doesn't exist." profile-name)))

(defun cfs-switch-profile ()
  (interactive)
  (let ((profile (completing-read "Set chinese-fonts-setup profile to:" cfs-profiles)))
    (cfs--select-profile profile)))

(defun cfs-next-profile (&optional step)
  (interactive)
  (let ((profiles cfs-profiles)
        (current-profile (cfs--get-current-profile t))
        next-profile)
    (setq next-profile
          (or (cadr (member current-profile profiles))
              (car profiles)))
    (when next-profile
      (setq cfs--current-profile next-profile)
      (customize-save-variable 'cfs--current-profile next-profile))
    (when (display-graphic-p)
      (cfs-set-font-with-saved-step))
    (message "Current chinese-fonts-setup profile is set to: \"%s\"" next-profile)))

(defun cfs-edit-profile ()
  (interactive)
  (let ((file (cfs--get-current-profile)))
    (unless (file-readable-p file)
      (cfs--save-profile cfs--fontnames-fallback
                         cfs--fontsizes-fallback))
    (if (yes-or-no-p "使用图形化工具来调整当前 profile 文件吗？")
        (cfs-ui)
      (find-file file)
      (cfs-profile-edit-mode 1)
      (goto-char (point-min)))))

(defun cfs-regenerate-profile ()
  (interactive)
  (let ((profile-name (completing-read "Regenerate profile: " cfs-profiles)))
    (if (yes-or-no-p (format "Regenerate (%s)? " profile-name))
        (cfs--save-profile cfs--fontnames-fallback
                           cfs--fontsizes-fallback profile-name)
      (message "Ignore regenerate profile!"))))

(defun cfs-test-fontsize-at-point ()
  "Test fontsizes list at point, which is usd to edit fontsizes list"
  (interactive)
  (let ((fontsizes-list (list-at-point)))
    (if (and (listp fontsizes-list)
             (numberp (car fontsizes-list)))
        (progn
          (cfs--set-font fontsizes-list)
          (cfs--show-font-effect fontsizes-list))
      ;; 如果当前 point 不在 profile 文件中的 `cfs--custom-set-fontsizes‘ 中
      ;; 使用一组预定义字体大小来查看字体效果。
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
            (min 50 (max 5 (+ step (string-to-number (match-string 0))))))))
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
  (let ((buffer (get-buffer-create "*Show-font-effect*")))
    (with-current-buffer buffer
      (erase-buffer)
      (setq truncate-lines 1)
      (setq cursor-type nil)
      (insert cfs--test-string)
      (goto-char (point-min))
      ;; Remove blank line at the beginning of buffer
      (delete-region (point)
                     (progn (forward-line 1)
                            (point))))
    (display-buffer buffer)
    (when (and (nth 0 fontsizes-list)
               (nth 1 fontsizes-list))
      (cfs--set-font fontsizes-list))
    (message cfs--minibuffer-echo-string)))

(defun cfs-insert-fontname ()
  "Select a valid font name, and insert at point."
  (interactive)
  (let ((all-fonts (font-family-list))
         fonts choose)
    (dolist (font all-fonts)
      (push (substring-no-properties
             (decode-coding-string font 'gbk))
            fonts)
      (push (substring-no-properties
             (decode-coding-string font 'utf-8))
            fonts))
    (setq fonts (delete-dups fonts))
    (setq choose (completing-read
                  "Which font name do you want to insert? "
                  (if (yes-or-no-p "Only show font names with Chinese? ")
                      (cl-remove-if
                       #'(lambda (x)
                           (not (string-match-p "\\cc" x)))
                       fonts)
                    fonts)))
    (when choose
      (insert (format "\"%s\"" choose)))))

;; Enable chinese-fonts-setup when launch emacs
(chinese-fonts-setup-enable)
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'chinese-fonts-setup)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-fonts-setup.el ends here
;; #+END_SRC
