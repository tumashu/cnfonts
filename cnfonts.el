;;; cnfonts.el --- A simple Chinese fonts config tool

;; * Header
;; Copyright (c) 2011-2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/cnfonts
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

;; * cnfonts README                                         :README:

;; ** 简介
;; 注意：cnfonts 原来叫： chinese-fonts-setup, 一开始使用三个词的首字
;; 母组成的字符串 "cfs-" 做为包的前缀，但不幸和 gnu 的项目 cfs.el 冲突，
;; 所以将包的前缀更改为 "cnfonts". chinese-fonts-setup 将做为 cnfonts
;; 的别名使用。

;; cnfonts 是一个 Emacs 中英文字体配置工具。可以比较方便地实
;; 现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。

;; 注： 这个 package 特别适用于需要处理中英文混合表格的中文 org-mode 用户。

;; ** 基本原理
;; cnfonts 的核心很简单，就是让中文字体和英文字体使用不同的字
;; 号，从而实现中英文对齐，它和下面的样例代码原理是一样的：

;; #+BEGIN_EXAMPLE
;; (set-frame-font "-unknown-PragmataPro-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "Microsoft Yahei" :size 16)))
;; #+END_EXAMPLE

;; ** 使用特点
;; cnfonts 添加了许多辅助工具，使配置和调节字体和字号的工作更
;; 加简便快捷，它有几个优点：

;; 1. 安装即用：cnfonts 内置字体 fallback 功能，只需安装，就能
;;    够配置中文字体和英文字体，让中文可以 *正确* 显示（但未必完美），不会
;;    因为 Emacs 配置中指定的字体不存在而报错。
;; 2. 设置方便：cnfonts 自带一个 profile 文件调整工具，这个工具
;;    有直观的图形界面，可以让用户设置字体名称和字体大小，分分钟实现中文字
;;    体和英文字体的等宽对齐。

;; ** 下载安装
;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET cnfonts RET
;; 3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：

;;    #+BEGIN_EXAMPLE
;;    (require 'cnfonts)
;;    ;; 让 cnfonts 随着 Emacs 自动生效。
;;    ;; (cnfonts-enable)
;;    ;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。
;;    ;; (cnfonts-set-spacemacs-fallback-fonts)
;;    #+END_EXAMPLE

;; ** 配置使用
;; *** 最简单的用法（懒人必备）
;; 通过下面几个命令，用户可以 *快速* 了解 cnfonts 的大部分功能，
;; 而不需要阅读整篇文档，如果用户想深入了解 cnfonts 或者自定义
;; 一些特殊的功能，阅读整篇文档是逃不开的。
;; | 命令                      | 功能         |
;; |---------------------------+--------------|
;; | cnfonts-edit-profile      | 调整字体设置 |
;; | cnfonts-increase-fontsize | 增大字号     |
;; | cnfonts-decrease-fontsize | 减小字号     |

;; *** profile 的概念
;; profile 代表了一套字体配置，cnfonts 使用 profile 的概念，
;; 来维护多套字体配置，从而实现特定的环境使用特定的字体配置，比如：在编程
;; 时使用 “Consolas + 微米黑”，在阅读文章时使用 “PragmataPro + 黑体”，
;; 等等。

;; 每一个 profile 都对应一个 emacs-lisp 文件, 保存在 `cnfonts-profiles-directory'
;; 目录中, 这些文件包含了英文字体设置，中文字体设置以及中文字体大小，
;; 其结构类似：

;; #+BEGIN_EXAMPLE
;; (setq cnfonts--custom-set-fontnames
;;       '(("PragmataPro" "Ubuntu Mono" "DejaVu Sans Mono")
;;         ("文泉驿等宽微米黑" "Ubuntu Mono" "隶书" "新宋体")
;;         ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB")))

;; (setq cnfonts--custom-set-fontsizes
;;       '((9    9.0  9.5 )
;;         (10   11.0 11.0)
;;         (11.5 12.5 12.5)
;;         (12.5 13.5 13.5)
;;         (14   15.0 15.0)
;;         (16   17.0 17.0)
;;         (18   18.0 18.0)
;;         (20   21.0 21.0)
;;         (22   23.0 23.0)
;;         (24   25.5 25.5)
;;         (26   27.0 27.0)
;;         (28   29.0 29.0)
;;         (30   32.0 32.0)
;;         (32   33.0 33.0)))
;; #+END_EXAMPLE

;; *** profile 命名与切换
;; cnfonts 默认使用三个 profile: profile1, profile2 和
;; profile3, 如果想使用其它有意义的名称，可以设置:

;; #+BEGIN_EXAMPLE
;; (setq cnfonts-profiles
;;     '("program" "org-mode" "read-book"))
;; #+END_EXAMPLE

;; cnfonts 使用下面两个命令来切换 profile ：

;; | Command                | Help                    |
;; |------------------------+-------------------------|
;; | cnfonts-switch-profile | 选择并切换 profile      |
;; | cnfonts-next-profile   | 直接切换到下一个profile |

;; *** 使用 cnfonts-edit-profile 命令调整 profile
;; 如果 *当前使用* 的字体不符合使用习惯，用户可以运行 `cnfonts-edit-profile'
;; 命令来调整 *当前* profile,这个命令会弹出一个图形化界面，类似：

;; [[./snapshots/cnfonts-ui-1.png]]
;; [[./snapshots/cnfonts-ui-2.png]]
;; [[./snapshots/cnfonts-ui-3.png]]
;; [[./snapshots/cnfonts-ui-4.png]]
;; [[./snapshots/cnfonts-ui-5.png]]
;; [[./snapshots/cnfonts-ui-6.png]]
;; [[./snapshots/cnfonts-ui-7.png]]

;; *** 使用 cnfonts-edit-profile-without-ui 命令编辑 profile
;; 除了使用 `cnfonts-edit-profile' , *有经验* 的用户也可以使用
;; `cnfonts-edit-profile-without-ui' 命令，直接编辑当前 profile 文件，
;; 两个命令的效果是一样的。

;; 在编辑的过程中，用户可以使用下面三个命令 *快速* 的测试编辑效果：

;; | Key     | Command                             | Help                                   |
;; |---------+-------------------------------------+----------------------------------------|
;; | C-c C-c | cnfonts-test-fontsizes-at-point     | 查看字体显示效果                       |
;; | C-up    | cnfonts-increment-fontsize-at-point | 增大光标下字号的大小，同时显示对齐效果 |
;; | C-down  | cnfonts-decrement-fontsize-at-point | 减小光标下字号的大小，同时显示对齐效果 |

;; 注1: 不建议 cnfonts 新用户使用这种方式

;; 注2: 配置完成后，有可能需要重启 Emacs, 参考：http://debbugs.gnu.org/db/17/1785.html

;; *** 使用 cnfonts-regenerate-profile 重置 profile
;; `cnfonts-regenerate-profile' 命令会使用 cnfonts 自带的
;; fallback 信息，覆盖需要 *重置* 的 profile, 这个 profile 原来的
;; 内容将丢失，请紧慎使用！

;; *** 调整字体大小
;; `cnfonts' 使用下述两个命令调整字体大小:

;; | Command                   | Help         |
;; |---------------------------+--------------|
;; | cnfonts-increase-fontsize | 增大字体大小 |
;; | cnfonts-decrease-fontsize | 减小字体大小 |

;; 注意：在调整字体大小的同时，字号信息也会保存到 `cnfonts-directory' 目录下
;; `cnfonts-config-filename' 对应的文件中。

;; [[./snapshots/cnfonts-increase-and-decrease-fontsize.gif]]

;; *** 使用 cnfonts-use-system-type
;; 有些用户希望将 profile 配置文件做为自己的 Emacs 配置，在不同
;; 的计算机上同步和管理，我建议这些用户将 `cnfonts-use-system-type'
;; 设置为 t, 这样，相同名称的 profile 在不同的操作系统下，保存的
;; 位置也不同，可以避免 profile 冲突。

;; *** 让 cnfonts 随着 Emacs 自动启动
;; `cnfonts-enable' 命令可以让 cnfonts 随着
;; Emacs 自动启动，这个命令将 `cnfonts-set-font-with-saved-step' 添加到
;; 下面两个 hook:

;; 1. `after-make-frame-functions'
;; 2. `window-setup-hook'

;; 用户也可以手动运行 `cnfonts-set-font-with-saved-step' 来让
;; cnfonts 生效。

;; *** cnfonts 与 org-mode 配合使用
;; 许多用户使用 org-mode 时，习惯让不同的标题，使用的字体大小也不同，这个
;; 特性需要用户设置：

;; #+BEGIN_EXAMPLE
;; (setq cnfonts-use-face-font-rescale t)
;; #+END_EXAMPLE

;; 注：这个功能不能在 window 系统下使用，它会让对齐功能失效，Linux 下
;; 这个功能 *一般* 可以使用，Mac 系统未测试，同学可以亲自试一试。

;; *** 使用 cnfonts 生成 elisp 字体配置片断
;; 有些用户觉得 cnfonts *太过厚重* , 他们喜欢使用简单的
;; 方式来配置字体，这些用户可以了解一下 `cnfonts-insert-fonts-configure'
;; 命令，这个命令可以根据 cnfonts 的设置自动生成一个
;; "字体配置 elisp 片断", 并插入光标处，将这个片断写入 .emacs 文件
;; 后，就不需要启动 cnfonts 来设置字体了。

;; *** cnfonts 高级功能
;; cnfonts *仅仅* 设置英文，中文和 EXT-B 字体，不处理
;; 其它字符的字体，比如：symbol 字符，但 cnfonts 可以
;; 通过 hook: `cnfonts-set-font-finish-hook' 来处理类似的问题（这个
;; hook 使用的函数只有一个参数 fontsizes-list, 用来记录 *当前使用*
;; 的英文字体，中文字体和 EXT-B 字体的字号）。

;; 下面是一些例子：
;; **** 设置 symbol 字符的字体
;; #+BEGIN_EXAMPLE
;; (defun my-set-symbol-fonts (fontsizes-list)
;;   (let* ((fontname "Inconsolata")
;;          (fontsize (nth 0 fontsizes-list))
;;          (fontspec (font-spec :name fontname
;;                               :size fontsize
;;                               :weight 'normal
;;                               :slant 'normal)))
;;     (if (cnfonts--fontspec-valid-p fontspec)
;;         (set-fontset-font "fontset-default" 'symbol fontspec nil 'append)
;;       (message "字体 %S 不存在！" fontname))))

;; (add-hook 'cnfonts-set-font-finish-hook 'my-set-symbol-fonts)
;; #+END_EXAMPLE

;; **** 设置一些不常用汉字字符的字体
;; #+BEGIN_EXAMPLE
;; (defun my-set-exta-fonts (fontsizes-list)
;;   (let* ((fontname "微软雅黑")
;;          (fontsize (nth 1 fontsizes-list))
;;          (fontspec (font-spec :name fontname
;;                               :size fontsize
;;                               :weight 'normal
;;                               :slant 'normal)))
;;     (if (cnfonts--fontspec-valid-p fontspec)
;;         (set-fontset-font "fontset-default" '(#x3400 . #x4DFF) fontspec nil 'append)
;;       (message "字体 %S 不存在！" fontname))))

;; (add-hook 'cnfonts-set-font-finish-hook 'my-set-exta-fonts)
;; #+END_EXAMPLE
;; 注意事项：

;; 1. "(#x3400 . #x4DFF)" 代表了待设字符在 unicode-bmp 中的范围。
;; 2. 用户可以通过下面的方式来确定待字符的范围
;;    1. 运行 `describe-char' 来显示 *待设字符* 的信息
;;    2. 点击 “code point in charset” 处的链接，来显示整个 unicode-bmp 表
;;    3. 获取范围
;; 3. 如果遇到 *部分符号* 无法正确对齐，可以参考:
;;    1. https://github.com/tumashu/cnfonts/issues/64#issuecomment-296414028

;; **** 设置行距随着字号自动调整

;; #+BEGIN_EXAMPLE
;; (defvar my-line-spacing-alist
;;       '((9 . 0.1) (10 . 0.9) (11.5 . 0.2)
;;         (12.5 . 0.2) (14 . 0.2) (16 . 0.2)
;;         (18 . 0.2) (20 . 1.0) (22 . 0.2)
;;         (24 . 0.2) (26 . 0.2) (28 . 0.2)
;;         (30 . 0.2) (32 . 0.2)))

;; (defun my-line-spacing-setup (fontsizes-list)
;;   (let ((fontsize (car fontsizes-list))
;;         (line-spacing-alist (copy-list my-line-spacing-alist)))
;;     (dolist (list line-spacing-alist)
;;       (when (= fontsize (car list))
;;         (setq line-spacing-alist nil)
;;         (setq-default line-spacing (cdr list))))))

;; (add-hook 'cnfonts-set-font-finish-hook #'my-line-spacing-setup)
;; #+END_EXAMPLE

;; ** Tips

;; 1. 如果用户需要在自己的 Emacs 配置中管理一些个人字体，可以使用变量
;;    `cnfonts-personal-fontnames' , 其结构与 `cnfonts--fontnames-fallback'一样。
;; 2. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
;; 3. 在 scratch 中写一行 elisp 代码：
;;    #+BEGIN_EXAMPLE
;;    (cl-prettyprint (font-family-list))
;;    #+END_EXAMPLE
;;    执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很有用
;;    的技巧。
;; 4. 命令：`cnfonts-insert-fontname', 可以让用户选择一个可用字体插入到当前光
;;    标处。
;; 5. Windows 用户 (特别是 Windows XP 用户) 可以安装 MacType 软件来优化字
;;    体显示效果，推荐使用。
;; 6. Mac 用户配置 profile 文件的时候，偶尔会遇到 'C-c C-c' 刷新缓慢的问
;;    题，这可能是 ext-b 字体缺失引起的，建议安装 ext-b 字体试试。
;;    1. Ext-B字符列表: https://cdo.wikipedia.org/wiki/Wikipedia:Unicode%E6%93%B4%E5%B1%95%E6%BC%A2%E5%AD%97
;;    2. HanaMinB 下载地址: https://osdn.jp/projects/hanazono-font/downloads/62072/hanazono-20141012.zip/
;; 7. 字体设置和 coding 设置也有关系，如果 cnfonts 的行为很奇怪，
;;    又找不到确切原因，可以参考：https://github.com/tumashu/cnfonts/issues/54#issuecomment-246228904

;; ** 参考文章
;; 1. http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; 2. http://zhuoqiang.me/torture-emacs.html

;;; Code:

;; * 代码                                                                 :code:
(require 'cl-lib)
(require 'format-spec)
(require 'thingatpt)
(require 'cnfonts-ui)

(defgroup cnfonts nil
  "Chinese fonts setup."
  :prefix "cnfonts-"
  :group 'applications)

(defcustom cnfonts-profiles '("profile1" "profile2" "profile3")
  "Lists cnfonts profiles."
  :group 'cnfonts
  :type 'list)

(defcustom cnfonts-directory (locate-user-emacs-file "cnfonts/")
  "Directory, cnfonts config file and profiles will be stored in."
  :group 'cnfonts
  :type 'directory)

(define-obsolete-variable-alias 'cnfonts-profiles-directory 'cnfonts-directory)

(defcustom cnfonts-config-filename "cnfonts.conf"
  "Filename of cnfonts config file.
It record the current profile and profile steps."
  :group 'cnfonts
  :type 'string)

(defcustom cnfonts-use-system-type nil
  "构建 profile 文件所在的目录时，是否考虑当前的 `system-type'.

假设当前系统为 Linux, 当这个选项设置为 t 后，profile1 文件的路径，
将从 'DIR/profile1.el' 转为 'DIR/SYSTEM-TYPE/profile.el'"
  :group 'cnfonts
  :type 'boolean)

(defcustom cnfonts-keep-frame-size t
  "在调整字体的时候，是否保持当前 frame 大小不变."
  :group 'cnfonts
  :type 'boolean)

(defcustom cnfonts-disable-bold nil
  "是否禁用英文粗体."
  :group 'cnfonts
  :type 'boolean)

(defcustom cnfonts-disable-italic nil
  "是否禁用英文斜体."
  :group 'cnfonts
  :type 'boolean)

(defcustom cnfonts-disable-bold-italic nil
  "是否禁用英文粗斜体."
  :group 'cnfonts
  :type 'boolean)

(defcustom cnfonts-save-current-profile t
  "是否保存将当前 profile 的信息."
  :group 'cnfonts
  :type 'boolean)

(defcustom cnfonts-use-face-font-rescale nil
  "是否通过设定 `face-font-rescale-alist' 来达到中英文对齐.

在 window 平台下，将这个变量设置为 t 会导致 cnfonts
字体对齐功能失效，在大多数 linux 平台下这个功能都可以正常使用。"
  :group 'cnfonts
  :type 'boolean)

(defcustom cnfonts-set-font-finish-hook nil
  "A hook, by which user can set additional fonts.
The below is an example which is used to set symbol fonts:

 (defun cnfonts-set-symbol-fonts (fontsizes-list)
   (set-fontset-font t 'symbol \"Inconsolata\" nil 'append)
   (set-fontset-font t 'symbol \"Symbola\" nil 'append)
   (set-fontset-font t 'unicode \"Segoe UI Emoji\" nil 'append)
   (set-fontset-font t 'unicode \"STIX\" nil 'append))
 (add-hook 'cnfonts-set-font-finish-hook 'cnfonts-set-symbol-fonts)"
  :group 'cnfonts
  :type 'hook)

(defcustom cnfonts-fonts-configure-template "
;; Auto generated by cnfonts
;; <https://github.com/tumashu/cnfonts>
(set-face-attribute
 'default nil
 :font (font-spec :name \"%E\"
                  :weight 'normal
                  :slant 'normal
                  :size %e))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :name \"%C\"
              :weight 'normal
              :slant 'normal
              :size %c)))
"
  "A string used to generate fonts configure snippet.
It can be inserted into '~/.emacs' file to config Emacs fonts.
其中：

1. %E   英文字体名称
2. %e   英文字体字号
3. %C   中文字体名称
3. %c   中文字体字号"
  :group 'cnfonts
  :type 'string)

(defvar cnfonts--current-profile nil
  "Current profile name used by cnfonts.")

(defvar cnfonts--profiles-steps nil
  "用来保存每一个 profile 使用 `cnfonts--fontsizes-fallback' 中第几个字号组合.")

(defconst cnfonts--fontsizes-fallback
  '((9    10.5 10.5)
    (10   12.0 12.0)
    (11.5 13.5 13.5)
    (12.5 15.0 15.0)
    (14   16.5 16.5)
    (15   18.0 18.0)
    (16   19.5 19.5)
    (18   21.0 21.0)
    (20   24.0 24.0)
    (22   25.5 25.5)
    (24   28.5 28.5)
    (26   31.5 31.5)
    (28   33.0 33.0)
    (30   36.0 36.0)
    (32   39.0 39.0))
  "一个列表，每一个元素都有类似结构：(英文字号 中文字号 EXT-B字体字号).")

(defcustom cnfonts-personal-fontnames nil
  "用户自己维护的字体列表，其结构与 `cnfonts--fontnames-fallback' 一致."
  :group 'cnfonts)

(defcustom cnfonts-verbose t
  "设置为 t 时， cnfonts 将 message 较多信息."
  :group 'cnfonts
  :type 'integer)

(defconst cnfonts--fontnames-fallback
  '(("Monaco" "Consolas" "DejaVu Sans Mono" "Droid Sans Mono" "PragmataPro"
     "Courier" "Courier New" "Ubuntu Mono" "Liberation Mono" "MonacoB" "MonacoB2"
     "MonacoBSemi" "Droid Sans Mono Pro" "Inconsolata" "Source Code Pro" "Lucida Console"
     "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter"
     "Panic Sans" "Hack" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc"
     "Excalibur Monospace" "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono"
     "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002"
     "ProggyCleanTT" "ProggyTinyTT")
    ("微软雅黑" "Noto Sans S Chinese Regular" "Microsoft Yahei" "Microsoft_Yahei" "Ubuntu Mono"
     "文泉驿等宽微米黑" "文泉驿等宽正黑" "黑体" "Hiragino Sans GB"  "文泉驿正黑" "文泉驿点阵正黑"
     "SimHei" "SimSun" "NSimSun" "FangSong" "KaiTi" "FangSong_GB2312" "KaiTi_GB2312" "LiSu"
     "YouYuan" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "STXihei" "STKaiti"
     "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti"
     "STXingkai" "STXinwei" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋"
     "华文彩云" "华文新魏" "华文细黑" "华文行楷")
    ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB")))

(defconst cnfonts--test-string "
| 如果此表格无法对齐，请调整下面变量中的数字 |
|       `cnfonts--custom-set-fontsizes'      |
| 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄇 |
")

(defconst cnfonts--profile-comment-1 "
;;; `cnfonts--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
;;; `cnfonts-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。")

(defconst cnfonts--profile-comment-2 "
;;; `cnfonts--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。")

(defvar cnfonts--minibuffer-echo-string nil)

(defvar cnfonts--custom-set-fontnames nil
  "*专用* 变量，只用与 cnfonts 的 profile 文件.
这些 profile 文件保存在 `cnfonts-profiles-directory' 对应的目录中。在其它地方
设置这个变量没有任何用处！")

(defvar cnfonts--custom-set-fontsizes nil
  "*专用* 变量，只用与 cnfonts 的 profile 文件.
这些 profile 文件保存在 `cnfonts-profiles-directory' 对应的目录中。在其它地方
设置这个变量没有任何用处！")

(defvar cnfonts--enabled-p nil)

(defun cnfonts-message (force-show &rest args)
  "Cnfonts's message function.
When FORCE-SHOW is non-nil, show message force.
ARGS is the same as message's ARGS."
  (if (or cnfonts-verbose force-show)
      (apply 'message args)
    (apply 'format args)))

(defun cnfonts--get-profile (profile-name)
  "Get profile file which name is PROFILE-NAME."
  (let* ((cnfonts-profile-version "v4") ;; 升级 profile 格式时改变版本号
         (directory-name
          (expand-file-name
           (file-name-as-directory
            (concat (file-name-as-directory cnfonts-profiles-directory)
                    cnfonts-profile-version
                    "/"
                    (if cnfonts-use-system-type
                        (replace-regexp-in-string
                         "/" "-" (symbol-name system-type))
                      ""))))))
    (make-directory directory-name t)
    (concat directory-name
            (replace-regexp-in-string
             "/" "-"
             profile-name) ".el")))

(defun cnfonts--get-current-profile (&optional return-profile-name)
  "Get current profile file.
When RETURN-PROFILE-NAME is non-nil, return current profile file's name."
  (let ((profile-name
         (if (member cnfonts--current-profile cnfonts-profiles)
             cnfonts--current-profile
           (car cnfonts-profiles))))
    (if return-profile-name
        profile-name
      (cnfonts--get-profile profile-name))))

(defun cnfonts--dump-variable (variable value)
  "Insert a \"(setq VARIABLE VALUE)\" in the current buffer."
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

(defun cnfonts--return-config-file-path ()
  "Return the path of config file."
  (expand-file-name
   (concat (file-name-as-directory cnfonts-directory)
           cnfonts-config-filename)))

(defun cnfonts--save-config-file (profile-name &optional step)
  "Save PROFILE-NAME and STEP into config file."
  (when step
    (if (assoc profile-name cnfonts--profiles-steps)
        (setf (cdr (assoc profile-name cnfonts--profiles-steps)) step)
      (push `(,profile-name . ,step) cnfonts--profiles-steps)))
  (with-temp-file (cnfonts--return-config-file-path)
    (when cnfonts-save-current-profile
      (prin1 (list cnfonts--current-profile) (current-buffer)))
    (prin1 cnfonts--profiles-steps (current-buffer))))

(defun cnfonts--read-config-file ()
  "Read cnfonts's config file."
  (let ((save-file (cnfonts--return-config-file-path)))
    (if (file-readable-p save-file)
        (with-temp-buffer
          (insert-file-contents save-file)
          (ignore-errors
            (when cnfonts-save-current-profile
              (setq cnfonts--current-profile (car (read (current-buffer)))))
            (setq cnfonts--profiles-steps (read (current-buffer))))))))

(defun cnfonts--get-profile-step (profile-name)
  "Get the step info from profile which name is PROFILE-NAME."
  (or (cdr (assoc profile-name cnfonts--profiles-steps)) 4))

(defun cnfonts--save-profile (fontnames fontsizes &optional profile-name)
  "Save FONTNAMES and FONTSIZES to current profile.
When PROFILE-NAME is non-nil, save to this profile instead."
  (with-temp-buffer
    (erase-buffer)
    (insert (replace-regexp-in-string
             "^ *\n" ""
             cnfonts--profile-comment-1))
    (cnfonts--dump-variable 'cnfonts--custom-set-fontnames fontnames)
    (insert cnfonts--profile-comment-2)
    (cnfonts--dump-variable 'cnfonts--custom-set-fontsizes fontsizes)
    (write-file (cnfonts--get-profile
                 (or profile-name (cnfonts--get-current-profile t))))))

(defun cnfonts--read-profile ()
  "Get previously saved fontnames and fontsizes from current profile."
  (interactive)
  (let ((file (cnfonts--get-current-profile)))
    (if (file-readable-p file)
        (progn (when (load (expand-file-name file) nil t)
                 (cnfonts-message t "[cnfonts]: load %S successfully." (cnfonts--get-current-profile t)))
               (list
                (if cnfonts--custom-set-fontnames
                    (cnfonts--merge-fontname-list cnfonts--custom-set-fontnames
                                                  cnfonts-personal-fontnames
                                                  cnfonts--fontnames-fallback)
                  (cnfonts--merge-fontname-list cnfonts-personal-fontnames
                                                cnfonts--fontnames-fallback))
                (or cnfonts--custom-set-fontsizes
                    cnfonts--fontsizes-fallback)))
      (list cnfonts--fontnames-fallback
            cnfonts--fontsizes-fallback))))

(defun cnfonts--upgrade-profile-need-p ()
  "测试是否需要升级 profile 格式."
  (let* ((profile-info (cnfonts--read-profile))
         (profile-fontnames (nth 0 profile-info))
         (profile-fontsizes (nth 1 profile-info)))
    (not (and (= (length profile-fontnames)
                 (length cnfonts--fontnames-fallback))
              (= (length profile-fontsizes)
                 (length cnfonts--fontsizes-fallback))))))

(defun cnfonts--merge-fontname-list (list1 list2 &optional list3)
  "Merge fontname lists  LIST1, LIST2 and LIST3 into one."
  (mapcar #'(lambda (lst)
              (cl-remove-duplicates lst :from-end t :test 'equal))
          `((,@(nth 0 list1) ,@(nth 0 list2) ,@(nth 0 list3))
            (,@(nth 1 list1) ,@(nth 1 list2) ,@(nth 1 list3))
            (,@(nth 2 list1) ,@(nth 2 list2) ,@(nth 2 list3)))))

(defun cnfonts--font-exists-p (font)
  "Test FONT exist or not."
  (or (cnfonts--get-xlfd font)
      (let ((all-fonts (font-family-list))
            result)
        (dolist (x all-fonts)
          (when (or (equal font x)
                    (equal (encode-coding-string font 'gbk) x)
                    (equal (encode-coding-string font 'utf-8) x))
            (setq result font)
            (setq all-fonts nil)))
        result)))

(defun cnfonts--get-valid-fonts (&optional prefer-shortname)
  "Get a list of valid fonts.
If PREFER-SHORTNAME is non-nil, return shortname list instead."
  (mapcar #'(lambda (x)
              (let ((font (cl-find-if #'cnfonts--font-exists-p x)))
                (when font
                  (if prefer-shortname
                      font
                    (or (cnfonts--get-xlfd font) font)))))
          (car (cnfonts--read-profile))))

(defun cnfonts--get-xlfd (fontname &optional uncheck)
  "返回 FONTNAME 对应的 xlfd 格式的 fontset.
如果 UNCHECK 是 non-nil, 不检查返回的 xlfd 格式
是否为有效的 xlfd.  字体中含有 \"-\" 往往返回有问题
的 xlfd."
  (when fontname
    (let* ((font-xlfd (car (x-list-fonts fontname nil nil 1))))
      (when (and font-xlfd
                 ;; 当字体名称中包含 "-" 时，`x-list-fonts'
                 ;; 返回无效的 XLFD 字符串，具体细节请参考 emacs bug#17457 。
                 ;; 忽略无效 XLFD 字符串。
                 (or uncheck (x-decompose-font-name font-xlfd)))
        font-xlfd))))

;; (cnfonts--get-fontset "courier" 10 'italic)

(defun cnfonts--get-fontsizes (&optional step)
  "获取 STEP 对应的 fontsize."
  (let* ((fontsizes-list (car (cdr (cnfonts--read-profile)))))
    (unless (file-exists-p (cnfonts--get-current-profile))
      (cnfonts-message t "如果中英文不能对齐，请运行`cnfonts-edit-profile'编辑当前profile。"))
    (if (numberp step)
        (nth (- step 1) fontsizes-list)
      12.5)))

(defun cnfonts--set-font (fontsizes-list)
  "根据 FONTSIZES-LIST 调整当前 frame 使用的字体.
当全局变量 `cnfonts-keep-frame-size'设置为 t 时，调整字体时保持当前 frame 大小不变。"
  (let ((frame (selected-frame))
        height width)

    (when cnfonts-use-face-font-rescale
      (cnfonts--set-face-font-rescale fontsizes-list)
      ;; 通过设定 `face-font-rescale-alist' 来实现中英文对齐时，
      ;; 只设定英文字体字号，中文等字体字号不设定。
      (setq fontsizes-list
            (list (car fontsizes-list))))

    (when (display-multi-font-p frame)
      (when cnfonts-keep-frame-size
        (setq height (* (frame-parameter frame 'height)
                        (frame-char-height frame))
              width  (* (frame-parameter frame 'width)
                        (frame-char-width frame))))
      (cnfonts--set-font-1 fontsizes-list)
      (run-hook-with-args 'cnfonts-set-font-finish-hook fontsizes-list)
      (when cnfonts-keep-frame-size
        (modify-frame-parameters
         frame
         (list (cons 'height (round height (frame-char-height frame)))
               (cons 'width  (round width  (frame-char-width frame)))))))))

(defun cnfonts--set-face-font-rescale (fontsizes-list)
  "根据 FONTSIZES-LIST 设定 `face-font-rescale-alist' 系数."
  (setq face-font-rescale-alist
        (cl-loop for font in (cnfonts--get-valid-fonts t)
                 for size in fontsizes-list
                 collect (cons font (/ (float size)
                                       (car fontsizes-list))))))

(defun cnfonts--fontspec-valid-p (fontspec)
  "检查 FONTSPEC 是否有效."
  (and fontspec (list-fonts fontspec)))

(defun cnfonts--float (num)
  "确保一个 NUM 总是浮点格式."
  (when (numberp num)
    (float num)))

(defun cnfonts--set-font-1 (fontsizes-list)
  "核心函数，用于设置字体.
参数 FONTSIZES-LIST 是一个列表，其结构类似：

    (英文字体字号 中文字体字号 EXT-B字体字号
                  英文symbol字体字号 中文symbol字体字号)

其中，英文字体字号必须设定，其余字体字号可以设定，也可以省略。"
  (let* ((valid-fonts (cnfonts--get-valid-fonts))
         (valid-short-fontnames (cnfonts--get-valid-fonts t))

         (english-main-fontname (nth 0 valid-fonts))
         (chinese-main-fontname (nth 1 valid-fonts))
         (chinese-extra-fontname (nth 2 valid-fonts))

         (english-main-short-fontname (nth 0 valid-short-fontnames))
         (chinese-main-short-fontname (nth 1 valid-short-fontnames))
         (chinese-extra-short-fontname (nth 2 valid-short-fontnames))

         (english-main-fontsize (cnfonts--float (nth 0 fontsizes-list)))
         (chinese-main-fontsize (cnfonts--float (nth 1 fontsizes-list)))
         (chinese-extra-fontsize (cnfonts--float (nth 2 fontsizes-list)))

         (english-symbol-fontsize (cnfonts--float (nth 0 fontsizes-list)))
         (chinese-symbol-fontsize (cnfonts--float (nth 1 fontsizes-list)))

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

    (when (cnfonts--fontspec-valid-p english-main-fontspec)
      ;; 设置英文字体。
      (set-face-attribute
       'default nil :font english-main-fontspec)
      ;; 设置英文粗体。
      (if cnfonts-disable-bold
          (set-face-font 'bold english-main-fontspec)
        (if (cnfonts--fontspec-valid-p english-bold-fontspec)
            (set-face-font 'bold english-bold-fontspec)
          (cnfonts-message t "[cnfonts]: %S 对应的粗体没有找到，不作处理！"
                           english-main-short-fontname)))

      ;; 设置英文斜体。
      (if cnfonts-disable-italic
          (set-face-font 'italic english-main-fontspec)
        (if (cnfonts--fontspec-valid-p english-italic-fontspec)
            (set-face-font 'italic english-italic-fontspec)
          (cnfonts-message t "[cnfonts]: %S 对应的斜体没有找到，不作处理！"
                           english-main-short-fontname)))

      ;; 设置英文粗斜体。
      (if cnfonts-disable-bold-italic
          (set-face-font 'bold-italic english-main-fontspec)
        (if (cnfonts--fontspec-valid-p english-bold-italic-fontspec)
            (set-face-font 'bold-italic english-bold-italic-fontspec)
          (cnfonts-message t "[cnfonts]: %S 对应的粗斜体没有找到，不作处理！"
                           english-main-short-fontname))))

    ;; 设置中文字体，注意，不要使用 'unicode charset,
    ;; 否则上面的英文字体设置将会失效。
    (when (cnfonts--fontspec-valid-p chinese-main-fontspec)
      (dolist (charset '(kana han cjk-misc bopomofo gb18030))
        (set-fontset-font "fontset-default" charset chinese-main-fontspec)))

    ;; 设置 symbol 字体。
    (when (cnfonts--fontspec-valid-p english-main-fontspec)
      (set-fontset-font "fontset-default" 'symbol english-symbol-fontspec))

    ;; (when (cnfonts--fontspec-valid-p chinese-main-fontset)
    ;;   (set-fontset-font t 'symbol chinese-symbol-fontspec nil 'append))

    ;; 设置 fallback 字体，用于显示不常用的字符。
    (when (cnfonts--fontspec-valid-p chinese-extra-fontspec)
      (set-fontset-font "fontset-default" nil chinese-extra-fontspec nil 'prepend))

    (setq cnfonts--minibuffer-echo-string
          (format "[%s]: 英文字体: %s-%.1f，中文字体: %s, EXTB字体：%s"
                  (cnfonts--get-current-profile t)
                  (or english-main-short-fontname "无") english-main-fontsize
                  (or chinese-main-short-fontname "无")
                  (or chinese-extra-short-fontname "无")))
    (message "")))

(defun cnfonts--step-fontsize (num)
  "获取下 NUM 个 step 对应的 fontsize."
  (if (not (display-graphic-p))
      (cnfonts-message t "cnfonts 不支持 emacs 终端模式！")
    (let* ((profile-name (cnfonts--get-current-profile t))
           (profile-step
            (max 1 (min (+ num (cnfonts--get-profile-step profile-name))
                        (length cnfonts--fontsizes-fallback))))
           (fontsizes-list (cnfonts--get-fontsizes profile-step)))
      (cnfonts--set-font fontsizes-list)
      (cnfonts--save-config-file profile-name profile-step)
      (cnfonts-message t cnfonts--minibuffer-echo-string))))

;;;###autoload
(defun cnfonts-set-font-with-saved-step (&optional frame)
  "设置字体为：当前保存 step 对应的字体.
如果 FRAME 是 non-nil, 设置对应的 FRAME 的字体。"
  (interactive)
  (cnfonts--read-config-file)
  (let* ((profile-name (cnfonts--get-current-profile t))
         (profile-step (cnfonts--get-profile-step profile-name))
         (fontsizes-list (cnfonts--get-fontsizes profile-step)))
    (if frame
        (with-selected-frame frame
          (when (display-graphic-p)
            (cnfonts--set-font fontsizes-list)))
      (when (display-graphic-p)
        (cnfonts--set-font fontsizes-list)))
    ;; This is useful for exwm to adjust mode-line, please see:
    ;; https://github.com/ch11ng/exwm/issues/249#issuecomment-299692305
    (redisplay t)))

;;;###autoload
(defun cnfonts-decrease-fontsize ()
  "Cnfonts 减小字体."
  (interactive)
  (cnfonts--step-fontsize -1))

;;;###autoload
(defun cnfonts-increase-fontsize ()
  "Cnfonts 增大字体."
  (interactive)
  (cnfonts--step-fontsize 1))

(defvar cnfonts-profile-edit-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'cnfonts-test-fontsize-at-point)
    (define-key keymap (kbd "C-<up>") 'cnfonts-increment-fontsize-at-point)
    (define-key keymap (kbd "C-<down>") 'cnfonts-decrement-fontsize-at-point)
    (define-key keymap (kbd "C-<right>") 'cnfonts-increment-fontsize-at-point)
    (define-key keymap (kbd "C-<left>") 'cnfonts-decrement-fontsize-at-point)
    keymap)
  "Keymap for variable `cnfonts-profile-edit-mode', a minor mode used to setup fonts names and sizes.")

(define-minor-mode cnfonts-profile-edit-mode
  "Minor for setup fonts names and sizes"
  nil " Rem" cnfonts-profile-edit-mode-map)

(defun cnfonts--select-profile (profile-name)
  "选择 PROFILE-NAME."
  (if (member profile-name cnfonts-profiles)
      (progn (setq cnfonts--current-profile profile-name)
             (cnfonts--save-config-file profile-name)
             (cnfonts-set-font-with-saved-step))
    (cnfonts-message t "%s doesn't exist." profile-name)))

;;;###autoload
(defun cnfonts-switch-profile ()
  "切换 cnfonts profile."
  (interactive)
  (let ((profile (completing-read "Set cnfonts profile to:" cnfonts-profiles)))
    (cnfonts--select-profile profile)))

;;;###autoload
(defun cnfonts-next-profile (&optional step)
  "选择下一个 profile 中当前 STEP 对应的字体设置."
  (interactive)
  (let ((profiles cnfonts-profiles)
        (current-profile (cnfonts--get-current-profile t))
        next-profile)
    (setq next-profile
          (or (cadr (member current-profile profiles))
              (car profiles)))
    (when next-profile
      (setq cnfonts--current-profile next-profile)
      (cnfonts--save-config-file next-profile))
    (when (display-graphic-p)
      (cnfonts-set-font-with-saved-step))
    (cnfonts-message t "Current cnfonts profile is set to: \"%s\"" next-profile)))

;;;###autoload
(defun cnfonts-edit-profile ()
  "编辑当前 cnfonts profile."
  (interactive)
  (if (not (display-graphic-p))
      (cnfonts-message t "cnfonts 不支持 emacs 终端模式！")
    (let ((file (cnfonts--get-current-profile)))
      (unless (file-readable-p file)
        (cnfonts--save-profile cnfonts--fontnames-fallback
                               cnfonts--fontsizes-fallback))
      (cnfonts-ui))))

;;;###autoload
(defun cnfonts-edit-profile-without-ui ()
  "编辑当前 cnfonts profile, 不使用 ‘cnfonts-ui’ 组件."
  (interactive)
  (if (not (display-graphic-p))
      (cnfonts-message t "cnfonts 不支持 emacs 终端模式！")
    (let ((file (cnfonts--get-current-profile)))
      (unless (file-readable-p file)
        (cnfonts--save-profile cnfonts--fontnames-fallback
                               cnfonts--fontsizes-fallback))
      (find-file file)
      (cnfonts-profile-edit-mode 1)
      (goto-char (point-min)))))

;;;###autoload
(defun cnfonts-regenerate-profile ()
  "重新生成当前 profile."
  (interactive)
  (let ((profile-name (completing-read "Regenerate profile: " cnfonts-profiles)))
    (if (yes-or-no-p (format "Regenerate (%s)? " profile-name))
        (cnfonts--save-profile cnfonts--fontnames-fallback
                               cnfonts--fontsizes-fallback profile-name)
      (cnfonts-message t "Ignore regenerate profile!"))))

(defun cnfonts-test-fontsize-at-point ()
  "Test fontsizes list at point, which is usd to edit fontsizes list."
  (interactive)
  (let ((fontsizes-list (list-at-point)))
    (if (and (listp fontsizes-list)
             (numberp (car fontsizes-list)))
        (progn
          (cnfonts--set-font fontsizes-list)
          (cnfonts--show-font-effect fontsizes-list))
      ;; 如果当前 point 不在 profile 文件中的 `cnfonts--custom-set-fontsizes‘ 中
      ;; 使用一组预定义字体大小来查看字体效果。
      (cnfonts--set-font '(14 15 15))
      (cnfonts--show-font-effect '(14 15 15)))))

(defun cnfonts-change-fontsize-at-point (step)
  "按照 STEP 改变光标处的字号对应的数字."
  (interactive)
  (skip-chars-backward "0123456789\\.")
  (or (looking-at "[0123456789.]+")
      (error "No number at point"))
  (replace-match
   (format "%.5s"
           (number-to-string
            (min 50 (max 5 (+ step (string-to-number (match-string 0))))))))
  (backward-char 1)
  (cnfonts-test-fontsize-at-point))

(defun cnfonts-increment-fontsize-at-point ()
  "增大光标处的字号数字."
  (interactive)
  (cnfonts-change-fontsize-at-point 0.5))

(defun cnfonts-decrement-fontsize-at-point ()
  "减小光标处的字号数字."
  (interactive)
  (cnfonts-change-fontsize-at-point -0.5))

(defun cnfonts--show-font-effect (&optional fontsizes-list)
  "Show font and its size in a new buffer.
FONTSIZES-LIST."
  (interactive)
  (let ((buffer (get-buffer-create "*Show-font-effect*")))
    (with-current-buffer buffer
      (erase-buffer)
      (setq truncate-lines 1)
      (setq cursor-type nil)
      (insert cnfonts--test-string)
      (goto-char (point-min))
      ;; Remove blank line at the beginning of buffer
      (delete-region (point)
                     (progn (forward-line 1)
                            (point))))
    (display-buffer buffer)
    (when (and (nth 0 fontsizes-list)
               (nth 1 fontsizes-list))
      (cnfonts--set-font fontsizes-list))
    (cnfonts-message t cnfonts--minibuffer-echo-string)))

;;;###autoload
(defun cnfonts-insert-fonts-configure ()
  "在光标处，插入一个 elisp 片断，这个 elisp 片断可以用来配置中文和英文字体."
  (interactive)
  (insert (cnfonts--return-fonts-configure-string)))

(defun cnfonts--get-current-fontsizes ()
  "获取当前使用的字号列表."
  (cnfonts--get-fontsizes
   (cnfonts--get-profile-step
    (cnfonts--get-current-profile t))))

(defun cnfonts--return-fonts-configure-string ()
  "返回一个 elisp 片断，这个 elisp 片断可以用来配置中文和英文字体."
  (let* ((fonts (cnfonts--get-valid-fonts))
         (fontsizes (cnfonts--get-current-fontsizes))
         (english-fontname (nth 0 fonts))
         (chinese-fontname (nth 1 fonts))
         (english-fontsize (cnfonts--float (nth 0 fontsizes)))
         (chinese-fontsize (cnfonts--float (nth 1 fontsizes))))
    (format-spec cnfonts-fonts-configure-template
                 `((?E . ,english-fontname)
                   (?C . ,chinese-fontname)
                   (?e . ,english-fontsize)
                   (?c . ,chinese-fontsize)))))

;;;###autoload
(defun cnfonts-insert-fontname ()
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

;;;###autoload
(defun cnfonts-enable ()
  "运行这个函数，可以让 Emacs 启动的时候就激活 cnfonts."
  (interactive)
  (setq cnfonts--enabled-p t)
  (add-hook 'after-make-frame-functions #'cnfonts-set-font-with-saved-step)
  (add-hook 'window-setup-hook #'cnfonts-set-font-with-saved-step))

;;;###autoload
(defun cnfonts-disable ()
  "清除与 cnfonts 相关的 hook 设定."
  (interactive)
  (setq cnfonts--enabled-p nil)
  (remove-hook 'after-make-frame-functions #'cnfonts-set-font-with-saved-step)
  (remove-hook 'window-setup-hook #'cnfonts-set-font-with-saved-step))

;; Steal code from `spacemacs/set-default-font'
(defun cnfonts--set-spacemacs-fallback-fonts (fontsizes-list)
  "Spacmacs 支持.
FONTSIZES-LIST."
  (let (fallback-font-name fallback-font-name2)
    (when (featurep 'spacemacs)
      (pcase system-type
        (`gnu/linux
         (setq fallback-font-name "NanumGothic")
         (setq fallback-font-name2 "NanumGothic"))
        (`darwin
         (setq fallback-font-name "Arial Unicode MS")
         (setq fallback-font-name2 "Arial Unicode MS"))
        (`windows-nt
         (setq fallback-font-name "MS Gothic")
         (setq fallback-font-name2 "Lucida Sans Unicode"))
        (`cygwin
         (setq fallback-font-name "MS Gothic")
         (setq fallback-font-name2 "Lucida Sans Unicode"))
        (other
         (setq fallback-font-name nil)
         (setq fallback-font-name2 nil)))
      (when (and fallback-font-name fallback-font-name2)
        (let ((fallback-spec (apply 'font-spec
                                    :name fallback-font-name
                                    :size (car fontsizes-list)))
              (fallback-spec2 (apply 'font-spec
                                     :name fallback-font-name2
                                     :size (car fontsizes-list))))
          ;; window numbers
          (set-fontset-font "fontset-default"
                            '(#x2776 . #x2793) fallback-spec nil 'prepend)
          ;; mode-line circled letters
          (set-fontset-font "fontset-default"
                            '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
          ;; mode-line additional characters
          (set-fontset-font "fontset-default"
                            '(#x2295 . #x22a1) fallback-spec nil 'prepend)
          ;; new version lighter
          (set-fontset-font "fontset-default"
                            '(#x2190 . #x2200) fallback-spec2 nil 'prepend))))))

;;;###autoload
(defun cnfonts-set-spacemacs-fallback-fonts ()
  "显示 Spacemace mode-line 上面有一些 Unicode 字符.
这些字符需要专门的字体来显示，spacemacs 将这些字体的名字内置在
`spacemacs/set-default-font' 的代码中。运行这个函数后，cnfonts
将使用同样的字体来显示这些 Unicode 字符。"
  (interactive)
  (add-hook 'cnfonts-set-font-finish-hook
            #'cnfonts--set-spacemacs-fallback-fonts)
  (cnfonts-message nil "[cnfonts]: 激活 spacemacs fallback 字体，用于显示 mode-line 中的漂亮图标。"))

;; * Footer
(provide 'cnfonts)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; cnfonts.el ends here
