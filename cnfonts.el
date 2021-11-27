;;; cnfonts.el --- A simple Chinese fonts config tool  -*- lexical-binding: t; -*-

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

;; cnfonts 原来叫: chinese-fonts-setup, 是一个 Emacs 中英文字体配置工
;; 具。可以比较方便地实现中文字体和英文字体等宽（也就是大家常说的中英
;; 文对齐）。

;; 注：这个 package 特别适用于需要处理中英文混合表格的中文 org-mode 用
;; 户。

;; ** 基本原理

;; cnfonts 的核心很简单，就是让中文字体和英文字体使用不同的字号，从而
;; 实现中英文对齐。

;; ** 使用特点

;; cnfonts 添加了许多辅助工具，使配置和调节字体和字号的工作更加简便快
;; 捷，它有几个优点：

;; 1. 安装即用：cnfonts 内置字体 fallback 功能，只需安装，就能够配置中
;;    文字体和英文字体，让中文可以 *正确* 显示（但未必完美），不会因为
;;    Emacs 配置中指定的字体不存在而报错。

;; 2. 设置方便：cnfonts 自带一个 profile 文件调整工具，这个工具有直观
;;    的图形界面，可以让用户设置字体名称和字体大小，分分钟实现中文字体
;;    和英文字体的等宽对齐。

;; ** 下载安装

;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET cnfonts RET
;; 3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：

;;    #+BEGIN_EXAMPLE
;;    (require 'cnfonts)
;;    ;; 让 cnfonts 随着 Emacs 自动生效。
;;    (cnfonts-enable)
;;    #+END_EXAMPLE

;; ** 配置使用
;; *** 最简单的用法（懒人必备）

;; 通过下面几个命令，用户可以 *快速* 了解 cnfonts 的大部分功能，而不需
;; 要阅读整篇文档，如果用户想深入了解 cnfonts 或者自定义一些特殊的功能，
;; 阅读整篇文档是逃不开的。

;; | 命令                      | 功能         |
;; |---------------------------+--------------|
;; | cnfonts-edit-profile      | 调整字体设置 |
;; | cnfonts-increase-fontsize | 增大字号     |
;; | cnfonts-decrease-fontsize | 减小字号     |

;; *** profile 的概念

;; profile 代表了一套字体配置，cnfonts 使用 profile 的概念，来维护多套
;; 字体配置，从而实现特定的环境使用特定的字体配置，比如：在编程时使用
;; “Consolas + 微米黑”，在阅读文章时使用 “PragmataPro + 黑体”，等等。

;; 每一个 profile 都对应一个 emacs-lisp 文件, 保存在
;; `cnfonts-directory' 对应的目录中, 这些文件包含了英文字体设置，中文
;; 字体设置以及中文字体大小等。

;; *** profile 命名与切换

;; cnfonts 默认使用三个 profile: profile1, profile2 和 profile3, 如果
;; 想使用其它有意义的名称，可以设置:

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

;; 如果 *当前使用* 的字体不符合使用习惯，用户可以运行
;; `cnfonts-edit-profile'命令来调整 *当前* profile,这个命令会弹出一个
;; 图形化界面，类似：

;; [[./snapshots/cnfonts-ui-1.png]]
;; [[./snapshots/cnfonts-ui-2.png]]
;; [[./snapshots/cnfonts-ui-3.png]]
;; [[./snapshots/cnfonts-ui-4.png]]
;; [[./snapshots/cnfonts-ui-5.png]]
;; [[./snapshots/cnfonts-ui-6.png]]
;; [[./snapshots/cnfonts-ui-7.png]]

;; 注1: 配置完成后，有可能需要重启 Emacs, 参考：
;; http://debbugs.gnu.org/db/17/1785.html

;; *** 使用 cnfonts-regenerate-profile 重置 profile

;; `cnfonts-regenerate-profile' 命令会使用 cnfonts 自带的 fallback 信
;; 息，覆盖需要 *重置* 的 profile, 这个 profile 原来的内容将丢失，请紧
;; 慎使用！

;; *** 调整字体大小
;; `cnfonts' 使用下述两个命令调整字体大小:

;; | Command                   | Help         |
;; |---------------------------+--------------|
;; | cnfonts-increase-fontsize | 增大字体大小 |
;; | cnfonts-decrease-fontsize | 减小字体大小 |

;; 注意：在调整字体大小的同时，字号信息也会保存到 `cnfonts-directory'
;; 目录下`cnfonts-config-filename' 对应的文件中。

;; [[./snapshots/cnfonts-increase-and-decrease-fontsize.gif]]

;; *** 使用 cnfonts-use-system-type

;; 有些用户希望将 profile 配置文件做为自己的 Emacs 配置，在不同的计算
;; 机上同步和管理，我建议这些用户将 `cnfonts-use-system-type'设置为 t,
;; 这样，相同名称的 profile 在不同的操作系统下，保存的位置也不同，可以
;; 避免 profile 冲突。

;; *** 让 cnfonts 随着 Emacs 自动启动

;; `cnfonts-enable' 命令可以让 cnfonts 随着 Emacs 自动启动，这个命令将
;; `cnfonts-set-font-with-saved-fontsize' 添加到下面两个 hook:

;; 1. `after-make-frame-functions'
;; 2. `window-setup-hook'

;; 用户也可以手动运行 `cnfonts-set-font-with-saved-fontsize' 来让
;; cnfonts 生效。

;; *** cnfonts 与 org-mode 配合使用

;; 许多用户使用 org-mode 时，习惯让不同的标题，使用的字体大小也不同，
;; 这个特性需要用户设置：

;; #+BEGIN_EXAMPLE
;; (setq cnfonts-use-face-font-rescale t)
;; #+END_EXAMPLE

;; 注：这个功能不能在 window 系统下使用，它会让对齐功能失效，Linux 下
;; 这个功能 *一般* 可以使用，Mac 系统未测试，同学可以亲自试一试。

;; *** cnfonts 高级功能

;; **** 设置一些不常用汉字字符的字体

;; #+BEGIN_EXAMPLE
;; (push '(#x3400 . #x4DFF) cnfonts-ornaments)
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
;;    `cnfonts-personal-fontnames' , 其结构与
;;    `cnfonts--fontnames-fallback'一样。
;; 2. 使用命令: `describe-char' 可以了解光标处字符使用什么字体。
;; 3. 在 scratch 中写一行 elisp 代码：

;;    #+BEGIN_EXAMPLE
;;    (cl-prettyprint (font-family-list))
;;    #+END_EXAMPLE

;;    执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很
;;    有用的技巧。

;; 4. Windows 用户 (特别是 Windows XP 用户) 可以安装 MacType 软件来优
;;    化字体显示效果，推荐使用。
;; 5. Mac 用户配置 profile 文件的时候，偶尔会遇到 'C-c C-c' 刷新缓慢的
;;    问题，这可能是 ext-b 字体缺失引起的，建议安装 ext-b 字体试试。
;;    1. Ext-B字符列表: https://cdo.wikipedia.org/wiki/Wikipedia:Unicode%E6%93%B4%E5%B1%95%E6%BC%A2%E5%AD%97
;;    2. HanaMinB 下载地址: https://osdn.jp/projects/hanazono-font/downloads/62072/hanazono-20141012.zip/
;; 6. 字体设置和 coding 设置也有关系，如果 cnfonts 的行为很奇怪，又找
;;    不到确切原因，可以参考：
;;    https://github.com/tumashu/cnfonts/issues/54#issuecomment-246228904

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
  :type '(repeat string))

(defcustom cnfonts-default-fontsize 12.5
  "Default cnfonts fontsize."
  :type 'number)

(defcustom cnfonts-directory (locate-user-emacs-file "cnfonts/")
  "Directory, cnfonts config file and profiles will be stored in."
  :type 'directory)

(defcustom cnfonts-config-filename "cnfonts.conf"
  "Filename of cnfonts config file.
It record the current profile and profile fontsize."
  :type 'string)

(defcustom cnfonts-use-system-type nil
  "构建 profile 文件所在的目录时，是否考虑当前的 `system-type'.

假设当前系统为 Linux, 当这个选项设置为 t 后，profile1 文件的路径，
将从 'DIR/profile1.el' 转为 'DIR/SYSTEM-TYPE/profile.el'"
  :type 'boolean)

(defcustom cnfonts-keep-frame-size t
  "在调整字体的时候，是否保持当前 frame 大小不变."
  :type 'boolean)

(defcustom cnfonts-disable-bold nil
  "是否禁用英文粗体."
  :type 'boolean)

(defcustom cnfonts-disable-italic nil
  "是否禁用英文斜体."
  :type 'boolean)

(defcustom cnfonts-disable-bold-italic nil
  "是否禁用英文粗斜体."
  :type 'boolean)

(defcustom cnfonts-save-current-profile t
  "是否保存将当前 profile 的信息."
  :type 'boolean)

(defcustom cnfonts-use-face-font-rescale nil
  "是否通过设定 `face-font-rescale-alist' 来达到中英文对齐.

在 window 平台下，将这个变量设置为 t 会导致 cnfonts
字体对齐功能失效，在大多数 linux 平台下这个功能都可以正常使用。"
  :type 'boolean)

(defcustom cnfonts-use-display-property-alist
  '((org-mode . org-at-table-p)
    (markdown-mode . markdown-table-at-point-p)
    (org-agenda-mode . t)
    (gnus-summary-mode . t)
    (cnfonts-ui-mode . t))
  "设置是否使用 display property 来实现中英文对齐, 比如 min-width。

这个选项设置之后，中英文对齐的操作余地变大，理论上只要中文字体的
宽度不超过英文字体宽度的两倍，就可以实现对齐，但由于在
`post-command-hook' 中添加了一个命令, 所以会一些命令的性能产生少
许影响。

另外，这个选项要求 emacs 版本不小于 29."
  :type 'sexp)

(defcustom cnfonts-set-font-finish-hook nil
  "A hook, by which user can set additional fonts."
  :type 'hook)

(defvar cnfonts-use-cache nil
  "是否使用缓存.

cnfont 的设置都保存在文件中，在默认情况下，每次读取 profile
和 config 都需要从硬盘上读取相关文件，如果这个选项设置为 t，
那么 cnfonts 会缓存上次读取的结果，从而加快运行，但这个选项
只适用下面两种情况：

1. 加速 Emacs 启动 （启动的时候，cnfonts 的配置是不会改变的）
2. cnfonts 的配置长期不变的用户。

其他情况下，将这个变量设置为t，很有可能会让 cnfonts 调整字体的
功能失效，请谨慎使用。")

(defvar cnfonts--current-profile-cache nil
  "这个变量用于保存 profile 设置的缓存.")

(defvar cnfonts--current-profile-name nil
  "Current profile name used by cnfonts.")

(defvar cnfonts--profiles-fontsize nil
  "用来保存每一个 profile 使用 `cnfonts--fontsizes-fallback' 中第几个字号组合.")

(defvar cnfonts--read-config-file-p nil)

(defconst cnfonts--fontsizes-fallback
  '((9    10.5  10.5  9    9   )
    (10   12.0  12.0  10   10  )
    (11.5 13.5  13.5  11.5 11.5)
    (12.5 15.0  15.0  12.5 12.5)
    (14   16.5  16.5  14   14.5)
    (15   18.0  18.0  15   15  )
    (16   19.5  19.5  16   16  )
    (18   21.0  21.0  18   18  )
    (20   24.0  24.0  20   20  )
    (22   25.5  25.5  22   22  )
    (24   28.5  28.5  24   24  )
    (26   31.5  31.5  26   26  )
    (28   33.0  33.0  28   28  )
    (30   36.0  36.0  30   30  )
    (32   39.0  39.0  32   32  ))
  "一个列表，每一个元素都有类似结构：(英文字号 中文字号 EXT-B字体字号 Symbol字体字号).")

(defcustom cnfonts-personal-fontnames nil
  "用户自己维护的字体列表，其结构与 `cnfonts--fontnames-fallback' 一致."
  :group 'cnfonts
  :type '(choice (const :tag "None" nil)
                 (list (repeat :tag "English fonts" string)
                       (repeat :tag "Chinese fonts" string)
                       (repeat :tag "Ext-B fonts" string))))

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
     "ProggyCleanTT" "ProggyTinyTT" "Iosevka Term" "Inconsolata-dz" "American Typewriter"
     "Menlo" "Ubuntu Mono" "Anka/Coder Condensed" "Fantasque Sans Mono" "M+ 1m" "CamingoCode"
     "Office Code Pro" "Roboto Mono" "Input Mono" "Courier Prime Code" "NanumGothicCoding"
     "Monoid" "Edlo" "Iosevka" "Mononoki" "Robot Mono" "Fantasque" "Fira Code" "Go Mono"
     "Noto Sans Mono CJK" "InputMonoCompressed" "Hasklig" "Terminus" "FantasqueSansMono"
     "AnonymousPro" "3270" "Arimo" "D2Coding" "Inconsolata-g" "ProFont for Powerline"
     "Meslo" "Meslo Dotted" "Noto Mono" "Symbol Neu" "Tinos" "Space Mono" "SFMono Nerd Font")
    ("微软雅黑" "Noto Sans S Chinese Regular" "Microsoft Yahei" "Microsoft YaHei Mono"
     "Microsoft_Yahei" "Ubuntu Mono" "文泉驿等宽微米黑" "文泉驿等宽正黑" "黑体" "Source Han Sans SC"
     "Source Han Serif SC" "思源黑体 CN Regular" "思源黑体 CN Medium" "思源黑体 CN Normal" "思源宋体 CN"
     "思源宋体 CN Medium" "思源宋体 CN SemiBold" "Hiragino Sans GB"  "文泉驿正黑" "文泉驿点阵正黑"
     "SimHei" "SimSun" "NSimSun" "FangSong" "KaiTi" "FangSong_GB2312" "KaiTi_GB2312" "LiSu"
     "YouYuan" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "STXihei" "STKaiti"
     "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti"
     "STXingkai" "STXinwei" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋"
     "华文行楷" "华文彩云" "华文仿宋" "华文中宋" "华文隶书" "华文行楷" "华文细黑" "华文琥珀"
     "华文楷体" "华文新魏" "微软雅黑")
    ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB"
     "Hanazono Mincho" "Hanazono Mincho A" "Hanazono Mincho B"
     "Hanazono Mincho C" "Hanazono Mincho Ex" "Hanazono Mincho Ex A1"
     "Hanazono Mincho Ex A2" "Hanazono Mincho Ex B" "Hanazono Mincho Ex C"
     "Hanazono Mincho I")
    ("Segoe UI Symbol" "Symbola" "Standard Symbols L")
    ("NanumGothic" "Arial Unicode MS" "MS Gothic" "Lucida Sans Unicode")))

(defcustom cnfonts-ornaments
  (list
   ;; spacemacs window numbers
   '(#x2776 . #x2793)
   ;; spacemacs mode-line circled letters
   '(#x24b6 . #x24fe)
   ;; spacemacs mode-line additional characters
   '(#x2295 . #x22a1)
   ;; spacemacs new version lighter
   '(#x2190 . #x2200))
  "字符区间组成的列表，emacs 社区配置来美化和点缀。"
  :type 'sexp)

(defconst cnfonts--profile-comment-1 "
;;; `cnfonts--custom-set-fontsnames' 的结构与 `cnfonts--fontnames-fallback' 相同。")

(defconst cnfonts--profile-comment-2 "
;;; `cnfonts--custom-set-fontsizes' 的结构与 `cnfonts--fontsizes-fallback' 相同。")

(defvar cnfonts--minibuffer-echo-string nil)

(defvar cnfonts--custom-set-fontnames nil
  "*专用* 变量，只用与 cnfonts 的 profile 文件.
这些 profile 文件保存在 `cnfonts-directory' 对应的目录中。在其它地方
设置这个变量没有任何用处！")

(defvar cnfonts--custom-set-fontsizes nil
  "*专用* 变量，只用与 cnfonts 的 profile 文件.
这些 profile 文件保存在 `cnfonts-directory' 对应的目录中。在其它地方
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
            (concat (file-name-as-directory cnfonts-directory)
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
         (if (member cnfonts--current-profile-name cnfonts-profiles)
             cnfonts--current-profile-name
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

(defun cnfonts--save-config-file (profile-name &optional fontsize)
  "Save PROFILE-NAME and FONTSIZE into config file."
  (when fontsize
    (if (assoc profile-name cnfonts--profiles-fontsize)
        (setf (cdr (assoc profile-name cnfonts--profiles-fontsize)) fontsize)
      (push `(,profile-name . ,fontsize) cnfonts--profiles-fontsize)))
  (with-temp-file (cnfonts--return-config-file-path)
    (when cnfonts-save-current-profile
      (prin1 (list cnfonts--current-profile-name) (current-buffer)))
    (prin1 cnfonts--profiles-fontsize (current-buffer))))

(defun cnfonts--read-config-file ()
  "Read cnfonts's config file."
  (unless (and cnfonts-use-cache
               cnfonts--read-config-file-p)
    (let ((save-file (cnfonts--return-config-file-path)))
      (if (file-readable-p save-file)
          (with-temp-buffer
            (insert-file-contents save-file)
            (ignore-errors
              (when cnfonts-save-current-profile
                (setq cnfonts--current-profile-name (car (read (current-buffer)))))
              (setq cnfonts--profiles-fontsize (read (current-buffer)))))))))

(defun cnfonts--get-profile-fontsize (profile-name)
  "Get the font size info from profile which name is PROFILE-NAME."
  (let ((fontsize (cdr (assoc profile-name cnfonts--profiles-fontsize))))
    (min (max (or fontsize 12.5) 9) 32)))

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
  (if (and cnfonts-use-cache
           cnfonts--current-profile-cache)
      cnfonts--current-profile-cache
    (let ((file (cnfonts--get-current-profile)))
      (if (file-readable-p file)
          (progn (when (load (expand-file-name file) nil t)
                   (cnfonts-message t "[cnfonts]: load %S successfully." (cnfonts--get-current-profile t)))
                 (setq cnfonts--current-profile-cache
                       (list
                        (if cnfonts--custom-set-fontnames
                            (cnfonts--merge-fontname-list cnfonts--custom-set-fontnames
                                                          cnfonts-personal-fontnames
                                                          cnfonts--fontnames-fallback)
                          (cnfonts--merge-fontname-list cnfonts-personal-fontnames
                                                        cnfonts--fontnames-fallback))

                        (mapcar
                         (lambda (fontsizes)
                           ;; 添加 symbol 和点缀字符的字体支持之后，做的兼容。
                           (if (< (length fontsizes) 5)
                               `(,@fontsizes ,(car fontsizes) ,(car fontsizes))
                             fontsizes))
                         (or cnfonts--custom-set-fontsizes
                             cnfonts--fontsizes-fallback)))))
        (setq cnfonts--current-profile-cache
              (list cnfonts--fontnames-fallback
                    cnfonts--fontsizes-fallback))))))

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
            (,@(nth 2 list1) ,@(nth 2 list2) ,@(nth 2 list3))
            (,@(ignore-errors (nth 3 list1))
             ,@(ignore-errors (nth 3 list2))
             ,@(nth 3 list3))
            (,@(ignore-errors (nth 4 list1))
             ,@(ignore-errors (nth 4 list2))
             ,@(nth 4 list3)))))

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

(defun cnfonts--get-fontsizes (&optional fontsize)
  "获取 FONTSIZE 对应的 fontsize-list."
  (let ((fontsizes-list (car (cdr (cnfonts--read-profile)))))
    (unless (file-exists-p (cnfonts--get-current-profile))
      (cnfonts-message t "如果中英文不能对齐，请运行`cnfonts-edit-profile'编辑当前profile。"))
    (when (numberp fontsize)
      (assoc fontsize fontsizes-list #'=))))

(defun cnfonts--set-font (fontsizes-list)
  "根据 FONTSIZES-LIST 调整当前 frame 使用的字体.
当全局变量 `cnfonts-keep-frame-size'设置为 t 时，调整字体时保持当前 frame 大小不变。"
  (let ((frame (selected-frame))
        height width)

    (if (not cnfonts-use-face-font-rescale)
        (setq face-font-rescale-alist nil)
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
         (symbol-fontname (nth 3 valid-fonts))
         (ornament-fontname (nth 4 valid-fonts))

         (english-main-short-fontname (nth 0 valid-short-fontnames))
         (chinese-main-short-fontname (nth 1 valid-short-fontnames))
         (chinese-extra-short-fontname (nth 2 valid-short-fontnames))

         (english-main-fontsize (cnfonts--float (nth 0 fontsizes-list)))
         (chinese-main-fontsize (cnfonts--float (nth 1 fontsizes-list)))
         (chinese-extra-fontsize (cnfonts--float (nth 2 fontsizes-list)))
         (symbol-fontsize (cnfonts--float (nth 3 fontsizes-list)))
         (ornament-fontsize (cnfonts--float (nth 4 fontsizes-list)))

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
         (symbol-fontspec
          (when symbol-fontname
            (font-spec :name symbol-fontname
                       :size symbol-fontsize
                       :weight 'normal
                       :slant 'normal)))
         (ornament-fontspec
          (when ornament-fontname
            (font-spec :name ornament-fontname
                       :size ornament-fontsize
                       :weight 'normal
                       :slant 'normal)))

         (chinese-main-fontspec
          (when chinese-main-fontname
            (font-spec :name chinese-main-fontname
                       :size chinese-main-fontsize
                       :weight 'normal
                       :slant 'normal)))
         (chinese-extra-fontspec
          (when chinese-extra-fontname
            (font-spec :name chinese-extra-fontname
                       :size (or chinese-extra-fontsize
                                 chinese-main-fontsize)
                       :weight 'normal
                       :slant 'normal))))

    (when (fontp english-main-fontspec)
      ;; 设置英文字体。
      (set-face-attribute
       'default nil :font english-main-fontspec)
      ;; 设置英文粗体。
      (if cnfonts-disable-bold
          (set-face-font 'bold english-main-fontspec)
        (if (fontp english-bold-fontspec)
            (set-face-font 'bold english-bold-fontspec)
          (cnfonts-message t "[cnfonts]: %S 对应的粗体没有找到，不作处理！"
                           english-main-short-fontname)))

      ;; 设置英文斜体。
      (if cnfonts-disable-italic
          (set-face-font 'italic english-main-fontspec)
        (if (fontp english-italic-fontspec)
            (set-face-font 'italic english-italic-fontspec)
          (cnfonts-message t "[cnfonts]: %S 对应的斜体没有找到，不作处理！"
                           english-main-short-fontname)))

      ;; 设置英文粗斜体。
      (if cnfonts-disable-bold-italic
          (set-face-font 'bold-italic english-main-fontspec)
        (if (fontp english-bold-italic-fontspec)
            (set-face-font 'bold-italic english-bold-italic-fontspec)
          (cnfonts-message t "[cnfonts]: %S 对应的粗斜体没有找到，不作处理！"
                           english-main-short-fontname))))

    ;; 设置中文字体，注意，不要使用 'unicode charset,
    ;; 否则上面的英文字体设置将会失效。
    (when (fontp chinese-main-fontspec)
      (dolist (charset '(kana han cjk-misc bopomofo gb18030))
        (set-fontset-font "fontset-default" charset chinese-main-fontspec)))

    ;; 设置 symbol 字体。
    (when (fontp symbol-fontspec)
      (set-fontset-font "fontset-default" 'symbol symbol-fontspec nil 'prepend))

    ;; 设置 fallback 字体，用于显示不常用的字符。
    (when (fontp chinese-extra-fontspec)
      (set-fontset-font "fontset-default" nil chinese-extra-fontspec nil 'prepend))

    ;; 设置点缀字符的字体。
    (when (fontp ornament-fontspec)
      (dolist (charset cnfonts-ornaments)
        (set-fontset-font "fontset-default" charset ornament-fontspec nil 'prepend)))

    (setq cnfonts--minibuffer-echo-string
          (format "[%s]: 英文字体: %s-%.1f，中文字体: %s, EXTB字体：%s"
                  (cnfonts--get-current-profile t)
                  (or english-main-short-fontname "无") english-main-fontsize
                  (or chinese-main-short-fontname "无")
                  (or chinese-extra-short-fontname "无")))
    (message "")))

(defun cnfonts--next-fontsize (n)
  "使用下N个字号."
  (if (not (display-graphic-p))
      (cnfonts-message t "cnfonts 不支持 emacs 终端模式！")
    (let* ((steps (mapcar #'car cnfonts--fontsizes-fallback))
           (profile-name (cnfonts--get-current-profile t))
           (profile-fontsize (cnfonts--get-profile-fontsize profile-name))
           (index (+ (cl-position profile-fontsize steps :test #'=) n))
           (fontsizes-list (cnfonts--get-fontsizes (nth index steps))))
      (when fontsizes-list
        (cnfonts--set-font fontsizes-list)
        (cnfonts--save-config-file profile-name (car fontsizes-list))
        (cnfonts-message t cnfonts--minibuffer-echo-string)))))

(define-obsolete-function-alias
  'cnfonts-set-font-with-saved-step
  'cnfonts-set-font-with-saved-fontsize
  "1.0")

;;;###autoload
(defun cnfonts-set-font-with-saved-fontsize (&optional frame)
  "使用已经保存的字号设置字体.
如果 FRAME 是 non-nil, 设置对应的 FRAME 的字体。"
  (interactive)
  (cnfonts--read-config-file)
  (let* ((profile-name (cnfonts--get-current-profile t))
         (profile-fontsize (cnfonts--get-profile-fontsize profile-name))
         (fontsizes-list (cnfonts--get-fontsizes profile-fontsize)))
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

(defun cnfonts-reset-fontsize ()
  "使用 `cnfonts-default-fontsize' 重置字号."
  (interactive)
  (cnfonts--next-fontsize 0))

;;;###autoload
(defun cnfonts-decrease-fontsize ()
  "Cnfonts 减小字体."
  (interactive)
  (cnfonts--next-fontsize -1))

;;;###autoload
(defun cnfonts-increase-fontsize ()
  "Cnfonts 增大字体."
  (interactive)
  (cnfonts--next-fontsize 1))

(defun cnfonts--select-profile (profile-name)
  "选择 PROFILE-NAME."
  (if (member profile-name cnfonts-profiles)
      (progn (setq cnfonts--current-profile-name profile-name)
             (cnfonts--save-config-file profile-name)
             (cnfonts-set-font-with-saved-fontsize))
    (cnfonts-message t "%s doesn't exist." profile-name)))

;;;###autoload
(defun cnfonts-switch-profile ()
  "切换 cnfonts profile."
  (interactive)
  (let ((profile (completing-read "Set cnfonts profile to:" cnfonts-profiles)))
    (cnfonts--select-profile profile)))

;;;###autoload
(defun cnfonts-next-profile (&optional _)
  "选择下一个字体设置 profile."
  (interactive)
  (let ((profiles cnfonts-profiles)
        (current-profile (cnfonts--get-current-profile t))
        next-profile)
    (setq next-profile
          (or (cadr (member current-profile profiles))
              (car profiles)))
    (when next-profile
      (setq cnfonts--current-profile-name next-profile)
      (cnfonts--save-config-file next-profile))
    (when (display-graphic-p)
      (cnfonts-set-font-with-saved-fontsize))
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
(defun cnfonts-regenerate-profile ()
  "重新生成当前 profile."
  (interactive)
  (let ((profile-name (completing-read "Regenerate profile: " cnfonts-profiles)))
    (if (yes-or-no-p (format "Regenerate (%s)? " profile-name))
        (cnfonts--save-profile cnfonts--fontnames-fallback
                               cnfonts--fontsizes-fallback profile-name)
      (cnfonts-message t "Ignore regenerate profile!"))))

(defun cnfonts-set-font-first-time (&optional frame)
  "Emacs 启动后，第一次设置 FRAME 字体使用的函数.

这个函数会使用 cnfonts 缓存机制，设置字体速度较快。"
  (let ((cnfonts-use-cache t))
    (cnfonts-set-font-with-saved-fontsize frame)))

(defun cnfonts-use-display-property ()
  "Add display property :min-width '(2.0) to all Chinese Char."
  (interactive)
  (ignore-errors
    (unless (version< emacs-version "29.0.50")
      (dolist (window (window-list))
        (with-selected-window window
          (let ((buffer (window-buffer window))
                func)
            (with-current-buffer buffer
              (when (setq func (cdr (assq major-mode cnfonts-use-display-property-alist)))
                (let* ((n1 (window-start window))
                       (n2 (min (window-end window)
                                (+ n1 (* (window-width)
                                         (window-height)))))
                       (inhibit-read-only t)
                       (flag t))
                  (save-excursion
                    (with-silent-modifications
                      (while flag
                        (goto-char n1)
                        (if (< n1 n2)
                            (let ((str (buffer-substring n1 (+ n1 1))))
                              (when (and str
                                         (stringp str)
                                         (= (string-width str) 2)
                                         (not (equal (get-display-property n1 'min-width) '(2.0)))
                                         (or (eq func t)
                                             (and (functionp func)
                                                  (funcall func))))
                                (add-display-text-property n1 (+ n1 1) 'min-width (list 2.0))))
                          (setq flag nil))
                        (setq n1 (+ n1 1))))))))))))))

;;;###autoload
(defun cnfonts-enable ()
  "运行这个函数，可以让 Emacs 启动的时候就激活 cnfonts."
  (interactive)
  (setq cnfonts--enabled-p t)
  (add-hook 'after-make-frame-functions #'cnfonts-set-font-first-time)
  (add-hook 'window-setup-hook #'cnfonts-set-font-first-time)
  (when (and cnfonts-use-display-property-alist
             (not (version< emacs-version "29.0.50")))
    (add-hook 'post-command-hook #'cnfonts-use-display-property)))

;;;###autoload
(defun cnfonts-disable ()
  "清除与 cnfonts 相关的 hook 设定."
  (interactive)
  (setq cnfonts--enabled-p nil)
  (remove-hook 'after-make-frame-functions #'cnfonts-set-font-first-time)
  (remove-hook 'window-setup-hook #'cnfonts-set-font-first-time)
  (remove-hook 'post-command-hook #'cnfonts-use-display-property))

(define-obsolete-function-alias 'cnfonts-set-spacemacs-fallback-fonts 'ignore "1.0")

;; * Footer
(provide 'cnfonts)

;;; cnfonts.el ends here
