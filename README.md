- [cnfonts README](#org5887b9d)
  - [简介](#orgf6ad03e)
  - [基本原理](#orgfad3c59)
  - [使用特点](#org7a07fef)
  - [下载安装](#org8dffa7c)
  - [配置使用](#orgb5353dc)
    - [最简单的用法（懒人必备）](#org4e14544)
    - [profile 的概念](#orga2a49f3)
    - [profile 命名与切换](#org59c2a49)
    - [使用 cnfonts-edit-profile 命令调整 profile](#org765ec22)
    - [使用 cnfonts-edit-profile-without-ui 命令编辑 profile](#orgf5d7da9)
    - [使用 cnfonts-regenerate-profile 重置 profile](#org0fa86d7)
    - [调整字体大小](#orgb511c67)
    - [使用 cnfonts-use-system-type](#org1e75c31)
    - [让 cnfonts 随着 Emacs 自动启动](#org21041f6)
    - [cnfonts 与 org-mode 配合使用](#org6bd5448)
    - [使用 cnfonts 生成 elisp 字体配置片断](#org9ecbddf)
    - [cnfonts 高级功能](#org3216725)
  - [Tips](#orgdd8685e)
  - [参考文章](#org9f99658)


<a id="org5887b9d"></a>

# cnfonts README


<a id="orgf6ad03e"></a>

## 简介

注意：cnfonts 原来叫： chinese-fonts-setup, 一开始使用三个词的首字 母组成的字符串 "cfs-" 做为包的前缀，但不幸和 gnu 的项目 cfs.el 冲突， 所以将包的前缀更改为 "cnfonts". chinese-fonts-setup 将做为 cnfonts 的别名使用。

cnfonts 是一个 Emacs 中英文字体配置工具。可以比较方便地实 现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。

注： 这个 package 特别适用于需要处理中英文混合表格的中文 org-mode 用户。


<a id="orgfad3c59"></a>

## 基本原理

cnfonts 的核心很简单，就是让中文字体和英文字体使用不同的字 号，从而实现中英文对齐，它和下面的样例代码原理是一样的：

    (set-frame-font "-unknown-PragmataPro-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset (font-spec :family "Microsoft Yahei" :size 16)))


<a id="org7a07fef"></a>

## 使用特点

cnfonts 添加了许多辅助工具，使配置和调节字体和字号的工作更 加简便快捷，它有几个优点：

1.  安装即用：cnfonts 内置字体 fallback 功能，只需安装，就能 够配置中文字体和英文字体，让中文可以 **正确** 显示（但未必完美），不会 因为 Emacs 配置中指定的字体不存在而报错。
2.  设置方便：cnfonts 自带一个 profile 文件调整工具，这个工具 有直观的图形界面，可以让用户设置字体名称和字体大小，分分钟实现中文字 体和英文字体的等宽对齐。


<a id="org8dffa7c"></a>

## 下载安装

1.  配置melpa源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET cnfonts RET
3.  在emacs配置文件中（比如: ~/.emacs）添加如下代码：

        (require 'cnfonts)
        ;; 让 cnfonts 随着 Emacs 自动生效。
        ;; (cnfonts-enable)
        ;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。
        ;; (cnfonts-set-spacemacs-fallback-fonts)


<a id="orgb5353dc"></a>

## 配置使用


<a id="org4e14544"></a>

### 最简单的用法（懒人必备）

通过下面几个命令，用户可以 **快速** 了解 cnfonts 的大部分功能， 而不需要阅读整篇文档，如果用户想深入了解 cnfonts 或者自定义 一些特殊的功能，阅读整篇文档是逃不开的。

| 命令                      | 功能   |
|------------------------- |------ |
| cnfonts-edit-profile      | 调整字体设置 |
| cnfonts-increase-fontsize | 增大字号 |
| cnfonts-decrease-fontsize | 减小字号 |


<a id="orga2a49f3"></a>

### profile 的概念

profile 代表了一套字体配置，cnfonts 使用 profile 的概念， 来维护多套字体配置，从而实现特定的环境使用特定的字体配置，比如：在编程 时使用 “Consolas + 微米黑”，在阅读文章时使用 “PragmataPro + 黑体”， 等等。

每一个 profile 都对应一个 emacs-lisp 文件, 保存在 \`cnfonts-profiles-directory' 目录中, 这些文件包含了英文字体设置，中文字体设置以及中文字体大小， 其结构类似：

    (setq cnfonts--custom-set-fontnames
          '(("PragmataPro" "Ubuntu Mono" "DejaVu Sans Mono")
            ("文泉驿等宽微米黑" "Ubuntu Mono" "隶书" "新宋体")
            ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB")))

    (setq cnfonts--custom-set-fontsizes
          '((9    9.0  9.5 )
            (10   11.0 11.0)
            (11.5 12.5 12.5)
            (12.5 13.5 13.5)
            (14   15.0 15.0)
            (16   17.0 17.0)
            (18   18.0 18.0)
            (20   21.0 21.0)
            (22   23.0 23.0)
            (24   25.5 25.5)
            (26   27.0 27.0)
            (28   29.0 29.0)
            (30   32.0 32.0)
            (32   33.0 33.0)))


<a id="org59c2a49"></a>

### profile 命名与切换

cnfonts 默认使用三个 profile: profile1, profile2 和 profile3, 如果想使用其它有意义的名称，可以设置:

    (setq cnfonts-profiles
        '("program" "org-mode" "read-book"))

cnfonts 使用下面两个命令来切换 profile ：

| Command                | Help            |
|---------------------- |--------------- |
| cnfonts-switch-profile | 选择并切换 profile |
| cnfonts-next-profile   | 直接切换到下一个profile |


<a id="org765ec22"></a>

### 使用 cnfonts-edit-profile 命令调整 profile

如果 **当前使用** 的字体不符合使用习惯，用户可以运行 \`cnfonts-edit-profile' 命令来调整 **当前** profile,这个命令会弹出一个图形化界面，类似：

![img](./snapshots/cnfonts-ui-1.png) ![img](./snapshots/cnfonts-ui-2.png) ![img](./snapshots/cnfonts-ui-3.png) ![img](./snapshots/cnfonts-ui-4.png) ![img](./snapshots/cnfonts-ui-5.png) ![img](./snapshots/cnfonts-ui-6.png) ![img](./snapshots/cnfonts-ui-7.png)


<a id="orgf5d7da9"></a>

### 使用 cnfonts-edit-profile-without-ui 命令编辑 profile

除了使用 \`cnfonts-edit-profile' , **有经验** 的用户也可以使用 \`cnfonts-edit-profile-without-ui' 命令，直接编辑当前 profile 文件， 两个命令的效果是一样的。

在编辑的过程中，用户可以使用下面三个命令 **快速** 的测试编辑效果：

| Key     | Command                             | Help                |
|------- |----------------------------------- |------------------- |
| C-c C-c | cnfonts-test-fontsizes-at-point     | 查看字体显示效果    |
| C-up    | cnfonts-increment-fontsize-at-point | 增大光标下字号的大小，同时显示对齐效果 |
| C-down  | cnfonts-decrement-fontsize-at-point | 减小光标下字号的大小，同时显示对齐效果 |

注1: 不建议 cnfonts 新用户使用这种方式

注2: 配置完成后，有可能需要重启 Emacs, 参考：<http://debbugs.gnu.org/db/17/1785.html>


<a id="org0fa86d7"></a>

### 使用 cnfonts-regenerate-profile 重置 profile

\`cnfonts-regenerate-profile' 命令会使用 cnfonts 自带的 fallback 信息，覆盖需要 **重置** 的 profile, 这个 profile 原来的 内容将丢失，请谨慎使用！


<a id="orgb511c67"></a>

### 调整字体大小

\`cnfonts' 使用下述两个命令调整字体大小:

| Command                   | Help   |
|------------------------- |------ |
| cnfonts-increase-fontsize | 增大字体大小 |
| cnfonts-decrease-fontsize | 减小字体大小 |

注意：在调整字体大小的同时，字号信息也会保存到 \`cnfonts-directory' 目录下 \`cnfonts-config-filename' 对应的文件中。

![img](./snapshots/cnfonts-increase-and-decrease-fontsize.gif)


<a id="org1e75c31"></a>

### 使用 cnfonts-use-system-type

有些用户希望将 profile 配置文件做为自己的 Emacs 配置，在不同 的计算机上同步和管理，我建议这些用户将 \`cnfonts-use-system-type' 设置为 t, 这样，相同名称的 profile 在不同的操作系统下，保存的 位置也不同，可以避免 profile 冲突。


<a id="org21041f6"></a>

### 让 cnfonts 随着 Emacs 自动启动

\`cnfonts-enable' 命令可以让 cnfonts 随着 Emacs 自动启动，这个命令将 \`cnfonts-set-font-with-saved-step' 添加到 下面两个 hook:

1.  \`after-make-frame-functions'
2.  \`window-setup-hook'

用户也可以手动运行 \`cnfonts-set-font-with-saved-step' 来让 cnfonts 生效。


<a id="org6bd5448"></a>

### cnfonts 与 org-mode 配合使用

许多用户使用 org-mode 时，习惯让不同的标题，使用的字体大小也不同，这个 特性需要用户设置：

    (setq cnfonts-use-face-font-rescale t)

注：这个功能不能在 window 系统下使用，它会让对齐功能失效，Linux 下 这个功能 **一般** 可以使用，Mac 系统未测试，同学可以亲自试一试。


<a id="org9ecbddf"></a>

### 使用 cnfonts 生成 elisp 字体配置片断

有些用户觉得 cnfonts **太过厚重** , 他们喜欢使用简单的 方式来配置字体，这些用户可以了解一下 \`cnfonts-insert-fonts-configure' 命令，这个命令可以根据 cnfonts 的设置自动生成一个 "字体配置 elisp 片断", 并插入光标处，将这个片断写入 .emacs 文件 后，就不需要启动 cnfonts 来设置字体了。


<a id="org3216725"></a>

### cnfonts 高级功能

cnfonts **仅仅** 设置英文，中文和 EXT-B 字体，不处理 其它字符的字体，比如：symbol 字符，但 cnfonts 可以 通过 hook: \`cnfonts-set-font-finish-hook' 来处理类似的问题（这个 hook 使用的函数只有一个参数 fontsizes-list, 用来记录 **当前使用** 的英文字体，中文字体和 EXT-B 字体的字号）。

下面是一些例子：

1.  设置 symbol 字符的字体

        (defun my-set-symbol-fonts (fontsizes-list)
          (let* ((fontname "Inconsolata")
                 (fontsize (nth 0 fontsizes-list))
                 (fontspec (font-spec :name fontname
                                      :size fontsize
                                      :weight 'normal
                                      :slant 'normal)))
            (if (cnfonts--fontspec-valid-p fontspec)
                (set-fontset-font "fontset-default" 'symbol fontspec nil 'append)
              (message "字体 %S 不存在！" fontname))))

        (add-hook 'cnfonts-set-font-finish-hook 'my-set-symbol-fonts)

2.  设置一些不常用汉字字符的字体

        (defun my-set-exta-fonts (fontsizes-list)
          (let* ((fontname "微软雅黑")
                 (fontsize (nth 1 fontsizes-list))
                 (fontspec (font-spec :name fontname
                                      :size fontsize
                                      :weight 'normal
                                      :slant 'normal)))
            (if (cnfonts--fontspec-valid-p fontspec)
                (set-fontset-font "fontset-default" '(#x3400 . #x4DFF) fontspec nil 'append)
              (message "字体 %S 不存在！" fontname))))

        (add-hook 'cnfonts-set-font-finish-hook 'my-set-exta-fonts)

    注意事项：

    1.  "(#x3400 . #x4DFF)" 代表了待设字符在 unicode-bmp 中的范围。
    2.  用户可以通过下面的方式来确定待字符的范围
        1.  运行 \`describe-char' 来显示 **待设字符** 的信息
        2.  点击 “code point in charset” 处的链接，来显示整个 unicode-bmp 表
        3.  获取范围
    3.  如果遇到 **部分符号** 无法正确对齐，可以参考:
        1.  <https://github.com/tumashu/cnfonts/issues/64#issuecomment-296414028>

3.  设置行距随着字号自动调整

        (defvar my-line-spacing-alist
              '((9 . 0.1) (10 . 0.9) (11.5 . 0.2)
                (12.5 . 0.2) (14 . 0.2) (16 . 0.2)
                (18 . 0.2) (20 . 1.0) (22 . 0.2)
                (24 . 0.2) (26 . 0.2) (28 . 0.2)
                (30 . 0.2) (32 . 0.2)))

        (defun my-line-spacing-setup (fontsizes-list)
          (let ((fontsize (car fontsizes-list))
                (line-spacing-alist (copy-list my-line-spacing-alist)))
            (dolist (list line-spacing-alist)
              (when (= fontsize (car list))
                (setq line-spacing-alist nil)
                (setq-default line-spacing (cdr list))))))

        (add-hook 'cnfonts-set-font-finish-hook #'my-line-spacing-setup)


<a id="orgdd8685e"></a>

## Tips

1.  如果用户需要在自己的 Emacs 配置中管理一些个人字体，可以使用变量 \`cnfonts-personal-fontnames' , 其结构与 \`cnfonts&#x2013;fontnames-fallback'一样。
2.  使用命令: \`describe-char' 可以了解光标处字符使用什么字体。
3.  在 scratch 中写一行 elisp 代码：

        (cl-prettyprint (font-family-list))

    执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很有用 的技巧。
4.  命令：\`cnfonts-insert-fontname', 可以让用户选择一个可用字体插入到当前光 标处。
5.  Windows 用户 (特别是 Windows XP 用户) 可以安装 MacType 软件来优化字 体显示效果，推荐使用。
6.  Mac 用户配置 profile 文件的时候，偶尔会遇到 'C-c C-c' 刷新缓慢的问 题，这可能是 ext-b 字体缺失引起的，建议安装 ext-b 字体试试。
    1.  Ext-B字符列表: <https://cdo.wikipedia.org/wiki/Wikipedia:Unicode%E6%93%B4%E5%B1%95%E6%BC%A2%E5%AD%97>
    2.  HanaMinB 下载地址: <https://osdn.jp/projects/hanazono-font/downloads/62072/hanazono-20141012.zip/>
7.  字体设置和 coding 设置也有关系，如果 cnfonts 的行为很奇怪， 又找不到确切原因，可以参考：<https://github.com/tumashu/cnfonts/issues/54#issuecomment-246228904>


<a id="org9f99658"></a>

## 参考文章

1.  <http://baohaojun.github.io/perfect-emacs-chinese-font.html>
2.  <http://zhuoqiang.me/torture-emacs.html>


Converted from cnfonts.el by [el2org](https://github.com/tumashu/el2org) .
