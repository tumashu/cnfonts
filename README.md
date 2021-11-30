Note: this file is auto converted from cnfonts.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [cnfonts README](#org3d3a52f)
    1.  [简介](#orgfb848af)
    2.  [基本原理](#org1fa1f4f)
    3.  [使用特点](#orgb54ea55)
    4.  [下载安装](#orgca51d15)
    5.  [配置使用](#orgaad5bf8)
        1.  [最简单的用法（懒人必备）](#org5cc16b9)
        2.  [profile 的概念](#org6a57b2d)
        3.  [profile 命名与切换](#orgeb3b4a1)
        4.  [使用 cnfonts-edit-profile 命令调整 profile](#org438b8bd)
        5.  [使用 cnfonts-regenerate-profile 重置 profile](#org5f641ed)
        6.  [调整字体大小](#org7138129)
        7.  [使用 cnfonts-use-system-type](#org8ebf27f)
        8.  [让 cnfonts 随着 Emacs 自动启动](#orgfb6a8b8)
        9.  [cnfonts 与 org-mode 配合使用](#org3aaa17d)
        10. [cnfonts 高级功能](#orgf95fea6)
    6.  [Tips](#orgaf75091)
    7.  [参考文章](#org7fce6d5)


<a id="org3d3a52f"></a>

# cnfonts README


<a id="orgfb848af"></a>

## 简介

cnfonts 原来叫: chinese-fonts-setup, 是一个 Emacs 中英文字体配置工具。可以比较方便地实现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。

注：这个 package 特别适用于需要处理中英文混合表格的中文 org-mode 用户。


<a id="org1fa1f4f"></a>

## 基本原理

cnfonts 的核心很简单，就是让中文字体和英文字体使用不同的字号，从而实现中英文对齐。


<a id="orgb54ea55"></a>

## 使用特点

cnfonts 添加了许多辅助工具，使配置和调节字体和字号的工作更加简便快捷，它有几个优点：

1.  安装即用：cnfonts 内置字体 fallback 功能，只需安装，就能够配置中文字体和英文字体，让中文可以 **正确** 显示（但未必完美），不会因为
    Emacs 配置中指定的字体不存在而报错。

2.  设置方便：cnfonts 自带一个 profile 文件调整工具，这个工具有直观的图形界面，可以让用户设置字体名称和字体大小，分分钟实现中文字体和英文字体的等宽对齐。


<a id="orgca51d15"></a>

## 下载安装

1.  配置melpa源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET cnfonts RET
3.  在emacs配置文件中（比如: ~/.emacs）添加如下代码：
    
        (require 'cnfonts)
        ;; 让 cnfonts 在 Emacs 启动时自动生效。
        (cnfonts-mode 1)
        ;; 添加两个字号增大缩小的快捷键
        ;; (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
        ;; (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)


<a id="orgaad5bf8"></a>

## 配置使用


<a id="org5cc16b9"></a>

### 最简单的用法（懒人必备）

通过下面几个命令，用户可以 **快速** 了解 cnfonts 的大部分功能，而不需要阅读整篇文档，如果用户想深入了解 cnfonts 或者自定义一些特殊的功能，阅读整篇文档是逃不开的。

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">命令</th>
<th scope="col" class="org-left">功能</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">cnfonts-edit-profile</td>
<td class="org-left">调整字体设置</td>
</tr>


<tr>
<td class="org-left">cnfonts-increase-fontsize</td>
<td class="org-left">增大字号</td>
</tr>


<tr>
<td class="org-left">cnfonts-decrease-fontsize</td>
<td class="org-left">减小字号</td>
</tr>
</tbody>
</table>


<a id="org6a57b2d"></a>

### profile 的概念

profile 代表了一套字体配置，cnfonts 使用 profile 的概念，来维护多套字体配置，从而实现特定的环境使用特定的字体配置，比如：在编程时使用“Consolas + 微米黑”，在阅读文章时使用 “PragmataPro + 黑体”，等等。

每一个 profile 都对应一个 emacs-lisp 文件, 保存在
\`cnfonts-directory' 对应的目录中, 这些文件包含了英文字体设置，中文字体设置以及中文字体大小等。


<a id="orgeb3b4a1"></a>

### profile 命名与切换

cnfonts 默认使用三个 profile: profile1, profile2 和 profile3, 如果想使用其它有意义的名称，可以设置:

    (setq cnfonts-profiles
        '("program" "org-mode" "read-book"))

cnfonts 使用下面两个命令来切换 profile ：

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Help</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">cnfonts-switch-profile</td>
<td class="org-left">选择并切换 profile</td>
</tr>


<tr>
<td class="org-left">cnfonts-next-profile</td>
<td class="org-left">直接切换到下一个profile</td>
</tr>
</tbody>
</table>


<a id="org438b8bd"></a>

### 使用 cnfonts-edit-profile 命令调整 profile

如果 **当前使用** 的字体不符合使用习惯，用户可以运行
\`cnfonts-edit-profile'命令来调整 **当前** profile,这个命令会弹出一个图形化界面，类似：

![img](./snapshots/cnfonts-ui-1.png)
![img](./snapshots/cnfonts-ui-2.png)
![img](./snapshots/cnfonts-ui-3.png)
![img](./snapshots/cnfonts-ui-4.png)
![img](./snapshots/cnfonts-ui-5.png)
![img](./snapshots/cnfonts-ui-6.png)
![img](./snapshots/cnfonts-ui-7.png)

注1: 配置完成后，有可能需要重启 Emacs, 参考：
<http://debbugs.gnu.org/db/17/1785.html>


<a id="org5f641ed"></a>

### 使用 cnfonts-regenerate-profile 重置 profile

\`cnfonts-regenerate-profile' 命令会使用 cnfonts 自带的 fallback 信息，覆盖需要 **重置** 的 profile, 这个 profile 原来的内容将丢失，请紧慎使用！


<a id="org7138129"></a>

### 调整字体大小

\`cnfonts' 使用下述两个命令调整字体大小:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Help</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">cnfonts-increase-fontsize</td>
<td class="org-left">增大字体大小</td>
</tr>


<tr>
<td class="org-left">cnfonts-decrease-fontsize</td>
<td class="org-left">减小字体大小</td>
</tr>
</tbody>
</table>

注意：在调整字体大小的同时，字号信息也会保存到 \`cnfonts-directory'
目录下\`cnfonts-config-filename' 对应的文件中。

![img](./snapshots/cnfonts-increase-and-decrease-fontsize.gif)


<a id="org8ebf27f"></a>

### 使用 cnfonts-use-system-type

有些用户希望将 profile 配置文件做为自己的 Emacs 配置，在不同的计算机上同步和管理，我建议这些用户将 \`cnfonts-use-system-type'设置为 t,
这样，相同名称的 profile 在不同的操作系统下，保存的位置也不同，可以避免 profile 冲突。


<a id="orgfb6a8b8"></a>

### 让 cnfonts 随着 Emacs 自动启动

\`cnfonts-mode' 命令可以让 cnfonts 随着 Emacs 自动启动，这个命令将
\`cnfonts-set-font-with-saved-fontsize' 添加到下面两个 hook:

1.  \`after-make-frame-functions'
2.  \`window-setup-hook'

用户也可以手动运行 \`cnfonts-set-font-with-saved-fontsize' 来让
cnfonts 生效。


<a id="org3aaa17d"></a>

### cnfonts 与 org-mode 配合使用

许多用户使用 org-mode 时，习惯让不同的标题，使用的字体大小也不同，这个特性需要用户设置：

    (setq cnfonts-use-face-font-rescale t)

注：这个功能不能在 window 系统下使用，它会让对齐功能失效，Linux 下这个功能 **一般** 可以使用，Mac 系统未测试，同学可以亲自试一试。


<a id="orgf95fea6"></a>

### cnfonts 高级功能

1.  设置一些不常用汉字字符的字体

        (push '(#x3400 . #x4DFF) cnfonts-ornaments)
    
    注意事项：
    
    1.  "(#x3400 . #x4DFF)" 代表了待设字符在 unicode-bmp 中的范围。
    2.  用户可以通过下面的方式来确定待字符的范围
        1.  运行 \`describe-char' 来显示 **待设字符** 的信息
        2.  点击 “code point in charset” 处的链接，来显示整个 unicode-bmp 表
        3.  获取范围
    3.  如果遇到 **部分符号** 无法正确对齐，可以参考:
        1.  <https://github.com/tumashu/cnfonts/issues/64#issuecomment-296414028>

2.  设置行距随着字号自动调整

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


<a id="orgaf75091"></a>

## Tips

1.  如果用户需要在自己的 Emacs 配置中管理一些个人字体，可以使用变量
    \`cnfonts-personal-fontnames' , 其结构与
    \`cnfonts&#x2013;fontnames-fallback'一样。
2.  使用命令: \`describe-char' 可以了解光标处字符使用什么字体。
3.  在 scratch 中写一行 elisp 代码：
    
        (cl-prettyprint (font-family-list))
    
    执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很有用的技巧。

4.  Windows 用户 (特别是 Windows XP 用户) 可以安装 MacType 软件来优化字体显示效果，推荐使用。
5.  Mac 用户配置 profile 文件的时候，偶尔会遇到 'C-c C-c' 刷新缓慢的问题，这可能是 ext-b 字体缺失引起的，建议安装 ext-b 字体试试。
    1.  Ext-B字符列表: <https://cdo.wikipedia.org/wiki/Wikipedia:Unicode%E6%93%B4%E5%B1%95%E6%BC%A2%E5%AD%97>
    2.  HanaMinB 下载地址: <https://osdn.jp/projects/hanazono-font/downloads/62072/hanazono-20141012.zip/>
6.  字体设置和 coding 设置也有关系，如果 cnfonts 的行为很奇怪，又找不到确切原因，可以参考：
    <https://github.com/tumashu/cnfonts/issues/54#issuecomment-246228904>


<a id="org7fce6d5"></a>

## 参考文章

1.  <http://baohaojun.github.io/perfect-emacs-chinese-font.html>
2.  <http://zhuoqiang.me/torture-emacs.html>

