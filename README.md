- [Chinese-fonts-setup 简要说明](#chinese-fonts-setup-简要说明)
  - [过程展示](#过程展示)
  - [下载](#下载)
  - [安装](#安装)
  - [配置](#配置)
  - [调整字体大小](#调整字体大小)
  - [Tips](#tips)
  - [参考文章](#参考文章)

# Chinese-fonts-setup 简要说明<a id="orgheadline8"></a>

\`chinese-fonts-setup' 是一个emacs中文字体配置工具。可以比较方便地实现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。

这个package特别适用于需要处理中英文混合表格的中文org-mode用户。

## 过程展示<a id="orgheadline1"></a>

<http://www.tudou.com/programs/view/v7Kr0_a9INw/>

## 下载<a id="orgheadline2"></a>

<https://github.com/tumashu/chinese-fonts-setup>

## 安装<a id="orgheadline3"></a>

1.  配置melpa源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET chinese-fonts-setup RET
3.  在emacs配置文件中（比如: ~/.emacs）添加如下代码：

    (require 'chinese-fonts-setup)

## 配置<a id="orgheadline4"></a>

chinese-fonts-setup 使用profile的概念，来实现特定的环境使用特定的字体配置，比如：在编程时使用 “Consolas + 微米黑”，在阅读文章时使用“PragmataPro + 黑体”。

每一个profile都是一个emacs-lisp文件。其中包括了英文字体设置，中文字体设置以及中文字体大小。

chinese-fonts-setup 默认使用三个profile: profile1, profile2 和 profile3,
如果想使用其他有意义的名称，可以使用下面类似的方式配置:

    (setq cfs-profiles
        '("program" "org-mode" "read-book"))

所有的profile文件都保存在 \`cfs-profiles-directory' 对应的目录中。如果profile文件不存在， \`chinese-fonts-setup' 将使用其自带的fallback
信息来配置字体。

在运行profile编辑命令\`cfs-edit-profile'的时候，缺失的falback文件将会自动创建，其原始内容为软件包自带的fallback信息。

\`chinese-fonts-setup' 默认不会覆盖已经存在的profile文件。当需要重置某个profile文件时，可以使用命令：\`cfs-regenerate-profile'。这个命令会强制覆盖profile文件，请做好备份。

切换 profile 的命令有：

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
<td class="org-left">cfs-switch-profile</td>
<td class="org-left">使用ido切换profile</td>
</tr>


<tr>
<td class="org-left">cfs-next-profile</td>
<td class="org-left">直接切换到下一个profile</td>
</tr>
</tbody>
</table>

如果当前的profile不适合时，可以通过\`cfs-edit-profile'来编辑当前的profile文件。chinese-fonts-setup自带一个profile-edit编辑模式。

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Help</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-c</td>
<td class="org-left">cfs-test-fontsizes-at-point</td>
<td class="org-left">查看字体显示效果</td>
</tr>


<tr>
<td class="org-left">C-up</td>
<td class="org-left">cfs-increment-fontsize-at-point</td>
<td class="org-left">增大光标下字号的大小，同时显示对齐效果</td>
</tr>


<tr>
<td class="org-left">C-down</td>
<td class="org-left">cfs-decrement-fontsize-at-point</td>
<td class="org-left">减小光标下字号的大小，同时显示对齐效果</td>
</tr>
</tbody>
</table>

配置完成后，有可能需要重启 Emacs。(参考：<http://debbugs.gnu.org/db/17/1785.html>)

## 调整字体大小<a id="orgheadline5"></a>

\`chinese-fonts-setup' 使用下述两个命令调整字体大小:

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
<td class="org-left">cfs-increase-fontsize</td>
<td class="org-left">增大字体大小</td>
</tr>


<tr>
<td class="org-left">cfs-decrease-fontsize</td>
<td class="org-left">减小字体大小</td>
</tr>
</tbody>
</table>

在调整字体大小的同时，字号信息也通过customize-save-variable函数保存到~/.emacs中了。

## Tips<a id="orgheadline6"></a>

1.  使用命令: \`describe-char' 可以了解光标处字符使用什么字体。
2.  在 scratch 中写一行 elisp 代码： (cl-prettyprint (font-family-list)),
    执行后，就会在 scratch 中插入当前可用字体的名称列表，这是一个很有用的技巧。
3.  命令：\`cfs-insert-fontname', 可以让用户选择一个可用字体插入到当前光标处。
4.  Windows 用户 (特别是 Windows XP 用户) 可以安装 MacType 软件来优化字体显示效果，推荐使用。
5.  Mac用户在配置profile文件时如果遇到'C-c C-c'刷新缓慢和第三行Ext-B文字无法显示的问题，下载并安装ext-b字体即可解决。[wikipedia]: https://cdo.wikipedia.org/wiki/Wikipedia:Unicode%E6%93%B4%E5%B1%95%E6%BC%A2%E5%AD%97 有Ext-B字符列表。默认Ext-B字体HanaMinB下载地址是 [fonts.jp/hanazono]: https://osdn.jp/projects/hanazono-font/downloads/62072/hanazono-20141012.zip/

## 参考文章<a id="orgheadline7"></a>

1.  <http://baohaojun.github.io/perfect-emacs-chinese-font.html>
2.  <http://zhuoqiang.me/torture-emacs.html>
