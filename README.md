# chinese-fonts-setup - 实现中英文字体等宽对齐的字体配置工具。

*Author:* Feng Shu <tumashu@gmail.com><br>
*Version:* 0.0.1<br>

`chinese-fonts-setup` 是一个emacs中文字体配置工具。可以比较方便的
的实现中文字体和英文字体等宽（也就是大家常说的中英文对齐）。

这个package特别适用于需要处理中英文混合表格的中文org-mode用户。

### 过程展示 ###

    http://www.tudou.com/programs/view/v7Kr0_a9INw/

### 下载 ###

    https://github.com/tumashu/chinese-fonts-setup

### 安装 ###
将这个文件放到任意一个emacs搜索目录之下，然后在~/.emacs中添加：

	(require 'chinese-fonts-setup)

### 配置 ###
chinese-fonts-setup 使用profile的概念，来实现特定的环境使用特定的
字体配置，比如：在编程时使用 “Consolas + 微米黑”，在阅读文章时使用
“PragmataPro + 黑体”。

每一个profile都是一个emacs-lisp文件。其中包括了英文字体设置，中文字体设置
以及中文字体调整系数（scale）。

chinese-fonts-setup 默认使用三个profile: profile1, profile2 和 profile3,
如果想使用其他有意义的名称，可以使用下面类似的方式配置:

	 (setq cfs--profiles
	       '("program" "org-mode" "read-book"))

profile文件保存在`cfs--profiles-directory`对应的目录中。如果文件不存在，
chinese-fonts-setup 在切换 profile 时通过自带的falback信息创建一个。

切换 profile 的命令有：
1. `cfs--switch-profile` (选择profile)
2. `cfs--next-profile`   (直接切换到下一个profile)

如果当前的profile不适合时，可以通过`cfs--edit-profile`来编辑当前
的profile文件。chinese-fonts-setup自带一个profile-edit编辑模式。

1.  C-c C-c     `cfs--test-scale-at-point`
		 察看字体显示效果
2.  C-<up>      `cfs--increment-font-scale-at-point`
		 增大光标下的scale数字，同时显示增加后的字体对齐效果
3.  C-<down>    `cfs--decrement-font-scale-at-point`
		 减小光标下的scale数字，同时显示减小后的字体对齐效果



---
Converted from `chinese-fonts-setup.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
