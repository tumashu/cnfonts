;;; chinese-fonts-setup.el --- A simple Chinese fonts config tool

;; * Header
;; Copyright (c) 2011-2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-fonts-setup
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
;;  chinese-fonts-setup is outdate, use cnfonts instead.

;;; Code:

;; * 代码                                                                 :code:
(require 'cnfonts)
(require 'cnfonts-ui)

(defun chinese-fonts-setup-enable ()
  "运行这个函数，可以让 Emacs 启动的时候就激活 chinese-fonts-setup."
  (interactive)
  (message "

------------------------------------------------------------------
|                Chinese-fonts-setup 重要更新                    |
|                                                                |
| 由于 Chinese-fonts-setup 使用的前缀 cfs- 和一个 gnu 项目       |
| cfs-el(https://www.gnu.org/software/cfs-el/cfs-el.html)使用的  |
| 前缀冲突, 在加上 cfs 太过缩减，不好记忆，所以现决定将          |
| chinese-fonts-setup 的前缀更改为 cnfonts-,                     |
| 请使用 chinese-fonts-setup 的朋友更新自己的配置：              |
|                                                                |
| 1. chinese-fonts-setup-enable  -> cnfonts-enable               |
| 2. chinese-fonts-setup-disable -> cnfonts-disable              |
| 3. cfs-*                       -> cnfonts-*                    |
|                                                                |
| 给大家带来的不便我深表歉意，感谢大家的支持和理解。             |
------------------------------------------------------------------

")
  (call-interactively #'cnfonts-enable))

(defun chinese-fonts-setup-disable ()
  "清除与 chinese-fonts-setup 相关的 hook 设定."
  (interactive)
  (call-interactively #'cnfonts-disable))

;; * Footer
(provide 'chinese-fonts-setup)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-fonts-setup.el ends here
