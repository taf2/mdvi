# mdvi
<p align="center"><a href="./README.md">English</a> · <a href="./README.zh-CN.md">简体中文</a> · <a href="./README.ja.md">日本語</a></p>

`mdvi` 是一款支持 Vim 风格导航的终端 Markdown 查看器。

它将 Markdown 渲染为精致的全屏 TUI，提供快速的键盘导航、清晰的排版，并能在处理大型文件时保持可预期的行为。

<img width="1595" height="582" alt="Screenshot 2026-02-10 at 4 58 34 PM" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## 功能

- 全屏终端查看器（`crossterm` + `ratatui`）
- Vim 风格的导航控制
- 状态栏中显示可见光标及 `line:column` 光标位置
- 通过 `pulldown-cmark` 提供高质量 Markdown 渲染
- 支持：
  - 标题
  - 列表和任务列表
  - 引用块
  - 内联 Markdown 图片（`![alt](...)`）和 HTML `<img ...>` 标签
  - 延迟加载图片，同时立即为占位布局预留空间
  - 带语法高亮的围栏代码块和内联代码
  - 链接、表格、脚注、强调、加粗和删除线
- 可通过 `--image-protocol` 配置图片后端（`auto`、`halfblocks`、`sixel`、`kitty`、`iterm2`）
- 文件在磁盘上发生变化时可实时重新加载（`r`）
- 从指定行开始（`--line`）
- 支持标准终端按键（方向键、Page Up/Down、Home/End）

注意：
- 支持本地、`file://` 及远程 `http://` / `https://` 图片源。
- HTML `<img>` 的 `width` 和 `height` 提示会在延迟加载完成前用于预留图片空间。
- 在 macOS 上，终端原生图片协议可能触发“Allow Terminal-Initiated Display?”提示。
  如果希望在不显示该对话框的情况下渲染图片，请使用 `--image-protocol halfblocks`。
- 带语言标签的围栏代码块（例如标记为 `rust` 的代码块）会使用语法高亮渲染。

## 安装

### Homebrew（tap）

```bash
brew tap taf2/tap
brew install mdvi
```

### 从源代码安装

```bash
cargo install --path .
```

### 无需安装直接运行

```bash
cargo run -- README.md
```

## 用法

```bash
mdvi [OPTIONS] <PATH>
```

示例：

```bash
mdvi docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol halfblocks README.md
```

## 导航

- `j` / `Down`：向下滚动一行
- `k` / `Up`：向上滚动一行
- `d`：向下滚动半页（less 风格）
- `u`：向上滚动半页（less 风格）
- `Ctrl-d`：向下滚动半页
- `Ctrl-u`：向上滚动半页
- `PageDown`：向下滚动一整页
- `PageUp`：向上滚动一整页
- `Ctrl-f`：向下滚动一整页（Vim 风格）
- `Ctrl-b`：向上滚动一整页（Vim 风格）
- `g` / `Home`：跳转到顶部
- `G` / `End`：跳转到底部
- `r`：从磁盘重新加载文件
- `/`：开始搜索
- `n`：下一个搜索匹配项
- `N`：上一个搜索匹配项
- `?`：切换帮助行
- `q`：退出

## 为什么使用 Rust？

Rust 非常适合用于严肃的 CLI 查看器：

- 精确的终端控制
- 处理大型文件时性能出色
- 可分发单个静态二进制文件
- 成熟的 TUI 和 Markdown 解析生态系统

## 开发

```bash
cargo test
cargo fmt
cargo clippy -- -D warnings
```

## 许可证

MIT
