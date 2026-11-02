# mdvi
<p align="center"><a href="./README.md">English</a> · <a href="./README.zh-CN.md">简体中文</a> · <a href="./README.ja.md">日本語</a></p>

**一款快速的全屏终端 Markdown 查看器，支持 Vim 风格导航。**

`mdvi` 以清晰的排版渲染 Markdown，支持语法高亮代码、列对齐表格和内联图片，
在处理大型文件时依然保持流畅。

<img width="1595" height="582" alt="mdvi 在终端中渲染 Markdown 文档" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## 快速开始

```bash
brew tap taf2/tap
brew install mdvi

mdvi README.md
```

按 `?` 查看帮助，按 `q` 退出。其他安装方式请参阅[安装](#安装)。

## 功能

**渲染**
- 标题、列表、任务列表、引用块、脚注、链接，以及强调、加粗和删除线
- 带语法高亮的围栏代码块。保留原有换行，制表符展开到每四列一个的制表位，
  宽代码块可横向滚动而不会自动换行
- 表头样式鲜明、列对齐的表格。宽表格可横向滚动
- 自动换行，同时保持滚动、搜索、选择和图片的位置一致
- 支持 Unicode 的布局，包括宽字符和 emoji
- 可选的最大内容宽度（`--width 80`）

**图片**
- Markdown 图片（`![alt](...)`）和 HTML `<img>` 标签
- 支持本地、`file://` 及远程 `http(s)://` 图片源，延迟加载并预先预留空间
- 支持 SVG，使用 `resvg` 栅格化
- 在可用时使用原生图形协议（Kitty、iTerm2、Sixel）
- 其他终端使用高细节文本模式显示（参见[图片](#图片)）

**导航**
- Vim 风格的移动操作，并支持方向键、Page Up/Down 和 Home/End
- 可见光标，状态栏中显示 `line:column`
- 不区分大小写的搜索，并高亮匹配项
- 矩形选择，可复制到剪贴板
- 无需重启即可从磁盘重新加载（`r`）

## 安装

### Homebrew

```bash
brew tap taf2/tap
brew install mdvi
```

### 从源代码安装

需要当前稳定版 Rust 工具链（包括 Cargo）、C 链接器和 Make。

```bash
git clone https://github.com/taf2/mdvi.git
cd mdvi
make            # 构建 target/release/mdvi
make install    # 安装到 Cargo 的可执行文件目录（通常为 ~/.cargo/bin）
```

请确保 Cargo 的可执行文件目录已加入 `PATH`。如果没有 Make，可使用
`cargo build --release --locked` 构建，或使用 `cargo install --locked --path .` 安装。

无需安装直接试用：

```bash
make run                                  # 打开 README.md
make run ARGS='examples/rendering.md'     # 渲染示例
```

## 用法

```bash
mdvi [OPTIONS] <PATH>
```

| 选项 | 说明 |
| --- | --- |
| `-l`, `--line <N>` | 从第 `N` 行开始（从 1 开始计数）。默认：`1` |
| `--width <COLS>` | 内容的最大宽度（列数），不包括边框。终端较窄时自动缩小。默认：终端全宽 |
| `--image-protocol <P>` | 图片后端：`auto`（默认）、`symbols`、`halfblocks`、`sixel`、`kitty`、`iterm2` |
| `-h`, `--help` | 显示帮助 |
| `-V`, `--version` | 显示版本 |

示例：

```bash
mdvi docs/spec.md
mdvi --width 80 docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol symbols README.md
```

## 快捷键

### 移动

| 按键 | 操作 |
| --- | --- |
| `j` / `Down` | 向下一行 |
| `k` / `Up` | 向上一行 |
| `h` / `Left`、`l` / `Right` | 向左 / 向右移动光标；可横向滚动宽表格和代码块 |
| `w` / `b` | 下一个 / 上一个单词 |
| `W` / `B` | 下一个 / 上一个以空白字符分隔的词组 |
| `d` / `Ctrl-d` | 向下半页 |
| `u` / `Ctrl-u` | 向上半页 |
| `Ctrl-f` / `PageDown` | 向下一整页 |
| `Ctrl-b` / `PageUp` | 向上一整页 |
| `g` / `Home` | 跳转到顶部 |
| `G` / `End` | 跳转到底部 |

### 搜索

| 按键 | 操作 |
| --- | --- |
| `/` | 开始搜索 |
| `Enter` / `Esc` | 提交 / 取消搜索输入 |
| `n` / `N` | 下一个 / 上一个匹配项 |

搜索按字面文本匹配，不区分大小写。提交空搜索可清除高亮。

### 选择及其他

| 按键 | 操作 |
| --- | --- |
| `Ctrl-v` | 切换矩形选择 |
| `y` | 将选区复制到剪贴板 |
| `Esc` | 清除选区 |
| `r` | 从磁盘重新加载文件 |
| `?` | 切换帮助行 |
| `q` | 退出 |
| `Ctrl-c` | 从任意模式退出 |

复制需要当前环境提供受支持的剪贴板服务。调整终端大小时，光标会保持在原来的文本位置；
宽度变化时，当前选区会被清除，因为其屏幕坐标已不再对应。

## 图片

使用 `--image-protocol auto`（默认）时，`mdvi` 会在检测到终端的原生图形协议时使用该协议。
否则以文本方式绘制图片：每个字符单元会从六分块、四分块和八分块字符中选择最合适的字形，
SVG 中的文字则以真实的终端文字渲染，保持清晰。

- HTML `<img>` 的 `width` 和 `height` 属性会在图片加载完成前用于预留空间。
- 在 macOS 上，原生图片协议可能触发“Allow Terminal-Initiated Display?”提示。
  使用 `--image-protocol symbols` 可在不出现该提示的情况下渲染图片。

## 开发

```bash
make help     # 列出所有命令
make check    # 格式检查、测试和 Clippy（将警告视为错误）
make fmt      # 格式化 Rust 源代码
make test     # 仅运行测试
make debug    # 更快的开发构建，输出到 target/debug/mdvi
make clean    # 清理构建产物
```

构建和测试使用已提交的 `Cargo.lock`，并启用 `--locked`。CI 在 Linux 和 macOS 上运行检查和发布构建。
可以通过 `make CARGO=/path/to/cargo` 指定 Cargo；也支持 `CARGO_TARGET_DIR` 和
`CARGO_BUILD_JOBS` 等标准 Cargo 环境变量。

变更记录见 [CHANGELOG.md](CHANGELOG.md)。

## 许可证

[MIT](LICENSE)
