# mdvi
<p align="center"><a href="./README.md">English</a> · <a href="./README.zh-CN.md">简体中文</a> · <a href="./README.ja.md">日本語</a></p>

`mdvi` 是一款支持 Vim 风格导航的终端 Markdown 查看器。

它将 Markdown 渲染为精致的全屏 TUI，提供快速的键盘导航、清晰的排版，并能在处理大型文件时保持可预期的行为。

<img width="1595" height="582" alt="Screenshot 2026-02-10 at 4 58 34 PM" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## 功能

- 全屏终端查看器（`crossterm` + `ratatui`）
- Vim 风格的导航控制
- 显示可见光标，并在状态栏中以 `line:column` 格式显示光标位置
- 通过 `pulldown-cmark` 提供高质量 Markdown 渲染
- 自动换行，同时保持滚动、搜索、选择和内联图片的位置一致
- 通过 `--width` 设置内容宽度（例如终端的 72 列或 80 列）
- 支持 Unicode 的布局和光标移动，包括宽字符和 emoji
- 支持：
  - 标题
  - 列表和任务列表
  - 引用块
  - 内联 Markdown 图片（`![alt](...)`）和 HTML `<img ...>` 标签
  - 延迟加载图片，同时立即为占位布局预留空间
  - 带语法高亮的围栏代码块和内联代码
  - 链接、列对齐且表头样式鲜明的表格、脚注、强调、加粗和删除线
- 可通过 `--image-protocol` 配置图片后端（`auto`、`symbols`、`halfblocks`、`sixel`、`kitty`、`iterm2`）
- 使用 `resvg` 栅格化 SVG 图片
- 在不支持图形协议的终端中，以高细节文本模式显示图片：每个字符单元会从六分块、四分块和八分块字符中选择最合适的字形，SVG 中的文字则以清晰的终端文字显示
- 按需从磁盘重新加载（`r`）
- 从指定行开始（`--line`）
- 支持标准终端按键（方向键、Page Up/Down、Home/End）

注意：
- 支持本地、`file://` 及远程 `http://` / `https://` 图片源。
- HTML `<img>` 的 `width` 和 `height` 提示会在延迟加载完成前用于预留图片空间。
- 在 macOS 上，终端原生图片协议可能触发“Allow Terminal-Initiated Display?”提示。
  如果希望在不显示该对话框的情况下渲染图片，请使用 `--image-protocol symbols`。
- 带语言标签的围栏代码块（例如标记为 `rust` 的代码块）会使用语法高亮渲染。
- 代码块保留原有换行和空行，并将制表符展开到每四列一个的制表位。
  宽代码块与表格一样，可使用 `h` / `l` 或方向键横向滚动。
- 调整终端大小时，光标会保持在原来的文本位置。宽度变化时，当前矩形选区会被清除，因为其屏幕坐标发生了变化。

运行 `make run ARGS='examples/rendering.md'` 可查看渲染示例。

## 安装

### Homebrew（tap）

```bash
brew tap taf2/tap
brew install mdvi
```

### 从源代码安装

安装当前稳定版 Rust 工具链（包括 Cargo）、C 链接器和 Make。
在项目根目录下运行以下命令，构建经过优化的可执行文件：

```bash
make
./target/release/mdvi README.md
```

首次构建需要下载依赖，耗时较长；后续构建会复用 Cargo 的缓存。
要将 `mdvi` 安装到 Cargo 的可执行文件目录（通常为 `~/.cargo/bin`），请运行：

```bash
make install
```

如果没有 Make，可使用 `cargo build --release --locked` 构建，或使用
`cargo install --locked --path .` 安装。使用安装后的命令时，请确保 Cargo 的可执行文件目录已加入 `PATH`。

### 无需安装直接运行

```bash
make run
make run ARGS='--image-protocol symbols README.md'
```

## 用法

```bash
mdvi [OPTIONS] <PATH>
```

示例：

```bash
mdvi docs/spec.md
mdvi --width 80 docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol symbols README.md
```

`--width` 设置内容的最大宽度，不包括边框。终端较窄时会自动缩小宽度；
省略此选项则使用终端的可用宽度。代码块和表格保留原有行，不自动换行。

## 导航

- `j` / `Down`：向下滚动一行
- `k` / `Up`：向上滚动一行
- `h` / `Left`、`l` / `Right`：水平移动光标
- 宽表格会保持列对齐；使用 `l` / `h` 或方向键左右移动，即可横向滚动表格。
- `w` / `b`：移动到下一个 / 上一个单词
- `W` / `B`：移动到下一个 / 上一个以空白字符分隔的词组
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
- `Enter`：提交搜索；`Esc`：取消搜索输入
- `Ctrl-v`：切换矩形选择模式；`y`：将选区复制到剪贴板
- `Esc`：清除矩形选区
- `?`：切换帮助行
- `q`：退出
- `Ctrl-c`：从任意模式退出

搜索按字面文本匹配，不区分大小写。提交空搜索可清除高亮。
复制到剪贴板需要当前环境提供受支持的剪贴板服务。

## 为什么使用 Rust？

Rust 非常适合用于功能完善的 CLI 查看器：

- 精确的终端控制
- 处理大型文件时性能出色
- 可作为单个可执行文件分发
- 成熟的 TUI 和 Markdown 解析生态系统

## 开发

```bash
make help     # 列出所有命令
make check    # 格式检查、测试和 Clippy（将警告视为错误）
make fmt      # 格式化 Rust 源代码
make test     # 仅运行测试
make debug    # 更快的开发构建，输出到 target/debug/mdvi
make clean    # 清理构建产物
```

构建和测试使用已提交的 `Cargo.lock`，并启用 `--locked`，以确保可复现性。
CI 在 Linux 和 macOS 上运行检查和发布构建。可以通过 `make CARGO=/path/to/cargo` 指定 Cargo；
也支持 `CARGO_TARGET_DIR` 和 `CARGO_BUILD_JOBS` 等标准 Cargo 环境变量。

## 许可证

MIT
