# mdvi
<p align="center"><a href="./README.md">English</a> · <a href="./README.zh-CN.md">简体中文</a> · <a href="./README.ja.md">日本語</a></p>

`mdvi` は Vim スタイルのナビゲーションを備えたターミナル用 Markdown ビューアです。

Markdown を洗練されたフルスクリーン TUI として描画し、高速なキーボード操作、読みやすいタイポグラフィ、大きなファイルでも予測可能な動作を提供します。

<img width="1595" height="582" alt="Screenshot 2026-02-10 at 4 58 34 PM" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## 機能

- フルスクリーンのターミナルビューア（`crossterm` + `ratatui`）
- Vim スタイルのナビゲーション操作
- 表示されるカーソルと、ステータスバー内の `line:column` 形式のカーソル位置
- `pulldown-cmark` による高品質な Markdown 描画
- 次の要素をサポート：
  - 見出し
  - リストとタスクリスト
  - 引用
  - インライン Markdown 画像（`![alt](...)`）と HTML の `<img ...>` タグ
  - 画像を遅延読み込みしつつ、プレースホルダー用の領域を即座に確保
  - シンタックスハイライト付きのフェンスコードブロックとインラインコード
  - リンク、表、脚注、強調、太字、取り消し線
- `--image-protocol` で画像バックエンドを設定可能（`auto`、`halfblocks`、`sixel`、`kitty`、`iterm2`）
- ディスク上のファイル変更時にライブリロード（`r`）
- 指定行から開始（`--line`）
- 標準的なターミナルキーに対応（矢印キー、Page Up/Down、Home/End）

注意：
- ローカル、`file://`、リモートの `http://` / `https://` 画像ソースに対応しています。
- HTML の `<img>` にある `width` と `height` の指定は、遅延読み込みの完了前に画像領域を確保するために使用されます。
- macOS では、ターミナルネイティブの画像プロトコルによって「Allow Terminal-Initiated Display?」という確認が表示される場合があります。
  このダイアログを表示せずに画像を描画するには、`--image-protocol halfblocks` を使用してください。
- 言語タグ付きのフェンスコードブロック（たとえば `rust` と指定したブロック）は、シンタックスハイライト付きで描画されます。

## インストール

### Homebrew（tap）

```bash
brew tap taf2/tap
brew install mdvi
```

### ソースから

```bash
cargo install --path .
```

### インストールせずに実行

```bash
cargo run -- README.md
```

## 使い方

```bash
mdvi [OPTIONS] <PATH>
```

例：

```bash
mdvi docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol halfblocks README.md
```

## ナビゲーション

- `j` / `Down`：1 行下にスクロール
- `k` / `Up`：1 行上にスクロール
- `d`：半ページ下にスクロール（less スタイル）
- `u`：半ページ上にスクロール（less スタイル）
- `Ctrl-d`：半ページ下にスクロール
- `Ctrl-u`：半ページ上にスクロール
- `PageDown`：1 ページ下にスクロール
- `PageUp`：1 ページ上にスクロール
- `Ctrl-f`：1 ページ下にスクロール（Vim スタイル）
- `Ctrl-b`：1 ページ上にスクロール（Vim スタイル）
- `g` / `Home`：先頭へ移動
- `G` / `End`：末尾へ移動
- `r`：ディスクからファイルを再読み込み
- `/`：検索を開始
- `n`：次の検索一致へ移動
- `N`：前の検索一致へ移動
- `?`：ヘルプ行の表示を切り替え
- `q`：終了

## Rust を選ぶ理由

Rust は本格的な CLI ビューアに適しています。

- 精密なターミナル制御
- 大きなファイルでも優れたパフォーマンス
- 単一の静的バイナリとして配布可能
- TUI と Markdown 解析の成熟したエコシステム

## 開発

```bash
cargo test
cargo fmt
cargo clippy -- -D warnings
```

## ライセンス

MIT
