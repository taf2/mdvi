# mdvi
<p align="center"><a href="./README.md">English</a> · <a href="./README.zh-CN.md">简体中文</a> · <a href="./README.ja.md">日本語</a></p>

**Vim スタイルのナビゲーションを備えた、高速なフルスクリーンのターミナル用 Markdown ビューア。**

`mdvi` は読みやすいタイポグラフィで Markdown を描画し、シンタックスハイライト付きのコード、
列が整列した表、インライン画像に対応しています。大きなファイルでも軽快に動作します。

<img width="1595" height="582" alt="ターミナルで Markdown ドキュメントを表示する mdvi" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## クイックスタート

```bash
brew tap taf2/tap
brew install mdvi

mdvi README.md
```

`?` でヘルプ、`q` で終了します。その他のインストール方法は[インストール](#インストール)を参照してください。

## 機能

**描画**
- 見出し、リスト、タスクリスト、引用、脚注、リンク、強調・太字・取り消し線
- シンタックスハイライト付きのフェンスコードブロック。元の改行を保持し、タブを 4 列ごとのタブ位置まで展開し、
  幅の広いブロックは折り返さずに横スクロール
- ヘッダーを識別しやすく、列が整列した表。幅の広い表は横スクロール
- スクロール、検索、選択、画像の位置関係を保つ折り返し表示
- 全角文字や絵文字を含む、Unicode に対応したレイアウト
- 表示幅の上限を任意で設定（`--width 80`）

**画像**
- Markdown 画像（`![alt](...)`）と HTML の `<img>` タグ
- ローカル、`file://`、リモートの `http(s)://` 画像ソース。遅延読み込みしつつ、領域を事前に確保
- `resvg` でラスタライズする SVG に対応
- 利用可能な場合はネイティブのグラフィックスプロトコル（Kitty、iTerm2、Sixel）を使用
- その他のターミナルでは精細なテキストモードで表示（[画像](#画像)を参照）

**ナビゲーション**
- Vim スタイルの移動操作に加え、矢印キー、Page Up/Down、Home/End に対応
- 視認できるカーソルと、ステータスバーの `line:column` 表示
- 大文字と小文字を区別しない検索と一致箇所のハイライト
- 矩形選択とクリップボードへのコピー
- 再起動せずにディスクから再読み込み（`r`）

## インストール

### Homebrew

```bash
brew tap taf2/tap
brew install mdvi
```

### ソースから

最新の安定版 Rust ツールチェーン（Cargo を含む）、C リンカー、Make が必要です。

```bash
git clone https://github.com/taf2/mdvi.git
cd mdvi
make            # target/release/mdvi をビルド
make install    # Cargo の bin ディレクトリ（通常は ~/.cargo/bin）にインストール
```

Cargo の bin ディレクトリが `PATH` に含まれていることを確認してください。Make がない場合は、
`cargo build --release --locked` でビルド、`cargo install --locked --path .` でインストールできます。

インストールせずに試すには：

```bash
make run                                  # README.md を開く
make run ARGS='examples/rendering.md'     # 描画サンプル
```

## 使い方

```bash
mdvi [OPTIONS] <PATH>
```

| オプション | 説明 |
| --- | --- |
| `-l`, `--line <N>` | `N` 行目から開始（1 始まり）。デフォルト：`1` |
| `--width <COLS>` | 表示幅の上限（列数、枠線を除く）。ターミナルが狭い場合は自動で縮小。デフォルト：ターミナルの全幅 |
| `--image-protocol <P>` | 画像バックエンド：`auto`（デフォルト）、`symbols`、`halfblocks`、`sixel`、`kitty`、`iterm2` |
| `-h`, `--help` | ヘルプを表示 |
| `-V`, `--version` | バージョンを表示 |

例：

```bash
mdvi docs/spec.md
mdvi --width 80 docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol symbols README.md
```

## キー操作

### 移動

| キー | 操作 |
| --- | --- |
| `j` / `Down` | 1 行下へ |
| `k` / `Up` | 1 行上へ |
| `h` / `Left`、`l` / `Right` | カーソルを左 / 右へ移動。幅の広い表やコードブロックを横スクロール |
| `w` / `b` | 次 / 前の単語へ |
| `W` / `B` | 次 / 前の空白区切りの単語へ |
| `d` / `Ctrl-d` | 半ページ下へ |
| `u` / `Ctrl-u` | 半ページ上へ |
| `Ctrl-f` / `PageDown` | 1 ページ下へ |
| `Ctrl-b` / `PageUp` | 1 ページ上へ |
| `g` / `Home` | 先頭へ移動 |
| `G` / `End` | 末尾へ移動 |

### 検索

| キー | 操作 |
| --- | --- |
| `/` | 検索を開始 |
| `Enter` / `Esc` | 検索入力を確定 / キャンセル |
| `n` / `N` | 次 / 前の一致箇所へ |

検索はリテラル一致で、大文字と小文字を区別しません。空の検索を確定するとハイライトが消えます。

### 選択とその他

| キー | 操作 |
| --- | --- |
| `Ctrl-v` | 矩形選択の切り替え |
| `y` | 選択範囲をクリップボードにコピー |
| `Esc` | 選択を解除 |
| `r` | ディスクからファイルを再読み込み |
| `?` | ヘルプ行の表示切り替え |
| `q` | 終了 |
| `Ctrl-c` | どのモードからでも終了 |

コピーには、環境でサポートされているクリップボードサービスが必要です。ターミナルのサイズを変更しても、
カーソルは同じテキスト位置に保たれます。幅が変わると画面座標が一致しなくなるため、選択は解除されます。

## 画像

`--image-protocol auto`（デフォルト）では、ターミナルのネイティブなグラフィックスプロトコルを検出した場合にそれを使用します。
検出できない場合はテキストで画像を描画します。各セルに最適な六分割・四分割・八分割のブロック文字を選び、
SVG 内の文字は実際のターミナル文字として描画されるため鮮明に表示されます。

- HTML の `<img>` にある `width` と `height` 属性は、画像の読み込み完了前に領域を確保するために使用されます。
- macOS では、ネイティブの画像プロトコルによって「Allow Terminal-Initiated Display?」という確認が表示される場合があります。
  `--image-protocol symbols` を使用すると、この確認なしで画像を描画できます。

## 開発

```bash
make help     # すべてのコマンドを一覧表示
make check    # フォーマット確認、テスト、Clippy（警告はエラー扱い）
make fmt      # Rust ソースをフォーマット
make test     # テストのみ実行
make debug    # 高速な開発用ビルド（target/debug/mdvi）
make clean    # ビルド成果物を削除
```

ビルドとテストは、コミット済みの `Cargo.lock` と `--locked` を使用します。CI は Linux と macOS でチェックとリリースビルドを実行します。
`make CARGO=/path/to/cargo` で Cargo を指定できます。`CARGO_TARGET_DIR` や `CARGO_BUILD_JOBS` などの
標準的な Cargo 環境変数にも対応しています。

変更履歴は [CHANGELOG.md](CHANGELOG.md) を参照してください。

## ライセンス

[MIT](LICENSE)
