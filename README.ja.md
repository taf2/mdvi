# mdvi
<p align="center"><a href="./README.md">English</a> · <a href="./README.zh-CN.md">简体中文</a> · <a href="./README.ja.md">日本語</a></p>

`mdvi` は Vim スタイルのナビゲーションを備えたターミナル用 Markdown ビューアです。

Markdown を洗練されたフルスクリーン TUI として描画し、高速なキーボード操作、読みやすいタイポグラフィ、大きなファイルでも予測可能な動作を提供します。

<img width="1595" height="582" alt="Screenshot 2026-02-10 at 4 58 34 PM" src="https://github.com/user-attachments/assets/1b2da7a1-b5fb-4169-9d10-bcd97a07ae45" />

## 機能

- フルスクリーンのターミナルビューア（`crossterm` + `ratatui`）
- Vim スタイルのナビゲーション操作
- 視認できるカーソルと、ステータスバー内の `line:column` 形式のカーソル位置
- `pulldown-cmark` による高品質な Markdown 描画
- スクロール、検索、選択、インライン画像の位置関係を保つ折り返し表示
- `--width` で表示幅を設定可能（例：ターミナルの 72 列または 80 列）
- 全角文字や絵文字を含む、Unicode に対応したレイアウトとカーソル移動
- 次の要素をサポート：
  - 見出し
  - リストとタスクリスト
  - 引用
  - インライン Markdown 画像（`![alt](...)`）と HTML の `<img ...>` タグ
  - 画像を遅延読み込みしつつ、プレースホルダー用の領域を即座に確保
  - シンタックスハイライト付きのフェンスコードブロックとインラインコード
  - リンク、列が整列しヘッダーを識別しやすい表、脚注、強調、太字、取り消し線
- `--image-protocol` で画像バックエンドを設定可能（`auto`、`symbols`、`halfblocks`、`sixel`、`kitty`、`iterm2`）
- `resvg` でラスタライズする SVG 画像
- グラフィックスプロトコルのないターミナルでも、精細なテキストモード画像を表示：各セルに最適な六分割・四分割・八分割のブロック文字を選び、SVG の文字は鮮明なターミナル文字として描画
- 必要に応じてディスクから再読み込み（`r`）
- 指定行から開始（`--line`）
- 標準的なターミナルキーに対応（矢印キー、Page Up/Down、Home/End）

注意：
- ローカル、`file://`、リモートの `http://` / `https://` 画像ソースに対応しています。
- HTML の `<img>` にある `width` と `height` の指定は、遅延読み込みの完了前に画像領域を確保するために使用されます。
- macOS では、ターミナルネイティブの画像プロトコルによって「Allow Terminal-Initiated Display?」という確認が表示される場合があります。
  このダイアログを表示せずに画像を描画するには、`--image-protocol symbols` を使用してください。
- 言語タグ付きのフェンスコードブロック（たとえば `rust` と指定したブロック）は、シンタックスハイライト付きで描画されます。
- コードブロックは元の改行と空行を保持し、タブを 4 列ごとのタブ位置まで展開します。
  幅の広いコードブロックは、表と同様に `h` / `l` または矢印キーで横方向にスクロールできます。
- ターミナルのサイズを変更しても、カーソルは同じテキスト位置に保たれます。幅が変わると画面座標が変化するため、矩形選択は解除されます。

`make run ARGS='examples/rendering.md'` で描画サンプルを試せます。

## インストール

### Homebrew（tap）

```bash
brew tap taf2/tap
brew install mdvi
```

### ソースから

最新の安定版 Rust ツールチェーン（Cargo を含む）、C リンカー、Make をインストールしてください。
プロジェクトのルートで次を実行すると、最適化された実行ファイルをビルドできます。

```bash
make
./target/release/mdvi README.md
```

初回のビルドでは依存関係をダウンロードするため時間がかかりますが、次回以降は Cargo のキャッシュを再利用します。
`mdvi` を Cargo の実行ファイルディレクトリ（通常は `~/.cargo/bin`）にインストールするには、次を実行します。

```bash
make install
```

Make がない場合は、`cargo build --release --locked` でビルド、または
`cargo install --locked --path .` でインストールできます。インストールしたコマンドを使う際は、Cargo の実行ファイルディレクトリが `PATH` に含まれていることを確認してください。

### インストールせずに実行

```bash
make run
make run ARGS='--image-protocol symbols README.md'
```

## 使い方

```bash
mdvi [OPTIONS] <PATH>
```

例：

```bash
mdvi docs/spec.md
mdvi --width 80 docs/spec.md
mdvi --line 120 CHANGELOG.md
mdvi --image-protocol symbols README.md
```

`--width` は枠線を除いた内容の最大表示幅を指定します。ターミナルがそれより狭い場合は幅を縮めます。
省略するとターミナルの利用可能な幅を使います。コードブロックと表は折り返さず、元の行を保持します。

## ナビゲーション

- `j` / `Down`：1 行下にスクロール
- `k` / `Up`：1 行上にスクロール
- `h` / `Left`、`l` / `Right`：カーソルを左右に移動
- 幅の広い表でも列の整列を維持します。`l` / `h` または矢印キーで左右に移動すると、表を横方向にスクロールできます。
- `w` / `b`：次 / 前の単語へ移動
- `W` / `B`：次 / 前の空白区切りの語へ移動
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
- `Enter`：検索を確定；`Esc`：検索入力をキャンセル
- `Ctrl-v`：矩形選択を切り替え；`y`：選択範囲をクリップボードにコピー
- `Esc`：矩形選択を解除
- `?`：ヘルプ行の表示を切り替え
- `q`：終了
- `Ctrl-c`：どのモードからでも終了

検索は文字列をそのまま照合し、大文字と小文字を区別しません。空の検索を確定するとハイライトを解除できます。
クリップボードへのコピーには、実行環境で対応するクリップボードサービスが必要です。

## Rust を選ぶ理由

Rust は本格的な CLI ビューアに適しています。

- 精密なターミナル制御
- 大きなファイルでも優れたパフォーマンス
- 単一の実行ファイルとして配布可能
- TUI と Markdown 解析の成熟したエコシステム

## 開発

```bash
make help     # すべてのコマンドを表示
make check    # フォーマット検査、テスト、Clippy（警告はエラーとして扱う）
make fmt      # Rust ソースを整形
make test     # テストのみ実行
make debug    # target/debug/mdvi に開発用の高速ビルドを作成
make clean    # ビルド成果物を削除
```

ビルドとテストでは、再現性を確保するため、コミット済みの `Cargo.lock` と `--locked` を使用します。
CI は Linux と macOS で検査とリリースビルドを実行します。`make CARGO=/path/to/cargo` で Cargo を指定でき、
`CARGO_TARGET_DIR` や `CARGO_BUILD_JOBS` などの標準 Cargo 環境変数も使用できます。

## ライセンス

MIT
