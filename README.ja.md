# Claude Code Emacs

[![CI Tests](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml)

Claude CodeをEmacs内で実行するためのパッケージです。

## 機能

### MCP（Model Context Protocol）統合
Claude Code EmacsにはMCPサーバー統合が含まれており、Claude Codeが直接Emacs環境と対話できます。

#### 利用可能なMCPツール
- **ファイルを開く**: プロジェクト内の任意のファイルを開く（オプションでテキスト選択）
- **開いているバッファを取得**: 現在のプロジェクトの全開いているバッファを一覧表示
- **現在の選択範囲を取得**: Emacsで現在選択されているテキストを取得
- **診断情報を取得**: プロジェクトファイルのLSP診断情報にアクセス

MCPサーバーはデフォルトでポート8766で動作し、Claude CodeとEmacs間の通信のためのWebSocketブリッジを提供します。

## Claude Codeの起動と終了
- `claude-code-emacs-run` - 現在のプロジェクトでClaude Codeを起動
- `claude-code-emacs-close` - Claude Codeバッファを表示しているウィンドウを閉じる
- `claude-code-emacs-quit` - Claude Codeセッションを終了してバッファを削除

## プロンプト管理機能
各プロジェクトのルートディレクトリに`.claude-code-emacs.prompt.md`ファイルが作成されます。
バッファは`switch-to-buffer-other-window`で開かれます。

### キーバインド
- `C-c C-s`: カーソル位置のmarkdownセクションをClaude Codeバッファに送信
- `C-c C-r`: 選択したリージョンをClaude Codeバッファに送信
- `C-c C-o`: Claude Codeセッションを開く
- `C-c C-i`: プロジェクトファイルのパスを選択して挿入（@プレフィックス付き）
- `C-c C-a`: 開いているバッファのファイルパスを挿入
- `C-c C-t`: プロンプトバッファ用のtransientメニューを表示
- `@`: @を入力すると自動的にファイル補完が起動

## Transientメニュー
### メインメニュー
`M-x claude-code-emacs-transient`でメインメニューを表示

Claude Codeのスラッシュコマンドの詳細については、[公式Claude Codeドキュメント](https://docs.anthropic.com/ja/docs/claude-code/cli-usage#%E3%82%B9%E3%83%A9%E3%83%83%E3%82%B7%E3%83%A5%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89)をご覧ください。

#### セッション
- `c`: Claude Codeを起動
- `b`: Claude Codeバッファに切り替え
- `q`: Claude Codeウィンドウを閉じる
- `Q`: Claude Codeセッションを終了
- `p`: プロンプトファイルを開く
- `s`: リージョンを送信

#### クイック送信キー
- `1` または `y`: "1"を送信（「はい」の応答に便利）
- `2`: "2"を送信
- `3`: "3"を送信
- `g`: "commit"を送信
- `e`: Escapeを送信
- `m`: Returnを送信
- `r`: Ctrl+Rを送信（展開の切り替え）

#### コマンド
- `i`: /init
- `k`: /clear
- `h`: /help
- `x`: プロジェクトカスタムコマンドを実行
- `X`: グローバルコマンドを実行（/user:）

#### メモリと設定
- `M`: /memory
- `C`: /config
- `o`: /compact

#### レビュー
- `R`: /review
- `P`: /pr_comments

#### 情報とアカウント
- `$`: /cost
- `S`: /status
- `l`: /login
- `L`: /logout
- `B`: /bug
- `D`: /doctor

### プロンプトバッファメニュー
プロンプトバッファ内で`C-c C-t`または`M-x claude-code-emacs-prompt-transient`でメニューを表示

## カスタムコマンド
Claude Code Emacsはマークダウンファイルとして保存されたカスタムコマンドをサポートしています。

### プロジェクト固有のコマンド
プロジェクトディレクトリ内の`.claude/commands/*.md`にコマンドを保存します。これらは`/project:command-name`で実行できます。

- `.claude/commands/`ディレクトリにマークダウンファイルを作成
- ユーザー入力のプレースホルダーとして`$ARGUMENTS`を使用
- `M-x claude-code-emacs-execute-custom-command`またはTransientメニューの`x`で実行
- 例：`.claude/commands/deploy.md`は`/project:deploy`として実行可能

### ユーザー固有のコマンド
グローバルコマンドを`~/.claude/commands/*.md`に保存します。これらは`/user:command-name`で実行できます。

- `~/.claude/commands/`ディレクトリにマークダウンファイルを作成
- ユーザー入力のプレースホルダーとして`$ARGUMENTS`を使用
- `M-x claude-code-emacs-execute-global-command`またはTransientメニューの`X`で実行
- 例：`~/.claude/commands/format.md`は`/user:format`として実行可能

### 引数の使用
コマンドに`$ARGUMENTS`プレースホルダーが含まれている場合、各出現箇所に対して値の入力を求められます。

## その他の機能
### リージョン送信
`claude-code-emacs-send-region` - 選択したリージョンまたはバッファ全体をClaude Codeに送信

## テスト
### テストの実行
```bash
# Makefileを使用
make test

# または直接実行
emacs -batch -l run-tests.el
```

### 全てのタスク（クリーン、コンパイル、テスト）
```bash
make all
```

## 依存パッケージ
- `projectile` - プロジェクトルート検出用
- `vterm` - ターミナルエミュレーション用
- `transient` - メニューシステム用
- `markdown-mode` - プロンプトファイルのベースモード
- `websocket` - MCPサーバー通信用
- `lsp-mode`（オプション） - 診断情報用

## インストール
このリポジトリをクローンして、Emacsの設定に追加してください：

```elisp
(add-to-list 'load-path "/path/to/claude-code-emacs")
(require 'claude-code-emacs)

;; オプション: MCP統合を有効化
(require 'claude-code-emacs-mcp)

;; オプション: メインメニューのグローバルキーバインドを設定
(global-set-key (kbd "C-c c") 'claude-code-emacs-transient)
```

### MCPサーバーのセットアップ
MCPサーバーにはNode.jsが必要です。依存関係のインストールとビルド：

```bash
# 全ての依存関係をインストール（MCPサーバーを含む）
make install-deps

# MCPサーバーをビルド
make mcp-build
```

## 使い方
1. `M-x claude-code-emacs-run`を実行して現在のプロジェクトでClaude Codeを起動
2. `M-x claude-code-emacs-open-prompt-file`でプロジェクト固有のプロンプトを作成/編集
3. `C-c c`でtransientメニューから全てのコマンドにアクセス
4. `claude mcp add-json ...`でMCPサーバーとしてEmacsを追加

### Claude Code に MCPサーバーを追加する方法
```shell
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "node",
  "args": [
    "/path/to/claude-code-emacs/mcp-server/dist/index.js"
  ]
}'
```

## ライセンス
このプログラムはフリーソフトウェアです。Free Software Foundationが公開する
GNU General Public License（バージョン3またはそれ以降のバージョン）の
条件の下で再配布および/または変更することができます。
