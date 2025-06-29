# Claude Code Emacs

[![CI Tests](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml)

Claude Code CLIをEmacs内で実行するためのパッケージです。このパッケージはClaude Codeとのシームレスな統合を提供し、AI駆動のコーディングセッションをEmacsで直接実行できます。

## 機能

- **プロジェクト固有のセッション**: 各プロジェクトが独自の独立したClaude Codeセッションを持ちます
- **シームレスなバッファ管理**: 自動的なバッファ作成と切り替え
- **スマートなファイル補完**: `@`を入力してプロジェクトファイルを素早く参照
- **カスタムコマンド**: プロジェクト固有およびグローバルコマンドのサポート
- **Transientメニュー**: 全操作のための直感的なメニューシステム
- **プロンプト管理**: Claude Codeプロンプト管理用の専用モード
- **MCP統合**: Claude CodeとEmacs間の直接的な相互作用

### MCP（Model Context Protocol）統合
Claude Code EmacsにはMCPサーバー統合が含まれており、Claude Codeが直接Emacs環境と対話できます。

#### 利用可能なMCPツール
- **ファイルを開く**: プロジェクト内の任意のファイルを開く（オプションでテキスト選択）
- **開いているバッファを取得**: 現在のプロジェクトの全開いているバッファを一覧表示
- **現在の選択範囲を取得**: Emacsで現在選択されているテキストを取得
- **診断情報を取得**: プロジェクト全体のLSP診断情報を取得
- **定義を取得**: LSPを使用してシンボル定義を検索（プレビュー付き）
- **コマンドを実行**: Emacsコマンドを実行（セキュリティチェック付き）
- **差分ツール**: 強力なファイル比較ツール群：
  - **openDiff**: 2つのファイルまたはバッファを比較
  - **openDiff3**: マージ競合用の3ウェイファイル比較
  - **openRevisionDiff**: ファイルを任意のgitリビジョンと比較
  - **openCurrentChanges**: ediffでコミットされていない変更を表示
  - **applyPatch**: ediffを使用してパッチファイルを適用
- **通知を送信**: Emacsのアラートシステムを使用してユーザーに通知を送信
  - `alert`パッケージが利用可能な場合は使用、なければ`message`関数にフォールバック
  - タスクの完了やエラーの通知に便利
  - 常に`claude-code`カテゴリを使用して一貫したアラート処理を実現

MCPサーバーは各プロジェクトごとに動的にポートを割り当て、Claude CodeとEmacs間の通信のためのWebSocketブリッジを提供します。

## Claude Codeの起動と終了
- `claude-code-emacs-run` - 現在のプロジェクトでClaude Codeを起動
  - `C-u M-x claude-code-emacs-run`で対話的にオプションを選択：
    - `--verbose`: 詳細ログを有効化
    - `--model sonnet` または `--model opus`: AIモデルを選択
    - `--resume`: 最後の会話を再開
    - `--continue`: 特定のチャットUUIDから続行
    - `--dangerously-skip-permissions`: 権限チェックをスキップ
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
- `TAB`: Shift+Tabを送信（自動承認の切り替え）

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
プロジェクトディレクトリ内の`.claude/commands/*.md`にコマンドを保存します。これらは`/project:command-name`として実行されます。

- `.claude/commands/`ディレクトリにマークダウンファイルを作成
- ユーザー入力のプレースホルダーとして`$ARGUMENTS`を使用
- `M-x claude-code-emacs-execute-custom-command`またはTransientメニューの`x`で実行
- 例：`.claude/commands/deploy.md`は`/project:deploy`として実行

### ユーザー固有のコマンド
グローバルコマンドを`~/.claude/commands/*.md`に保存します。これらは`/user:command-name`として実行されます。

- `~/.claude/commands/`ディレクトリにマークダウンファイルを作成
- ユーザー入力のプレースホルダーとして`$ARGUMENTS`を使用
- `M-x claude-code-emacs-execute-global-command`またはTransientメニューの`X`で実行
- 例：`~/.claude/commands/format.md`は`/user:format`として実行

### 引数の使用
コマンドに`$ARGUMENTS`プレースホルダーが含まれている場合、各出現箇所に対して値の入力を求められます。

## その他の機能
### リージョン送信
`claude-code-emacs-send-region` - 選択したリージョンをClaude Codeに送信（リージョンが選択されていない場合はエラー）

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
- `alert`（オプション） - デスクトップ通知用（利用できない場合は`message`にフォールバック）

## インストール

### 前提条件
- Emacs 28.1以降
- [Claude Code CLI](https://docs.anthropic.com/ja/docs/claude-code)がインストール済みで設定済み
- Node.js v16以降（MCPサーバー用）
- 必須Emacsパッケージ: `projectile`、`vterm`、`transient`、`markdown-mode`、`websocket`
- オプション: `lsp-mode`（拡張診断機能用）

### 基本セットアップ
このリポジトリをクローンして、Emacsの設定に追加してください：

```elisp
(add-to-list 'load-path "/path/to/claude-code-emacs")
(require 'claude-code-emacs)

;; オプション: メインメニューのグローバルキーバインドを設定
(global-set-key (kbd "C-c c") 'claude-code-emacs-transient)
```

注意: パッケージはモジュール化されました。`claude-code-emacs`をrequireすると、全てのモジュールが自動的に読み込まれます。

### MCPサーバーのセットアップ
MCPサーバーを使用すると、Claude CodeがEmacs環境と対話できるようになります：

```bash
# 全ての依存関係をインストール（MCPサーバーを含む）
make install-deps

# MCPサーバーをビルド
make mcp-build
```

詳しいMCPセットアップ手順については、[docs/MCP-SETUP.md](docs/MCP-SETUP.md)を参照してください。

## クイックスタート

1. **Claude Codeを起動**: `M-x claude-code-emacs-run`（推奨キーバインドでは`C-c c c`）
2. **Transientメニューを開く**: `C-c c`で利用可能な全コマンドを表示
3. **プロンプトを作成**: `M-x claude-code-emacs-open-prompt-file`でプロジェクトプロンプトを管理

### EmacsをMCPサーバーとして追加
MCP統合を有効にするには、Claude Codeを設定します：

```shell
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "node",
  "args": [
    "/path/to/claude-code-emacs/mcp-server/dist/index.js"
  ]
}'
```

その後、Claude Codeセッションで `/mcp` と入力してMCPツールを有効化します。

## アーキテクチャ

パッケージは以下のモジュールに整理されています：

- **claude-code-emacs.el** - メインエントリーポイント、全モジュールを読み込み
- **claude-code-emacs-core.el** - コアユーティリティ（チャンキング、エラーハンドリング、セッション管理）
- **claude-code-emacs-buffer.el** - バッファ名前付けと管理
- **claude-code-emacs-commands.el** - コマンド実行とスラッシュコマンド
- **claude-code-emacs-ui.el** - Transientメニューインターフェース
- **claude-code-emacs-prompt.el** - プロンプトファイルモードと操作
- **claude-code-emacs-mcp.el** - MCP WebSocketクライアント統合
- **claude-code-emacs-mcp-connection.el** - WebSocket接続管理（自動再接続機能付き）
- **claude-code-emacs-mcp-protocol.el** - MCPプロトコル実装
- **claude-code-emacs-mcp-tools.el** - MCPツールハンドラー（ファイル操作、診断、差分ツール）

## MCPサーバー機能

### 接続管理
MCPサーバーは安定したWebSocket接続を維持し、自動ヘルスモニタリングを提供します：
- **Ping/Pongハートビート**: 接続問題を検出するため定期的にpingメッセージを送信（デフォルト30秒間隔）
- **設定可能なタイムアウト**: `claude-code-emacs-mcp-ping-interval`と`claude-code-emacs-mcp-ping-timeout`でping間隔とタイムアウトをカスタマイズ

### 利用可能なMCPツール
- **openFile**: オプションのテキスト選択付きでファイルを開く
- **getOpenBuffers**: 現在のプロジェクトの全開いているバッファを一覧表示
- **getCurrentSelection**: 現在選択されているテキストを取得
- **getDiagnostics**: プロジェクト全体のLSP診断情報を取得（`lsp-mode`が必要）
- **getDefinition**: LSPを使用してシンボル定義を検索（前後3行のプレビュー付き）
- **openDiff**: ediffを使用して2つのファイルまたはバッファを比較
- **openDiff3**: 3ウェイファイル比較
- **openRevisionDiff**: ファイルをgitリビジョンと比較
- **openCurrentChanges**: コミットされていない変更を表示
- **applyPatch**: ediffを使用してパッチファイルを適用
- **runCommand**: セキュリティ制限付きでEmacsコマンドを実行

### 利用可能なMCPリソース
MCPサーバーはClaude Codeがアクセスできるリソースとして以下のEmacsデータを公開します：
- **バッファリソース** (`file://`): 未保存の変更を含む開いているバッファのコンテンツにアクセス
- **プロジェクトリソース** (`emacs://project/`): プロジェクトメタデータとファイルリスト
- **診断リソース** (`emacs://diagnostics/`): バッファとプロジェクトのLSP診断情報

## コントリビューション

コントリビューションを歓迎します！以下の手順に従ってください：
1. リポジトリをフォーク
2. 機能ブランチを作成
3. 新機能のテストを追加
4. `make test`で全テストが通ることを確認
5. プルリクエストを送信

## ロードマップ

### 計画中のMCPサーバー機能
- **getWorkspaceFolders**: 全プロジェクトフォルダを一覧表示
- **checkDocumentDirty**: 未保存の変更をチェック
- **saveDocument**: 未保存の変更があるファイルを保存
- **getSymbols**: コードシンボルと定義にアクセス
- **getReferences**: 参照先にアクセス

実装の詳細についてはコード内の[TODO](#)セクションを参照してください。

## ライセンス
このプログラムはフリーソフトウェアです。Free Software Foundationが公開する
GNU General Public License（バージョン3またはそれ以降のバージョン）の
条件の下で再配布および/または変更することができます。
