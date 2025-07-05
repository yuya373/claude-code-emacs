# Claude Code Emacs

[![CI Tests](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml)

[Claude Code](https://docs.anthropic.com/ja/docs/claude-code) AIコーディングセッションをEmacsで直接実行。強力なMCP統合付き。

## クイックスタート

```elisp
;; init.elに追加
(add-to-list 'load-path "/path/to/claude-code-emacs")
(require 'claude-code-emacs)
(global-set-key (kbd "C-c c") 'claude-code-emacs-transient)
```

```bash
# MCPサーバーをグローバルにインストール
npm install -g claude-code-emacs-mcp-server

# Claude CodeでMCPを設定
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "claude-code-emacs-mcp"
}'
```

`M-x claude-code-emacs-run` でセッションを開始！

## 主な機能

### 🚀 コア機能
- **プロジェクト別セッション** - 各プロジェクトが独立したClaude Codeバッファを持つ
- **スマートファイル補完** - プロンプトで`@`を入力してファイル参照
- **Transientメニュー** - `C-c c`でメインメニューと全コマンドにアクセス
- **カスタムコマンド** - `.claude/commands/*.md`に再利用可能なコマンドを定義
- **プロジェクトプロンプト** - プロジェクトごとの`.claude-code-emacs.prompt.md`ファイルで永続的なコンテキスト

### 🔌 MCP統合
Claude CodeがEmacs環境と直接やり取り：
- **バッファ操作** - 開いているバッファの一覧/読み取り、選択テキストの取得
- **LSP統合** - 診断取得、定義/参照の検索、シンボル説明
- **差分ツール** - ファイル比較、git変更表示
- **リアルタイムイベント** - バッファ変更と診断が自動的にClaude Codeに送信

### ⌨️ キーバインド

#### グローバル
| キー | アクション |
|-----|--------|
| `C-c c` | メインTransientメニューを開く |
| `C-u M-x claude-code-emacs-run` | オプション付きで起動（モデル、再開など） |

#### Claude Codeセッション内（vtermモード）
| キー | アクション |
|-----|--------|
| `C-c C-q` | Claude Codeウィンドウを閉じる |
| `C-c C-g` | Escapeキーを送信 |
| `C-c C-r` | Ctrl+Rを送信（展開の切り替え） |
| `C-c C-e` | Ctrl+Eを送信（さらに展開） |
| `C-c RET` | Returnキーを送信 |
| `C-c TAB` | Shift+Tabを送信（自動承認の切り替え） |
| `C-c C-t` | Transientメニューを開く |

#### プロンプトバッファ内
| キー | アクション |
|-----|--------|
| `C-c C-s` | カーソル位置のセクションを送信 |
| `C-c C-r` | 選択範囲を送信 |
| `@` | ファイル補完 |

## よくある使い方

### プロジェクトプロンプト
各プロジェクトのルートに`.claude-code-emacs.prompt.md`ファイルが作成されます：
```markdown
# プロジェクトコンテキスト
TypeScriptを使用したReactアプリです...

# 現在のタスク
ユーザー認証機能の実装

# コーディングスタイル
- 関数コンポーネントを使用
- クラスコンポーネントよりフックを優先
```
`M-x claude-code-emacs-open-prompt-file`またはTransientメニューの`p`で開きます。

### LSPエラーの修正
```elisp
M-x claude-code-emacs-fix-diagnostic
;; またはTransientメニューで 'f'
```

### カスタムコマンド
`.claude/commands/refactor.md`を作成：
```markdown
次のコードをリファクタリングしてください: $ARGUMENTS
```
Transientメニューで`x` → "refactor"を選択して実行

### Git操作
メインメニューで`g`を押してgitコマンド：
- `g` - コミット
- `p` - プッシュ
- `r` - 変更をレビュー
- `c` - PRコメント

## 必要環境

- Emacs 28.1以上
- [Claude Code CLI](https://docs.anthropic.com/ja/docs/claude-code)インストール済み
- Node.js 16以上（MCP用）
- 必須パッケージ: `projectile`、`vterm`、`transient`、`markdown-mode`
- オプションパッケージ:
  - `lsp-mode` (9.0.0以上): LSP診断修正とMCPツール統合用
  - `websocket` (1.15以上): MCPサーバーのWebSocket通信用
  - `alert`: デスクトップ通知用

## インストールの詳細

### MCPサーバーのインストール

#### オプション1: グローバルインストール（推奨）
```bash
# npmからグローバルにインストール
npm install -g claude-code-emacs-mcp-server

# Claude Codeを設定
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "claude-code-emacs-mcp"
}'
```

#### オプション2: npxを使用（インストール不要）
```bash
# Claude Codeでnpxを使うよう設定
# npxはパッケージを必要に応じてダウンロードして実行
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "npx",
  "args": ["claude-code-emacs-mcp-server"]
}'
```

#### オプション3: ソースからビルド
```bash
# リポジトリをクローンした場合
cd /path/to/claude-code-emacs/mcp-server
npm install
npm run build

# Claude Codeを設定
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "node",
  "args": ["/path/to/claude-code-emacs/mcp-server/dist/index.js"]
}'
```

詳しいMCP設定は[docs/MCP-SETUP.md](docs/MCP-SETUP.md)を参照。

## アーキテクチャ

- **モジュラー設計** - バッファ管理、コマンド、UI、MCPで個別モジュール
- **プロジェクト別WebSocket** - 各プロジェクトが独自のMCP接続を維持
- **自動再接続** - ping/pongによるMCP接続の健全性監視
- **イベントバッチング** - デバウンス付きの効率的なリアルタイム通知

## コントリビューション

1. リポジトリをフォーク
2. 機能ブランチを作成
3. 新機能のテストを追加
4. `make test`で全テストの成功を確認
5. プルリクエストを送信

### 開発コマンド

```bash
# すべての依存関係（オプションも含む）でコンパイル
make compile

# オプションの依存関係（websocket、lsp-mode）なしでコンパイル
# MELPAの互換性テストに便利
make compile-minimal

# テストを実行
make test

# package-lintを実行
make lint
```

## リリースプロセス

GitHub Actionsを使用した自動リリース：

### 新しいリリースの作成

#### オプション1: GitHub CLIを使用（推奨）
```bash
# ドラフトリリースを作成
./scripts/create-release.sh 0.2.0

# または即座に公開
./scripts/create-release.sh 0.2.0 --publish
```

#### オプション2: GitHub Actionsを使用
1. Actions → Create Release Draft → Run workflowに移動
2. バージョン番号を入力（例：0.2.0）

#### 最終ステップ
1. 自動生成されたリリースノートを確認
2. 必要に応じて編集
3. 「Publish release」をクリック（ドラフトの場合）

3. **自動化されるステップ**（公開後）：
   - Gitタグが自動的に作成されます
   - 以下のファイルのバージョン番号が更新されます：
     - `claude-code-emacs.el`
     - `mcp-server/package.json`
   - MCPサーバーがnpmに公開されます
   - MELPAレシピが生成されます

### 手動バージョン更新

ローカルでのバージョン更新：
```bash
./scripts/update-version.sh 0.2.0
```

## ライセンス

GPL-3.0-or-later
