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
# 依存関係のインストールとMCPサーバーのビルド
make install-deps
make mcp-build

# Claude CodeでMCPを設定
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "node",
  "args": ["/path/to/claude-code-emacs/mcp-server/dist/index.js"]
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
- **差分ツール** - ファイル比較、git変更表示、パッチ適用
- **リアルタイムイベント** - バッファ変更と診断が自動的にClaude Codeに送信

### ⌨️ キーバインド

| キー | アクション |
|-----|--------|
| `C-c c` | メインTransientメニューを開く |
| `C-u M-x claude-code-emacs-run` | オプション付きで起動（モデル、再開など） |
| `1`/`y` | 素早い「はい」応答 |
| `TAB` | 自動承認の切り替え |
| `C-c C-s` | カーソル位置のセクションを送信（プロンプトバッファ内） |
| `@` | ファイル補完（プロンプトバッファ内） |

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
- パッケージ: `projectile`、`vterm`、`transient`、`markdown-mode`、`websocket`
- オプション: `lsp-mode`、`alert`

## インストールの詳細

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

## ライセンス

GPL-3.0-or-later
