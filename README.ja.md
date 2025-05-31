# Claude Code Emacs

[![CI Tests](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml)

Claude CodeをEmacs内で実行するためのパッケージです。

## Claude Codeの起動
`claude-code-emacs-run`

## プロンプト管理機能
各プロジェクトのルートディレクトリに`.claude-code-emacs.prompt.md`ファイルが作成されます。
バッファは`switch-to-buffer-other-window`で開かれます。

### キーバインド
- `C-c C-s`: カーソル位置のmarkdownセクションをClaude Codeバッファに送信
- `C-c C-r`: 選択したリージョンをClaude Codeバッファに送信
- `C-c C-o`: Claude Codeセッションを開く

## Transientメニュー
### メインメニュー
`M-x claude-code-emacs-transient`でメインメニューを表示

#### クイック送信キー
- `1` または `y`: "1"を送信（「はい」の応答に便利）
- `2`: "2"を送信
- `3`: "3"を送信
- `g`: "commit"を送信
- `e`: Escapeを送信
- `m`: Returnを送信

### プロンプトバッファメニュー
プロンプトバッファ内で`C-c C-t`または`M-x claude-code-emacs-prompt-transient`でメニューを表示

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

## インストール
このリポジトリをクローンして、Emacsの設定に追加してください：

```elisp
(add-to-list 'load-path "/path/to/claude-code-emacs")
(require 'claude-code-emacs)

;; オプション: メインメニューのグローバルキーバインドを設定
(global-set-key (kbd "C-c c") 'claude-code-emacs-transient)
```

## 使い方
1. `M-x claude-code-emacs-run`を実行して現在のプロジェクトでClaude Codeを起動
2. `M-x claude-code-emacs-open-prompt-file`でプロジェクト固有のプロンプトを作成/編集
3. `C-c c`でtransientメニューから全てのコマンドにアクセス

## ライセンス
このプログラムはフリーソフトウェアです。Free Software Foundationが公開する
GNU General Public License（バージョン3またはそれ以降のバージョン）の
条件の下で再配布および/または変更することができます。
