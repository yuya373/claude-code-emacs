# Claude Code Emacs
claude codeをEmacsで動かすpackageです

## claude codeを起動
`claude-code-emacs-run`

## プロンプト管理機能
プロジェクト毎にプロジェクトルートに`.claude-code-emacs.prompt.md`が作成される
`switch-to-buffer-other-window`でbufferが開かれる

### キーバインド
- `C-c C-s`: カーソル位置のmarkdownセクションをClaude Codeバッファに送信
- `C-c C-r`: 選択したリージョンをClaude Codeバッファに送信
- `C-c C-o`: Claude Codeセッションを開く

## Transientメニュー
### メインメニュー
`M-x claude-code-emacs-transient` でメインメニューを表示

### プロンプトバッファメニュー
プロンプトバッファ内で `C-c C-t` または `M-x claude-code-emacs-prompt-transient` でメニューを表示

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
