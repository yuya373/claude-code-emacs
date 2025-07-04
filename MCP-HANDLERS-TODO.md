# MCP Handlers 動作確認 TODO リスト

**emacsのMCPサーバーの再起動はユーザーが行なう必要があるため通知すること**

## 高優先度 (完了)
- [x] **getOpenBuffers** - 開いているバッファのリストを取得
  - エラー: `active`と`modified`が`null`になっていた
  - 修正: `:false`を`json-false`に修正済み (2025-01-04)
  - テスト結果: 正常動作確認済み ✓

- [x] **openDiffContent** - テキストコンテンツを比較  
  - エラー: `structuredContent`が提供されていなかった
  - 修正: `index.ts`で全diffツールに`structuredContent`を追加 (2025-01-04)
  - 追加修正: エラー時もstructuredContentを返すように改善
  - テスト結果: 正常動作確認済み ✓

- [x] **getDiagnostics** - LSP診断情報を取得
  - 注意: `buffer`パラメータが必須（LSPコンテキスト用）
  - テスト結果: TypeScriptファイルで正常動作確認済み ✓
  - 実際のエラー検出も確認（index.tsの不要なexportを発見・修正）

## 中優先度 (完了)
- [x] **getDefinition** - シンボルの定義を検索
  - パラメータ: `file`, `line`, `symbol`
  - テスト結果: 正常動作確認済み ✓
  - EmacsBridgeやDiffToolResultの定義位置を正確に取得

- [x] **findReferences** - シンボルの参照を検索
  - パラメータ: `file`, `line`, `symbol`, `includeDeclaration`(optional)
  - テスト結果: 正常動作確認済み ✓
  - DiffToolResponseの8箇所の参照を正確に検出

- [x] **describeSymbol** - シンボルの詳細情報を取得
  - パラメータ: `file`, `line`, `symbol`
  - テスト結果: 正常動作確認済み ✓
  - 関数シグネチャやインポート情報を正しく表示

- [x] **openDiff** - 2つのファイルを比較
  - パラメータ: `fileA`, `fileB`
  - テスト結果: 正常動作確認済み ✓

- [x] **openDiff3** - 3つのファイルを比較
  - パラメータ: `fileA`, `fileB`, `fileC`, `ancestor`(optional)
  - テスト結果: 正常動作確認済み ✓

- [x] **openRevisionDiff** - ファイルとGitリビジョンを比較
  - パラメータ: `file`, `revision`(default: HEAD)
  - テスト結果: 正常動作確認済み ✓
  - HEAD~1との比較も正常に実行

- [x] **openCurrentChanges** - 未コミットの変更を表示
  - パラメータ: `file`(optional, default: current file)
  - テスト結果: 正常動作確認済み ✓
  - 未追跡ファイルではエラーメッセージが適切に表示

- [ ] **applyPatch** - パッチファイルを適用
  - パラメータ: `patchFile`, `targetFile`
  - 前提: 有効なパッチファイル
  - 注意: パッチファイルが必要なため未テスト

## リソース系
- [ ] **bufferResource** - バッファの内容を取得
  - リソースタイプ: `file://`
  - 機能:
    - list: プロジェクト内の開いているバッファをリスト表示
    - read: 特定のバッファの内容を読み取り
  - 注意: ResourceTemplateで実装されているため、個別のリソースとしてはリストに表示されない

- [x] **projectResource** - プロジェクト情報を取得
  - リソースタイプ: `emacs://project/info`
  - 機能:
    - list: プロジェクト情報リソースを表示 ✓
    - read: プロジェクトのルート、名前、VCSタイプなどの情報を取得 ✓
  - テスト結果: JSON形式でプロジェクト情報（root, name, type, vcs, branch, lastModified）が正しく取得できることを確認

- [x] **diagnosticsResource** - 診断情報を取得
  - リソースタイプ: `emacs://diagnostics/all`
  - 機能:
    - list: 診断情報リソースを表示 ✓
    - read: 全ファイルの診断情報を取得
  - 修正: メソッド名を`get-diagnostics`から`get-diagnostics-resource`に変更 (2025-01-04)
  - 理由: `get-diagnostics`がtools用とresources用で重複していたため
  - 要確認: MCPサーバーの再ビルドと再起動後に動作確認が必要

## 完了済み
- [x] **getCurrentSelection** - 現在選択されているテキストを取得
  - 結果: 正常動作確認済み（選択なし・選択ありの両方）
  - テスト結果:
    - 選択なし: 空のテキストが返る ✓
    - 選択あり: 選択されたテキストと正確な位置情報（ファイル名、行番号、カラム）が返る ✓

- [x] **sendNotification** - 通知を送信
  - 結果: 正常動作確認済み

## テスト方法
1. MCPサーバーを再起動
2. 各ハンドラーを順番にテスト
3. エラーが発生した場合は、ログファイルを確認: `.claude-code-emacs-mcp.log`
4. 必要に応じてEmacs側とTypeScript側のコードを修正

## 注意事項
- LSP関連のツールはLSPが有効なバッファでのみ動作
- Git関連のツールはGitリポジトリ内でのみ動作
- `:false`は使わず、`json-false`を使用すること（Emacs Lisp）

## 2025-01-04 作業記録 (更新)

### 午後の追加作業
1. **LSP関連ツールのテスト完了**
   - getDefinition: EmacsBridgeやDiffToolResultの定義を正確に取得 ✓
   - findReferences: 8箇所の参照を正確に検出 ✓
   - describeSymbol: 関数シグネチャを正しく表示 ✓

2. **Diffツールのテスト完了**
   - openDiff, openDiff3, openRevisionDiff, openCurrentChanges: 全て正常動作 ✓
   - applyPatchは実際のパッチファイルが必要なため未テスト

3. **リソースのテスト**
   - projectResource: 正常動作確認済み ✓
   - diagnosticsResource: メソッド名エラーで要修正
   - bufferResource: ResourceTemplateで実装されているため個別リストには非表示

### 午前の修正内容
1. **getOpenBuffers の修正完了**
   - 問題: `active`と`modified`が`:false`でJSONでnullになっていた
   - 修正: `claude-code-emacs-mcp-tools.el`で`json-false`に変更
   - テスト結果: 正常動作確認済み ✓

2. **openDiffContent の修正完了**
   - 問題: `structuredContent`が提供されていなかった
   - 修正1: `index.ts`で全diffツールに`structuredContent`を追加
   - 修正2: `diff-tools.ts`でエラー時もstructuredContentを返すように改善
   - 修正3: `DiffToolResponse`インターフェースに`[x: string]: unknown`を追加（型エラー解決）
   - テスト結果: 正常動作確認済み ✓

3. **getDiagnostics のテスト完了**
   - TypeScriptファイルでLSP診断機能をテスト
   - 実際のエラー（index.tsの不要な`hoge`エクスポート）を検出・修正
   - テスト結果: 正常動作確認済み ✓

### 次のステップ
1. **中優先度ハンドラーのテスト**
   - getDefinition, findReferences, describeSymbol（LSP関連）
   - 各種diffツール（openDiff, openDiff3, openRevisionDiff等）

2. **リソース系のテスト**
   - bufferResource（バッファ一覧と内容取得）
   - diagnosticsResource（LSP診断情報）

## 2025-01-03 作業記録とコンテキスト

### 昨日の修正内容
1. **TODOコメント削除**
   - `mcp-server/src/index.ts`の3行目の不要なTODOコメントを削除
   - McpServerクラスは存在せず、Serverクラスが正しい実装であることを確認

### テスト環境の準備
- MCPサーバーのプロセスを確認: `ps aux | grep -E "node.*claude-code-emacs.*mcp"`
- 必要に応じてkillして再起動
- ログファイルの確認: `.claude-code-emacs-mcp.log`

### CI/CDステータス
- 最新のpush（TODOコメント削除）はCI成功
- Emacs 28.1, 29.1, snapshotすべてでテストパス
