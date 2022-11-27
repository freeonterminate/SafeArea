# SafeArea
全 OS の SafeArea を返すクラス

# 使い方
uses に PK.HardInfo.SafeArea を追加

TForm.OnCreate などで TSafeArea.DpRect を確認して、はみ出ていれば修正する。
もっとも簡単な方法は TLayout を TForm に置き他の部品は全て TLayout の上に構築する。
そして TLayout の Margin を SafeArea 内になるように設定すれば SafeArea 対応になる。
そのための簡単なメソッド TSafeArea.GetMarginRect が用意されている。
TLayout の名前を Root とすると以下のようになる。

```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
  Root.Margin := TSafeArea.GetMarginRect(Self);
end;
```

# ライセンス
Copyright (c) 2022 HOSOKAWA Jun
Twitter: @pik

Released under the MIT license
http://opensource.org/licenses/mit-license.php

