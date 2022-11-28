# SafeArea
全 OS の SafeArea を返すクラス

# 使い方
uses に PK.HardInfo.SafeArea を追加します。

TSafeArea はウィンドウが表示されていないと正しい値を返しません。<br>
そのため、TForm.OnCreate ではなく TForm.OnShow といったウィンドウが表示されたタイミングのイベントを利用します。<br>

TSafeArea.DpRect は SafeArea を FireMonkey の座標系で返します。<br>
TSafeArea.DpRect と Form の位置とサイズを比較しはみ出している場合は修正します。<br>

この作業の最も簡単な方法は TLayout を TForm に最上位に置き他の部品は全て TLayout の上に構築するようにします。<br>
そして TLayout の Margin を SafeArea 内になるように設定すれば SafeArea 対応になります。<br>
そのための簡単なメソッド TSafeArea.GetMarginRect が用意されています。<br>

TLayout の名前を Root とすると以下のようになります。<br>

```delphi
procedure TForm1.FormShow(Sender: TObject);
begin
  Root.Margin := TSafeArea.GetMarginRect(Self);
end;
```

# ライセンス
Copyright (c) 2022 HOSOKAWA Jun
Twitter: @pik

Released under the MIT license
http://opensource.org/licenses/mit-license.php

