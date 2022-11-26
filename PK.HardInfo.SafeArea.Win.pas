(*
 * SafeArea Rect Utils Win
 *
 * LICENSE
 *   Copyright (c) 2022 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *)

unit PK.HardInfo.SafeArea.Win;

{$IFNDEF MSWINDOWS}
{$WARNINGS OFF 1011}
interface
implementation
end.
{$ENDIF}

interface

procedure RegisterSafeArea;

implementation

uses
  System.Types
  , FMX.Forms
  , FMX.Platform
  , FMX.Platform.Win
  , PK.HardInfo.SafeArea.Default
  ;

type
  TWinSafeArea = class(TInterfacedObject, ISafeArea)
  private var
    FPxWorkArea: TRect;
  private
    function GetPxRect: TRect;
    function GetDpRect: TRectF;
    function GetMarginRect: TRectF;
    procedure Update;
  end;

  TWinSafeAreaFactory = class(TSafeAreaFactory)
  public
    function CreateSafeArea: ISafeArea; override;
  end;

procedure RegisterSafeArea;
begin
  var Factory := TWinSafeAreaFactory.Create;
  TPlatformServices.Current.AddPlatformService(ISafeAreaFactory, Factory);
end;


{ TWinSafeAreaFactory }

function TWinSafeAreaFactory.CreateSafeArea: ISafeArea;
begin
  Result := TWinSafeArea.Create;
end;

{ TWinSafeArea }

function TWinSafeArea.GetDpRect: TRectF;
begin
  var R := FPxWorkArea;
  Result.TopLeft := PxToDp(R.TopLeft);
  Result.BottomRight := PxToDp(R.BottomRight);
end;

function TWinSafeArea.GetMarginRect: TRectF;
begin
  // Windows は OS が全部面倒を見てくれるので何もしない
  Result := TRectF.Empty;
end;

function TWinSafeArea.GetPxRect: TRect;
begin
  Result := FPxWorkArea;
end;

procedure TWinSafeArea.Update;
begin
  for var i := 0 to Screen.DisplayCount - 1 do
  begin
    var D := Screen.Displays[i];

    if D.Primary then
    begin
      FPxWorkArea := D.PhysicalWorkarea;
      Break;
    end;
  end;
end;

end.
