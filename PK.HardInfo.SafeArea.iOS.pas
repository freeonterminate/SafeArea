(*
 * SafeArea Rect Utils iOS
 *
 * LICENSE
 *   Copyright (c) 2022 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *)

unit PK.HardInfo.SafeArea.iOS;

{$IFNDEF IOS}
{$WARNINGS OFF 1011}
interface
implementation
end.
{$ENDIF}

interface

procedure RegisterSafeArea;

implementation

uses
  System.Classes
  , System.SysUtils
  , System.Types
  , FMX.Forms
  , FMX.Helpers.iOS
  , FMX.Platform
  , FMX.Platform.iOS
  , FMX.Types
  , PK.HardInfo.SafeArea.Default
  ;

type
  TiOSSafeArea = class(TInterfacedObject, ISafeArea)
  private var
    FPxWorkArea: TRectF;
    FPxRect: TRect;
  private
    function GetPxRect: TRect;
    function GetDpRect: TRectF;
    function GetMarginRect: TRectF;
    procedure Update;
  public
    constructor Create; reintroduce;
  end;

  TiOSSafeAreaFactory = class(TSafeAreaFactory)
  public
    function CreateSafeArea: ISafeArea; override;
  end;

procedure RegisterSafeArea;
begin
  TThread.ForceQueue(
    nil,
    procedure
    begin
      var Factory := TiOSSafeAreaFactory.Create;
      TPlatformServices.Current.AddPlatformService(ISafeAreaFactory, Factory);
    end
  );
end;

{ TiOSSafeAreaFactory }

function TiOSSafeAreaFactory.CreateSafeArea: ISafeArea;
begin
  Result := TiOSSafeArea.Create;
end;

{ TiOSSafeArea }

constructor TiOSSafeArea.Create;
begin
  inherited Create;
  Update;
end;

function TiOSSafeArea.GetDpRect: TRectF;
begin
  // iOS は DP と PX が一致する
  Result := GetPxRect;
end;

function TiOSSafeArea.GetMarginRect: TRectF;
begin
  // iOS は Form.Top 分すでに避けられているので、その分を引く
  Result :=
    RectF(
      FPxRect.Left,
      FPxRect.Top - Application.MainForm.Top,
      FPxWorkArea.Right - FPxRect.Right,
      FPxWorkArea.Bottom - FPxRect.Bottom
    );
end;

function TiOSSafeArea.GetPxRect: TRect;
begin
  Result := FPxRect;
end;

procedure TiOSSafeArea.Update;
begin
  FPxWorkArea := MainScreen.bounds.ToRectF;

  var R := TRectF.Empty;
  if (Application.MainForm <> nil) and TOSVersion.Check(11) then
  begin
    var Insets :=
      WindowHandleToPlatform(Application.MainForm.Handle).Wnd.safeAreaInsets;
    R := RectF(Insets.left, Insets.top, Insets.right, Insets.bottom);
  end;

  FPxRect.Left := Trunc(FPxWorkArea.Left + R.left);
  FPxRect.Top := Trunc(FPxWorkArea.Top + R.top);
  FPxRect.Right := Trunc(FPxWorkArea.Right - R.right);
  FPxRect.Bottom := Trunc(FPxWorkArea.Bottom - R.bottom);
end;

initialization
  RegisterSafeArea;

end.

