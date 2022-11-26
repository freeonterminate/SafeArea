(*
 * SafeArea Rect Utils Mac
 *
 * LICENSE
 *   Copyright (c) 2022 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *)

unit PK.HardInfo.SafeArea.Mac;

{$IFNDEF OSX}
{$WARNINGS OFF 1011}
interface
implementation
end.
{$ENDIF}

interface

procedure RegisterSafeArea;

implementation

uses
  System.SysUtils
  , System.Types
  , FMX.Forms
  , FMX.Types
  , FMX.Platform
  , Macapi.AppKit
  , Macapi.ObjectiveC
  , Macapi.Helpers
  , FMX.Helpers.Mac
  , PK.HardInfo.SafeArea.Default
  ;

type
  TMacSafeArea = class(TInterfacedObject, ISafeArea)
  private var
    FDpWorkArea: TRectF;
    FPxRect: TRect;
    FScale: Single;
  private
    function GetPxRect: TRect;
    function GetDpRect: TRectF;
    function GetMarginRect: TRectF;
    procedure Update;
  public
    constructor Create; reintroduce;
  end;

  TMacSafeAreaFactory = class(TSafeAreaFactory)
  public
    function CreateSafeArea: ISafeArea; override;
  end;

  NSScreen12 = interface(NSScreen)
    function safeAreaInsets: NSEdgeInsets; cdecl;
  end;
  TNSScreen12 = class(TOCGenericImport<NSScreenClass, NSScreen12>)  end;

procedure RegisterSafeArea;
begin
  var Factory := TMacSafeAreaFactory.Create;
  TPlatformServices.Current.AddPlatformService(ISafeAreaFactory, Factory);
end;

{ TMacSafeAreaFactory }

function TMacSafeAreaFactory.CreateSafeArea: ISafeArea;
begin
  Result := TMacSafeArea.Create;
end;

{ TMacSafeArea }

constructor TMacSafeArea.Create;
begin
  inherited Create;
  Update;
end;

function TMacSafeArea.GetDpRect: TRectF;
begin
  Result :=
    RectF(
      FPxRect.Left / FScale,
      FPxRect.Top / FScale,
      FPxRect.Right / FScale,
      FPxRect.Bottom / FScale
    );
end;

function TMacSafeArea.GetMarginRect: TRectF;
begin
  var Form := Screen.ActiveForm;
  if Form = nil then
    Form := Application.MainForm;

  // macOS ではフルスクリーン時以外関係ない
  if (Form = nil) or (not Form.FullScreen) then
    Exit(TRectF.Empty);

  var DpRect := GetDpRect;

  Result :=
    RectF(
      DpRect.Left,
      DpRect.Top,
      FDpWorkArea.Right - DpRect.Right,
      FDpWorkArea.Bottom - DpRect.Bottom
    );
end;

function TMacSafeArea.GetPxRect: TRect;
begin
  Result := FPxRect;
end;

procedure TMacSafeArea.Update;
begin
  FDpWorkArea := TRectF.Empty;
  for var i := 0 to Screen.DisplayCount - 1 do
  begin
    var D := Screen.Displays[i];

    if D.Primary then
    begin
      FDpWorkArea := D.Bounds;
      Break;
    end;
  end;

  FScale := 1;
  var Service: IFMXDeviceMetricsService;
  if
    TPlatformServices.Current.SupportsPlatformService(
      IFMXDeviceMetricsService,
      Service)
  then
    FScale := Service.GetDisplayMetrics.ScreenScale;

  var R := TRectF.Empty;
  if TOSVersion.Check(12) then
  begin
    var Insets := TNSScreen12.Wrap(NSObjectToID(MainScreen)).safeAreaInsets;
    R := RectF(Insets.left, Insets.top, Insets.right, Insets.bottom);
  end;

  FPxRect :=
    Rect(
      Trunc(FDpWorkArea.Left * FScale + R.left),
      Trunc(FDpWorkArea.Top * FScale + R.top),
      Trunc(FDpWorkArea.Right * FScale + R.top),
      Trunc(FDpWorkArea.Bottom * FScale - R.bottom)
    );
end;

initialization
  RegisterSafeArea;

end.

