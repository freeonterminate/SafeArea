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
  , Macapi.Foundation
  , Macapi.ObjectiveC
  , Macapi.Helpers
  , FMX.Helpers.Mac
  , PK.HardInfo.SafeArea.Default
  ;

type
  TMacSafeArea = class(TCustomSafeArea)
  protected
    procedure Update(const AForm: TCommonCustomForm); override;
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

procedure TMacSafeArea.Update(const AForm: TCommonCustomForm);
begin
  var F := TSafeAreaUtils.GetActiveForm(AForm);
  var D := TSafeAreaUtils.GetFormDisplay(F);

  var DpWorkArea := D.Bounds;

  var R := TRectF.Empty;
  if TOSVersion.Check(12) then
  begin
    var Insets := TNSScreen12.Wrap(NSObjectToID(MainScreen)).safeAreaInsets;
    R := RectF(Insets.left, Insets.top, Insets.right, Insets.bottom);
  end;

  FPxRect :=
    Rect(
      Trunc(DpWorkArea.Left * D.Scale + R.left),
      Trunc(DpWorkArea.Top * D.Scale + R.top),
      Trunc(DpWorkArea.Right * D.Scale + R.top),
      Trunc(DpWorkArea.Bottom * D.Scale - R.bottom)
    );

  FDpRect :=
    RectF(
      FPxRect.Left / D.Scale,
      FPxRect.Top / D.Scale,
      FPxRect.Right / D.Scale,
      FPxRect.Bottom / D.Scale
    );

  // macOS ではフルスクリーン時以外関係ない
  if (F = nil) or (not F.FullScreen) then
    FMarginRect := TRectF.Empty
  else
    FMarginRect :=
      RectF(
        FDpRect.Left,
        FDpRect.Top,
        DpWorkArea.Right - FDpRect.Right,
        DpWorkArea.Bottom - FDpRect.Bottom
      );
end;

initialization
  RegisterSafeArea;

end.

