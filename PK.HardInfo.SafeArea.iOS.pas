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
  , iOSapi.UIKit
  , FMX.Forms
  , FMX.Helpers.iOS
  , FMX.Platform
  , FMX.Platform.iOS
  , FMX.Types
  , PK.HardInfo.SafeArea.Default
  ;

type
  TiOSSafeArea = class(TCustomSafeArea)
  protected
    procedure Measure(const AForm: TCommonCustomForm); override;
  end;

  TiOSSafeAreaFactory = class(TSafeAreaFactory)
  public
    function CreateSafeArea: ISafeArea; override;
  end;

procedure RegisterSafeArea;
begin
  var Factory := TiOSSafeAreaFactory.Create;
  TPlatformServices.Current.AddPlatformService(ISafeAreaFactory, Factory);
end;

{ TiOSSafeAreaFactory }

function TiOSSafeAreaFactory.CreateSafeArea: ISafeArea;
begin
  Result := TiOSSafeArea.Create;
end;

{ TiOSSafeArea }

procedure TiOSSafeArea.Measure(const AForm: TCommonCustomForm);
begin
  var FormTop := 0.0;
  var Insets := TRectF.Empty;
  var DpWorkArea := MainScreen.bounds.ToRectF;

  var Wnd: UIWindow := nil;

  var F := TSafeAreaUtils.GetActiveForm(AForm);
  if F <> nil then
    Wnd := WindowHandleToPlatform(F.Handle).Wnd;

  if
    (Wnd = nil) and
    (SharedApplication.windows <> nil) and
    (SharedApplication.windows.count > 0)
  then
    Wnd := TUIWindow.Wrap(SharedApplication.windows.objectAtIndex(0));

  if TOSVersion.Check(11) and (Wnd <> nil) then
  begin
    var OrgInsets := Wnd.safeAreaInsets;
    Insets :=
      RectF(OrgInsets.left, OrgInsets.top, OrgInsets.right, OrgInsets.bottom);

    DpWorkArea := Wnd.bounds.ToRectF;
    FormTop := DpWorkArea.Top;
  end;

  FDpRect.Left := DpWorkArea.Left + Insets.left;
  FDpRect.Top := DpWorkArea.Top + Insets.top;
  FDpRect.Right := DpWorkArea.Right - Insets.right;
  FDpRect.Bottom := DpWorkArea.Bottom - Insets.bottom;

  var S := TSafeAreaUtils.GetScale(F);
  FPxRect :=
    RectF(
      FDpRect.Left * S,
      FDpRect.Top * S,
      FDpRect.Right * S,
      FDpRect.Bottom * S
    ).Round;

  // iOS は Form.Top 分すでに避けられているので、その分を引く
  FMarginRect :=
    RectF(
      FDpRect.Left,
      FDpRect.Top - FormTop,
      DpWorkArea.Right - FDpRect.Right,
      DpWorkArea.Bottom - FDpRect.Bottom
    );
end;

initialization
  RegisterSafeArea;

end.

