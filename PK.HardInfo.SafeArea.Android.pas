(*
 * SafeArea Rect Utils Android
 *
 * LICENSE
 *   Copyright (c) 2022 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *)

unit PK.HardInfo.SafeArea.Android;

{$IFNDEF Android}
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
  , Androidapi.JNIBridge
  , Androidapi.JNI.App
  , Androidapi.JNI.GraphicsContentViewText
  , Androidapi.JNI.JavaTypes
  , Androidapi.Helpers
  , FMX.Forms
  , FMX.Helpers.Android
  , FMX.Platform
  , FMX.Platform.Android
  , FMX.Types
  , PK.HardInfo.SafeArea.Default
  ;

type
  TAndroidSafeArea = class(TCustomSafeArea)
  protected
    procedure Measure(const AForm: TCommonCustomForm); override;
  end;

  TAndroidSafeAreaFactory = class(TSafeAreaFactory)
  public
    function CreateSafeArea: ISafeArea; override;
  end;

{$REGION 'Android APIs'}
  // New SDK 30: Window Metrics
  JWindowMetricsClass = interface(JObjectClass)
    ['{039F927D-93D1-4B95-BEAB-6A8588DAF375}']
  end;

  [JavaSignature('android/view/WindowMetrics')]
  JWindowMetrics = interface(JObject)
    ['{C8921F16-C1AE-4C58-BD81-E8288D705C5C}']
    function getBounds: JRect; cdecl;
    function getWindowInsets: JWindowInsets; cdecl;
  end;
  TJWindowMetrics =
    class(TJavaGenericImport<JObjectClass, JWindowMetrics>) end;

  // New SDK 30: Window Insets Type
  JWindowInsets_TypeClass = interface(JObjectClass)
    ['{36B08B04-FF9D-4D19-94D5-8AF85DF6BAF9}']
    {class} function captionBar: Integer; cdecl;
    {class} function displayCutout: Integer; cdecl;
    {class} function ime: Integer; cdecl;
    {class} function mandatorySystemGestures: Integer; cdecl;
    {class} function navigationBars: Integer; cdecl;
    {class} function statusBars: Integer; cdecl;
    {class} function systemBars: Integer; cdecl;
    {class} function systemGestures: Integer; cdecl;
    {class} function tappableElement: Integer; cdecl;
  end;

  [JavaSignature('android/view/WindowInsets$Type')]
  JWindowInsets_Type = interface(JObject)
    ['{2A3D2158-33F1-481B-A1D4-A22BF014C177}']
  end;

  TJWindowInsets_Type =
    class(TJavaGenericImport<JWindowInsets_TypeClass, JWindowInsets_Type>) end;

  // ReDefine: DisplayCutout
  [JavaSignature('android/view/DisplayCutout')]
  JDisplayCutout = interface(JObject)
    ['{AD82BADF-58B4-40C8-ACDA-B1802CCEC1E2}']
    function getSafeInsetBottom: Integer; cdecl;
    function getSafeInsetLeft: Integer; cdecl;
    function getSafeInsetRight: Integer; cdecl;
    function getSafeInsetTop: Integer; cdecl;
  end;

  // ReDefine: Window Manager
  [JavaSignature('android/view/WindowManager')]
  JWindowManager30 = interface(JWindowManager)
    ['{5E63F8B9-E489-4002-A1F1-1DAF4705F030}']
    function getCurrentWindowMetrics: JWindowMetrics; cdecl;
  end;
  TJWindowManager30 =
    class(TJavaGenericImport<JWindowManagerClass, JWindowManager30>) end;

  // ReDefine: Insets
  [JavaSignature('android/graphics/Insets')]
  JInsets30 = interface(JObject)
    ['{4990E1CE-0EFD-4269-A218-CD719C4B77FB}']
    function _Getbottom: Integer; cdecl;
    function _Getleft: Integer; cdecl;
    function _Getright: Integer; cdecl;
    function _Gettop: Integer; cdecl;
    property bottom: Integer read _Getbottom;
    property left: Integer read _Getleft;
    property right: Integer read _Getright;
    property top: Integer read _Gettop;
  end;

  // ReDefine: Window Insets
  [JavaSignature('android/view/WindowInsets')]
  JWindowInsets30 = interface(JWindowInsets)
    ['{A5F7AF8D-3F5B-49A4-9956-800BEFA5F293}']
    function getInsetsIgnoringVisibility(typeMask: Integer): JInsets30; cdecl;
    function getDisplayCutout: JDisplayCutout; cdecl;
  end;
  TJWindowInsets30 =
    class(TJavaGenericImport<JWindowInsetsClass, JWindowInsets30>) end;
{$ENDREGION}

procedure RegisterSafeArea;
begin
  var Factory := TAndroidSafeAreaFactory.Create;
  TPlatformServices.Current.AddPlatformService(ISafeAreaFactory, Factory);
end;

{ TAndroidSafeAreaFactory }

function TAndroidSafeAreaFactory.CreateSafeArea: ISafeArea;
begin
  Result := TAndroidSafeArea.Create;
end;

{ TAndroidSafeArea }

procedure TAndroidSafeArea.Measure(const AForm: TCommonCustomForm);
begin
  // WorkArea / Scale / SysScale 取得
  var SysScale := 1.0;
  var Scale := 1.0;

  var MetricsServ: IFMXDeviceMetricsService;
  if
    TPlatformServices.Current.SupportsPlatformService(
      IFMXDeviceMetricsService,
      MetricsServ)
  then
  begin
    var M := MetricsServ.GetDisplayMetrics;

    Scale := M.ScreenScale;

    // 1 Pixel = Scale * Inch(2.54) * Virtual-DPI(96) / PPI
    SysScale := Scale * 2.54 * 96 / M.PixelsPerInch;
  end;

  // Cutout 取得
  var D := TSafeAreaUtils.GetFormDisplay(AForm);

  var PxWorkArea := D.PhysicalWorkArea;
  var PxCutoutTop := 0.0;
  var BasePxRect := TRectF.Empty;

  if TOSVersion.Check(13) then
  begin
    // Version 13 以降
    var Metrics :=
      TJWindowManager30.Wrap(
        TAndroidHelper.Activity.getWindowManager
      ).getCurrentWindowMetrics;

    var Insets :=
      TJWindowInsets30.Wrap(
        TAndroidHelper.JObjectToID(Metrics.getWindowInsets)
      ).getInsetsIgnoringVisibility(
        TJWindowInsets_Type.JavaClass.displayCutout
      );

    if Insets <> nil then
    begin
      PxCutoutTop := Insets.top;

      BasePxRect.left := PxWorkArea.left + Insets.left;
      BasePxRect.Top := PxWorkArea.Top + PxCutoutTop;
      BasePxRect.Right := PxWorkArea.Right - Insets.right;
      BasePxRect.Bottom := PxWorkArea.Bottom - Insets.bottom;
    end;
  end
  else
  begin
    if TOSVersion.Check(12) then
    begin
      // Version 12
      var Cutout :=
        TJWindowInsets30.Wrap(
          TAndroidHelper.JObjectToID(
            TAndroidHelper.Activity.getWindow.getDecorView.getRootWindowInsets
          )
        ).getDisplayCutout;

      if Cutout <> nil then
      begin
        PxCutoutTop := Cutout.getSafeInsetTop;

        BasePxRect.left := PxWorkArea.left + Cutout.getSafeInsetLeft;
        BasePxRect.Top := PxWorkArea.Top + PxCutoutTop;
        BasePxRect.Right := PxWorkArea.Right - Cutout.getSafeInsetRight;
        BasePxRect.Bottom := PxWorkArea.Bottom - Cutout.getSafeInsetBottom;
      end;
    end
    else
    begin
      // Version 12 未満
      BasePxRect := PxWorkArea;
    end;
  end;

  // Dp 化
  var DpWorkArea :=
    RectF(
      PxWorkArea.Left / Scale,
      PxWorkArea.Top / Scale,
      PxWorkArea.Right / Scale,
      PxWorkArea.Bottom / Scale
    );

  FDpRect :=
    RectF(
      BasePxRect.Left / Scale,
      BasePxRect.Top / Scale,
      BasePxRect.Right / Scale,
      BasePxRect.Bottom / Scale
    );

  FMarginRect :=
    RectF(
      FDpRect.Left,
      PxCutoutTop / Scale,
      DpWorkArea.Right - FDpRect.Right,
      DpWorkArea.Bottom - FDpRect.Bottom
    );

  // FMX.Px to System.Px
  FPxRect :=
    RectF(
      BasePxRect.Left * SysScale,
      BasePxRect.Top * SysScale,
      BasePxRect.Right * SysScale,
      BasePxRect.Bottom * SysScale
    ).Round;
end;

initialization
  RegisterSafeArea;

end.

