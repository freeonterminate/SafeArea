(*
 * SafeArea Rect Utils
 *
 * PLATFORM
 *   Windows, macOS, iOS, Android
 *   Delphi 11.2 or later
 *
 * LICENSE
 *   Copyright (c) 2022 HOSOKAWA Jun
 *   Released under the MIT license
 *   http://opensource.org/licenses/mit-license.php
 *
 * DISCRIPTION
 *   TSafeArea.DpRect: SafeArea by Dp
 *   TSafeArea.PxRect: SafeArea by Pixel (Physical Coordinates)
 *
 * EXAMPLE
 *   // Root is a TLayout placed on top of Form and Root.Align = Contents.
 *   // Place any other control on top of the Root and
 *   // it will automatically support the SafeArea.
 *   Root.Margin.Rect := TSafeArea.MerginRect;
 *
 * HISTORY
 *   2022/11/27 Version 1.0.0
 *)

unit PK.HardInfo.SafeArea;

interface

uses
  System.SysUtils
  , System.Types
  , PK.HardInfo.SafeArea.Default
  ;

type
  TSafeArea = class
  private class var
    FSafeArea: ISafeArea;
  private
    class function GetPxRect: TRect; static;
    class function GetDpRect: TRectF; static;
    class function GetMarginRect: TRectF; static;
  public
    class constructor CreateClass; reintroduce;
    class property PxRect: TRect read GetPxRect;
    class property DpRect: TRectF read GetDpRect;
    class property MarginRect: TRectF read GetMarginRect;
    class procedure Update;
  end;

implementation

uses
  FMX.Forms
  , FMX.Platform
  {$IFDEF MSWINDOWS}
  , PK.HardInfo.SafeArea.Win
  {$ENDIF}
  {$IFDEF OSX}
  , PK.HardInfo.SafeArea.Mac
  {$ENDIF}
  {$IFDEF IOS}
  , PK.HardInfo.SafeArea.iOS
  {$ENDIF}
  {$IFDEF ANDROID}
  , PK.HardInfo.SafeArea.Android
  {$ENDIF}
  ;

{ TSafeArea }

class constructor TSafeArea.CreateClass;
begin
  var Factory: ISafeAreaFactory;
  if
    TPlatformServices.Current.SupportsPlatformService(
      ISafeAreaFactory,
      IInterface(Factory)
    )
  then
  begin
    FSafeArea := Factory.CreateSafeArea;
  end;
end;

class function TSafeArea.GetDpRect: TRectF;
begin
  if FSafeArea = nil then
    Result := Screen.WorkAreaRect
  else
    Result := FSafeArea.GetDpRect;
end;

class function TSafeArea.GetMarginRect: TRectF;
begin
  if FSafeArea = nil then
    Result := TRectF.Empty
  else
    Result := FSafeArea.GetMarginRect;
end;

class function TSafeArea.GetPxRect: TRect;
begin
  if FSafeArea = nil then
    Result := Screen.WorkAreaRect.Round
  else
    Result := FSafeArea.GetPxRect;
end;

class procedure TSafeArea.Update;
begin
  if FSafeArea <> nil then
    FSafeArea.Update;
end;

end.
