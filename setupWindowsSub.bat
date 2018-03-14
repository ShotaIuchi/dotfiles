@echo off
REM emacs
mklink /D %USERPROFILE%\AppData\Roaming\.emacs.d %~dp0.emacs.d
pause
