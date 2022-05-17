SET REG_PATH="HKLM\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\SxS"
FOR /F "tokens=2* skip=2" %%a IN ('reg query %REG_PATH% /s') DO SET VS_PATH=%%b

SET TOOLS_PATH=%VS_PATH%VC\Tools\MSVC

mkdir %LOCALAPPDATA%\qwr
copy std\* %LOCALAPPDATA%\qwr

for /f "delims=" %%a in ('dir "%TOOLS_PATH%" /AD /B') do set VS_VERSION=%%a

SET BIN_PATH=%TOOLS_PATH%\%VS_VERSION%\bin\Hostx64\x64
SET LINKER_PATH=%BIN_PATH%\link.exe
SET LIB_PATH=%TOOLS_PATH%\%VS_VERSION%\lib\x64\

10.0.17763.0\\um\\x64" "-libpath:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.17763.0\\ucrt\\x64

SET WIN_KIT_BASE=C:\Program Files (x86)\Windows Kits\10\Lib\
for /f "delims=" %%a in ('dir "%WIN_KIT_BASE%" /AD /B') do set WIN_KIT_VERSION=%%a
SET UM_LIB="-libpath:%WIN_KIT_BASE%%WIN_KIT_VERSION%\um\x64"
SET UCRT_LIB="-libpath:%WIN_KIT_BASE%%WIN_KIT_VERSION%\ucrt\x64"

echo %LINKER_PATH%> %LOCALAPPDATA%\qwr\win_conf.txt
echo %LIB_PATH%>> %LOCALAPPDATA%\qwr\win_conf.txt
echo %UM_LIB%>> %LOCALAPPDATA%\qwr\win_conf.txt
echo %UCRT_LIB%>> %LOCALAPPDATA%\qwr\win_conf.txt