@echo off

setlocal EnableExtensions

if defined GIT_USERNAME set "GIT_USER=%GIT_USERNAME%"
if defined GIT_PASSWORD set "GIT_PASS=%GIT_PASSWORD%"

set "_GIT_PROMPT=%1"
if /i "%_GIT_PROMPT:~0,8%"=="Username" echo %GIT_USER%
if /i "%_GIT_PROMPT:~0,8%"=="Password" echo %GIT_PASS%

