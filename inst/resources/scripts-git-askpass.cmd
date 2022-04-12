@echo off
setlocal EnableExtensions

set "_GIT_PROMPT=%~1"

if defined GIT_USERNAME set "GIT_USER=%GIT_USERNAME%"
if defined GIT_PASSWORD set "GIT_PASS=%GIT_PASSWORD%"

if not defined GIT_USER set "GIT_USER=nobody"
if not defined GIT_PASS set "GIT_PASS=nobody"

if /i "%_GIT_PROMPT:~0,8%"=="Username" echo %GIT_USER%
if /i "%_GIT_PROMPT:~0,8%"=="Password" echo %GIT_PASS%

