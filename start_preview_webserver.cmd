@echo off
rem Starts a temporary web server for the current directory for fast and easy preview
rem (Opens the web browser first because http.server listens for CTRL-C)

cd _book
start http://127.0.0.1:8000/
python -m http.server 8000
